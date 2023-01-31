###################################################################################################################################
# Load Data
###################################################################################################################################

cwd_df_1 <- suppressWarnings(read_excel("~/Documents/Data/WDNR_surveillance/1999-2021countydeer.xlsx",sheet=1))
cwd_df_2 <- suppressWarnings(read_excel("~/Documents/Data/WDNR_surveillance/1999-2021countydeer.xlsx",sheet=2))
cwd_df_3 <- suppressWarnings(read_excel("~/Documents/Data/WDNR_surveillance/1999-2021countydeer.xlsx",sheet=3))

cwd_df <- as.data.frame(rbind(cwd_df_1,cwd_df_2,cwd_df_3))
names(cwd_df) <- gsub(" ","_",names(cwd_df))
names(cwd_df) <- gsub("/","_",names(cwd_df))
names(cwd_df) <- tolower(names(cwd_df))

###################################################################################################################################
# Cleaning Data
###################################################################################################################################

###
### Restricting to counties we care about
###

counties <- c("Iowa","Dane","Grant")
cwd_df <- cwd_df[cwd_df$county %in% counties,]

###
### Cleaning up age classes
###
# "44291" = "4-5"
# "44450" = "9-11"
# "44355" = "6-8"
# "44656" = "4-5"
# "44720" =  "6-8"
# "44815" = "9-11"

cwd_df$age <- as.factor(cwd_df$age)
levels(cwd_df$age) <- c("1","12+","2","3","4-5","6-8","9-11","4-5","6-8","9-11","ADULT","0")
cwd_df <- cwd_df[cwd_df$age != "ADULT", ]
cwd_df$age <- as.factor(as.character(cwd_df$age))
cwd_df$kill_date <- as.Date(cwd_df$kill_date,origin="1899-12-30")

#removing surveillence data with no town, range, range direction, or section
cwd_df <- cwd_df[!is.na(cwd_df$dtrs),]
cwd_df <- cwd_df[!is.na(cwd_df$sect),]
cwd_df <- cwd_df[!is.na(cwd_df$town),]
cwd_df <- cwd_df[!is.na(cwd_df$range),]
cwd_df <- cwd_df[!is.na(cwd_df$range_dir),]

# cwd_df$trs <- paste0(cwd_df$town,"-",cwd_df$range,"-",cwd_df$sect)
# removing deer without a kill date
cwd_df <- cwd_df[order(cwd_df$kill_date),]
cwd_df <- cwd_df[!is.na(cwd_df$kill_date),]

source("cleanData_foi.R")

cwd_df <- cleanData(cwd_df)

# ###
# ### Restricing to the core area - should change to our study area eventually
# ###

# core.df <- sf::st_read("~/Documents/Data/Core_Area/core_sections.shp")
# names(core.df)
# # core.df = read.dbf("~/Documents/Data/Study_Area/study_sections.dbf")
# # names(core.df) = tolower(names(core.df))
# core.df$trs = paste0(core.df$twp, "-", core.df$rng, "-", core.df$sec)

# #creating unique section id's for all sections in the study area
# cwd_df$sectionid <- do.call(paste, c(cwd_df[c("range", "town", "sect")], sep = "-"))

# cwd_df <- cwd_df[cwd_df$trs %in% core.df$trs,]
# dim(cwd_df)
# cwd_df <- cwd_df[cwd_df$sectionid %in% core.df$sectionid,]
# dim(cwd_df)

#######################################
###
### Restricing to the study area
###
#######################################

study_df <- sf::st_read("~/Documents/Data/Study_Area/secrdtrs_selection.shp")

#creating sections that account for range_direction first
study_df$dsection <- paste0(study_df$dir,"-",study_df$sectionid)
cwd_df$dsection <- do.call(paste, c(cwd_df[c("range_dir","range", "town", "sect")], sep = "-"))
cwd_df <- cwd_df[cwd_df$dsection %in% study_df$dsection, ]
cwd_df$year <- lubridate::year(cwd_df$kill_date)
#setting all deer killed in January 2022 to be part of study year 2021
cwd_df$year[cwd_df$year==2022] <- 2021

######################################################
# Aggregate Oldest Age Class of Males
######################################################

male6 <- cwd_df %>% filter(age_num == 6 & sex == 0)
cwd_df$agedays[cwd_df$age_num==9 & cwd_df$sex == 0] <- max(male6$agedays)

#######################################################
### setting these to the maximum number of 
### possible days, weeks and months
### that a 6.5-8.5 year old could live until
### aging into the next age class
#######################################################

cwd_df$ageweeks[cwd_df$age_num == 9 & cwd_df$sex == 0] <- 467#max(male6$ageweeks)
cwd_df$agemonths[cwd_df$age_num == 9 & cwd_df$sex == 0] <- 107#max(male6$agemonths)
cwd_df$age_num[cwd_df$age_num == 9 & cwd_df$sex == 0] <- 6
ageclass <- as.numeric(levels(as.factor(cwd_df$age_num)))
