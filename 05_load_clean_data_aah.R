###########################################################################################
###
### Load data for survival models
###
###########################################################################################

# rm(list=ls())
# 
# library(Hmisc)
# library(lubridate)
# library(xlsx)

###
### Load Data and fix column names
###

filepath <- "~/Documents/Data/Harvest/"

df_age_notest <-  read_excel(paste0(filepath,"AgingDaneIowaGrant_2014-2021.xlsx"),1)
# df_harvest_iowa <- read_excel(paste0(filepath,"IowaGrantDaneCountyHarvest.xlsx"),1)
# df_harvest_grant <- read_excel(paste0(filepath,"IowaGrantDaneCountyHarvest.xlsx"),2)
# df_harvest_dane <- read_excel(paste0(filepath,"IowaGrantDaneCountyHarvest.xlsx"),3)
df_harv <- read_excel(paste0(filepath,"HarvestDaneIowaGrant_1992-2021.xlsx"),1)

# df_dmu_harvest <- read_excel(paste0(filepath,"HarvestDMUs70A_73A_70C_70D_2002_2013.xlsx"))

#changing column names
names(df_age_notest) <- tolower(gsub("[[:punct:]]","",names(df_age_notest)))
# names(df_harvest_iowa) <- tolower(gsub("[[:punct:]]","",names(df_harvest_iowa)))
# names(df_harvest_dane) <- tolower(gsub("[[:punct:]]","",names(df_harvest_dane)))
# names(df_harvest_grant) <- tolower(gsub("[[:punct:]]","",names(df_harvest_grant)))
names(df_harv) <- tolower(gsub("[[:punct:]]","",names(df_harv)))
# names(df_dmu_harvest) <- tolower(gsub("[[:punct:]]","",names(df_dmu_harvest)))

# df_harv <- rbind(df_harvest_dane,df_harvest_iowa,df_harvest_grant)

### Double check loading correctly
head(df_age_notest)
head(df_harv)
# head(df_dmu_harvest)

###################################################################################################################################
# already loaded CWD surveillance data for calculating ages for CWD tested deer 
###################################################################################################################################


########no disease status included######################################
# df_age_cwd <- cwd_df %>% group_by(year,sex,age) %>% summarise(n=n())
# df_age_cwd$sex <- as.factor(df_age_cwd$sex)
# levels(df_age_cwd$sex) <- c("Male","Female")
########################################################################

df_age_cwdpos <- cwd_df %>%filter(teststatus==1)%>% group_by(year,sex,age) %>% summarise(n=n())
df_age_cwdneg <- cwd_df %>%filter(teststatus==0)%>% group_by(year,sex,age) %>% summarise(n=n())

df_age_cwdpos$sex <- as.factor(df_age_cwdpos$sex)
levels(df_age_cwdpos$sex) <- c("Male","Female")
df_age_cwdpos

df_age_cwdneg$sex <- as.factor(df_age_cwdneg$sex)
levels(df_age_cwdneg$sex) <- c("Male","Female")
df_age_cwdneg

###
### Aged data without CWD testing
###

df_age_nocwd <- df_age_notest %>% group_by(yr) %>% summarise(mfawn = sum(mfawn),
                                      m1 = sum(m1forked,
                                               m1sublegal,
                                               m1legalspike,
                                               m1unknown,
                                               na.rm=TRUE),
                                      m2 = sum(m2,na.rm=TRUE),
                                      m3 = sum(m3,na.rm=TRUE),
                                      m4 = sum(m45,na.rm=TRUE),
                                      m6 = sum(m68,na.rm=TRUE),
                                      m9 = sum(m911,na.rm=TRUE),
                                      ffawn = sum(ffawn),
                                      f1 = sum(f1,na.rm=TRUE),
                                      f2 = sum(f2,na.rm=TRUE),
                                      f3 = sum(f3,na.rm=TRUE),
                                      f4 = sum(f45,na.rm=TRUE),
                                      f6 = sum(f68,na.rm=TRUE),
                                      f9 = sum(f911,f12,na.rm=TRUE)) %>% pivot_longer(cols=-yr)

#####################################################
###
### Combine cwd aged and nocwd aged data frames
###
#####################################################

names(df_age_nocwd) <- c("year", "age", "n")
class(df_age_nocwd$age)
df_age_nocwd$sex <- rep(c(rep("Male",7), rep("Female", 7)), 8)
df_age_nocwd$age <- as.factor(df_age_nocwd$age)
levels(df_age_nocwd$age) <- c("1","2","3","4","6","9","0","1","2","3","4","6","9","0")
df_age_nocwd <- df_age_nocwd[,c(1,4,2,3)]

df_age_inf <- df_age_cwdpos
df_age_sus <- df_age_cwdneg



#correct for these sex/age classes without any observations, and set those to 0 within the data frame
# c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$sex=="Male" & df_age_sus$age=="9"]))]
# c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="9"]))]
# c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="6"]))]
# c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="9"]))]
# c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="6"]))]

fixn9m <-length(which(!(2002:2021 %in% df_age_sus$year[df_age_sus$sex=="Male" & df_age_sus$age=="9"])))
df_age_sus <-rbind(df_age_sus,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_sus$year[df_age_sus$sex=="Male" & df_age_sus$age=="9"]))],sex=rep("Male",fixn9m),age=rep("9",fixn9m),n=rep(0,fixn9m)))

fixp9m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="9"])))
fixp6m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="6"])))
# fixp4m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="4"])))
# fixp3m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="3"])))
# fixp2m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="2"])))
# fixp1m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="1"])))
fixp0m <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="0"])))

df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="9"]))],sex=rep("Male",fixp9m),age=rep("9",fixp9m),n=rep(0,fixp9m)))
df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="6"]))],sex=rep("Male",fixp6m),age=rep("6",fixp6m),n=rep(0,fixp6m)))
df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Male" & df_age_inf$age=="0"]))],sex=rep("Male",fixp0m),age=rep("0",fixp0m),n=rep(0,fixp0m)))


fixp9f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="9"])))
fixp6f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="6"])))
# fixp4f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="4"])))
# fixp3f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="3"])))
# fixp2f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="2"])))
# fixp1f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="1"])))
fixp0f <-length(which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="0"])))

df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="9"]))],sex=rep("Female",fixp9f),age=rep("9",fixp9f),n=rep(0,fixp9f)))
df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="6"]))],sex=rep("Female",fixp6f),age=rep("6",fixp6f),n=rep(0,fixp6f)))
df_age_inf <-rbind(df_age_inf,data.frame(year=c(2002:2021)[which(!(2002:2021 %in% df_age_inf$year[df_age_inf$sex=="Female" & df_age_inf$age=="0"]))],sex=rep("Female",fixp0f),age=rep("0",fixp6f),n=rep(0,fixp0f)))


df_age_inf <- arrange(df_age_inf,year,sex,age)
df_age_sus <- arrange(df_age_sus,year,sex,age)


for(i in 1:nrow(df_age_sus)){
    for(j in 1:nrow(df_age_nocwd)){
        if(df_age_sus$year[i] == df_age_nocwd$year[j] & 
            df_age_sus$sex[i] == df_age_nocwd$sex[j] & 
            df_age_sus$age[i] == df_age_nocwd$age[j]) {
            df_age_sus$n[i] <- df_age_sus$n[i]+df_age_nocwd$n[j]
        }
    }
}

### Number of age classes and sex classes
Age <- 7 
Sex <- 2

#structuring classification data to fit into the model
Cage_sus <- array(NA,c(length(unique(df_age_sus$year)),Sex,Age))
for(j in 1:20){
    Cage_sus[j,1,] <- df_age_sus$n[df_age_sus$year == (2001+j) & df_age_sus$sex == "Female"]
    Cage_sus[j,2,] <- df_age_sus$n[df_age_sus$year == (2001+j) & df_age_sus$sex == "Male"]
}
Cage_sus[,2,]

#structuring classification data to fit into the model
Cage_inf <- array(NA,c(length(unique(df_age_inf$year)),Sex,Age))
for(j in 1:20){
    Cage_inf[j,1,] <- df_age_inf$n[df_age_inf$year == (2001+j) & df_age_inf$sex == "Female"]
    Cage_inf[j,2,] <- df_age_inf$n[df_age_inf$year == (2001+j) & df_age_inf$sex == "Male"]
}
# Cage_inf[,1,]
# Cage_inf[,2,]

Cage <- Cage_sus + Cage_inf

#Aggregating the oldest age class for males into the next oldest age
Cage[,2,6] <- Cage[,2,6] + Cage[,2,7]
Cage[,2,7] <- 0

####################################################################################
###
### setting up aah harvest data for estimating period effects
###
#####################################################################################
class(df_age_nocwd$age)

df_age_nocwd$agemonths <- df_age_nocwd$age
df_age_nocwd$ageweeks <- df_age_nocwd$age


levels(df_age_nocwd$ageweeks) <- c(floor(as.duration(ymd("2014-05-15") %--% ymd("2015-11-30"))/dweeks(1)),#1
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2016-11-30"))/dweeks(1)),#2
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2017-11-30"))/dweeks(1)),#3
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2018-11-30"))/dweeks(1)),#4
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2020-11-30"))/dweeks(1)),#6
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2023-11-30"))/dweeks(1)),#9
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2014-11-30"))/dweeks(1)))#0

levels(df_age_nocwd$agemonths) <- c(floor(as.duration(ymd("2014-05-15") %--% ymd("2015-11-30"))/dmonths(1)),#1
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2016-11-30"))/dmonths(1)),#2
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2017-11-30"))/dmonths(1)),#3
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2018-11-30"))/dmonths(1)),#4
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2020-11-30"))/dmonths(1)),#6
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2023-11-30"))/dmonths(1)),#9
                                    floor(as.duration(ymd("2014-05-15") %--% ymd("2014-11-30"))/dmonths(1)))#0

df_age_nocwd$ageweeks <- as.numeric(as.character(df_age_nocwd$ageweeks))
df_age_nocwd$agemonths <- as.numeric(as.character(df_age_nocwd$agemonths))
# sex=0=females, sex=1=males
df_age_nocwd$sexnum <- as.factor(df_age_nocwd$sex)
levels(df_age_nocwd$sexnum) <- c(0,1)
df_age_nocwd$sexnum <- as.numeric(as.character(df_age_nocwd$sexnum))

#age2date - period effects
#calculating the birth weeks in period notation
df_age_nocwd$birthweek
df_age_nocwd$agenum <- as.numeric(df_age_nocwd$age)

df_age_nocwd$birth_date <- NA
for(j in 1:dim(df_age_nocwd)[1]) {
    df_age_nocwd$birth_date[j] <- paste0(df_age_nocwd$year[j] -
                                        df_age_nocwd$agenum[j],
                                        "-05-15")
}
df_age_nocwd$birth_date <- as.Date(df_age_nocwd$birth_date)


####################################################################################
###
### Total harvest data
###
#####################################################################################

### if not using data by gun/bow types:
# df_harv <- df_harv[df_harv$yr>2001,]
# df_harvest <- df_harv %>% group_by(yr)%>%mutate(antlered = sum(antlered),
#                                   antlerless = sum(antlerless),
#                                   unk = sum(unk),
#                                   total = sum(total))
# df_harvest <- df_harvest[1:20,]
# df_harvest$cty <- NULL
# df_harvest
# df_harvest$total_minus_unknown <- df_harvest$total - df_harvest$unk
# Ototal <- df_harvest[,2:3]


#### using data that is restricted to bow/gun types
year <-1992:2021
df_harvest <- data.frame(year)
df_harvest$antlered <- df_harvest$antlerless <- c()
df_harvest$antlered_gun <- df_harvest$antlerless_gun <- c()
df_harvest$antlered_bow <- df_harvest$antlerless_bow <- c()
df_harvest$gun_unk <- df_harvest$bow_unk <- c()

for(i in 1992:2021){
    df_harvest$antlered[i-1992+1] <- sum(df_harv$antleredgun[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antleredbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antleredcrossbow[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlerless[i-1992+1] <- sum(df_harv$antlerlessgun[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antlerlessbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antlerlesscrossbow[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlered_gun[i-1992+1] <- sum(df_harv$antleredgun[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlerless_gun[i-1992+1] <- sum(df_harv$antlerlessgun[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlered_bow[i-1992+1] <- sum(df_harv$antleredbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antleredcrossbow[df_harv$yr == i],na.rm=TRUE)
    df_harvest$antlerless_bow[i-1992+1] <- sum(df_harv$antlerlessbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$antlerlesscrossbow[df_harv$yr == i],na.rm=TRUE)
    df_harvest$gun_unk[i-1992+1] <- sum(df_harv$unknowngun[df_harv$yr == i],na.rm=TRUE) 
    df_harvest$bow_unk[i-1992+1] <- sum(df_harv$unknownbow[df_harv$yr == i],na.rm=TRUE) +
                                     sum(df_harv$unknowncrossbow[df_harv$yr == i],na.rm=TRUE)    
}
df_harvest$unk_total <- df_harvest$gun_unk+df_harvest$bow_unk

# O <- df_harvest[df_harvest$year>2001,]
O <- df_harvest
Y <- nrow(O)

Ototal <- O[,c(1,2,3,10)]
Ogun<- O[,c(1,4,5,8)]
Obow <- O[,c(1,6:7,9)]

############################################################
###
### Separating the overall total number of harvested deer
###
############################################################

#removing the total number of positive deer from the surveillance data 
#from the overall total, so that we can have a separate Ototal_inf and Ototal_sus
#an assumption here is that negative deer in the subscripts could techinically
#be positive, they just tested negative (given the uncertainty of the diagnostic tests)


Ototal_inf <- data.frame(year=2002:2021,
                         antlered = apply(Cage_inf[,2,],1,sum) + Cage_inf[,1,1],
                         antlerless = apply(Cage_inf[,1,2:7],1,sum)
                         )

Ototal_sus  <-  Ototal

nysus <- dim(Ototal_sus)[1]
nyinf <- dim(Ototal_inf)[1]

#removing infected deer from the susceptible harvest deer overall
Ototal_sus$antlered[(nysus-nyinf+1):nysus] <- Ototal_sus$antlered[(nysus-nyinf+1):nysus] - Ototal_inf$antlered
Ototal_sus$antlerless[(nysus-nyinf+1):nysus] <- Ototal_sus$antlerless[(nysus-nyinf+1):nysus]  - Ototal_inf$antlerless


####################################################################################
###
### Loading and cleaning harvest compliance rate data
###
#####################################################################################

report_df <- suppressWarnings(read_excel("~/Documents/Data/Harvest/ComplianceRate2020.xlsx",sheet=7))
report_hyp_sum <- apply(report_df[,2:3],2,mean)

beta.moments <- function(mu,sigma){
	alpha = (mu^2-mu^3-mu*sigma^2)/sigma^2
	beta = (mu-2*mu^2+mu^3-sigma^2+mu*sigma^2)/(sigma^2)
	return(list(alpha=alpha,beta=beta))
}

report_hyp_all <- unlist(beta.moments(report_hyp_sum[1],report_hyp_sum[2]))
report_hyp_y <- matrix(NA,nrow(report_df),2)

for(i in 1:nrow(report_df)){
    report_hyp_y[i,] <- unlist(beta.moments(report_df$compliance_rate[i],report_df$se[i]))
}
report_hyp_y <- data.frame(report_hyp_y)
names(report_hyp_y) <- c("alpha","beta")

####################################################################################
###
### Loading and cleaning fawn:doe ratio estimates data
###
#####################################################################################

#from raw data
# fawndoe_df <- read.csv("~/Documents/Data/fawn_doe_ratio/County_Fawn_Doe_Ratio_Data_1997_2017.csv", header=TRUE)
# county <- fawndoe_df[,1]
# type <- fawndoe_df[,2]
# fawndoe_df <- data.frame(t(fawndoe_df[,3:23]))
# names(fawndoe_df) <- paste0(county,"_",type)
# fawndoe_df$year <- 1997:2017
# fawndoe_df <- fawndoe_df[,c(10,1:9)]
# rownames(fawndoe_df) <- NULL
# write.csv(fawndoe_df,file="~/Documents/Data/fawn_doe_ratio/fawndoe_1997_2017.csv",row.names=FALSE)

fawndoe_df <- read.csv("~/Documents/Data/fawn_doe_ratio/fawndoe_1997_2017.csv",header=TRUE)

#calculating overall fawn:doe ratios across all three counties
fawndoe_df$overall_doe <- fawndoe_df$dane_num_doe + fawndoe_df$iowa_num_doe + fawndoe_df$grant_num_doe
fawndoe_df$overall_fawn <- fawndoe_df$dane_num_fawn + fawndoe_df$iowa_num_fawn + fawndoe_df$grant_num_fawn
fawndoe_df$overall_fd <- fawndoe_df$overall_fawn/fawndoe_df$overall_doe

#Restricting to the years of the study
# fawndoe_df <- fawndoe_df[fawndoe_df$year>2001 & fawndoe_df$year<2017,]
fawndoe_df <- fawndoe_df[fawndoe_df$year<2017,]

#2017-2021
df_camtrap_fd <- read.csv("~/Documents/Data/fawn_doe_ratio/Iowa_FDR_2017-2021_with_sd.csv")

fd_older_df <- read_excel("~/Documents/Data/fawn_doe_ratio/SW_FDR_1992-2015.xlsx",1)
fd_older_df  <- fd_older_df%>%filter(year>1991 & year < 1997)
fd_older_df

names(fd_older_df) <- c("spatial.unit","year","overall_fawn","overall_doe","overall_fd")
for(j in 1:5){
    fawndoe_df[nrow(fawndoe_df)+1,] <- NA
}
indx_add <- which(is.na(fawndoe_df$year))

fawndoe_df$year[indx_add] <- fd_older_df$year
fawndoe_df$overall_doe[indx_add] <- fd_older_df$overall_doe
fawndoe_df$overall_fawn[indx_add] <- fd_older_df$overall_fawn
fawndoe_df$overall_fd[indx_add] <- fd_older_df$overall_fd
fawndoe_df <- fawndoe_df[order(fawndoe_df$year),]



####################################################################################
###
### Plotting DMU total harvest data vs County total harvest data
###
#####################################################################################

### Ototal == Ogun+Obow 
### checks okay

###
### Initial values
###

#initial popsize by sex and age class, just using the first year of aged data...
#but we should do this better
# logN <- log(Cage[1,,])
#the first year has 0 for males in the oldest age class, which is -Inf
#so setting it to 0, which is equivalent to popsize of 1, in that age class

