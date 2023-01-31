###########################################################################################
###
### Load database
###
###########################################################################################

#load Access Database into R using Hmisc package with the function mdb.get (must use this on Linux)
# database <- mdb.get('~/Documents/Data/SWDPPdeerDB.mdb')

d_cap <- mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Adult_Capture")
d_fawncap <- mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Fawn Capture")
d_mort <- mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Mortalities")
d_cens <- mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Censor")
d_cwd <- mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "RAMALT")
d_post_cwd <- mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Postmortem_CWD")
d_tooth <- mdb.get('~/Documents/Data/SWDPPdeerDB.mdb',tables= "Tooth")
d_age <- read.csv('~/Documents/Data/Age_Uncertain.csv')

#changing column names
names(d_cap) <- tolower(gsub("[[:punct:]]","",names(d_cap)))
names(d_fawncap) <- tolower(gsub("[[:punct:]]","",names(d_fawncap)))
names(d_mort) <- tolower(gsub("[[:punct:]]","",names(d_mort)))
names(d_cens) <- tolower(gsub("[[:punct:]]","",names(d_cens)))
names(d_cwd) <- tolower(gsub('[[:punct:]]',"",names(d_cwd)))
names(d_post_cwd) <- tolower(gsub('[[:punct:]]',"",names(d_post_cwd)))
names(d_tooth) <- tolower(gsub('[[:punct:]]',"",names(d_tooth)))
names(d_age) <- tolower(names(d_age))

### Double check loading correctly
# head(d_cap)
# head(d_fawncap)
# head(d_mort)
# head(d_cens)
# head(d_cwd)
# head(d_post_cwd)

#########################################################
###
### Removing all deer that were not GPS collared
### There were 47 of these, while they may be tagged
### we should not include these in the analysis
###
#########################################################

low_rm <- d_cap$lowtag[d_cap$collar == "No"]
d_cap <- d_cap[d_cap$collar != "No",]

sum(d_mort$lowtag %in% low_rm)
d_mort <- d_mort[!(d_mort$lowtag %in% low_rm),]

sum(d_cens$lowtag %in% low_rm)
d_cens <- d_cens[!(d_cens$lowtag %in% low_rm),]

#43/47 of these deer were antemortem tested....
sum(d_cwd$lowtag %in% low_rm)
d_cwd <- d_cwd[!(d_cwd$lowtag %in% low_rm),]

#14 were postmortem tested
sum(d_post_cwd$lowtag %in% low_rm)
d_post_cwd <- d_post_cwd[!(d_post_cwd$lowtag %in% low_rm),]

#all of these were in the tooth sheet
sum(d_tooth$lowtag %in% low_rm)
d_tooth <- d_tooth[!(d_tooth$lowtag %in% low_rm),]

#one of these had an age undetermined
sum(d_age$lowtag %in% low_rm)
d_age <- d_age[!(d_age$lowtag %in% low_rm), ]

#########################################################
###
### Number individuals within each dataframe
###
#########################################################

n_cap <- dim(d_cap)[1]
n_cens <- dim(d_cens)[1]
n_fawncap <- dim(d_fawncap)[1]
n_mort <- dim(d_mort)[1]
n_cwdtest <- dim(d_cwd)[1]
n_postcwd <- dim(d_post_cwd)[1]
n_tooth <- dim(d_tooth)[1]

#change class of CWD tests to integer for d_cwd
class(d_cwd$lowtag) <- "integer"

#########################################################
###
### Add Antemortem CWD status to capture dataframe
###
#########################################################

d_cap$cwdstatus1 <- rep(NA,n_cap)#2016-2017
d_cap$cwdstatus2 <- rep(NA,n_cap)#2017-2018
d_cap$cwdstatus3 <- rep(NA,n_cap)#2018-2019
d_cap$cwdstatus4 <- rep(NA,n_cap)#2019-2020

for(i in 1:n_cap){
  for(j in 1:n_cwdtest){
    if(d_cap$lowtag[i]==d_cwd$lowtag[j]){
      d_cap$cwdstatus1[i] <- as.character(d_cwd$resultseason1[j])
      d_cap$cwdstatus2[i] <- as.character(d_cwd$resultseason2[j])
      d_cap$cwdstatus3[i] <- as.character(d_cwd$resultseason3[j])
      d_cap$cwdstatus4[i] <- as.character(d_cwd$resultseason4[j])
    }
  }
}
d_cap$cwdstatus1 <- as.factor(d_cap$cwdstatus1)
d_cap$cwdstatus2 <- as.factor(d_cap$cwdstatus2)
d_cap$cwdstatus3 <- as.factor(d_cap$cwdstatus3)
d_cap$cwdstatus4 <- as.factor(d_cap$cwdstatus4)


#using levels of factor to update the status based on the following:
#if missing, ISF,Negative, or not tested, set to Negative otherwise they are Positive
#""           "ISF"        "Negative"   "Not tested" "Positive"  

### converting cwd status to a 0/1 variable, all negative
levels(d_cap$cwdstatus1) <- c("Negative","Negative","Negative","Negative","Positive")
levels(d_cap$cwdstatus2) <- c("Negative","Negative","Negative","Negative","Positive")
levels(d_cap$cwdstatus3) <- c("Negative","Negative","Negative","Negative","Positive")
levels(d_cap$cwdstatus4) <- c("Negative","Negative","Negative","Negative","Positive")

### if CWD status at capture is unknown, that deer is considered Negative
d_cap$cwdstatus1[which(is.na(d_cap$cwdstatus1))] <- "Negative"
d_cap$cwdstatus2[which(is.na(d_cap$cwdstatus2))] <- "Negative"
d_cap$cwdstatus3[which(is.na(d_cap$cwdstatus3))] <- "Negative"
d_cap$cwdstatus4[which(is.na(d_cap$cwdstatus4))] <- "Negative"

#converting this to 0/1, where 1 ==infected
d_cap$cwdstatus1 <- as.numeric(d_cap$cwdstatus1)-1
d_cap$cwdstatus2 <- as.numeric(d_cap$cwdstatus2)-1
d_cap$cwdstatus3 <- as.numeric(d_cap$cwdstatus3)-1
d_cap$cwdstatus4 <- as.numeric(d_cap$cwdstatus4)-1


#calculating study year for the capture
d_cap$year <- year(d_cap$date)

#deer captured in December are lumped into the study year based on the subsequent calendar year
d_cap$year[month(d_cap$date) == 12 & d_cap$year == 2019] <- 2020
d_cap$year[month(d_cap$date) == 12 & d_cap$year == 2018] <- 2019
d_cap$year[month(d_cap$date) == 12 & d_cap$year == 2017] <- 2018

#start of the study (in weeks)
start_week <- week(ymd('2017-01-09'))

#year of capture
d_cap$year <- NA
d_cap$year[d_cap$date < "2017-12-01"] <- 2017
d_cap$year[d_cap$date>"2017-12-01" & d_cap$date<"2018-12-01"] <- 2018
d_cap$year[d_cap$date>"2018-12-01" & d_cap$date<"2019-12-01"] <- 2019
d_cap$year[d_cap$date>"2019-12-01" & d_cap$date<"2020-12-01"] <- 2020

#######################################################################
###
### Recaptured deer, got collars replaced etc
###
########################################################################

#seperating recaptures from the capture database
d_recap <- d_cap[d_cap$capturestatus == "Recap", ]
low_recap <-  d_recap$lowtag

#######################################################################
###
### Fawns
###
########################################################################

# fawn capture year
d_fawncap$year <- year(d_fawncap$date)

##########################################################################
###
### Cleaning the sex factor variable in the d_fawncap data
###
##########################################################################

#there are 2 fawns w/o sex info. These sexes will be randomly drawn an assigned.
d_fawncap$sex[d_fawncap$sex == ""] <- sample(c("Female","Male"),
                                             size = 2,
                                             prob=rep(.5,2))
d_fawncap$sex <- as.factor(as.character(d_fawncap$sex))

##########################################################################
###
### Age at capture
### Formatting and cleaning d_tooth dataframe
### d_tooth are the results of Cementum Annuli
###
###
#########################################################################
# plot(d_tooth$age)
d_tooth[is.na(d_tooth$age), ]$age <- "NOAGE"
d_tooth$age[d_tooth$age == "0-1"] <- "1.5"
d_tooth$age[d_tooth$age == "1-2"] <- "1.5"
d_tooth$age[d_tooth$age == "2-3"] <- "2.5"
# d_tooth$age[d_tooth$lowtag == 5621] <- "5.5"
# d_tooth$age[d_tooth$lowtag == 5050] <- "1.5"#ageclass 20months

d_tooth$age <- as.factor(d_tooth$age)
levels(d_tooth$age)
levels(d_tooth$age) <- c("",
                          "1.5",
                          "1.5",
                          "10.5",
                          "10.5",
                          "11.5",
                          "12.5",
                          "13.5",
                          "14.5",
                          "15.5",
                          "17.5",
                          "2.5",
                          "2.5",
                          "3.5",
                          "3.5",
                          "4.5",
                          "4.5",
                          "5.5",
                          "6.5",
                          "6.5",
                          "7.5",
                          "7.5",
                          "8.5",
                          "9.5",
                          "9.5",
                          "NOAGE")

d_tooth$age <- as.character(d_tooth$age)
# table(d_tooth$age)

#all deer that did not have a tooth pulled are either fawns or yearlings
#they do not have cementum annuli age results
no_ca_young <- d_tooth$lowtag[d_tooth$toothpulled == 0 & d_tooth$age==""] 

#there are 11 individuals that had teeth pulled but no age 
#was determined_ in 2 cases, the tooth broke so bad sample
#what should we do about these? impute? how?
no_ca <- unique(c(d_tooth$lowtag[d_tooth$age == "" & d_tooth$toothpulled == 1],
          d_tooth$lowtag[d_tooth$age == "NOAGE" & d_tooth$toothpulled == 1]))

###
### Adding the age to the capture dataframe 
###

n_tooth <- dim(d_tooth)[1]
d_cap$ageatcap <- NA
for(j in 1:n_tooth){
    d_cap$ageatcap[d_cap$lowtag %in% d_tooth$lowtag[j]] <- d_tooth$age[j]
}

# Setting age for fawns captured at 8 months
d_cap$ageatcap[d_cap$ageclass == "8mo"] <- ".5"

#including age data from age determined from necropsy
d_age <- d_age[!is.na(d_age$age_at_capture),]
for(i in 1:nrow(d_age)){
  d_cap$ageatcap[d_cap$lowtag %in% d_age$lowtag[i]] <- d_age$age_at_capture[i]
}

# Setting age for deer without teeth pulled
# that were yearlings at capture
d_cap$ageatcap[d_cap$ageclass == "20mo" & d_cap$ageatcap == ""] <- "1.5"
# d_cap$ageatcap[d_cap$ageclass == "20mo" & d_cap$ageatcap == "NOAGE"] <- " 1.5"

#there's still 4 that have ages that are NA, of those, 3 are 20months old, and 
#a single >2yr old that was a capture mortality, so will be removed anyway.
#assigning all to be 20months

d_cap$observationsnotes[is.na(d_cap$ageatcap)]
d_cap$ageclass[is.na(d_cap$ageatcap)]
d_cap$lowtag[is.na(d_cap$ageatcap)]

d_cap$ageatcap[is.na(d_cap$ageatcap)] <- "1.5"
d_cap$ageatcap[d_cap$lowtag==5971] <- "2.5"

#################################################################################### 
###
### Ages special cases
### back aged after mortality
###
####################################################################################

# no_cementum annuli, but we know information about tooth wear from necropsy
# d_cap$ageatcap[d_cap$lowtag == 6616] <- "7.5" #8 years old at death on 2021-04-10
# d_cap$ageatcap[d_cap$lowtag == 6338]  <- "3.5" # 4 years old at death, 1 yr after capture
# d_cap$ageatcap[d_cap$lowtag == 7912]  <- "2.5" #still alive collared 2020.  Age 2 at capture, birth year 2017
# d_cap$ageatcap[d_cap$lowtag == 8125]  <- "2.5"  # cap yr = 20 mort year = 21   3 yr old (from CWD test) 3 years old at death
# d_cap$ageatcap[d_cap$lowtag == 7981]  <- ".5"   # cap yr =2019 mort year =2022   3 yr old (from CWD test)  3 years old at death
# d_cap$ageatcap[d_cap$lowtag == 5050]  <- "3.5"  # cap yr 17, mort year 17,      aged as 3 yr old from CWD  3 years old at death
# d_cap$ageatcap[d_cap$lowtag == 5621]  <- "2.5"  # cap yr 18, mort year 20 , called >4 in necropsy.  4 at death
# d_cap$ageatcap[d_cap$lowtag == 6019]  <- "2.5"  #still alive.  Collared 2018 age 2 at capture, birth year of 2015.  
# d_cap$ageatcap[d_cap$lowtag == 7202]  <- "4.5"                  #collared 2017 died 2017. Aged 4 at cwd sampling.  4 years of age at death
# d_cap$ageatcap[d_cap$lowtag == 5621] <- "5.5"

# d_cap$ageatcap[d_cap$lowtag == 6734]                    #no extra info.  Collared 2018   >2 years at capture birth year 2015. 
# d_cap$ageatcap[d_cap$lowtag == 6827]                   #no extra info. Collared 2018   >2 years at capture birth year 2015. 


#there's 14 individuals with unknown ages at capture
lowtag_no_tooth <- d_cap$lowtag[d_cap$ageatcap == "" | d_cap$ageatcap == "NOAGE"]


check_mort_agecap <- d_mort$lowtag[d_mort$lowtag %in% lowtag_no_tooth]
check_cens_agecap <- d_cens$lowtag[d_cens$lowtag %in% lowtag_no_tooth]

d_cens[d_cens$lowtag %in% check_cens_agecap,]
d_cap[d_cap$lowtag %in% check_mort_agecap,c(4,5,27,42,116)]


#using the observed age distribution from the data to draw the ages of deer that are uncertain
freq_age_adult_female <- table(d_cap$ageatcap[!(d_cap$ageatcap==".5"| d_cap$ageatcap=="1.5" | d_cap$ageatcap == "NOAGE" | d_cap$ageatcap == "" | d_cap$sex=="Male")])[
      order(as.numeric(names(table(d_cap$ageatcap[!(d_cap$ageatcap==".5"| d_cap$ageatcap=="1.5" | d_cap$ageatcap == "NOAGE" | d_cap$ageatcap == ""| d_cap$sex=="Male")]))))
      ]
freq_age_adult_male <- table(d_cap$ageatcap[!(d_cap$ageatcap==".5"| d_cap$ageatcap=="1.5" | d_cap$ageatcap == "NOAGE" | d_cap$ageatcap == "" | d_cap$sex=="Female")])[
      order(as.numeric(names(table(d_cap$ageatcap[!(d_cap$ageatcap==".5"| d_cap$ageatcap=="1.5" | d_cap$ageatcap == "NOAGE" | d_cap$ageatcap == ""| d_cap$sex=="Female")]))))
      ]

prob_age_female <- freq_age_adult_female/sum(freq_age_adult_female)
prob_age_male <- freq_age_adult_male/sum(freq_age_adult_male)

#Randomly drawing the ages for the 41 individuals for which we don't have any age info except that their ages are >2yrs
#must do this based on the probability distributions of the different ages within sex.

# png("figures/freq_age_adult.png",height = 700, width=900)
# par(mfrow=c(2,2))
# plot(freq_age_adult_female,main="Female age distribution adults")
# plot(freq_age_adult_male,main="Male age distribution adults")
# plot(prob_age_female,main="Female age proportions adults")
# plot(prob_age_male,main="Male age proportions adults")
# dev.off()

#impute the missing ages
indx_impute <- which(d_cap$ageatcap == "" | d_cap$ageatcap=="NOAGE")
indx_impute_male <-indx_impute[which(d_cap$sex[indx_impute]=="Male")]
indx_impute_female <-indx_impute[which(d_cap$sex[indx_impute]=="Female")]

d_cap$ageatcap[indx_impute_female] <- nimble::rcat(length(indx_impute_female),prob=prob_age_female)+1.5
d_cap$ageatcap[indx_impute_male] <- nimble::rcat(length(indx_impute_male),prob=prob_age_male)+1.5

#converting age at capture to numeric
d_cap$ageatcap <- as.numeric(d_cap$ageatcap)

#calculating age at capture in months
# for(j in 2005:2017){cat(week(paste(j,"-05-21",sep="")),"\n")}
#21 week of year
pulse_week <- week("2018-05-25")
d_cap$ageweek <- NA
for (i in 1:n_cap){
  
  #if d_cap$date < May 25
  if(week(ymd(d_cap$date[i]))<pulse_week){
    d_cap$ageweek[i] <- week(ymd(d_cap$date[i])) + (52 - pulse_week) + floor(d_cap$ageatcap[i]) * 52
  }
  #if d_cap$date > May 25
  else{
    d_cap$ageweek[i] <- week(ymd(d_cap$date[i])) - pulse_week + floor(d_cap$ageatcap[i]) * 52
  } 
}

###
### based on june 1st flip into next age
###

pulse_month <- 6
d_cap$agemonth <- c()

for (i in 1:n_cap){
  
  #if d_cap$date < June 1
  if(month(ymd(d_cap$date[i]))< 6 ){
    d_cap$agemonth[i] <- month(ymd(d_cap$date[i])) + (12 - 6) + floor(d_cap$ageatcap[i]) * 12
  }
  #if d_cap$date > June 1
  else{
    d_cap$agemonth[i] <- month(ymd(d_cap$date[i])) - 6 + floor(d_cap$ageatcap[i]) * 12
  } 
}

##########################################################################
###
### making a single data frame for all deer
### with relevant data for all analysis
###
#########################################################################

df_use_cap <- d_cap[, c(4:5,10:11,27,29,30,31,43,44,51,59,77,85,103,111:118)]

#adding columns to be consistent with fawns
df_use_cap$weightkg <- df_use_cap$weightlbs/2.2046
df_use_cap$weightlbs <- NULL

df_use_cap <- df_use_cap[,c(1:6,23,7:22)]

df_use_cap$hoofgrowthmm <- df_use_cap$umbilicus <- NA
df_use_cap$hoofconsistency <- df_use_cap$hoofcolor <- df_use_cap$hoofgelat <- NA

names(df_use_cap)[2:ncol(df_use_cap)] <- paste0(names(df_use_cap)[2:ncol(df_use_cap)],"_cap")

names(d_fawncap)[13] <- "hindlegcm"
d_fawncap$ageclass <- "Fawn"
d_fawncap$cwdstatus1 <- d_fawncap$cwdstatus2 <- 0
d_fawncap$cwdstatus3 <- d_fawncap$cwdstatus4 <- 0
d_fawncap$girthcm <- d_fawncap$bodycondition <- d_fawncap$bodyconditionscore110 <- NA
d_fawncap$recapdate <- d_fawncap$recapage <- d_fawncap$recap2date <- d_fawncap$recap2age <- NA
d_fawncap$ageatcap <- 0
d_fawncap$ageweek <- d_fawncap$agemonth <- 1
df_use_fawncap <- d_fawncap[,c(2,3,8,9,32,10,
                               11,13,39,38,37,
                               43:40,34,33,36,35,
                               31,44,45,46,
                               14,12,22,21,20)]

names(df_use_fawncap)[2:ncol(df_use_fawncap)] <- paste0(names(df_use_fawncap)[2:ncol(df_use_fawncap)],"_cap")

#########################################################################################################################3
###
### Combining adult and fawn data frames
###
#########################################################################################################################3

df_cap <- rbind(df_use_cap,df_use_fawncap)
df_cap <- df_cap[order(df_cap$lowtag),]
df_cap$ageweek_recap <- df_cap$agemonth_recap <- NA
low_af_neg <- df_cap$lowtag[duplicated(df_cap$lowtag)]

#Given fawns that are recaptured, which individuals then test positive at recapture
#must draw their infection status between birth and recapture date
dup <- df_cap[df_cap$lowtag%in% low_af_neg,]
low_af_pos_recap <- unique(c(dup$lowtag[dup$cwdstatus3_cap>0],dup$lowtag[dup$cwdstatus4_cap>0]))
low_af_neg <- low_af_neg[!(low_af_neg %in% low_af_pos_recap)]

#For the fawns that were recaptured, we include the recapture date in the recap and recap2 columns

for(i in 1:length(low_af_neg)){
  temp<-df_cap[df_cap$lowtag == low_af_neg[i],]
  temp$recapdate_cap[2] <- temp$date_cap[1]
  temp$recapage_cap[2] <- temp$ageclass_cap[1]
  temp$agemonth_recap[2] <- temp$agemonth_cap[1]
  temp$ageweek_recap[2] <- temp$ageweek_cap[1]
  df_cap[df_cap$lowtag == low_af_neg[i],] <- temp
}

#for these recaptured fawns that were positive, we have to include that info in the cwdstatus col
for(i in 1:length(low_af_pos_recap)){
  temp<-df_cap[df_cap$lowtag == low_af_pos_recap[i],]
  temp$recapdate_cap[2] <- temp$date_cap[1]
  temp$recapage_cap[2] <- temp$ageclass_cap[1]
  temp$agemonth_recap[2] <- temp$agemonth_cap[1]
  temp$ageweek_recap[2] <- temp$ageweek_cap[1]
  temp$cwdstatus3_cap[2] <- temp$cwdstatus3_cap[1]
  temp$cwdstatus4_cap[2] <- temp$cwdstatus4_cap[1]
  df_cap[df_cap$lowtag == low_af_pos_recap[i],] <- temp
}

#removing duplicates because necessary info should be in the fawn column
low_af_dub <-c(low_af_neg,low_af_pos_recap)
rm_recap <- which(df_cap$lowtag %in% low_af_dub)[seq(1,length(low_af_dub)*2,by=2)]
df_cap <- df_cap[-rm_recap, ]

#######################################################################
###
### setting CWD at capture
###
#######################################################################

df_cap$cwd_cap <- c()
df_cap$cwd_cap[df_cap$year_cap==2017] <- df_cap$cwdstatus1_cap[df_cap$year_cap==2017]
df_cap$cwd_cap[df_cap$year_cap==2018] <- df_cap$cwdstatus2_cap[df_cap$year_cap==2018]
df_cap$cwd_cap[df_cap$year_cap==2019] <- df_cap$cwdstatus3_cap[df_cap$year_cap==2019]
df_cap$cwd_cap[df_cap$year_cap==2020] <- df_cap$cwdstatus4_cap[df_cap$year_cap==2020]

df_cap$recap_cwd <- rep(NA,nrow(df_cap))

#2017 recaptured deer that were initially captured in 2017
colindx_recap1 <- year(df_cap$recapdate_cap[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2017)])-2017
temp <- df_cap[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2017),17:19]
yr1recap_cwd <- c()
for(i in 1:nrow(temp)){
  yr1recap_cwd[i]<-temp[i,colindx_recap1[i]]
}
df_cap$recap_cwd[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2017)] <- yr1recap_cwd

#2018 recaptured deer that were initially captured in 2018
colindx_recap2 <- year(df_cap$recapdate_cap[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2018)])-2017
temp <- df_cap[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2018),17:19]
yr2recap_cwd <- c()
for(i in 1:nrow(temp)){
  yr2recap_cwd[i]<-temp[i,colindx_recap2[i]]
}
which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2018)[yr2recap_cwd==1]
df_cap$recap_cwd[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2018)] <- yr2recap_cwd

#2019 recaptured deer that were initially captured in 2019
colindx_recap3 <- year(df_cap$recapdate_cap[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2019)])-2017
temp <- df_cap[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2019),17:19]
yr3recap_cwd <- c()
for(i in 1:nrow(temp)){
  yr3recap_cwd[i]<-temp[i,colindx_recap3[i]]
}
yr3recap_cwd
df_cap$recap_cwd[which(!is.na(df_cap$recapdate_cap)&df_cap$year_cap==2019)] <- yr3recap_cwd
df_cap$recap_cwd_date <- df_cap$recap

#there was one deer that was recaptured twice and was CWD+,
#it tested CWD+ upon first recapture, and not retested upon
#second recapture Should use the first recapture for dn

# df_cap[which(!is.na(df_cap$recap2date)),16:19]
# df_cap[which(!is.na(df_cap$recap2date))[5],]

##########################################################
###
### cleaning mortality causes
### removing capture related mortalities
###
##########################################################

d_mort$cause1  <- tolower(d_mort$cause1)
d_mort$cause2  <- tolower(d_mort$cause2)
d_mort$cause3  <- tolower(d_mort$cause3)
d_mort$weapon  <- tolower(d_mort$weapon)
# hist(d_mort$r)
# hist(d_mort$s)

cbind(table(d_mort$cause1))
cbind(table(d_mort$cause2))
cbind(table(d_mort$cause3))

#removing capture related morts
rm_caprelated_morts <- d_mort$lowtag[d_mort$cause1 == "capture related"]

#there were 33 deer removed from the study for being capture related mortalities
length(rm_caprelated_morts)

df_cap <- df_cap[!(df_cap$lowtag %in% rm_caprelated_morts),]
d_mort <- d_mort[!(d_mort$lowtag %in% rm_caprelated_morts),]
d_cens <- d_cens[!(d_cens$lowtag %in% rm_caprelated_morts),]

# d_mort[d_mort$cause2=="capture related",c(2,31:37)]
# d_mort[d_mort$cause3=="capture related",c(2,31:37)]
# cap_mort <- c(d_mort$lowtag[d_mort$cause2 == "capture related"],d_mort$lowtag[d_mort$cause3 == "capture related"])
# d_cwd[d_cwd$lowtag %in% cap_mort,]
# d_post_cwd[d_post_cwd$lowtag %in% cap_mort,]

# Just removing these capture related morts for now, but this should
# be changed after Dan Storm deals with it
# df_cap <- df_cap[!(df_cap$lowtag %in% cap_mort),]
# d_mort <- d_mort[!(d_mort$lowtag %in% cap_mort),]
# d_cens <- d_cens[!(d_cens$lowtag %in% cap_mort),]

##########################################################################
###
### Conflicts for censor versus mortalities 
###
#########################################################################

d_mort <- d_mort[order(d_mort$lowtag),]
d_cens <- d_cens[order(d_cens$lowtag),]

d_mort$estmortdate <- as.Date(d_mort$estmortdate, format = "%Y-%m-%d")
d_mort$mortalertdate <- as.Date(d_mort$mortalertdate, format = "%Y-%m-%d")
d_mort$collarfound <- as.Date(d_mort$collarfound, format = "%Y-%m-%d")
d_cens$censordate <- as.Date(d_cens$censordate , format = "%Y-%m-%d")

#which should these be censored and which should be morts?
# d_mort[d_mort$lowtag %in% d_cens$lowtag,]
low_cens_inmort <- d_cens$lowtag[d_cens$lowtag %in% d_mort$lowtag ]

mortdate <- list()
for(i in 1:nrow(d_mort)){
  mortdate[[i]] <- c(d_mort$estmortdate[i],
      d_mort$mortalertdate[i],
      d_mort$collarfound[i])}
temp<- lapply(mortdate, min, na.rm=TRUE)
d_mort$mortdate <- Reduce(c,temp)

#checking that the deer that are in both the censor and mortality 
# sheets have the censor date before the mortality date
check <- low_cens_inmort[!(d_mort$mortdate[d_mort$lowtag %in% low_cens_inmort] > d_cens$censordate[d_cens$lowtag%in% low_cens_inmort])]

#none of these were censored twice
#all of these have censor date and mort date to be the same
cbind(d_mort[d_mort$lowtag %in% check,c(2,55)],d_cens[d_cens$lowtag %in% check,10])
# d_cens[d_cens$lowtag %in% check,]
# d_mort[d_mort$lowtag %in% check,c(14:16)]
# d_post_cwd[d_post_cwd$lowtag %in% check,]
d_mort$mortdate[d_mort$lowtag %in% check] <- d_mort$estmortdate[d_mort$lowtag %in% check]
check2 <- low_cens_inmort[!(d_mort$mortdate[d_mort$lowtag %in% low_cens_inmort] > d_cens$censordate[d_cens$lowtag%in% low_cens_inmort])]

#6402 should stay as a censor, 6872 should also stay as a censor
d_mort <- d_mort[d_mort$lowtag != check2, ]
low_cens_inmort <- d_cens$lowtag[d_cens$lowtag %in% d_mort$lowtag]

#If they are censored, then they will be removed from the mortality data
# d_mort  <- d_mort[!(d_mort$lowtag %in% low_mort_incens),]
# d_cens$cencomments[d_cens$lowtag %in% low_cens_inmort]

#of all the deer that were censored and in the mort data, 
#there was only 1 that was cwd+ at capture
low_cens_inmort[which(df_cap$cwd_cap[df_cap$lowtag %in% low_cens_inmort]==1)]

#there were 2 that were recaptured and test cwd+
low_cens_inmort[which(df_cap$recap_cwd[df_cap$lowtag %in% low_cens_inmort]==1)]

keep_mort <- c(low_cens_inmort[which(df_cap$cwd_cap[df_cap$lowtag %in% low_cens_inmort]==1)],
              low_cens_inmort[which(df_cap$recap_cwd[df_cap$lowtag %in% low_cens_inmort] == 1)]
              )

d_cens <- d_cens[!(d_cens$lowtag %in% keep_mort),]
low_cens_inmort <- d_cens$lowtag[d_cens$lowtag %in% d_mort$lowtag ]

cwd_cens_mort <- d_post_cwd[d_post_cwd$lowtag %in% low_cens_inmort, ]
keep_mort_post_pos <- cwd_cens_mort$lowtag[cwd_cens_mort$cwdresult == "Positive"]

d_cens <- d_cens[!(d_cens$lowtag %in% keep_mort_post_pos),]
low_cens_inmort <- d_cens$lowtag[d_cens$lowtag %in% d_mort$lowtag]


##########################################################################
###
### Conflicts for multiple censored deer
### There are 3 deer where they were censored twice... 
### these were collared more than once, use the censor2date
###
#########################################################################

# for these 3 use the last censor date
d_cens[!is.na(d_cens$censor2lefteartag),c(1,10,17,32,39)]
low_double_cens <- d_cens$lowtag[!is.na(d_cens$censor2lefteartag)]

#setting the vector of censordate_use which is the censordate to use in the model
#for those that were recaptured after initial censoring, and then recollared, use the final censor date
d_cens$censordate_use <- d_cens$censordate
d_cens$censordate_use[d_cens$lowtag %in% low_double_cens] <- d_cens$censor2censordate[d_cens$lowtag %in% low_double_cens]

##########################################################################
###
### Overall survival in this version defaults to censored even
### if the mortality is learned of later 
### except, I kept all cwd+ (antemortem or postmortem) as mortalities
###
#########################################################################

#removing these censors from censor data because assuming these are 
#morts
d_mort <- d_mort[!(d_mort$lowtag %in% low_cens_inmort),]
n_mort <- nrow(d_mort)

##########################################################
###  
### setting mortality causes 
### 
##########################################################

d_mort$cause1 <- as.factor(d_mort$cause1)
# levels(d_mort$cause1)
# d_mort[d_mort$cause1=="accident",37]# anthro?
# d_mort[d_mort$cause1=="eutenasia",37]# censored? mort? disease?
# d_mort[d_mort$cause1=="euthanasia",37]# censored? mort? disease? capture related?

table(d_mort$cause1)
levels(d_mort$cause1) <- c("anthro",# accident
                           "predation", #bobcat
                           "predation", #coyote
                           "disease",#cranaial abscess?
                           "disease",#disease
                           "anthro",#domestic dog
                           "disease",#ehd
                           "disease", #enterocolitis
                           "anthro",#eutenasia
                           "anthro",#euthanasia
                           "disease",#"gastroenteritis"  
                           "anthro",#"haybine"
                           "hunt",#hunter harvest"
                           "disease",#"infection"        
                           "anthro",# injury
                           "pneumonia",# pneumonia
                           "anthro",#poaching         
                           "predation", #predation
                           "starvation",#starvation
                           "anthro",# "vehicle collision"
                           "wound")#"wounding loss"

d_mort$cause2 <- as.factor(d_mort$cause2)
table(d_mort$cause2)
levels(d_mort$cause2) <- c("", #                    553
                      "predation",# bobcat              18
                      "predation",#coyote
                      "disease",# disease             40
                      "disease",# ehd                  1
                      "disease",# enterocolitis        2
                      "anthro",# haybine              1
                      "disease",# infection            1
                      "anthro",# injury               2
                      "pneumonia",# pneumonia           21
                      "predation",# predation           15
                      "starvation",# starvation          28
                      "anthro",# vehicle collision    6
                      "wound")# wounding loss        7

d_mort$cause3 <- as.factor(d_mort$cause3)
levels(d_mort$cause3)
levels(d_mort$cause3) <- c("",#                   671
                      "predation",#bobcat               1
                      "predation",#coyote               2
                      "disease",#disease              5
                      "anthro",#injury               1
                      "pneumonia",#pneumonia            2
                      "anthro",#poaching             1
                      "predation",#predation            4
                      "starvation",#starvation          18
                      "anthro",#vehicle collision    5
                      "wound")#wounding loss        5

table(d_mort$cause1)
table(d_mort$cause2)
table(d_mort$cause3)

levels(d_mort$cause1)
levels(d_mort$cause2)
levels(d_mort$cause3)

#setting NAs in cause weights to 0, so that they sum to 1
d_mort$cause1wt[is.na(d_mort$cause1wt)] <- 0
d_mort$cause2wt[is.na(d_mort$cause2wt)] <- 0
d_mort$cause3wt[is.na(d_mort$cause3wt)] <- 0


# d_mort$lowtag[which(apply(d_mort[, c(32, 34, 36)], 1, sum) != 100)]
# d_mort[which(apply(d_mort[,c(32,34,36)],1,sum)!=100),]
# d_mort[which(apply(d_mort[,c(32,34,36)],1,sum)!=100),c(2,31:37)]
# d_mort[which(apply(d_mort[,c(32,34,36)],1,sum)!=100),c(2)]

#checking for cause weights to not sum to 100
# set_causewts <- d_mort$lowtag[which(apply(d_mort[, c(32, 34, 36)], 1, sum) != 100)]
# set_causewts 

# d_mort[d_mort$lowtag == set_causewts[1], 32] <- 60
# d_mort[d_mort$lowtag == set_causewts[1], 34] <- 30
# d_mort[d_mort$lowtag == set_causewts[1], 36] <- 10

class(d_mort[,32]) <- "numeric"
class(d_mort[,34]) <- "numeric"
class(d_mort[,36]) <- "numeric"

#rescaling between 0/1
d_mort[, c(32, 34, 36)] <- d_mort[, c(32, 34, 36)] / 100

causes <- c("anthro",
            "predation",
            "disease",
            "hunt",
            "pneumonia",
            "starvation",
            "wound")

levels(d_mort$cause1) <- as.character(1:7)
levels(d_mort$cause2) <- as.character(c(0,2:3,1,5:7))
levels(d_mort$cause3) <- as.character(c(0,2:3,1,5:7))

d_mort$cause1<- as.numeric(as.character(d_mort$cause1))
d_mort$cause2<- as.numeric(as.character(d_mort$cause2))
d_mort$cause3<- as.numeric(as.character(d_mort$cause3))

n_causes <- length(causes)
check_case <- c()
check_sum <- c()
p_ls <- vector(mode = "list", length = nrow(d_mort))

for (i in 1:nrow(d_mort)) {
  p <- rep(0,n_causes)
  if (d_mort[i, 32] == 1) {
        p[d_mort[i, 31]] <- 1
      } else {
        if (d_mort[i, 31] == d_mort[i, 33] &
            d_mort[i, 31] == d_mort[i, 35] &
            d_mort[i, 33] == d_mort[i, 35]) {
            p[d_mort[i, 31]] <- d_mort[i, 32] +
                                d_mort[i, 34] +
                                d_mort[i, 36]
        }else{
                if(d_mort[i, 31] != d_mort[i, 33] &
                   d_mort[i, 31] != d_mort[i, 35] &
                   d_mort[i, 33] != d_mort[i, 35]) {
                  p[d_mort[i, 31]] <- d_mort[i, 32]
                  p[d_mort[i, 33]] <- d_mort[i, 34]
                  p[d_mort[i, 35]] <- d_mort[i, 36]
                }else{
                  if (d_mort[i, 31] == d_mort[i, 33]) {
                    p[d_mort[i, 31]] <- d_mort[i, 32] + d_mort[i, 34]
                    p[d_mort[i, 35]] <- d_mort[i, 36]
                  }
                  if (d_mort[i, 31] == d_mort[i, 35]) {
                    p[d_mort[i, 31]] <- d_mort[i, 32] + d_mort[i, 36]
                    p[d_mort[i, 33]] <- d_mort[i, 34]
                  }
                  if (d_mort[i, 33] == d_mort[i, 35]) {
                    p[d_mort[i, 31]] <- d_mort[i, 32]
                    p[d_mort[i, 33]] <- d_mort[i, 34] + d_mort[i, 36]
                  }
                }
        }
      }
  if(sum(p) != 1){check_case <- c(check_case, i)
    check_sum <- c(check_sum, sum(p))
  }
  p_ls[[i]] <- p
}
# check_case
#setting the 2 cases that are not adding to 1, to make them sum to 1
#numeric summation problem, just setting to what it was with a numeric vector
p_ls[[check_case[1]]] <- c(0,.9, .1,0, 0,0, 0)
p_ls[[check_case[2]]] <- c(0,.9, 0,0, 0,.1, 0)
check_case <- NULL

#convert to a data frame
p_cause_obs <- do.call(rbind, p_ls)

d_mort$pc1 <- p_cause_obs[, 1]
d_mort$pc2 <- p_cause_obs[, 2]
d_mort$pc3 <- p_cause_obs[, 3]
d_mort$pc4 <- p_cause_obs[, 4]
d_mort$pc5 <- p_cause_obs[, 5]
d_mort$pc6 <- p_cause_obs[, 6]
d_mort$pc7 <- p_cause_obs[, 7]


##################################################
###
### calculating e,r,s for survival model
###
###################################################

#start of the study (in weeks)
start_week <- week(ymd('2017-01-09'))
start_month <- month(ymd('2017-01-09'))

df_cap <- df_cap[order(df_cap$date),]
df_cap$eweek <- (week(ymd(df_cap$date)) - start_week + 1)
df_cap$emonth<- (month(ymd(df_cap$date)) - start_month + 1)

#adding weeks to make each year a continuous time step from the very first year
df_cap$eweek[df_cap$year == 2018 & month(df_cap$date) != 12] <- df_cap$eweek[df_cap$year == 2018 & month(df_cap$date) != 12] + 52#=df_cap$eweek[df_cap$year==2018]+51
df_cap$eweek[df_cap$year == 2019 & month(df_cap$date) == 12] <- df_cap$eweek[df_cap$year == 2019 & month(df_cap$date) == 12] + 52
df_cap$eweek[df_cap$year == 2019 & month(df_cap$date) != 12] <- df_cap$eweek[df_cap$year == 2019 & month(df_cap$date) != 12] + 104
df_cap$eweek[df_cap$year == 2020 & month(df_cap$date) == 12] <- df_cap$eweek[df_cap$year == 2020 & month(df_cap$date) == 12] + 104
df_cap$eweek[df_cap$year == 2020 & month(df_cap$date) != 12] <- df_cap$eweek[df_cap$year == 2020 & month(df_cap$date) != 12] + 3*52



#adding weeks to make each year a continuous time step from the very first year
df_cap$emonth[df_cap$year == 2018 & month(df_cap$date) != 12] <- df_cap$emonth[df_cap$year == 2018 & month(df_cap$date) != 12] + 12#=df_cap$emonth[df_cap$year==2018]+51
df_cap$emonth[df_cap$year == 2019 & month(df_cap$date) == 12] <- df_cap$emonth[df_cap$year == 2019 & month(df_cap$date) == 12] + 12
df_cap$emonth[df_cap$year == 2019 & month(df_cap$date) != 12] <- df_cap$emonth[df_cap$year == 2019 & month(df_cap$date) != 12] + 2*12
df_cap$emonth[df_cap$year == 2020 & month(df_cap$date) == 12] <- df_cap$emonth[df_cap$year == 2020 & month(df_cap$date) == 12] + 2*12
df_cap$emonth[df_cap$year == 2020 & month(df_cap$date) != 12] <- df_cap$emonth[df_cap$year == 2020 & month(df_cap$date) != 12] + 3*12



####################################################################################
###
### r = minimum of discrete estimated mortality date, collarfound date
###
#####################################################################################
d_mort$sweek <- c()
for(i in 1:nrow(d_mort)){
  d_mort$sweek[i] <- week(d_mort$mortdate[i]) - start_week + 1
  d_mort$year[i] <- year(d_mort$mortdate[i])
}

#recalibrating mortality weeks by study intervals in weeks
#to account for different years
for(i in 1:5){
  d_mort$sweek[d_mort$year == (i + 2017)] <- d_mort$sweek[d_mort$year == (i + 2017)] + (52 *i)
}

# pdf("figures/mortality_hist_weeks.pdf")
  # hist(d_mort$sweek,breaks=100)
# dev.off()

d_mort$rweek <- d_mort$sweek -1

d_mort$smonth <- c()
for(i in 1:nrow(d_mort)){
  d_mort$smonth[i] <- month(d_mort$mortdate[i]) - start_month + 1
  # d_mort$year[i] <- year(d_mort$mortdate[i])
}

#recalibrating mortality weeks by study intervals in weeks
#to account for different years
for(i in 1:5){
  d_mort$smonth[d_mort$year == (i + 2017)] <- d_mort$smonth[d_mort$year == (i + 2017)] + (12 * i)
}

# pdf("figures/mortality_hist_month.pdf")
#   hist(d_mort$smonth,breaks=200)
# dev.off()

d_mort$rmonth <- d_mort$smonth - 1

##########################################################################
###
### discrete censor dates
###
#########################################################################
d_cens$year <- year(d_cens$censordate_use)
d_cens$rweek <- week(d_cens$censordate_use) - start_week + 1
d_cens$rmonth <- month(d_cens$censordate_use) - start_month + 1

# table(d_cens$year)
yrs<-2018:2022
for(i in 1:length(yrs)){
  d_cens$rweek[d_cens$year==yrs[i]] <- d_cens$rweek[d_cens$year==yrs[i]] + i*52
  d_cens$rmonth[d_cens$year==yrs[i]] <- d_cens$rmonth[d_cens$year==yrs[i]] + i*12
}
# hist(d_cens$rweek,breaks=200)
# hist(d_cens$rmonth,breaks=200)

##########################################################################
###
### calculating the discrete intervals of period/age of recapture
### for the 8 that were recaptured
###
#########################################################################

df_cap$recap_disweek <- 0
df_cap$recap_disagewk <- 0
df_cap$recap_dismonth <- 0
df_cap$recap_disagemth <- 0

df_cap$recap_disweek[!is.na(df_cap$recap_cwd)] <- df_cap$eweek[!is.na(df_cap$recap_cwd)] + round(difftime(df_cap$recapdate_cap[!is.na(df_cap$recap_cwd)],
                                                    df_cap$date_cap[!is.na(df_cap$recap_cwd)], 
                                                    units="weeks"))

df_cap$recap_disagewk[!is.na(df_cap$recap_cwd)] <- df_cap$ageweek_cap[!is.na(df_cap$recap_cwd)]+round(difftime(df_cap$recapdate_cap[!is.na(df_cap$recap_cwd)],
                                                    df_cap$date_cap[!is.na(df_cap$recap_cwd)], 
                                                    units="weeks"))

df_cap$recap_dismonth[!is.na(df_cap$recap_cwd)] <- df_cap$emonth[!is.na(df_cap$recap_cwd)] + 
                                                interval(df_cap$date_cap[!is.na(df_cap$recap_cwd)],
                                                         df_cap$recapdate_cap[!is.na(df_cap$recap_cwd)]) %/% months(1)

df_cap$recap_disagemth[!is.na(df_cap$recap_cwd)] <- df_cap$agemonth_cap[!is.na(df_cap$recap_cwd)] +
                                                  interval(df_cap$date_cap[!is.na(df_cap$recap_cwd)],
                                                           df_cap$recapdate_cap[!is.na(df_cap$recap_cwd)]) %/% months(1)

low_recap <- d_cap$lowtag[!is.na(d_cap$recapdate)]
low_recap_neg <- low_recap[!(low_recap %in% df_cap$lowtag[df_cap$recap_cwd==1])]
low_recap_neg <- df_cap$lowtag[df_cap$lowtag %in% low_recap_neg]
low_recap_pos <- low_recap[!(low_recap %in% low_recap_neg)]


##########################################################################
###
### Cleaning the Post-mortem CWD diagnostic test
###
##########################################################################

d_post_cwd$cwdresult[d_post_cwd$cwdresult=='Inconclusive'] <- "Negative"
d_post_cwd$cwdresult[d_post_cwd$cwdresult=='Pending'] <- "Negative"
d_post_cwd$cwdresult[d_post_cwd$cwdresult=='Not tested'] <- "Negative"
d_post_cwd$cwdresult <- as.character(d_post_cwd$cwdresult)

# table(d_post_cwd$cwdresult)
# d_post_cwd$cwdresult=as.numeric(as.factor(d_post_cwd$cwdresult))-1

n_cap <- nrow(df_cap)
n_mort <- nrow(d_mort)
n_cens <- nrow(d_cens)
n_cap
n_mort
n_cens

# n_endlive <- n_cap - (n_mort + n_cens)

