###
### Alison Ketz 6/13/2019
###
### This is a function that we can use to clean up the data.frames from the database
### Clean Data - Heisey Non-spatial Models
###

cleanData <- function(x){

  ### convert age class to numeric
  x$age <- as.factor(x$age)
  levels(x$age) <- c("0","1","9","2","3","4","6","9")
  x$age_num <- as.numeric(as.character(x$age))

  #proportion of tested deer without results
  # table(x$has_results)[1]/sum(table(x$has_results)) #.0061

  #removing cases where there were no cwd results
  x <- x[x$has_results == "Y",]

  #cleaning kill types that are not harvest
  x$kill_type[is.na(x$kill_type)] <- "HUNTER"
  x <- x[x$kill_type != "CAR KILL",]
  x <- x[x$kill_type != "FOUND DEAD",]
  x <- x[x$kill_type != "ILLEGAL HARVEST",]

  #remove cases without sex
  x <- x[!is.na(x$sex), ]
  
  #calculate approximate birthdate based on age at kill_date
  ageclass <- as.numeric(levels(as.factor(x$age_num)))
  
  #add birth_date to dataframe and calculate values for it
  # assuming birth date is May 15, depending on kill_date and ageclass at kill_date
  
  x$kill_date <- as.Date(x$kill_date)
  x <- x[!is.na(x$kill_date),]
  
  x$birth_date <- NA
  for(j in 1:dim(x)[1]) {
    if(format(x$kill_date[j], format="%m-%d")<="05-15"){
      x$birth_date[j] <- paste0(year(x$kill_date[j]) - x$age_num[j] - 1, "-05-15")
    }else {
      x$birth_date[j] <- paste0(year(x$kill_date[j]) - x$age_num[j], "-05-15")
    }
  }
  x$birth_date <- as.Date(x$birth_date)

  #calculating age at death in days, weeks, and months
  x$agedays <- difftime(x$kill_date,x$birth_date) 
  x$ageweeks  <- ceiling(difftime(x$kill_date,x$birth_date, units = "weeks")) 
  x$agemonths <- (lubridate::interval(x$birth_date,x$kill_date) %/% months(1))

  #removing the 7 fawns that were negative and only 1 month old
  x <- x[which(x$agemonths != min(x$agemonths)), ]
  
  #correcting ages in months
  x$age_num[x$age_num == 0] <- .5
  x$kill_year <- year(x$kill_date)

  #setting the maximum age in months and weeks 
  #to max out when they turn 9 yrs old
  x$agemonths[x$agemonths > 9*12] <- 9*12
  x$ageweeks[x$ageweeks > 9*52] <- 9*52

  x$teststatus <- 0
  x$teststatus[x$positive == "Y"] <- 1
  
  x$sex <- as.factor(x$sex)
  levels(x$sex) <- c(1,0)#females = 1,"males=0"
  x$sex <- as.numeric(as.character(x$sex))
 
  x$agedays <- as.integer(x$agedays)
  x$ageweeks <- as.integer(x$ageweeks)
  x$agemonths <- as.integer(x$agemonths)
  x
  
}

