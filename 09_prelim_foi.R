######################################################################################
###
### Preliminary constants for running in the model
###
######################################################################################

####################################################################################
###
### calculating age_lookup, which is the ageclass given the number of months age
###
####################################################################################

# ageclass <- as.numeric(levels(as.factor(cwd_df$age_num)))
# ageclass_num <- 1:length(unique(cwd_df$age))
# age_lookupm <- c()
# for (j in 1:4) {
#   age_lookupm <- c(age_lookupm, rep(ageclass_num[j], 12))
# }
# age_lookupm <- c(age_lookupm, rep(ageclass_num[j + 1], 2 * 12))
# age_lookupm <- c(age_lookupm, rep(ageclass_num[j + 2], 3 * 12 - 1))
# remain <- max(cwd_df$agemonths) - length(age_lookupm)
# age_lookupm <- c(age_lookupm, rep(ageclass_num[j + 3], remain))
# n_agem <- max(age_lookupm)

####################################################################################
###
### calculating age_lookup, which is the ageclass given the number of months age
###
####################################################################################

ageclass <- as.numeric(levels(as.factor(cwd_df$age_num)))
ageclass_num <- 1:length(unique(cwd_df$age))
age_lookupw <- c()
for (j in 1:4) {
  age_lookupw <- c(age_lookupw, rep(ageclass_num[j], 52))
}
age_lookupw <- c(age_lookupw, rep(ageclass_num[j + 1], 2 * 52))
age_lookupw <- c(age_lookupw, rep(ageclass_num[j + 2], 3 * 52 - 1))
remain <- max(cwd_df$ageweeks) - length(age_lookupw)
age_lookupw <- c(age_lookupw, rep(ageclass_num[j + 3], remain))
n_agew <- max(age_lookupw)
age_lookup_f <- age_lookup_m <- age_lookup <- age_lookupw
age_lookup_m[age_lookup_m==7] = 6
n_age <- n_agew
n_agem <- n_age - 1
n_agef <- n_age

################################################################################################
###
### Creating adjacency matrix and hyper parameter 
### values for the dcar_normal implementation
###
################################################################################################

# #create num vector
# num.age=c(1,rep(2,n_age-2),1)

# #create adjacency vector along both years
# temp<-as.matrix(bandSparse(n=n_age,k=c(1),symmetric = T))
# temp2<-matrix(0,n_age,n_age)
# for(i in 1:nrow(temp2)){
#   temp2[i,] = temp[i,]*c(1:n_age)
# }
# adj.age = t(temp2)[which(t(temp2)!=0)]

# #weight vector into single vector
# weights.age=rep(1,length(adj.age))

# #number of records in the adjacency vectors
# nn_agegroup=length(adj.age)

###################################################################################
### age to date conversion within model
###################################################################################

#initialize birthmonth from this month
# birth_start <- min(cwd_df$birth_date)
# death_end <- max(cwd_df$kill_date)
# cwd_df$birthmonth <- as.period(ymd(birth_start) %--% ymd(cwd_df$birth_date), unit = 'month')$month + 1
# study_start <- head(sort(cwd_df$kill_date),1)
# cwd_df$monthkill <- as.period(ymd(study_start) %--% ymd(cwd_df$kill_date), unit = 'month')$month + 1
# cwd_df$yearkill <- cwd_df$kill_year - year(study_start) + 1
# pre_study_months <-as.period(ymd(birth_start) %--% ymd(study_start),unit = 'month')$month + 1
# period_lookup <- sort(rep(unique(cwd_df$yearkill), 12))
# period_lookup <- period_lookup[-(1:2)]#remove janruary and february from period_lookup for hunter harvest surveillance data
# period_lookup <- c(rep(1, pre_study_months), period_lookup)
# period_lookup_hunt <- period_lookup
# n_period <- max(period_lookup)
# n_period_lookup <- length(period_lookup)
# n_age_lookupm <- length(age_lookupm)
# period_lookup_hunt <- period_lookup
# n_period_lookup_hunt <- length(period_lookup_hunt)

birth_start <- min(cwd_df$birth_date)
death_end <- max(d_mort$mortdate)
cwd_df$birthweek <-(lubridate::interval(birth_start,cwd_df$birth_date) %/% weeks(1)) + 1
study_start <- head(sort(cwd_df$kill_date),1)
cwd_df$weekkill <- (lubridate::interval(study_start,cwd_df$kill_date) %/% weeks(1)) + 1
cwd_df$yearkill <- cwd_df$kill_year - year(study_start) + 1
pre_study_weeks <-(lubridate::interval(birth_start,study_start) %/% weeks(1))+1
period_lookup <- sort(rep(unique(cwd_df$yearkill), 52))
#removing weeks in the lookup vector beyond the week in Feb 2022 with the last date
rm_indx <- 52-interval("2022-01-01",death_end) %/% weeks(1)
period_lookup <- period_lookup[1:(length(period_lookup)-rm_indx)]
period_lookup <- c(rep(1, pre_study_weeks), period_lookup)
period_lookup_hunt <- period_lookup
n_period <- max(period_lookup)
n_period_lookup <- length(period_lookup)
n_age_lookup <- length(age_lookup)
period_lookup_hunt <- period_lookup
n_period_lookup_hunt <- length(period_lookup_hunt)

#############################################################################################
###
### Creating adjacency matrix and hyper parameter values for the dcar_normal implementation
### For PERIOD 
###
#############################################################################################

#create num vector
num_period <- c(1, rep(2, n_period - 2), 1)

#create adjacency vector along both years
temp <- as.matrix(bandSparse(n = n_period, k = c(1), symmetric = T))
temp2 <- matrix(0, n_period, n_period)
for (i in 1:nrow(temp2)) {
  temp2[i, ] <- temp[i, ] * c(1:n_period)
}
adj_period <- t(temp2)[which(t(temp2) != 0)]
n_adj_period <- length(adj_period)
weights_period <- rep(1, length(adj_period))

########################################################################################
### creating adjacency matrix for the spatial terms
########################################################################################

study_nb <- poly2nb(study_df)
study_nb_ls <- nb2WB(study_nb)
adj_sp <- study_nb_ls$adj
weights_sp <- study_nb_ls$weights
num_sp <- sapply(study_nb,length)
n_adj_sp <- length(adj_sp)

#matching the order of the township-range-sections
cwd_df <- cwd_df[order(match(cwd_df$dsection, study_df$dsection)), ]

#constants for spatial model
n_sect <- length(unique(study_df$dsection))
sect_num <- as.numeric(as.factor(study_df$dsection))

#for indexing sections within the car model
sect <- rep(NA,n_sect)
d_surv$sect <- rep(NA,nrow(d_surv))
d_fit_sus_cens_posttest$sect <- rep(NA,nrow(d_fit_sus_cens_posttest))
d_fit_sus_cens_postno$sect <- rep(NA,nrow(d_fit_sus_cens_postno))
d_fit_sus_mort_posttest$sect <- rep(NA,nrow(d_fit_sus_mort_posttest))
d_fit_sus_mort_postno$sect <- rep(NA,nrow(d_fit_sus_mort_postno))
d_fit_icap_cens$sect <- rep(NA,nrow(d_fit_icap_cens))
d_fit_icap_mort$sect <- rep(NA,nrow(d_fit_icap_mort))
d_fit_rec_neg_cens_posttest$sect <- rep(NA,nrow(d_fit_rec_neg_cens_posttest))
d_fit_rec_neg_cens_postno$sect <- rep(NA,nrow(d_fit_rec_neg_cens_postno))
d_fit_rec_neg_mort$sect <- rep(NA,nrow(d_fit_rec_neg_mort))
d_fit_rec_pos_cens$sect <- rep(NA,nrow(d_fit_rec_pos_cens))
d_fit_rec_pos_mort$sect <- rep(NA,nrow(d_fit_rec_pos_mort))
d_fit_idead$sect <- rep(NA,nrow(d_fit_idead))
d_fit_endlive$sect <- rep(NA,nrow(d_fit_endlive))

for (j in 1:n_sect) {
  sect[which(cwd_df$dsection == study_df$dsection[j])] <- sect_num[j]

  d_surv$sect[which(d_surv$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_sus_cens_posttest$sect[which(d_fit_sus_cens_posttest$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_sus_cens_postno$sect[which(d_fit_sus_cens_postno$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_sus_mort_posttest$sect[which(d_fit_sus_mort_posttest$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_sus_mort_postno$sect[which(d_fit_sus_mort_postno$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_icap_cens$sect[which(d_fit_icap_cens$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_icap_mort$sect[which(d_fit_icap_mort$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_rec_neg_cens_posttest$sect[which(d_fit_rec_neg_cens_posttest$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_rec_neg_cens_postno$sect[which(d_fit_rec_neg_cens_postno$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_rec_neg_mort$sect[which(d_fit_rec_neg_mort$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_rec_pos_cens$sect[which(d_fit_rec_pos_cens$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_rec_pos_mort$sect[which(d_fit_rec_pos_mort$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_idead$sect[which(d_fit_idead$dsection == study_df$dsection[j])] <- sect_num[j]
  d_fit_endlive$sect[which(d_fit_endlive$dsection == study_df$dsection[j])] <- sect_num[j]
}

w_scale <- -nb2mat(study_nb, zero.policy = TRUE, style = "B")
diag(w_scale) <- abs(apply(w_scale, 1, sum))
Q <- inla.scale.model(w_scale,
                      constr = list(A = matrix(1,
                                           nrow = 1,
                                           ncol = nrow(w_scale)
                                           ),
                                  e = 0
                                  )
                      )
scale <- exp((1/nrow(w_scale))*sum(log(1/diag(Q))))

################################################################################
### hyperpriors for  the spatial term and heterogeneity term in the BYM model
################################################################################

h2 <- 1.84
h <- h2^2
s <- 1
s2 <- 1

#############################################################################################
#number of MCMCr iterations, Chains, and Burn-in
#############################################################################################

# reps <- 1000
# bin <- 0.5 * reps
# n_chains <- 3
# n_thin <- 1

#############################################################################################
#number of MCMC iterations, Chains, and Burn-in
#############################################################################################


#length(age_week_indx) w/o 9+ age class is 462
#maximum age in weeks from survival models is 962

age_week_indx <- c(rep(1,52),#fawns
                   rep(2,52),#1
                   rep(3,52),#2
                   rep(4,52),#3
                   rep(5, 52 * 2),#4-5
                   rep(6, 52 * 3),#6-8
                   rep(7,nT_age_surv-length(c(rep(1,52),#fawns
                                        rep(2,52),#1
                   rep(3,52),#2
                   rep(4,52),#3
                   rep(5, 52 * 2),#4-5
                   rep(6, 52 * 3)))))
# length(age_week_indx)
# max(inf_nT_age_surv)

period_week_indx <- c(rep(1,51),#2017
                   rep(2,52),#2018
                   rep(3,52),#2019
                   rep(4,52),#2020
                   rep(5,52),#2021
                   rep(6,nT_period - length(c(rep(1,51),#2017
                                              rep(2,52),#2018
                                              rep(3,52),#2019
                                              rep(4,52),#2020
                                              rep(5,52))))#2022
                   )

period_week_indx_col <- period_week_indx + 15


###########################################################################
###
### Functions for Data Augmentation for infection status
###
##########################################################################

#######################
###  testing prob foi
#######################

load("~/Documents/Transmission/Transmission_v3/27_urw2_bym2_timeRW1_2021/fit_sum.Rdata")
m_age <- fit_sum[grep("m_age",rownames(fit_sum)),1][1:n_age]
f_age <- fit_sum[grep("f_age",rownames(fit_sum)),1][1:n_age]
f_period <- fit_sum[grep("f_time",rownames(fit_sum)),1]
m_period <- fit_sum[grep("m_time",rownames(fit_sum)),1]

llambda_foi <- array(0, c(n_age, n_period, 2))
p_inf <- array(0, c(n_age, n_period, 2))
for (i in 1:n_age) {
  for(j in 1:n_period){
     llambda_foi[i, j, 1] <- m_age[i] + m_period[j]
     llambda_foi[i, j, 2] <- f_age[i] + f_period[j]
     p_inf[i, j, 1] <- 1 - exp(-sum(exp(llambda_foi[1:i, j, 1])))
     p_inf[i, j, 2] <- 1 - exp(-sum(exp(llambda_foi[1:i, j, 2])))
  }
}

###################################################################################
#the first birth of test infected at capture individuals was 2001-06-15
#indexing of all icap individuals is based off of initializing with this date
#which.min(d_fit_icap$left_period-d_fit_icap$left_age)
# d_fit_icap[61,]
# d_fit_icap$left_period[61]-d_fit_icap$left_age[61]
# (lubridate::interval("2001-06-15","2017-01-09") %/% weeks(1))
###################################################################################

birth_week_icap <- d_fit_icap_mort$left_period_e - d_fit_icap_mort$left_age_e
d_fit_icap_mort$birth_week <- birth_week_icap - min(birth_week_icap) + 1

birth_start_icap <- "2001-06-15"
pre_study_weeks_icap <-(lubridate::interval(birth_start_icap,study_start) %/% weeks(1)) + 1
period_lookup_icap <- rep(1:21, each=52)
period_lookup_icap <- c(rep(1, pre_study_weeks_icap), period_lookup_icap)
n_period_lookup_icap <- length(period_lookup_icap)

##############################################################
### separate the icap data for data augmentation 
### by whether individuals 
### were born prior to the start of the collar study (pre)
### or whether they were born after 
###the start of the collar study (post)
##############################################################

# pre_indx <- which(d_fit_icap_cens$birth_week<=812)
# post_indx <-  which(d_fit_icap_cens$birth_week>812)

# d_fit_icap_pre <- d_fit_icap[d_fit_icap$lowtag %in% d_fit_icap_cens$lowtag[pre_indx],]
# d_fit_icap_post <- d_fit_icap[d_fit_icap$lowtag %in% d_fit_icap_cens$lowtag[post_indx],]
# d_fit_icap_cens_pre <- d_fit_icap_cens[d_fit_icap_cens$lowtag %in% d_fit_icap_cens$lowtag[pre_indx],]
# d_fit_icap_cens_post <- d_fit_icap_cens[d_fit_icap_cens$lowtag %in% d_fit_icap_cens$lowtag[post_indx],]
# d_fit_icap_cens_post$birth_week_study <- d_fit_icap_cens_post$birth_week - 812

# #puting the right censored only individuals at the end of the data frames
# low_censonly <- as.integer(names(which(table(d_fit_icap_pre$lowtag)==1)))
# d_fit_icap_censonly_pre <- d_fit_icap_pre[d_fit_icap_pre$lowtag %in% low_censonly,]
# d_fit_icap_pre <- d_fit_icap_pre[!(d_fit_icap_pre$lowtag %in% low_censonly),]
# d_fit_icap_pre <- rbind(d_fit_icap_pre,d_fit_icap_censonly_pre)

# d_fit_icap_censonly_cens_pre <- d_fit_icap_cens_pre[d_fit_icap_cens_pre$lowtag %in% low_censonly,]
# d_fit_icap_cens_pre <- d_fit_icap_cens_pre[!(d_fit_icap_cens_pre$lowtag %in% low_censonly),]
# d_fit_icap_cens_pre <- rbind(d_fit_icap_cens_pre,d_fit_icap_censonly_cens_pre)

# n_icap_pre <- nrow(d_fit_icap_pre)
# n_icap_cens_pre <- nrow(d_fit_icap_cens_pre)
# n_icap_post <- nrow(d_fit_icap_post)
# n_icap_cens_post <- nrow(d_fit_icap_cens_post)

##################################################
### none of the "post" icap individuals
### (i.e. born after start of the study)
### were right censored
##################################################

# icap_cens_post_age2date <- d_fit_icap_cens_post$birth_week_study-1
# icap_post_la_indx <- seq(1,n_icap_post,by = 2)

# # length(unique(d_fit_icap_post$lowtag)) #they all die
# #removing left age because transition into the infected class is drawn within the model
# d_fit_icap_post$left_age[icap_post_la_indx] <- NA
# d_fit_icap_post$left_period[icap_post_la_indx] <- NA

#################################################
### there were 2 "pre" inidividuals
### that were right censored 
### (i.e. born before the start of the study)
### the likelihood is a little different
##################################################








##############################################################
### The icap data for data augmentation 
### for the individuals they were born after 
### the start of the collar study (post)
##############################################################

# icap_cens_post_left_age = d_fit_icap_cens_post$left_age
# icap_cens_post_left_period = d_fit_icap_cens_post$left_period

#   icap_post_age_inf <- c()
#   for(i in 1:n_icap_cens_post){

#     #Data Augmentation, age at infection based on FOI
#     icap_post_age_inf[i]  <- calc_age_inf_icap(llambda_foi = llambda_foi[1:n_age, 1:n_period, 1:2],
#             age_week_indx = age_week_indx[1:nT_age_surv],
#             period_week_indx = period_lookup_icap[1:n_period_lookup_icap],
#             left_age = icap_cens_post_left_age[i],
#             birth_week = icap_cens_post_birth_week[i],
#             sex = icap_cens_post_sex[i]
#             )
#     }
# icap_post_age_inf



##############################################################
### The icap data for data augmentation 
### for the individuals they were born before 
### the start of the collar study (pre)
##############################################################

# icap_pre_left_age = d_fit_icap_pre$left_age
# icap_pre_right_age = d_fit_icap_pre$right_age
# icap_pre_left_period = d_fit_icap_pre$left_period
# icap_pre_sex = d_fit_icap_pre$sex


# icap_cens_pre_left_age = d_fit_icap_cens_pre$left_age
# icap_cens_pre_birth_week = d_fit_icap_cens_pre$birth_week
# icap_cens_pre_sex = d_fit_icap_cens_pre$sex
# icap_cens_pre_left_period = d_fit_icap_cens_pre$left_period
# icap_cens_pre_birth_week_study = d_fit_icap_cens_pre$birth_week_study



# icap_censonly_pre_left_age = d_fit_icap_censonly_pre$left_age
# icap_censonly_pre_birth_week = d_fit_icap_censonly_pre$birth_week
# icap_censonly_pre_sex = d_fit_icap_censonly_pre$sex
# icap_censonly_pre_left_period = d_fit_icap_censonly_pre$left_period
# icap_censonly_pre_birth_week_study = d_fit_icap_censonly_pre$birth_week_study


###############################################################
###
### Separating hunter harvested
### CWD test positive deer from CWD test negative
###
### d_fit_hunt_pos
### d_fit_hunt_neg
###
##############################################################

d_fit_hunt_neg <- cwd_df[cwd_df$teststatus == 0, ]
d_fit_hunt_pos <- cwd_df[cwd_df$teststatus == 1, ]

sect_hunt_neg <- sect[cwd_df$teststatus == 0]
sect_hunt_pos <- sect[cwd_df$teststatus == 1]

d_fit_hunt_neg$sect_hunt_neg <- sect_hunt_neg
d_fit_hunt_pos$sect_hunt_pos <- sect_hunt_pos

test <- d_fit_hunt_neg %>% select(ageweeks,birthweek,sex,sect_hunt_neg) %>%
               group_by(ageweeks,birthweek,sex,sect_hunt_neg) %>% 
               count()

