
################################################################
###
### separating survival into 3 classes
###
################################################################

# all deer
low_sus <- d_surv$lowtag

# uninfected at recapture, antemortem negative
low_recap_neg 

# infected at capture, antemortem
low_icap <- d_surv$lowtag[d_surv$cwd_cap == 1]

# test + infected at mortality but not at capture or recapture
# remove infected at mortality from infected antemort
# remove deer that died and weren't tested at mortality
low_idead <- d_surv$lowtag[d_surv$cwd_mort == 1 & !is.na(d_surv$cwd_mort)]
low_idead <- low_idead[!(low_idead %in% c(low_recap, low_icap))]

# all individuals that are alive at the end of the study that weren't recaptured
# low_endlive <- d_surv$lowtag[!(d_surv$lowtag %in% as.integer(unique(c(d_cens$lowtag, d_mort$lowtag))))]
low_endlive 

# never tested positive, and were tested at 
low_sus <- low_sus[!(low_sus %in% unique(c(low_icap, low_recap, low_idead, low_endlive)))]

d_fit_sus <- d_surv[d_surv$lowtag %in% low_sus, ]
d_fit_icap <- d_surv[d_surv$lowtag %in% low_icap, ]
d_fit_idead <- d_surv[d_surv$lowtag %in% low_idead, ]
d_fit_rec_pos <- d_surv[d_surv$lowtag %in% low_recap_pos, ]
d_fit_rec_neg <- d_surv[d_surv$lowtag %in% low_recap_neg, ]
d_fit_endlive <- d_surv[d_surv$lowtag %in% low_endlive, ]

# there is more than one record for deer that die during the study
# so the number of individuals is less than the number of records for each
# class of animals

n_fit_sus <- nrow(d_fit_sus)
n_fit_icap <- nrow(d_fit_icap)
n_fit_idead <- nrow(d_fit_idead)
n_fit_rec_neg <- nrow(d_fit_rec_neg)
n_fit_rec_pos <- nrow(d_fit_rec_pos)
n_fit_endlive <- nrow(d_fit_endlive)

######################
### Recaptures
######################

# separating the censor part from the mortality part
# so to properly structure the data for the likelihood
# keeping sus and inf parts separated

# there is 1 recaptured deer that was right censored and not a mortality and did not live to end of study
# due to a dead battery
# this deer was test negative at capture, test positive at recapture, then interval censored

d_fit_rec_pos_cens <- d_fit_rec_pos[d_fit_rec_pos$lowtag == 6065, ]
d_fit_rec_pos <- d_fit_rec_pos[d_fit_rec_pos$lowtag != 6065, ]
n_fit_rec_pos <- nrow(d_fit_rec_pos)
n_fit_rec_pos_cens <- nrow(d_fit_rec_pos_cens)

# for the cwd test (-) deer at capture
# these deer were interval censored and tested neg at capture and at censoring (or not tested at censoring)
d_fit_rec_neg_cens <- d_fit_rec_neg[d_fit_rec_neg$censor == 1, ]
n_fit_rec_neg_cens <- nrow(d_fit_rec_neg_cens)

# these deer were test negative at capture, and recapture, and test negative at mort,
# all of them had post mortem tests
d_fit_rec_neg_mort <- d_fit_rec_neg[d_fit_rec_neg$censor == 0 & d_fit_rec_neg$cwd_mort == 0, ]
n_fit_rec_neg_mort <- nrow(d_fit_rec_neg_mort)

# these deer were test negative at capture, and recapture, and test positive at censor
d_fit_rec_pos_mort <- d_fit_rec_neg[d_fit_rec_neg$censor == 0, ]
n_fit_rec_pos_mort <- nrow(d_fit_rec_pos_mort)


############################################
### separating susceptible deer that were 
### test negative at capture into whether 
### tested after mortality or censoring
############################################

d_fit_sus_cens <- d_fit_sus[d_fit_sus$censored == 1, ]
d_fit_sus_mort <- d_fit_sus[d_fit_sus$censored == 0, ]

d_fit_sus_cens_posttest <- d_fit_sus_cens[!is.na(d_fit_sus_cens$cwd_mort), ]
d_fit_sus_cens_postno <- d_fit_sus_cens[is.na(d_fit_sus_cens$cwd_mort), ]

d_fit_sus_mort_posttest <- d_fit_sus_mort[!is.na(d_fit_sus_mort$cwd_mort), ]
d_fit_sus_mort_postno <- d_fit_sus_mort[is.na(d_fit_sus_mort$cwd_mort), ]

d_fit_rec_neg_cens_posttest <- d_fit_rec_neg_cens[!is.na(d_fit_rec_neg_cens$cwd_mort), ]
d_fit_rec_neg_cens_postno <- d_fit_rec_neg_cens[is.na(d_fit_rec_neg_cens$cwd_mort), ]


####################################################
### separating infected at capture deer
### that were born before or after start of study
#####################################################

d_fit_icap_cens <- d_fit_icap[d_fit_icap$censored == 1, ]
d_fit_icap_mort <- d_fit_icap[d_fit_icap$censored == 0, ]

# n_fit_sus_cens <- nrow(d_fit_sus_cens)
# n_fit_sus_mort <- nrow(d_fit_sus_mort)
n_fit_sus_cens_posttest <- nrow(d_fit_sus_cens_posttest)
n_fit_sus_cens_postno <- nrow(d_fit_sus_cens_postno)
n_fit_sus_mort_posttest <- nrow(d_fit_sus_mort_posttest)
n_fit_sus_mort_postno <- nrow(d_fit_sus_mort_postno)
n_fit_icap_cens <- nrow(d_fit_icap_cens)
n_fit_icap_mort <- nrow(d_fit_icap_mort)
n_fit_rec_neg_cens_posttest <- nrow(d_fit_rec_neg_cens_posttest)
n_fit_rec_neg_cens_postno <- nrow(d_fit_rec_neg_cens_postno)

#############################################################################################
# separating Hunter Harvest Data for likelihoods into positive or negative cases
#############################################################################################

d_fit_hunt_pos <- cwd_df[cwd_df$teststatus == 1, ]
d_fit_hunt_neg <- cwd_df[cwd_df$teststatus == 0, ]

n_fit_hunt_pos <- nrow(d_fit_hunt_pos)
n_fit_hunt_neg <- nrow(d_fit_hunt_neg)

###############################################################
###
### all data cases
###
###############################################################

# d_fit_sus_cens 
# d_fit_sus_mort 
# d_fit_icap_cens
# d_fit_icap_mort
# d_fit_rec_neg_cens
# d_fit_rec_neg_mort
# d_fit_rec_pos_cens
# d_fit_rec_pos_mort
# d_fit_idead
# d_fit_endlive

num_observations <- rbind(n_fit_hunt_neg,
        n_fit_hunt_pos,
        n_fit_sus_cens_posttest,
        n_fit_sus_cens_postno,
        n_fit_sus_mort_posttest,
        n_fit_sus_mort_postno,
        n_fit_icap_cens,
        n_fit_icap_mort,
        n_fit_rec_neg_cens_posttest,
        n_fit_rec_neg_cens_postno,
        n_fit_rec_neg_mort,
        n_fit_rec_pos_cens,
        n_fit_rec_pos_mort,
        n_fit_idead,
        n_fit_endlive)


data_cases <- rownames(rbind(n_fit_hunt_neg,
        n_fit_hunt_pos,
        n_fit_sus_cens_posttest,
        n_fit_sus_cens_postno,
        n_fit_sus_mort_posttest,
        n_fit_sus_mort_postno,
        n_fit_icap_cens,
        n_fit_icap_mort,
        n_fit_rec_neg_cens_posttest,
        n_fit_rec_neg_cens_postno,
        n_fit_rec_neg_mort,
        n_fit_rec_pos_cens,
        n_fit_rec_pos_mort,
        n_fit_idead,
        n_fit_endlive)
        )

obs_sample_sizes <- data.frame(data_cases, num_observations, row.names = NULL)

write.csv(obs_sample_sizes, file = "obs_sample_sizes.csv", row.names=FALSE)

d_fit_icap_cens$left_period_e - d_fit_icap_cens$left_age_e

png("icap_birth_relative_study.png")
hist(d_fit_icap_mort$left_period_e - d_fit_icap_mort$left_age_e,breaks=100)
dev.off()


obs_sample_sizes_desc <- read.csv("../obs_sample_sizes_description.csv")

# print(xtable(obs_sample_sizes_desc),include.rownames=FALSE)

