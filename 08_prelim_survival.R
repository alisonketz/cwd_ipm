
###
### calibrating collar study time with start of harvest study time
### for indexing
### in the likelihood
###
# min(d_fit_hunt_neg$kill_date)
# min(d_fit_hunt_pos$kill_date)
# nT_overall <- floor(as.duration(ymd("2002-03-01") %--% ymd("2022-05-08"))/dweeks(1)) - 1
# nT_period_presurv <- floor(as.duration(ymd("2002-03-01") %--% ymd("2017-01-07"))/dweeks(1)) - 1
# floor(as.duration(ymd("2017-01-07") %--% ymd("2022-05-08"))/dweeks(1))

#if we go from the first birth in 1992
nT_overall <- floor(as.duration(ymd("1992-05-15") %--% ymd("2022-05-15"))/dweeks(1)) - 1
nT_period_presurv <- floor(as.duration(ymd("1992-05-15") %--% ymd("2017-01-07"))/dweeks(1)) - 1

###
### calibrating age of deer with the study time for indexing
### in the likelihood
###

#left_period - left_age
sus_age2date <- d_fit_sus$left_period_e - d_fit_sus$left_age_e + nT_period_presurv
icap_cens_age2date <- d_fit_icap_cens$left_period_e - d_fit_icap_cens$left_age_e + nT_period_presurv
icap_mort_age2date <- d_fit_icap_mort$left_period_e - d_fit_icap_mort$left_age_e + nT_period_presurv
idead_age2date <- d_fit_idead$left_period_e - d_fit_idead$left_age_e + nT_period_presurv
rec_neg_cens_posttest_age2date <- d_fit_rec_neg_cens_posttest$left_period_e - d_fit_rec_neg_cens_posttest$left_age_e + nT_period_presurv
rec_neg_cens_postno_age2date <- d_fit_rec_neg_cens_postno$left_period_e - d_fit_rec_neg_cens_postno$left_age_e + nT_period_presurv
rec_neg_mort_age2date <- d_fit_rec_neg_mort$left_period_e - d_fit_rec_neg_mort$left_age_e + nT_period_presurv
rec_pos_cens_age2date <- d_fit_rec_pos_cens$left_period_e - d_fit_rec_pos_cens$left_age_e + nT_period_presurv
rec_pos_mort_age2date <- d_fit_rec_pos_mort$left_period_e - d_fit_rec_pos_mort$left_age_e + nT_period_presurv
sus_cens_postno_age2date <- d_fit_sus_cens_postno$left_period_e - d_fit_sus_cens_postno$left_age_e + nT_period_presurv
sus_cens_posttest_age2date <- d_fit_sus_cens_posttest$left_period_e - d_fit_sus_cens_posttest$left_age_e + nT_period_presurv
sus_mort_posttest_age2date <- d_fit_sus_mort_posttest$left_period_e - d_fit_sus_mort_posttest$left_age_e + nT_period_presurv
sus_mort_postno_age2date <- d_fit_sus_mort_postno$left_period_e - d_fit_sus_mort_postno$left_age_e + nT_period_presurv
endlive_age2date <- d_fit_endlive$left_period_e - d_fit_endlive$left_age_e + nT_period_presurv


###
### Number of age effects and number of period effects
###

nT_age_surv <- max(d_surv$right_age_s, na.rm = TRUE) - 1
# nT_period_surv <- max(d_surv$right_period_s, na.rm = TRUE) - 1
# nT_period_surv <- max(d_surv$right_period_s, na.rm = TRUE)

nT_period_surv <- nT_overall - nT_period_presurv + 1


###############################
###
### basis functions
### using the spline package
###
###############################

quant_age <- .05
knots_age <- unique(c(1,round(quantile(d_fit_sus$right_age_r,c(seq(quant_age,.99, by=quant_age),.99)))))
nknots_age <- length(knots_age)
splinebasis <- ns(1:nT_age_surv, knots = knots_age)#,intercept=TRUE,
constr_sumzero <- matrix(1, 1, nrow(splinebasis)) %*% splinebasis
qrc <- qr(t(constr_sumzero))
Z <- qr.Q(qrc,complete=TRUE)[,(nrow(constr_sumzero)+1):ncol(constr_sumzero)]
Z_age <- splinebasis%*%Z
nknots_age <- dim(Z_age)[2]


#############################################################
###
### plot of the basis functions
###
##############################################################
# pdf("figures/basis_function_age.pdf")
# plot(1:nT_age_surv,delta[,1],ylim=c(-1,1),type="l",main="Basis Function Age Effect")
# for(i in 2:nknots_age){
#   lines(1:nT_age_surv,delta[,i])
# }
# dev.off()

########################################
###
### Spline basis matrix for Period
###
##########################################

intvl_period <- 13
knots_period <- c(1,seq(intvl_period, nT_period_surv, by = intvl_period))
knots_period <- unique(knots_period)
nknots_period <- length(knots_period)
splinebasis <- bs(1:nT_period_surv, knots = knots_period)
constr_sumzero <- matrix(1, 1, nrow(splinebasis)) %*% splinebasis
qrc <- qr(t(constr_sumzero))
Z <- qr.Q(qrc,complete=TRUE)[,(nrow(constr_sumzero)+1):ncol(constr_sumzero)]
Z_period <- splinebasis%*%Z
nknots_period <- dim(Z_period)[2]


# pdf("figures/basis_function_time_bs.pdf")
# plot(1:nT_period_surv,Z_period[,1],type="l",ylim=c(-1,1),main="Basis Function Time Effect")
# for(i in 2:nknots_period){
#   lines(1:nT_period_surv,Z_period[,i])
# }
# dev.off()

###############################
###
### basis functions
### using the spline package
###
###############################

# inf_quant_age <- .05
# inf_knots_age <- unique(c(1,round(quantile(d_fit_icap$right_age - 1,
#                          c(seq(inf_quant_age,.99, by=inf_quant_age),
#                          .99)))))
# inf_nknots_age <- length(inf_knots_age)
# splinebasis <- ns(1:inf_nT_age_surv, knots = inf_knots_age)#,intercept=TRUE,
# constr_sumzero <- matrix(1, 1, nrow(splinebasis)) %*% splinebasis
# qrc <- qr(t(constr_sumzero))
# Z <- qr.Q(qrc,complete=TRUE)[,(nrow(constr_sumzero)+1):ncol(constr_sumzero)]
# inf_Z_age <- splinebasis%*%Z
# inf_nknots_age <- dim(inf_Z_age)[2]
# inf_nknots_age
# inf_quant_age <- .05
# inf_knots_age <- unique(c(1,round(quantile(d_fit_icap$right_age - 1,
#                          c(seq(inf_quant_age,.99, by=inf_quant_age),
#                          .99)))))
# inf_nknots_age <- length(inf_knots_age)
# splinebasis <- ns(1:nT_age_surv, knots = inf_knots_age)#,intercept=TRUE,
# constr_sumzero <- matrix(1, 1, nrow(splinebasis)) %*% splinebasis
# qrc <- qr(t(constr_sumzero))
# Z <- qr.Q(qrc,complete=TRUE)[,(nrow(constr_sumzero)+1):ncol(constr_sumzero)]
# inf_Z_age <- splinebasis%*%Z
# inf_nknots_age <- dim(inf_Z_age)[2]
# inf_nknots_age

#############################################################
###
### plot of the basis functions
###
##############################################################
# pdf("figures/basis_function_age.pdf")
# plot(1:nT_age_surv,delta[,1],ylim=c(-1,1),type="l",main="Basis Function Age Effect")
# for(i in 2:nknots_age){
#   lines(1:nT_age_surv,delta[,i])
# }
# dev.off()

########################################
###
### Spline basis matrix for Infected
### Period
###
##########################################

# inf_intvl_period <- 13
# inf_knots_period <- c(1,seq(inf_intvl_period, inf_nT_period_surv, by = inf_intvl_period))
# inf_knots_period <- unique(inf_knots_period)
# inf_nknots_period <- length(inf_knots_period)
# splinebasis <- bs(1:inf_nT_period_surv, knots = inf_knots_period)
# constr_sumzero <- matrix(1, 1, nrow(splinebasis)) %*% splinebasis
# qrc <- qr(t(constr_sumzero))
# Z <- qr.Q(qrc,complete=TRUE)[,(nrow(constr_sumzero)+1):ncol(constr_sumzero)]
# inf_Z_period <- splinebasis%*%Z
# inf_nknots_period <- dim(inf_Z_period)[2]
# inf_nknots_period
# inf_intvl_period <- 13
# inf_knots_period <- c(1,seq(inf_intvl_period, nT_period_surv, by = inf_intvl_period))
# inf_knots_period <- unique(inf_knots_period)
# inf_nknots_period <- length(inf_knots_period)
# splinebasis <- bs(1:nT_period_surv, knots = inf_knots_period)
# constr_sumzero <- matrix(1, 1, nrow(splinebasis)) %*% splinebasis
# qrc <- qr(t(constr_sumzero))
# Z <- qr.Q(qrc,complete=TRUE)[,(nrow(constr_sumzero)+1):ncol(constr_sumzero)]
# inf_Z_period <- splinebasis%*%Z
# inf_nknots_period <- dim(inf_Z_period)[2]
# inf_nknots_period


# pdf("figures/basis_function_time_bs.pdf")
# plot(1:nT_period_surv,Z_period[,1],type="l",ylim=c(-1,1),main="Basis Function Time Effect")
# for(i in 2:nknots_period){
#   lines(1:nT_period_surv,Z_period[,i])
# }
# dev.off()

#############################################################
###
### preliminary values to draw recapture infection week
###
#############################################################
# # num_rec_cens_prob <- d_fit_rec_cens$ageweek_recap - d_fit_rec_cens$left_age
# # rec_cens_prob <- matrix(0,
# #                         nrow = n_fit_rec_cens,
# #                         ncol = max(num_rec_cens_prob))
# # for(i in 1:n_fit_rec_cens){
# #     rec_cens_prob[i,1:num_rec_cens_prob[i]] <- rep(1/num_rec_cens_prob[i],num_rec_cens_prob[i])
# # }
# # rec_cens_age_add_init <- c()
# # for(i in 1:n_fit_rec_cens) {
# #   rec_cens_age_add_init[i]  <- rcat(1,prob=rec_cens_prob[i,1:num_rec_cens_prob[i]])
# # }

# # #making a matrix to feed into data for setting size of these probability vectors
# # #because this is a ragged array
# # rec_cens_prob_init <- rec_cens_prob
# # for(i in 1:n_fit_rec_cens){
# #   rec_cens_prob_init[i,which(rec_cens_prob[i,] != 0)] <- NA
# # }

# # #############################################################
# # ###
# # ### 1 individual with censor only contribution
# # ### preliminary values to draw recapture infection week
# # ###
# # #############################################################

# # num_rec_censonly_prob <- d_fit_rec_censonly$ageweek_recap - d_fit_rec_censonly$left_age
# # rec_censonly_prob <- rep(1/num_rec_censonly_prob,num_rec_censonly_prob)
# # rec_censonly_age_add_init <- rcat(1,prob=rec_censonly_prob)

# # #index to assign the drawn age of infection to left age
# # rec_la_indx <- seq(1,14,by=2)
# # d_fit_rec$left_age[rec_la_indx] <- NA

# # #############################################################
# # ###
# # ### Preliminary values to draw week of transition to infected
# # ### For individuals that were test - at capture and test +
# # ### at mortality
# # #############################################################

# # #the number of periods that the individual was alive,
# # #in study, and could transition to infected
# # num_idead_prob <- d_fit_idead_cens$right_age - d_fit_idead_cens$left_age
# # d_fit_idead_cens$lowtag[num_idead_prob < 5]
# # d_fit_idead <- d_fit_idead[!(d_fit_idead$lowtag %in% d_fit_idead_cens$lowtag[num_idead_prob < 5]),]
# # d_fit_idead_cens <- d_fit_idead_cens[!(d_fit_idead_cens$lowtag %in% d_fit_idead_cens$lowtag[num_idead_prob < 5]),]
# # n_fit_idead_cens <- nrow(d_fit_idead_cens)
# # n_fit_idead <- nrow(d_fit_idead)
# # num_idead_prob <- d_fit_idead_cens$right_age - d_fit_idead_cens$left_age

# idead_prob <- matrix(0,
#                         nrow = n_fit_idead_cens,
#                         ncol = max(num_idead_prob))
# for(i in 1:n_fit_idead_cens){
#     idead_prob[i,1:num_idead_prob[i]] <- rep(1/num_idead_prob[i],num_idead_prob[i])
# }
# idead_age_add_init <- c()
# for(i in 1:n_fit_idead_cens) {
#   idead_age_add_init[i]  <- rcat(1, prob = idead_prob[i, 1:num_idead_prob[i]])
# }

# #index to assign the drawn age of infection from left age
# idead_indx <- which(!duplicated(d_fit_idead$lowtag))
# idead_indx_mort <- which(duplicated(d_fit_idead$lowtag))
# idead_left_age <- d_fit_idead$left_age
# idead_left_age[idead_indx] <- NA
# idead_left_age_init <- d_fit_idead$left_age
# idead_left_age_init[idead_indx_mort] <- NA
# idead_cens_age2date <- d_fit_idead_cens$left_period - d_fit_idead_cens$left_age




