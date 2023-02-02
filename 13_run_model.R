

############################################################################################
############################################################################################
############################################################################################
###
### Model Statement
###
############################################################################################
############################################################################################
############################################################################################

modelcode <- nimbleCode({


  ##############################
  ### Priors
  ##############################
  
  beta_sex ~ dnorm(0, .01)

  ##############################
  ### Force of infection model
  ##############################
  mprec  ~ dgamma(1, 1)
  fprec  ~ dgamma(1, 1)
  mprec1 <- 0.0000001 * mprec
  fprec1 <- 0.0000001 * fprec
  m_age_foi[1] ~ dnorm(0, mprec1)
  f_age_foi[1] ~ dnorm(0, fprec1)
  m_age_foi[2] ~ dnorm(0, mprec1)
  f_age_foi[2] ~ dnorm(0, fprec1)
  for (i in 3:n_agem) {
    m_age_foi[i]~dnorm(2 * m_age_foi[i-1] - m_age_foi[i-2], mprec)
  }
  for (i in 3:n_agef) {
    f_age_foi[i]~dnorm(2 * f_age_foi[i-1] - f_age_foi[i-2], fprec)
  }
  m_age_foi_mu <- mean(m_age_foi[1:n_agem])
  f_age_foi_mu <- mean(f_age_foi[1:n_agef])

  # Period effects
  tmprec  ~ dgamma(1, 1)
  tfprec  ~ dgamma(1, 1)

  f_period_foi[1:n_period] ~ dcar_normal(adj = adj_period[1:n_adj_period],
                                  weights = weights_period[1:n_adj_period],
                                  num = num_period[1:n_period],
                                  tau = tfprec,
                                  zero_mean = 1)
  
  m_period_foi[1:n_period] ~ dcar_normal(adj = adj_period[1:n_adj_period],
                                  weights = weights_period[1:n_adj_period],
                                  num = num_period[1:n_period],
                                  tau = tmprec,
                                  zero_mean = 1)

  #Beseg-York-Mollie-2 Spatial model
  # phi[1:n_sect] ~ dcar_normal(sadj[1:n_adj_sp],
  #                             sweights[1:n_adj_sp],
  #                             snum[1:n_sect],
  #                             1,
  #                             zero_mean = 1) #sum to zero constraint
  # hprec ~ dgamma(1, 1)
  # # hsd ~ T(dnorm(1, 1), 0, 10)
  # hprec1 <- hprec * 0.0000001
  # sprec ~ dgamma(1, 1)  
  # rho ~ dbeta(1, 1)# mixing parameter
  # for (j in 1:n_sect){
  #   hetero[j]~dnorm(0, hprec1)
  #   space[j] <- (1 / sqrt(sprec)) * (sqrt((1 - rho)) * hetero[j] +
  #                sqrt(rho / scale) * phi[j])
  # }

  ############################################################
  ############################################################
  ### Age/period Survival Model
  ############################################################
  ############################################################

  ##############################
  ### Susceptibles
  ##############################

  #Priors for intercept and covariate
  beta0_sus_temp ~ dnorm(0, .01)
  sus_mix ~ dunif(-1, 1)
  beta0_sus <- beta0_sus_temp * sus_mix

  #Priors for Age and Period effects
  #Age effects
  for (k in 1:nknots_age) {
    b_age[k] ~ dnorm(0, tau_age)
  }
  tau_age ~ dgamma(1, 1)

  for (t in 1:nT_age_surv) {
    age_effect_survival[t] <- inprod(b_age[1:nknots_age],
                            Z_age[t, 1:nknots_age])
  }

  #Period effects
  for (k in 1:nknots_period) {
    b_period[k] ~ dnorm(0, tau_period)
  }
  tau_period ~ dgamma(1, 1)
  for (t in 1:nT_period_surv) {
    period_effect_surv[t] <- inprod(b_period[1:nknots_period],
                               Z_period[t, 1:nknots_period])
  }

  ##################################
  ## Infected survival intercept
  ##################################

  #Priors for intercept and covariate
  beta0_inf_temp ~ dnorm(0, .01)
  inf_mix ~ dunif(-1, 1)
  beta0_inf <- beta0_inf_temp * inf_mix

  ############################################################
  ## setting up period effects across full study timeline
  ############################################################

  period_effect_survival[(nT_period_presurv + 1):nT_overall] <- period_effect_surv[1:nT_period_surv]

  #######################################################################
  #######################################################################
  ## Likelihoods of Joint Model
  #######################################################################
  #######################################################################

  #######################################################################
  ###
  ###   User defined distribution for likelihood for
  ###   infected harvest deer
  ###
  ###   d_fit_hunt_pos
  ###   Overleaf Equation 3
  ###
  #######################################################################

  # for (i in 1:nInfHarvest) {
  #   y_hunt_pos[i] ~ dInfHarvest(
  #                 a = hunt_pos_ageweeks[i], #age (weeks) at harvest
  #                 sex = hunt_pos_sex[i],
  #                 age2date = hunt_pos_age2date[i],
  #                 beta_sex = beta_sex,
  #                 beta0_sus = beta0_sus,
  #                 beta0_inf = beta0_inf,
  #                 age_effect_surv = age_effect_survival[1:nT_age_surv],
  #                 period_effect_surv = period_effect_survival[1:nT_overall],
  #                 f_age_foi = f_age_foi[1:n_agef],
  #                 m_age_foi = m_age_foi[1:n_agem],
  #                 age_lookup_f = age_lookup_f[1:n_age_lookup_f],
  #                 age_lookup_m = age_lookup_m[1:n_age_lookup_m],
  #                 period_lookup = period_lookup[1:n_period_lookup],
  #                 f_period_foi = f_period_foi[1:n_period],
  #                 m_period_foi = m_period_foi[1:n_period],
  #                 space = space[sect_hunt_pos[i]]
  #                 )
  # }

#   y_hunt_pos[1:nInfHarvest] ~ dInfHarvest(n_samples = nInfHarvest,
# 				          a = hunt_pos_ageweeks[1:nInfHarvest], #age (weeks) at harvest
#                   sex = hunt_pos_sex[1:nInfHarvest],
#                   age2date = hunt_pos_age2date[1:nInfHarvest],
#                   beta_sex = beta_sex,
#                   beta0_sus = beta0_sus,
#                   beta0_inf = beta0_inf,
#                   age_effect_surv = age_effect_survival[1:nT_age_surv],
#                   period_effect_surv = period_effect_survival[1:nT_overall],
#                   f_age_foi = f_age_foi[1:n_agef],
#                   m_age_foi = m_age_foi[1:n_agem],
#                   age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#                   age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#                   period_lookup = period_lookup[1:n_period_lookup],
#                   f_period_foi = f_period_foi[1:n_period],
#                   m_period_foi = m_period_foi[1:n_period],
#                   space = space[1:n_sect],
#                   sect = sect_hunt_pos[1:nInfHarvest]
#                   )

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   uninfected harvest deer
# ###   d_fit_hunt_neg
# ###   Overleaf Equation (5)
# ###
# #######################################################################

# # for (i in 1:nSusHarvest) {
# #     y_hunt_neg[i] ~  dSusHarvest(
# #         a = hunt_neg_ageweeks[i],
# #         sex = hunt_neg_sex[i],
# #         age2date = hunt_neg_age2date[i],
# #         beta_sex = beta_sex,
# #         beta0_sus = beta0_sus,
# #         age_effect_surv = age_effect_survival[1:nT_age_surv],
# #         period_effect_surv = period_effect_survival[1:nT_overall],
# #         f_age_foi = f_age_foi[1:n_agef],
# #         m_age_foi = m_age_foi[1:n_agem],
# #         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
# #         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
# #         period_lookup = period_lookup[1:n_period_lookup],
# #         f_period_foi = f_period_foi[1:n_period],
# #         m_period_foi = m_period_foi[1:n_period],
# #         space = space[sect_hunt_neg[i]]
# #         )
# # }

#   y_hunt_neg[1:nSusHarvest] ~ dSusHarvest(n_samples = nSusHarvest,
# 				          a = hunt_neg_ageweeks[1:nSusHarvest], #age (weeks) at harvest
#                   sex = hunt_neg_sex[1:nSusHarvest],
#                   age2date = hunt_neg_age2date[1:nSusHarvest],
#                   beta_sex = beta_sex,
#                   beta0_sus = beta0_sus,
#                   age_effect_surv = age_effect_survival[1:nT_age_surv],
#                   period_effect_surv = period_effect_survival[1:nT_overall],
#                   f_age_foi = f_age_foi[1:n_agef],
#                   m_age_foi = m_age_foi[1:n_agem],
#                   age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#                   age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#                   period_lookup = period_lookup[1:n_period_lookup],
#                   f_period_foi = f_period_foi[1:n_period],
#                   m_period_foi = m_period_foi[1:n_period],
#                   space = space[1:n_sect],
#                   sect = sect_hunt_neg[1:nSusHarvest]
#                   )

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   Uninfected radio-marked deer right censor:
# ###   Test neg at cap and censoring
# ###
# ###   d_fit_sus_cens_posttest
# ###   Overleaf Equation 7
# ###
# #######################################################################


# for (i in 1:nSusCensTest) {
#     y_sus_cens_posttest[i] ~ dSusCensTest(
#         e = sus_cens_posttest_left_age_e[i],
#         r = sus_cens_posttest_right_age_r[i],
#         sex = sus_cens_posttest_sex[i],
#         age2date = sus_cens_posttest_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_sus_cens_posttest[i]]
#         )
#   }


# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   Uninfected radio-marked deer right censored:
# ###   Test neg at cap and censoring
# ###
# ###   d_fit_sus_cens_postno
# ###   d_fit_endlive
# ###
# ###   Overleaf Equation (9)
# ###
# #######################################################################

# for (i in 1:nSusCensNo) {
#     y_sus_cens_postno[i] ~ dSusCensNo(
#         e = sus_cens_postno_left_age_e[i],
#         r = sus_cens_postno_right_age_r[i],
#         sex = sus_cens_postno_sex[i],
#         age2date = sus_cens_postno_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         beta0_inf = beta0_inf,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_sus_cens_postno[i]]
#         )
#   }

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   uninfected radio-marked deer mortalities:
# ###   test neg at cap and tested mort
# ###
# ###   d_fit_sus_mort_posttest
# ###
# ###   Overleaf Equation (11)
# ###
# #######################################################################

# for (i in 1:nSusMortTest) {
#     y_sus_mort_posttest[i] ~ dSusMortTest(
#         e = sus_mort_posttest_left_age_e[i],
#         r = sus_mort_posttest_right_age_r[i],
#         s = sus_mort_posttest_right_age_s[i],
#         sex = sus_mort_posttest_sex[i],
#         fast = sus_mort_posttest_fast[i],
#         age2date = sus_mort_posttest_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_sus_mort_posttest[i]]
#         )
#   }

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   uninfected radio-marked deer mortalities:
# ###   test neg at cap and no test at mortality
# ###
# ###   d_fit_sus_mort_postno
# ###
# ###   Overleaf Equation (13)
# ###
# #######################################################################


# for (i in 1:nSusMortNoTest) {
#     y_sus_mort_postno[i] ~ dSusMortNoTest(
#         e = sus_mort_postno_left_age_e[i],
#         r = sus_mort_postno_right_age_r[i],
#         s = sus_mort_postno_right_age_s[i],
#         dn1 = sus_mort_postno_dn1[i],
#         sex = sus_mort_postno_sex[i],
#         age2date = sus_mort_postno_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         beta0_inf = beta0_inf,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_sus_mort_postno[i]]
#         )
#   }

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   infected deer mortalities for radio marked deer that
# ###   enter the study as test positive at capture
# ###
# ###   d_fit_icap_cens
# ###
# ###   Overleaf Equation (15)
# ###
# #######################################################################

# for (i in 1:nIcapCens) {
#     y_icap_cens[i] ~ dIcapCens(
#         e = icap_cens_left_age_e[i],
#         r = icap_cens_right_age_r[i],
#         sex = icap_cens_sex[i],
#         age2date = icap_cens_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         beta0_inf = beta0_inf,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_icap_cens[i]]
#         )
#   }

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   infected deer mortalities for radio marked deer that
# ###   enter the study as test positive at capture
# ###
# ###   d_fit_icap_mort
# ###
# ###   Overleaf Equation (17)
# ###
# #######################################################################

# for (i in 1:nIcapMort) {
#     y_icap_mort[i] ~ dIcapMort(
#         e = icap_mort_left_age_e[i],
#         r = icap_mort_right_age_r[i],
#         s = icap_mort_right_age_s[i],
#         sex = icap_mort_sex[i],
#         fast = icap_mort_fast[i],
#         age2date = icap_mort_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         beta0_inf = beta0_inf,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_icap_mort[i]]
#         )
#   }

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   uninfected deer that were test neg at capture,
# ###   then test negative at recap, that are right censored, 
# ###   and have been tested post censoring
# ###
# ###   d_fit_rec_neg_cens_posttest
# ###
# ###   Overleaf Equation (19)
# ###
# #######################################################################


# for (i in 1:nRecNegCensTest) {
#     y_rec_neg_cens_posttest[i] ~ dRecNegCensTest(
#         e = rec_neg_cens_posttest_left_age_e[i],
#         r = rec_neg_cens_posttest_right_age_r[i],
#         sex = rec_neg_cens_posttest_sex[i],
#         age2date = rec_neg_cens_posttest_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         beta0_inf = beta0_inf,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_rec_neg_cens_posttest[i]]
#         )
#   }


# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   uninfected deer that were test neg at capture,
# ###   then test negative at recap,
# ###   that die
# ###
# ###
# ###   d_fit_rec_neg_mort
# ###
# ###   Overleaf Equation (23)
# ###
# #######################################################################


# for (i in 1:nRecNegMort) {
#     y_rec_neg_mort[i] ~ dRecNegMort(
#         e = rec_neg_mort_left_age_e[i],
#         r = rec_neg_mort_right_age_r[i],
#         s = rec_neg_mort_right_age_s[i],
#         sex = rec_neg_mort_sex[i],
#         age2date = rec_neg_mort_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_rec_neg_mort[i]]
#         )
#   }

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   deer that were test neg at capture,
# ###   then test positive at recap,
# ###   than die
# ###
# ###   d_fit_rec_pos_mort
# ###
# ###   Overleaf Equation (25)
# ###
# #######################################################################

# for (i in 1:nRecPosMort) {
#     y_rec_pos_mort[i] ~ dRecPosMort(
#         e = rec_pos_mort_left_age_e[i],
#         r = rec_pos_mort_right_age_r[i],
#         s = rec_pos_mort_right_age_s[i],
#         dn1 = rec_pos_mort_dn1[i],
#         dn = rec_pos_mort_dn[i],
#         sex = rec_pos_mort_sex[i],
#         age2date = rec_pos_mort_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         beta0_inf = beta0_inf,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_rec_pos_mort[i]]
#         )
#   }

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   infected deer that were test neg at capture,
# ###   then test positive at recap,
# ###   that are right censored
# ###
# ###   d_fit_rec_pos_cens
# ###
# ###   Overleaf Equation (27)
# ###
# #######################################################################

#   y_rec_pos_cens ~ dRecPosCens(
#       e = rec_pos_cens_left_age_e,
#       r = rec_pos_cens_right_age_r,
#       dn1 = rec_pos_cens_dn1,
#       dn = rec_pos_cens_dn,
#       sex = rec_pos_cens_sex,
#       age2date = rec_pos_cens_age2date,
#       beta_sex = beta_sex,
#       beta0_sus = beta0_sus,
#       beta0_inf = beta0_inf,
#       age_effect_surv = age_effect_survival[1:nT_age_surv],
#       period_effect_surv = period_effect_survival[1:nT_overall],
#       f_age_foi = f_age_foi[1:n_agef],
#       m_age_foi = m_age_foi[1:n_agem],
#       age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#       age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#       period_lookup = period_lookup[1:n_period_lookup],
#       f_period_foi = f_period_foi[1:n_period],
#       m_period_foi = m_period_foi[1:n_period],
#       space = space[sect_rec_pos_cens]
#   )

# #######################################################################
# ###
# ###   User defined distribution for likelihood for
# ###   infected deer mortalities for radio marked deer that
# ###   enter the study as test negative at capture
# ###
# ###   d_fit_idead
# ###
# ###   Overleaf Equation (29)
# ###
# #######################################################################


# for (i in 1:nNegCapPosMort) {
#     y_idead[i] ~ dNegCapPosMort(
#         e = idead_left_age_e[i],
#         r = idead_right_age_r[i],
#         s = idead_right_age_s[i],
#         dn1 = idead_dn1[i],
#         dn = idead_dn[i],
#         sex = idead_sex[i],
#         age2date = idead_age2date[i],
#         beta_sex = beta_sex,
#         beta0_sus = beta0_sus,
#         beta0_inf = beta0_inf,
#         age_effect_surv = age_effect_survival[1:nT_age_surv],
#         period_effect_surv = period_effect_survival[1:nT_overall],
#         f_age_foi = f_age_foi[1:n_agef],
#         m_age_foi = m_age_foi[1:n_agem],
#         age_lookup_f = age_lookup_f[1:n_age_lookup_f],
#         age_lookup_m = age_lookup_m[1:n_age_lookup_m],
#         period_lookup = period_lookup[1:n_period_lookup],
#         f_period_foi = f_period_foi[1:n_period],
#         m_period_foi = m_period_foi[1:n_period],
#         space = space[sect_idead[i]]
#         )
#   }

#######################################################################
###
###   User defined distribution for likelihood for
###   age-at-harvest deer used to estimate period effects
###
###   d_aah
###
###   Overleaf Equation (31)
###
#######################################################################


for (i in 1:nAAH) {
  y_aah[i] ~ dAAH(
        a = aah_ageweeks[i],
        sex = aah_sex[i],
        age2date = aah_age2date[i],
        n_ind = aah_n[i],
        beta_sex = beta_sex,
        beta0_sus = beta0_sus,
        beta0_inf = beta0_inf,
        age_effect_surv = age_effect_survival_test,
        period_effect_surv = period_effect_survival_test,
        f_age_foi = f_age_foi,
        m_age_foi = m_age_foi,
        age_lookup_f = age_lookup_col_f,
        age_lookup_m = age_lookup_col_m,
        period_lookup = period_lookup,
        f_period_foi = f_period_foi,
        m_period_foi = m_period_foi
        )
}

})#end model statement

##############################################################################
##############################################################################
##############################################################################
#######################################
### Data for Model Fitting
#######################################

nimData <- list(Z_period = Z_period,
                Z_age = Z_age,
                num_period = num_period,
                adj_period = adj_period,
                weights_period = weights_period,
                age_lookup_f = age_lookup_f,
                age_lookup_m = age_lookup_m,
                period_effect_survival = period_effect_survival,
                y_hunt_pos = rep(1, nrow(d_fit_hunt_pos)),
                hunt_pos_ageweeks = d_fit_hunt_pos$ageweeks,
                hunt_pos_sex = d_fit_hunt_pos$sex,
                hunt_pos_age2date = d_fit_hunt_pos$birthweek - 1,
                y_hunt_neg = rep(1, nrow(d_fit_hunt_neg)),
                hunt_neg_ageweeks = d_fit_hunt_neg$ageweeks,
                hunt_neg_sex = d_fit_hunt_neg$sex,
                hunt_neg_age2date = d_fit_hunt_neg$birthweek - 1,
                y_sus_cens_posttest = rep(1, nrow(d_fit_sus_cens_posttest)),
                sus_cens_posttest_left_age_e = d_fit_sus_cens_posttest$left_age_e,
                sus_cens_posttest_right_age_r = d_fit_sus_cens_posttest$right_age_r,
                sus_cens_posttest_sex = d_fit_sus_cens_posttest$sex,
                sus_cens_posttest_age2date = sus_cens_posttest_age2date,
                y_sus_cens_postno = rep(1, nrow(d_fit_sus_cens_postno) + nrow(d_fit_endlive)),
                sus_cens_postno_left_age_e = c(d_fit_sus_cens_postno$left_age_e, d_fit_endlive$left_age_e),
                sus_cens_postno_right_age_r = c(d_fit_sus_cens_postno$right_age_r, d_fit_endlive$right_age_r),
                sus_cens_postno_sex = c(d_fit_sus_cens_postno$sex, d_fit_endlive$sex),
                sus_cens_postno_age2date = c(sus_cens_postno_age2date, endlive_age2date),
                y_sus_mort_posttest = rep(1, nrow(d_fit_sus_mort_posttest)),
                sus_mort_posttest_left_age_e = d_fit_sus_mort_posttest$left_age_e,
                sus_mort_posttest_right_age_r = d_fit_sus_mort_posttest$right_age_r,
                sus_mort_posttest_right_age_s = d_fit_sus_mort_posttest$right_age_s,
                sus_mort_posttest_fast = d_fit_sus_mort_posttest$fast,
                sus_mort_posttest_sex = d_fit_sus_mort_posttest$sex,
                sus_mort_posttest_age2date = sus_mort_posttest_age2date,
                y_sus_mort_postno = rep(1, nrow(d_fit_sus_mort_postno)),
                sus_mort_postno_left_age_e = d_fit_sus_mort_postno$left_age_e,
                sus_mort_postno_right_age_r = d_fit_sus_mort_postno$right_age_r,
                sus_mort_postno_right_age_s = d_fit_sus_mort_postno$right_age_s,
                sus_mort_postno_dn1 = d_fit_sus_mort_postno$left_age_e,
                sus_mort_postno_sex = d_fit_sus_mort_postno$sex,
                sus_mort_postno_age2date = sus_mort_postno_age2date,
                y_icap_cens = rep(1, nrow(d_fit_icap_cens)),
                icap_cens_left_age_e = d_fit_icap_cens$left_age_e,
                icap_cens_right_age_r = d_fit_icap_cens$right_age_r,
                icap_cens_sex = d_fit_icap_cens$sex,
                icap_cens_age2date = icap_cens_age2date,
                y_icap_mort = rep(1, nrow(d_fit_icap_mort)),
                icap_mort_left_age_e = d_fit_icap_mort$left_age_e,
                icap_mort_right_age_r = d_fit_icap_mort$right_age_r,
                icap_mort_right_age_s = d_fit_icap_mort$right_age_s,
                icap_mort_sex = d_fit_icap_mort$sex,
                icap_mort_fast = d_fit_icap_mort$fast,
                icap_mort_age2date = icap_mort_age2date,
                y_rec_neg_cens_posttest = rep(1, nrow(d_fit_rec_neg_cens_posttest)),
                rec_neg_cens_posttest_left_age_e = d_fit_rec_neg_cens_posttest$left_age_e,
                rec_neg_cens_posttest_right_age_r = d_fit_rec_neg_cens_posttest$right_age_r,
                rec_neg_cens_posttest_sex = d_fit_rec_neg_cens_posttest$sex,
                rec_neg_cens_posttest_age2date = rec_neg_cens_posttest_age2date,
                y_rec_neg_mort = rep(1, nrow(d_fit_rec_neg_mort)),
                rec_neg_mort_left_age_e = d_fit_rec_neg_mort$left_age_e,
                rec_neg_mort_right_age_r = d_fit_rec_neg_mort$right_age_r,
                rec_neg_mort_right_age_s = d_fit_rec_neg_mort$right_age_s,
                rec_neg_mort_sex = d_fit_rec_neg_mort$sex,
                rec_neg_mort_age2date = rec_neg_mort_age2date,
                y_rec_pos_mort = rep(1, nrow(d_fit_rec_pos_mort)),
                rec_pos_mort_left_age_e = d_fit_rec_pos_mort$left_age_e,
                rec_pos_mort_right_age_r = d_fit_rec_pos_mort$right_age_r,
                rec_pos_mort_right_age_s = d_fit_rec_pos_mort$right_age_s,
                rec_pos_mort_dn1 = d_fit_rec_pos_mort$left_age_e,
                rec_pos_mort_dn = d_fit_rec_pos_mort$ageweek_recap,
                rec_pos_mort_sex = d_fit_rec_pos_mort$sex,
                rec_pos_mort_age2date = rec_pos_mort_age2date,
                y_rec_pos_cens = rep(1, nrow(d_fit_rec_pos_cens)),
                rec_pos_cens_left_age_e = d_fit_rec_pos_cens$left_age_e,
                rec_pos_cens_right_age_r = d_fit_rec_pos_cens$right_age_r,
                rec_pos_cens_dn1 = d_fit_rec_pos_cens$ageweek_recap,
                rec_pos_cens_dn = d_fit_rec_pos_cens$right_age_r,
                rec_pos_cens_sex = d_fit_rec_pos_cens$sex,
                rec_pos_cens_age2date = rec_pos_cens_age2date,
                y_idead = rep(1, nrow(d_fit_idead)),
                idead_left_age_e = d_fit_idead$left_age_e,
                idead_right_age_r = d_fit_idead$right_age_r,
                idead_right_age_s = d_fit_idead$right_age_s,
                idead_dn1 = d_fit_idead$left_age_e,
                idead_dn = d_fit_idead$right_age_s,
                idead_sex = d_fit_idead$sex,
                idead_age2date = idead_age2date,
                y_aah = rep(1, nrow(d_fit_age_nocwd)),
                aah_ageweeks = d_fit_age_nocwd$ageweeks,
                aah_sex = d_fit_age_nocwd$sexnum,
                aah_age2date = d_fit_age_nocwd$age2date_weeks,
                aah_n = d_fit_age_nocwd$n
                )


#######################################
### Constants for MCMC
#######################################

nimConsts <- list(nT_overall = nT_overall,
                  nT_period_presurv = nT_period_presurv,
                  nknots_age = nknots_age,
                  nknots_period = nknots_period,
                  nT_age_surv = nT_age_surv,
                  nT_period_surv = nT_period_surv,
                  n_period_lookup = n_period_lookup,
                  n_agef = n_agef,
                  n_agem = n_agem,
                  n_period = n_period,
                  n_adj_period = n_adj_period,
                  # n_adj_sp = n_adj_sp,
                  n_sect = n_sect,
                  # scale = scale,
                  space = rep(0, n_sect),
                  n_age_lookup_f = length(age_lookup_f),
                  n_age_lookup_m = length(age_lookup_m),
                  period_lookup = period_lookup,
                  nInfHarvest = nrow(d_fit_hunt_pos),
                  nSusHarvest = nrow(d_fit_hunt_neg),
                  nSusCensTest = nrow(d_fit_sus_cens_posttest),
                  nSusCensNo = nrow(d_fit_sus_cens_postno) + nrow(d_fit_endlive),
                  nSusMortTest = nrow(d_fit_sus_mort_posttest),
                  nSusMortNoTest = nrow(d_fit_sus_mort_postno),
                  nIcapCens = nrow(d_fit_icap_cens),
                  nIcapMort = nrow(d_fit_icap_mort),
                  nRecNegCensTest = nrow(d_fit_rec_neg_cens_posttest),
                  nRecNegMort = nrow(d_fit_rec_neg_mort),
                  nRecPosMort = nrow(d_fit_rec_pos_mort),
                  nNegCapPosMort = nrow(d_fit_idead),
                  nAAH = nrow(df_age_nocwd),
                  sect_hunt_pos = sect_hunt_pos,
                  sect_hunt_neg = sect_hunt_neg,
                  sect_sus_cens_posttest = d_fit_sus_cens_posttest$sect,
                  sect_sus_cens_postno = c(d_fit_sus_cens_postno$sect,
                                          d_fit_endlive$sect),
                  sect_sus_mort_posttest = d_fit_sus_mort_posttest$sect,
                  sect_sus_mort_postno = d_fit_sus_mort_postno$sect,
                  sect_icap_cens = d_fit_icap_cens$sect,
                  sect_icap_mort = d_fit_icap_mort$sect,
                  sect_rec_neg_cens_posttest = d_fit_rec_neg_cens_posttest$sect,
                  sect_rec_neg_mort = d_fit_rec_neg_mort$sect,
                  sect_rec_pos_mort = d_fit_rec_pos_mort$sect,
                  sect_rec_pos_cens = d_fit_rec_pos_cens$sect,
                  sect_idead = d_fit_idead$sect
                  )


#######################################
### Initial Values for MCMC
#######################################
initsFun <- function()list(
                          beta_sex = rnorm(1, -.5, .01),
                          beta0_sus_temp = rnorm(1, -5.5, 0.0001),
                          sus_mix = 1,
                          beta0_inf_temp = rnorm(1, -4, 0.0001),
                          inf_mix = 1,
                          b_age = rnorm(nknots_age) * 10^-4,
                          b_period = rnorm(nknots_period) * 10^-4,
                          tau_period = runif(1, .1, 1),
                          tau_age = runif(1, .1, .4),
                          # hprec = runif(1, .01, 1),
                          # sprec = runif(1, 3, 4),
                          mprec = runif(1, 1.5, 1.7),
                          fprec = runif(1, 2.7, 4.2),
                          # tfsd = runif(1,1,2),
                          # tmsd = runif(1,3,4),
                          tmprec = runif(1, 4.2, 6.8),
                          tfprec = runif(1, 2.27, 3.44),
                          # phi = seq(.0001, .0005, length = n_sect),
                          # hetero = seq(.0001, .0005, length = n_sect),
                          # rho = rbeta(1, 1, 1),
                          m_period_foi = seq(-2, 2, length = n_period),
                          f_period_foi = seq(-2, 2, length = n_period),
                          m_age_foi = seq(-6, -4, length = n_agem),
                          f_age_foi = seq(-7, -5, length = n_agef)
                          )
nimInits <- initsFun()

# start_Rmodel <- Sys.time()
Rmodel <- nimbleModel(code = modelcode,
                      constants = nimConsts,
                      data = nimData,
                      inits = initsFun(),
                      calculate = FALSE,
                      check = FALSE
                      )
# end_Rmodel <- Sys.time() - start_Rmodel
# Rmodel$initializeInfo()
for(i in 1:10){beepr::beep(1)}

#######################################
### Parameters to trace in MCMC
#######################################

parameters <- c(
              "beta_sex",
              "beta0_sus",
              "beta0_inf",
              "age_effect_survival",
              "period_effect_surv",
              "tau_period",
              "tau_age",
              # "hprec",
              # "sprec",
             "f_age_foi",
              "m_age_foi",
              "mprec",
              "fprec",
              # "m_age_mu",
              # "f_age_mu",
              "f_period_foi",
              "m_period_foi",
              "tmprec",
              # "tmsd",
              "tfprec"
              # "tfsd",
              # "space",
              # "hetero",
              # "phi",
              # "rho"
               )

confMCMC <- configureMCMC(Rmodel,
                         monitors = parameters,
                         thin = 1,
                         # enableWAIC = TRUE,
                         useConjugacy = FALSE)
nimMCMC <- buildMCMC(confMCMC)
Cnim <- compileNimble(Rmodel)
CnimMCMC <- compileNimble(nimMCMC,
                         project = Rmodel)
for(i in 1:10){beepr::beep(1)}
set.seed(7654321)
starttime <- Sys.time()
mcmcout <- runMCMC(CnimMCMC,
                  niter = 1000,
                  nburnin = 0,
                  nchains = 1,
                  inits = initsFun,
                  samplesAsCodaMCMC = TRUE,
                  summary = TRUE
                  )
runtime <- difftime(Sys.time(),
                    starttime,
                    units = "min")
for (i in 1:10) {beepr::beep(1)}


# end_Rmodel
# endtime_rmodel_compile
# endtime_mcmc
runtime

sink("runtime_allsteps.txt")
# cat("Rmodel:\n")
# end_Rmodel
# cat("\nCompile Rmodel:\n")
# endtime_rmodel_compile
# cat("\nCompile MCMC:\n")
# endtime_mcmc
cat("\nRun MCMC 1000 iter:\n")
runtime
sink()

# reps  <- 2000
# bin <- reps * .5
# n_thin <- 1
# n_chains <- 3
# starttime <- Sys.time()
# cl <- makeCluster(n_chains)
# clusterExport(cl, c("modelcode",
#                     "initsFun",
#                     "nimData",
#                     "nimConsts",
#                     "parameters",
#                     "reps",
#                     "bin",
#                     "n_thin",
#                     "dSurvival",
#                     "dFOIcollar",
#                     "dFOIinf",
#                     "dFOIhunt",
#                     # "dSurvival_icap",
#                     "calc_age_inf_rec",
#                     "calc_age_inf_idead",
#                     "calc_age_inf_icap"
#                     ))
# for (j in seq_along(cl)) {
#   set.seed(j+1000)
#   init <- initsFun()
#   clusterExport(cl[j], "init")
# }
# mcmcout <-  mcmc.list(clusterEvalQ(cl, {
#   library(nimble)
#   library(coda)

#   Ccalc_age_inf_rec <- compileNimble(calc_age_inf_rec)
#   Ccalc_age_inf_idead <- compileNimble(calc_age_inf_idead)
#   Ccalc_age_inf_icap <- compileNimble(calc_age_inf_icap)
#   # Cpo_indx_fun <- compileNimble(po_indx_fun)
#   # Cpr_indx_fun <- compileNimble(pr_indx_fun)

#   ##############################################################
#   ###
#   ### Execute MCMC
#   ###
#   ##############################################################

#   Rmodel <- nimbleModel(code = modelcode,
#                         name = "modelcode",
#                         constants = nimConsts,
#                         data = nimData,
#                         inits = init)
#   confMCMC <- configureMCMC(Rmodel,
#                             monitors = parameters,
#                             thin = n_thin,
#                             useConjugacy = FALSE)
#   nimMCMC <- buildMCMC(confMCMC)
#   Cnim <- compileNimble(Rmodel)
#   CnimMCMC <- compileNimble(nimMCMC,
#                             project = Rmodel)
#   mcmcout <- runMCMC(CnimMCMC,
#                      niter = reps,
#                      nburnin = bin,
#                      samplesAsCodaMCMC = TRUE)

#   return(mcmcout)
# }))
# runtime <- difftime(Sys.time(), starttime, units = "min")
# stopCluster(cl)

save(mcmcout, file = "mcmcout.Rdata")
save(runtime, file = "runtime.Rdata")
# save(endtime_rmodel_compile, file = "endtime_rmodel_compile.Rdata")
# save(endtime_mcmc, file = "endtime_mcmc.Rdata")

#not calculating waic, because too many params would need to be traced
# posteriorSamplesMatrix <- rbind(mcmcout[[1]], mcmcout[[2]], mcmcout[[3]])
# CnimMCMC$run(5)   ## non-zero number of iterations
# nimble:::matrix2mv(posteriorSamplesMatrix, CnimMCMC$mvSamples)
# # CnimMCMC$enableWAIC <- TRUE
# waic_spline <- calculateWAIC(posteriorSamplesMatrix,Rmodel)


# waic_spline_covs <- mcmcout$WAIC
# save(waic_spline, file = "waic_spline.Rdata")



###
### save model run
###

# save(runtime,file="results/runtime.Rdata")
# save(mcmcout,file="results/mcmcout.Rdata")