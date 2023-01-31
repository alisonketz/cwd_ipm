#######################################################################
###
###   Likelihoods for each kind of data listed below
###
#######################################################################

# d_fit_hunt_neg
# d_fit_hunt_pos
# d_fit_sus_cens_posttest
# d_fit_sus_cens_postno
# d_fit_sus_mort_posttest
# d_fit_sus_mort_postno
# d_fit_icap_cens
# d_fit_icap_mort
# d_fit_rec_neg_mort
# d_fit_rec_neg_cens_posttest
# d_fit_rec_neg_cens_postno
# d_fit_rec_pos_mort
# d_fit_rec_pos_cens
# d_fit_idead
# d_fit_endlive


###########################################################
### Preliminaries
###########################################################

# rm(list = ls())

# setwd("~/Documents/integrate_s_foi/s_foi_v5/")

# library(viridis)
# library(RColorBrewer)
# library(Hmisc)
# library(lubridate)
# library(readxl)
# library(Matrix)
# library(ggplot2)
# library(gridExtra)
# library(xtable)
# library(nimble)
# library(tidyverse)
# library(dplyr)
# library(lattice)
# library(foreign)
# library(rgdal)
# library(rgeos)
# library(zoo)
# library(spdep)
# library(parallel)
# library(doParallel)
# library(coda)
# library(INLA)
# library(sf)
# library(terra)
# library(splines)
# library(MetBrewer)
# library(ggforce)


#######################################################################
###
### saving cleaned and formatted datafiles
###
#######################################################################

# save(d_fit_hunt_neg,file="datafiles/d_fit_hunt_neg.Rdata")
# save(d_fit_hunt_pos,file="datafiles/d_fit_hunt_pos.Rdata")
# save(d_fit_sus_cens_posttest,file="datafiles/d_fit_sus_cens_posttest.Rdata")
# save(d_fit_sus_cens_postno,file="datafiles/d_fit_sus_cens_postno.Rdata")
# save(d_fit_sus_mort_posttest,file="datafiles/d_fit_sus_mort_posttest.Rdata")
# save(d_fit_sus_mort_postno,file="datafiles/d_fit_sus_mort_postno.Rdata")
# save(d_fit_icap_cens,file="datafiles/d_fit_icap_cens.Rdata")
# save(d_fit_icap_mort,file="datafiles/d_fit_icap_mort.Rdata")
# save(d_fit_rec_neg_mort,file="datafiles/d_fit_rec_neg_mort.Rdata")
# save(d_fit_rec_neg_cens_posttest,file="datafiles/d_fit_rec_neg_cens_posttest.Rdata")
# save(d_fit_rec_neg_cens_postno,file="datafiles/d_fit_rec_neg_cens_postno.Rdata")
# save(d_fit_rec_pos_mort,file="datafiles/d_fit_rec_pos_mort.Rdata")
# save(d_fit_rec_pos_cens,file="datafiles/d_fit_rec_pos_cens.Rdata")
# save(d_fit_idead,file="datafiles/d_fit_idead.Rdata")
# save(d_fit_endlive,file="datafiles/d_fit_endlive.Rdata")
# save(Z_period,file="datafiles/Z_period.Rdata")
# save(Z_age,file="datafiles/Z_age.Rdata")
# save(num_period,file="datafiles/num_period.Rdata")
# save(adj_period,file="datafiles/adj_period.Rdata")
# save(weights_period,file="datafiles/weights_period.Rdata")
# save(age_lookup_f,file="datafiles/age_lookup_f.Rdata")
# save(age_lookup_m,file="datafiles/age_lookup_m.Rdata")
# save(period_effect_survival,file="datafiles/period_effect_survival.Rdata")
# save(sus_cens_posttest_age2date,file="datafiles/sus_cens_posttest_age2date.Rdata")
# save(sus_cens_postno_age2date,file="datafiles/sus_cens_postno_age2date.Rdata")
# save(endlive_age2date,file="datafiles/endlive_age2date.Rdata")
# save(sus_mort_posttest_age2date,file="datafiles/sus_mort_posttest_age2date.Rdata")
# save(sus_mort_postno_age2date,file="datafiles/sus_mort_postno_age2date.Rdata")
# save(icap_cens_age2date,file="datafiles/icap_cens_age2date.Rdata")
# save(icap_mort_age2date,file="datafiles/icap_mort_age2date.Rdata")
# save(rec_neg_cens_posttest_age2date,file="datafiles/rec_neg_cens_posttest_age2date.Rdata")
# save(rec_neg_cens_postno_age2date,file="datafiles/rec_neg_cens_postno_age2date.Rdata")
# save(rec_neg_mort_age2date,file="datafiles/rec_neg_mort_age2date.Rdata")
# save(rec_pos_mort_age2date,file="datafiles/rec_pos_mort_age2date.Rdata")
# save(rec_pos_cens_age2date,file="datafiles/rec_pos_cens_age2date.Rdata")
# save(idead_age2date,file="datafiles/idead_age2date.Rdata")
# save(nT_overall,file="datafiles/nT_overall.Rdata")
# save(nT_period_presurv,file="datafiles/nT_period_presurv.Rdata")
# save(nknots_age,file="datafiles/nknots_age.Rdata")
# save(nknots_period,file="datafiles/nknots_period.Rdata")
# save(nT_age_surv,file="datafiles/nT_age_surv.Rdata")
# save(nT_period_surv,file="datafiles/nT_period_surv.Rdata")
# save(n_period_lookup,file="datafiles/n_period_lookup.Rdata")
# save(n_agef,file="datafiles/n_agef.Rdata")
# save(n_agem,file="datafiles/n_agem.Rdata")
# save(n_period,file="datafiles/n_period.Rdata")
# save(n_adj_period,file="datafiles/n_adj_period.Rdata")
# save(n_adj_sp,file="datafiles/n_adj_sp.Rdata")
# save(n_sect,file="datafiles/n_sect.Rdata") 
# save(scale,file="datafiles/scale.Rdata") 
# save(period_lookup,file="datafiles/period_lookup.Rdata")
# save(age_lookup_col_f,file="datafiles/age_lookup_col_f.Rdata")
# save(age_lookup_col_m,file="datafiles/age_lookup_col_m.Rdata")
# save(sect_hunt_pos,file = "datafiles/sect_hunt_pos.Rdata")
# save(sect_hunt_neg,file = "datafiles/sect_hunt_neg.Rdata")

#######################################################################
###
### loading cleaned and formatted datafiles
###
#######################################################################

# load("datafiles/d_fit_hunt_neg.Rdata")
# load("datafiles/d_fit_hunt_pos.Rdata")
# load("datafiles/d_fit_sus_cens_posttest.Rdata")
# load("datafiles/d_fit_sus_cens_postno.Rdata")
# load("datafiles/d_fit_sus_mort_posttest.Rdata")
# load("datafiles/d_fit_sus_mort_postno.Rdata")
# load("datafiles/d_fit_icap_cens.Rdata")
# load("datafiles/d_fit_icap_mort.Rdata")
# load("datafiles/d_fit_rec_neg_mort.Rdata")
# load("datafiles/d_fit_rec_neg_cens_posttest.Rdata")
# load("datafiles/d_fit_rec_neg_cens_postno.Rdata")
# load("datafiles/d_fit_rec_pos_mort.Rdata")
# load("datafiles/d_fit_rec_pos_cens.Rdata")
# load("datafiles/d_fit_idead.Rdata")
# load("datafiles/d_fit_endlive.Rdata")
# load("datafiles/Z_period.Rdata")
# load("datafiles/Z_age.Rdata")
# load("datafiles/num_period.Rdata")
# load("datafiles/adj_period.Rdata")
# load("datafiles/weights_period.Rdata")
# load("datafiles/age_lookup_f.Rdata")
# load("datafiles/age_lookup_m.Rdata")
# load("datafiles/period_effect_survival.Rdata")
# load("datafiles/sus_cens_posttest_age2date.Rdata")
# load("datafiles/sus_cens_postno_age2date.Rdata")
# load("datafiles/endlive_age2date.Rdata")
# load("datafiles/sus_mort_posttest_age2date.Rdata")
# load("datafiles/sus_mort_postno_age2date.Rdata")
# load("datafiles/icap_cens_age2date.Rdata")
# load("datafiles/icap_mort_age2date.Rdata")
# load("datafiles/rec_neg_cens_posttest_age2date.Rdata")
# load("datafiles/rec_neg_cens_postno_age2date.Rdata")
# load("datafiles/rec_neg_mort_age2date.Rdata")
# load("datafiles/rec_pos_mort_age2date.Rdata")
# load("datafiles/rec_pos_cens_age2date.Rdata")
# load("datafiles/idead_age2date.Rdata")
# load("datafiles/nT_overall.Rdata")
# load("datafiles/nT_period_presurv.Rdata")
# load("datafiles/nknots_age.Rdata")
# load("datafiles/nknots_period.Rdata")
# load("datafiles/nT_age_surv.Rdata")
# load("datafiles/nT_period_surv.Rdata")
# load("datafiles/n_period_lookup.Rdata")
# load("datafiles/n_agef.Rdata")
# load("datafiles/n_agem.Rdata")
# load("datafiles/n_period.Rdata")
# load("datafiles/n_adj_period.Rdata")
# load("datafiles/n_adj_sp.Rdata")
# load("datafiles/n_sect.Rdata") 
# load("datafiles/scale.Rdata") 
# load("datafiles/period_lookup.Rdata")
# load("datafiles/age_lookup_col_f.Rdata")
# load("datafiles/age_lookup_col_m.Rdata")
# load("datafiles/sect_hunt_pos.Rdata")
# load("datafiles/sect_hunt_neg.Rdata")

####################################################################
###
### Data and paramters needed to calculate likelihoods
###
###
####################################################################

beta0_sus <- -5
beta0_inf <- -3.2
# floor(as.duration(ymd("1992-09-01") %--% ymd("2021-09-01"))/dweeks(1))
# floor(as.duration(ymd("1992-09-01") %--% ymd("2022-05-31"))/dweeks(1))
# floor(as.duration(ymd("1992-05-15") %--% ymd("1992-09-01"))/dweeks(1))
# floor(as.duration(ymd("1992-09-01") %--% ymd("1993-01-01"))/dweeks(1))
# floor(as.duration(ymd("1993-01-01") %--% ymd("1993-09-01"))/dweeks(1))
# floor(as.duration(ymd("1993-09-01") %--% ymd("1994-01-01"))/dweeks(1))
# floor(as.duration(ymd("1993-01-01") %--% ymd("2002-01-01")))
# floor(as.duration(ymd("2002-03-01") %--% ymd("2002-09-01"))/dweeks(1)) - 1
# floor(as.duration(ymd("2003-01-01") %--% ymd("2002-09-01"))/dweeks(1)) - 1
# floor(as.duration(ymd("2003-01-01") %--% ymd("2003-09-01"))/dweeks(1)) - 1
# floor(as.duration(ymd("2004-01-01") %--% ymd("2003-09-01"))/dweeks(1)) - 1
# floor(as.duration(ymd("2022-01-01") %--% ymd("2022-05-31"))/dweeks(1)) - 1

period_effect_survival_test <- c(
    rep(-6, 15), # may1992-sep1992
    rep(c(rep(-4.5, 18), rep(-6, 34)), 29), # sep1992 - sep2021
    rep(-4.5, 19), # sep2021-jan2022
    rep(-6, 20)
) # jan2022-May15,2022

age_effect_survival_test <- exp(-.01 * seq(1:962))
age_effect_survival_test <- age_effect_survival_test - mean(age_effect_survival_test)
beta_sex <- -.5

# load FOI parameters based on run w/o survival integrated
# load("~/Documents/Transmission/Transmission_v3/27_urw2_bym2_timeRW1_2021/fit_sum.Rdata")
# fit_sum_foi <- fit_sum
# f_age_foi <- fit_sum_foi[grep("f_age", rownames(fit_sum_foi)), ][1:7, 1]
# m_age_foi <- fit_sum_foi[grep("m_age", rownames(fit_sum_foi)), ][1:6, 1]
# f_period_foi <- c(fit_sum_foi[grep("f_time", rownames(fit_sum_foi)), 1])
# m_period_foi <- c(fit_sum_foi[grep("m_time", rownames(fit_sum_foi)), 1])
space_mn <- rep(0, n_sect) # leave 0 for now
# # space_mn  <- c(fit_sum_foi[grep("space",rownames(fit_sum_foi)),1])[sect_num]
# save(f_age_foi,file="datafiles/f_age_foi.Rdata")
# save(m_age_foi,file="datafiles/m_age_foi.Rdata")
# save(f_period_foi ,file="datafiles/f_period_foi.Rdata")
# save(m_period_foi ,file="datafiles/m_period_foi.Rdata")

load("datafiles/f_age_foi.Rdata")
load("datafiles/m_age_foi.Rdata")
load("datafiles/f_period_foi.Rdata")
load("datafiles/m_period_foi.Rdata")