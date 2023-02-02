#########################################################################################################################3
###
### Formatting/combining of data
###
#########################################################################################################################3


####################################################
###
### setup age at harvest to estimate period effects
###
###################################################

df_age_nocwd$birthweek <-(lubridate::interval(birth_start,df_age_nocwd$birth_date) %/% weeks(1)) + 1
df_age_nocwd$birthmonth <-(lubridate::interval(birth_start,df_age_nocwd$birth_date) %/% months(1)) + 1

df_age_nocwd$age2date_weeks <-(lubridate::interval(birth_start,df_age_nocwd$birth_date) %/% weeks(1))
df_age_nocwd$age2date_months <-(lubridate::interval(birth_start,df_age_nocwd$birth_date) %/% months(1))

####################################################
###
### setup for age at harvest pop model
###
###################################################

# n_year <- length(2002:2021)
n_year <- length(1992:2021)
n_ageclass <- 7
n_ageclassm <- 6
n_ageclassf <- 7
n_agem <- 7
n_agef <- 10
n_sex <- 2

#assumed starting population >>> by sex/age class (needs to be logpop for model)
assumN_sus <- array(0, dim = c(n_year,n_sex,n_agef))

#or we could values from lit? estimate these?
# assumN_sus.f.1 <- c(2229,2008,1115, 651,414,284,212,158,122,393) 
# assumN_sus.m.1 <- c(2529,2098,1115, 651,414,284,212,158,122,3)

###
### 1992-2001, no age classification data
###

for(y in 1:10){
  for(a in 1:4){
    assumN_sus[y,1,a] <- Ototal$antlerless[y]*Cage_sus[11,1,a]/sum(Cage_sus[11,1,]) #F,1,2,3,
  }
  assumN_sus[y,1,5] <- (Ototal$antlerless[y]*Cage_sus[11,1,5]/sum(Cage_sus[11,1,]))/2 #F,1,2,3,4
  assumN_sus[y,1,6] <- (Ototal$antlerless[y]*Cage_sus[11,1,5]/sum(Cage_sus[11,1,]))/2 #5
  assumN_sus[y,1,7] <- (Ototal$antlerless[y]*Cage_sus[11,1,6]/sum(Cage_sus[11,1,]))/3 #6
  assumN_sus[y,1,8] <- (Ototal$antlerless[y]*Cage_sus[11,1,6]/sum(Cage_sus[11,1,]))/3 #7
  assumN_sus[y,1,9] <- (Ototal$antlerless[y]*Cage_sus[11,1,6]/sum(Cage_sus[11,1,]))/3 #8
  assumN_sus[y,1,10] <- Ototal$antlerless[y]*Cage_sus[11,1,7]/sum(Cage_sus[11,1,]) #9+

  for(a in 1:4){
    assumN_sus[y,2,a] <- Ototal$antlered[y]*Cage_sus[11,1,a]/sum(Cage_sus[11,1,]) #F,1,2,3,
  }
  assumN_sus[y,2,5] <- (Ototal$antlered[y]*Cage_sus[11,1,5]/sum(Cage_sus[11,1,]))/2 #4
  assumN_sus[y,2,6] <- (Ototal$antlered[y]*Cage_sus[11,1,5]/sum(Cage_sus[11,1,]))/2 #5
  assumN_sus[y,2,7] <-  Ototal$antlered[y]*Cage_sus[11,1,6]/sum(Cage_sus[11,1,])
}


###
### For 2002 - 2021, when we have age classification data
###

for(y in 11:n_year){
  for(a in 1:4){
    assumN_sus[y,1,a] <- Ototal$antlerless[y]*Cage_sus[y - 10,1,a]/sum(Cage_sus[y - 10,1,]) #F,1,2,3,
  }
  assumN_sus[y,1,5] <- (Ototal$antlerless[y]*Cage_sus[y - 10,1,5]/sum(Cage_sus[y - 10,1,]))/2 #F,1,2,3,4
  assumN_sus[y,1,6] <- (Ototal$antlerless[y]*Cage_sus[y - 10,1,5]/sum(Cage_sus[y - 10,1,]))/2 #5
  assumN_sus[y,1,7] <- (Ototal$antlerless[y]*Cage_sus[y - 10,1,6]/sum(Cage_sus[y - 10,1,]))/3 #6
  assumN_sus[y,1,8] <- (Ototal$antlerless[y]*Cage_sus[y - 10,1,6]/sum(Cage_sus[y - 10,1,]))/3 #7
  assumN_sus[y,1,9] <- (Ototal$antlerless[y]*Cage_sus[y - 10,1,6]/sum(Cage_sus[y - 10,1,]))/3 #8
  assumN_sus[y,1,10] <- Ototal$antlerless[y]*Cage_sus[y - 10,1,7]/sum(Cage_sus[y - 10,1,]) #9+

  for(a in 1:4){
    assumN_sus[y,2,a] <- Ototal$antlered[y]*Cage_sus[y - 10,1,a]/sum(Cage_sus[y - 10,1,]) #F,1,2,3,
  }
  assumN_sus[y,2,5] <- (Ototal$antlered[y]*Cage_sus[y - 10,1,5]/sum(Cage_sus[y - 10,1,]))/2 #4
  assumN_sus[y,2,6] <- (Ototal$antlered[y]*Cage_sus[y - 10,1,5]/sum(Cage_sus[y - 10,1,]))/2 #5
  assumN_sus[y,2,7] <-  Ototal$antlered[y]*Cage_sus[y - 10,1,6]/sum(Cage_sus[y - 10,1,])
}
pop_sus_init <- assumN_sus
# pop_sus_init <- abind(assumN_sus,assumN_sus[20,,],along=1)


#assumed starting population >>> by sex/age class (needs to be logpop for model)
assumN_inf <- array(0, dim = c(n_year,n_sex,n_agef))

#or we could values from lit? estimate these?
# assumN_inf.f.1 <- c(2229,2008,1115, 651,414,284,212,158,122,393) 
# assumN_inf.m.1 <- c(2529,2098,1115, 651,414,284,212,158,122,3)
for(y in 11:n_year){
  for(a in 1:4){
    assumN_inf[y,1,a] <- Ototal$antlerless[y]*Cage_inf[y - 10,1,a]/sum(Cage_inf[y - 10,1,]) #F,1,2,3,
  }
  assumN_inf[y,1,5] <- (Ototal$antlerless[y]*Cage_inf[y - 10,1,5]/sum(Cage_inf[y - 10,1,]))/2 #F,1,2,3,4
  assumN_inf[y,1,6] <- (Ototal$antlerless[y]*Cage_inf[y - 10,1,5]/sum(Cage_inf[y - 10,1,]))/2 #5
  assumN_inf[y,1,7] <- (Ototal$antlerless[y]*Cage_inf[y - 10,1,6]/sum(Cage_inf[y - 10,1,]))/3 #6
  assumN_inf[y,1,8] <- (Ototal$antlerless[y]*Cage_inf[y - 10,1,6]/sum(Cage_inf[y - 10,1,]))/3 #7
  assumN_inf[y,1,9] <- (Ototal$antlerless[y]*Cage_inf[y - 10,1,6]/sum(Cage_inf[y - 10,1,]))/3 #8
  assumN_inf[y,1,10] <- Ototal$antlerless[y]*Cage_inf[y - 10,1,7]/sum(Cage_inf[y - 10,1,]) #9+


  for(a in 1:4){
    assumN_inf[y,2,a] <- Ototal$antlered[y]*Cage_inf[y - 10,1,a]/sum(Cage_inf[y - 10,1,]) #F,1,2,3,
  }
  assumN_inf[y,2,5] <- (Ototal$antlered[y]*Cage_inf[y - 10,1,5]/sum(Cage_inf[y - 10,1,]))/2 #4
  assumN_inf[y,2,6] <- (Ototal$antlered[y]*Cage_inf[y - 10,1,5]/sum(Cage_inf[y - 10,1,]))/2 #5
  assumN_inf[y,2,7] <-  Ototal$antlered[y]*Cage_inf[y - 10,1,6]/sum(Cage_inf[y - 10,1,])
}
pop_inf_init <- assumN_inf
 
f_logpop_inf <- log(assumN_inf[1,1,])
f_logpop_sus <- log(assumN_sus[1,1,])
m_logpop_inf <- log(assumN_inf[1,2,1:n_agem])
m_logpop_sus <- log(assumN_sus[1,2,1:n_agem])

# logpop_sus <- ifelse(logpop_sus<0,-5,logpop_sus)
f_logpop_sus <- ifelse(f_logpop_sus<0,-5,f_logpop_sus)
m_logpop_sus <- ifelse(m_logpop_sus<0,-5,m_logpop_sus)
f_logpop_inf <- ifelse(f_logpop_inf<0,-5,f_logpop_inf)
m_logpop_inf <- ifelse(m_logpop_inf<0,-5,m_logpop_inf)

# pop_inf_init <- abind(assumN_inf,assumN_inf[20,,],along=1)

################################################
###
### Setting up the Cage data
###
################################################

#adding antlerless male fawns to the Cage_less data
Cage_less <- cbind(Cage[,1,],Cage[,2,1])
Cage_ant <- Cage[,2,2:6]

sizeCage_f <- apply(Cage_less,1,sum)
sizeCage_m <- apply(Cage_ant,1,sum)

#############################################
###
### initial values for reporting rates
###
#############################################

report_overall_init <-rbeta(1,report_hyp_all[1],report_hyp_all[2])

report_init <- rep(report_overall_init,n_year)
for(y in 24:29){
    report_init[y] <- rbeta(1,report_hyp_y$alpha[y-23],report_hyp_y$beta[y-23])
}

#########################################################################
###
### Fecundity/FDR Preliminaries
###
#########################################################################

##########################################
### moment matching functions
##########################################

lognormal_moments <- function(barx,s){
	mu <- log(barx / sqrt((s^2) / (barx^2) + 1))
	sigma <- sqrt(log((s^2) / (barx^2) + 1))
	return(list(mu=mu,sigma=sigma))
}

gamma_moments <- function(mu,sigma){
	alpha <- (mu^2)/(sigma^2)
	beta <- mu/(sigma^2)
	return(list(alpha=alpha,beta=beta))
}
##########################################
### calculate moments 
##########################################

##########################
### Option 3: lognormal
##########################

# fdr_ct_moments_2002_2016 <- lognormal_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_moments_2017_2021 <- lognormal_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)

# #how to set the sd for the years without uncertainty?
# #approximate using the mean of the estimates from Jen's method and doubling it?
# # mean(df_camtrap_fd$fdr_sd)*2

# obs_ct_fd_mu  <- c(fdr_ct_moments_2002_2016$mu,fdr_ct_moments_2017_2021$mu)
# obs_ct_fd_sd <- c(fdr_ct_moments_2002_2016$sigma,fdr_ct_moments_2017_2021$sigma)


##########################
### Option 4: gamma 
##########################

# fdr_ct_gam_moments_2002_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
# fdr_ct_gam_moments_2017_2021 <- gamma_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)

# fdr_ct_gam_moments_2002_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
fdr_ct_gam_moments_1992_2016 <- gamma_moments(fawndoe_df$overall_fd,mean(df_camtrap_fd$fdr_sd)*2)
fdr_ct_gam_moments_2017_2021 <- gamma_moments(df_camtrap_fd$fdr_mean,df_camtrap_fd$fdr_sd)

# obs_ct_fd_alpha  <- c(fdr_ct_gam_moments_2002_2016$alpha,fdr_ct_gam_moments_2017_2021$alpha)
# obs_ct_fd_beta <- c(fdr_ct_gam_moments_2002_2016$beta,fdr_ct_gam_moments_2017_2021$beta)

obs_ct_fd_alpha  <- c(fdr_ct_gam_moments_1992_2016$alpha,fdr_ct_gam_moments_2017_2021$alpha)
obs_ct_fd_beta <- c(fdr_ct_gam_moments_1992_2016$beta,fdr_ct_gam_moments_2017_2021$beta)

###
### repeating fawn:doe ratios from 1997, to cover the fact that I currently don't have
### 

fec_init <- c(fawndoe_df$overall_fd,df_camtrap_fd$fdr_mean)

################################################################################
###
### read in DMU shapefiles
###
###############################################################################

# #these are the DMUs that roughly align with our study area
# units <-c("70C-CWD","70D-CWD","70A-CWD","73E-CWD")

# dmu_2002 <- st_read("/home/aketz/Documents/Data/DMU_shapefiles_2002_2013/dmu_2002.shp")
 
# # st_crs(dmu_2013) <- 4326
# # st_transform(dmu_2013,"+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000+y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" )

# dmu_2002$study<-ifelse(dmu_2002$UNIT_ID %in% units,1,0)

# dmu_2013 <- st_read("/home/aketz/Documents/Data/DMU_shapefiles_2002_2013/dmu_2013.shp")
# dmu_2013$study<-ifelse(dmu_2013$UNIT_ID %in% units,1,0)

# dmu_2013 <- st_read("/home/aketz/Documents/Data/DMU_shapefiles_2002_2013/dmu_2013.shp")
# dmu_2013$study<-ifelse(dmu_2013$UNIT_ID %in% units,1,0)


# study_area <- st_read("/home/aketz/Documents/Data/Study_Area/secrdtrsWGS84selection.shp")
# study_bound <-st_union(study_area)
# plot(study_bound)
# head(study_bound)

# ggplot()+
#   geom_sf(data=dmu_2002,color="black",aes(fill=study))+
#   geom_sf(data=study_bound,alpha=0,color="white")+ggtitle("2002")

# dmu_vs_study <- ggplot()+
#   geom_sf(data=dmu_2013,color="black",aes(fill=study))+
#   geom_sf(data=study_bound,alpha=0,color="white")+ggtitle("2013")
# dmu_vs_study
# ggsave(dmu_vs_study,file="figures/dmu_vs_study_2013.png")


# ####################################
# ###
# ### county vs study 
# ###
# ####################################

# counties_study = c("Dane","Iowa","Grant")
# county <- st_read("/home/aketz/Documents/Data/counties/dmu_2018_2020.shp")
# county$study<-ifelse(county$CTY_NAME %in% counties_study,1,0)

# county_vs_study <- ggplot()+
#   geom_sf(data = county, color = "black",aes(fill = study)) +
#   geom_sf(data = study_bound, alpha = 0, color = "white") + ggtitle("2018-2020")

# county_vs_study

# ggsave(county_vs_study,file="figures/county_vs_study_2018_2020.png")


# ####################################
# ###
# ### county vs dmu 
# ###
# ####################################

# county_dmu_plot <- ggplot()+
#   geom_sf(data=dmu_2013,color="grey")+
#   geom_sf(data=county,color="darkgrey",aes(fill=study),alpha=.2)+
#   geom_sf(data=study_bound,alpha=0,color="white")+ggtitle("County lines and deer DMU 2013")

# county_dmu_plot
# ggsave(county_dmu_plot,file="figures/county_dmu_plot.png")



# ####################################
# ###
# ### dane county proportion harvest 
# ###
# ####################################

# county_dmu_plot <- ggplot()+
#   geom_sf(data=dmu_2013,color="grey")+
#   geom_sf(data=county,color="darkgrey",aes(fill=study),alpha=.2)+
#   geom_sf(data=study_bound,alpha=0,color="white")+ggtitle("County lines and deer DMU 2013")

# county_dmu_plot
# ggsave(county_dmu_plot,file="figures/county_dmu_plot.png")


# #extracting dane from county shapefile
# st_crs(county)
# st_transform(study_bound,st_crs(county))
# st_intersection(county[county$dmu=="Dane",],study_bound)

# ######################################################################################


# df_dmu_totharvest <-df_dmu_harvest %>% group_by(yr) %>% summarise(antlered_gun = sum(antleredgun),
#                                               antlerlessgun = sum(antlerlessgun),
#                                               unknowngun = sum(unknowngun),
#                                               totalgun = sum(totalgun),
#                                               antleredbow = sum(antleredbow),
#                                               antlered = sum(antleredgun)+sum(antleredbow),
#                                               antlerless = sum(antlerlessgun),
#                                               unknown = sum(unknowngun)
#                                                ) #%>% pivot_longer(cols=-yr)



# df_dmu_totharvest$antlered/df_harvest$antlered[df_harvest$year %in% (2002:2013)]


# plot(2002:2013,df_dmu_totharvest$antlered/df_harvest$antlered[df_harvest$year %in% (2002:2013)])

# png("figures/dmu_county_harvest_num.png")
# plot(df_dmu_totharvest$antlered,df_harvest$antlered[df_harvest$year %in% (2002:2013)])
# dev.off()


# png("figures/dmu_county_harvest_prop.png")
# plot(2002:2013,df_dmu_totharvest$antlered/df_harvest$antlered[df_harvest$year %in% (2002:2013)])
# dev.off()

###
### calculate proportion area of county that is in study area for each county. 
###

#https://gis.stackexchange.com/questions/287602/how-to-calculate-the-polygon-area-and-create-a-column-of-results-in-r



###########################
###
###
###
###########################


# Ototal_long <- pivot_longer(Ototal[,1:3],2:3)
# names(Ototal_long) <- c("Year","Harvest_Type","Harvest_Total")
# Ototal_long$Harvest_Type <- factor(Ototal_long$Harvest_Type,labels=c("Antlered","Antlerless"))
# ototal_plot <- ggplot(Ototal_long,aes(x=Year, y=Harvest_Total, color = Harvest_Type)) +
#     geom_line(size=1.1) + 
#     geom_point(size=1.4) +
#     theme_bw() + 
#     scale_color_manual(values=met.brewer("Troy",2),name="Harvest Type") +
#     ylab("Harvest Total")+
#     theme(axis.text=element_text(size=14),
#             axis.title=element_text(size=16),
#             legend.text=element_text(size=14),
#             legend.title=element_text(size=16))
# ggsave("figures/total_harvest_plot.png",ototal_plot,height=7,width=9)


# Cage_antlerless  <- data.frame(Cage_less[,1:7])
# Cage_antlerless <- Cage_antlerless/apply(Cage_antlerless,1,sum)
# names(Cage_antlerless)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")
# Cage_antlerless$Year <- 2002:2021


# Cage_less_long <- pivot_longer(Cage_antlerless,cols=1:7)
# Cage_less_long$name <- factor(as.factor(Cage_less_long$name),levels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
# plotless_legend <- ggplot(Cage_less_long,aes(x=Year,y=value))+
#     geom_col(aes(fill=name))+theme_bw()+
#     scale_fill_manual(name="Age Class",values=rev(met.brewer("Veronese",7)))+
#     ylab("Proportion")+ggtitle("Female")
# ggsave("figures/plotless_legend.png",plotless_legend,height=6,width=10)


# Cage_less_long$name <- factor(as.factor(Cage_less_long$name),levels=rev(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")))
# plotless <- ggplot(Cage_less_long,aes(x=Year,y=value))+
#     geom_col(aes(fill=name))+theme_bw()+
#     scale_fill_manual(name="Age Class",values=met.brewer("Veronese",7))+
#     ylab("Proportion")+ggtitle("Female")+theme(legend.position="bottom")+
#     guides(color = guide_legend(nrow = 1))


# Cage_antler <- cbind(Cage_less[,8],Cage_ant)
# Cage_antler  <- data.frame(Cage_antler)
# Cage_antler <- Cage_antler/apply(Cage_antler,1,sum)
# names(Cage_antler)=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")
# Cage_antler$Year <- 2002:2021
# Cage_ant_long <- pivot_longer(Cage_antler,cols=1:6)
# Cage_ant_long$name <- factor(as.factor(Cage_ant_long$name),levels=rev(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5+")))

# plotant <- ggplot(Cage_ant_long,aes(x=Year,y=value))+
# geom_point()+
# facet_wrap(.~name,ncol=1)+
# theme_bw()+
# ylab("Number Deer")+ylim(0,1000)

# plotant <- ggplot(Cage_ant_long,aes(x=Year,y=value))+
#     geom_col(aes(fill=name))+theme_bw()+
#     scale_fill_manual(name="Age Class",values=met.brewer("Veronese",7)[2:7])+
#     ylab("Proportion")+ggtitle("Male")
# # plotant

# ggsave("figures/plotant.png",plotant,height=6,width=10)
# ggsave("figures/plotless.png",plotless,height=6,width=10)


# combo_ant_less <- grid.arrange(plotant + theme(legend.position="none"),
#                          plotless + theme(legend.position="none"),
#                          nrow=1)

# ggsave("figures/combo_ant_less.png",combo_ant_less,height=6,width=10)


# fd_df <- data.frame(year=2002:2021,fd_ratio=c(fawndoe_df$overall_fd[1:15],
# df_camtrap_fd$fdr_mean),fd_low=c(rep(NA,15),df_camtrap_fd$fdr_lower95),
# fd_up=c(rep(NA,15),df_camtrap_fd$fdr_upper95)
# )

# fec_plot <- ggplot(fd_df,aes(x=year,y=fd_ratio))+
#     geom_line(size=2)+
#     geom_point(size=2)+theme_bw()+ylab("Fawn:Doe Ratio")+
#     geom_errorbar(aes(ymin=fd_low, ymax=fd_up), width=.5,
#                  position=position_dodge(.9))+
#     theme(axis.text=element_text(size=14),
#             axis.title=element_text(size=16),
#             legend.text=element_text(size=14),
#             legend.title=element_text(size=16))+xlab("Year")


# ggsave("figures/fec_plot.png",fec_plot,width=8,height=6)



# report_plot <- ggplot(report_df,aes(x=year,y=compliance_rate)) +
#     geom_point() +
#     geom_line() +
#     geom_errorbar(aes(ymin=compliance_rate - 2*se,ymax = compliance_rate + 2*se),width=.2)+
#     geom_hline(yintercept =report_hyp_sum[1],linetype="dashed",color=met.brewer("Veronese",1))+
#     theme_bw()+
#     ggtitle("Compliance Rate of 1st Harvested Deer")+
#     ylab("Compliance Rate") +
#     xlab("Year")+
#     theme(axis.text=element_text(size=14),
#             axis.title=element_text(size=16),
#             legend.text=element_text(size=14),
#             legend.title=element_text(size=16),
#             title = element_text(size=16))

# ggsave("figures/report_plot.png",report_plot,height = 6, width = 8)






######################################
###
### checking for initial values and reasonable priors
###
#######################################

# mu_sn_inf <-  rnorm(1,cloglog(.2),2)

# cll_sn_inf <- c()
# sn_inf <- c()
#     for (t in 1:n_year) {
#       cll_sn_inf[t] = rnorm(1,mu_sn_inf, .2)
      
#       #change in variable to probability scale
#       sn_inf[t] <- exp(-exp(cll_sn_inf[t]))
#     }
# cll_sn_inf
# plot(sn_inf)
# round(sn_inf,2)  

##########################################################################
# preliminaries for survival model using GPS collar
###########################################################################
n_year_precollar <- length(1992:2016)
n_year_collar <- length(2017:2021)

which(1992:2021 == 2017)

period_indx <- cumsum(c(18,rep(52,4)))
age_indx_fawn <- 52
age_indx_yearling <- 104
age_indx_2 <- 3*52
age_indx_3 <- 4*52
#question: how to pull the indexes for age effects since we have age classes in the m
age_indx_4_5 <- 



##########################################################################
#loading age and period effects from imputation version of S/FOI model
###########################################################################


load("~/Documents/integrate_s_foi/s_foi_v3/mcmcout.Rdata")

age_effect <- mcmcout$summary$all.chains[grep("sus_age_effect",rownames(mcmcout$summary$all.chains)), 1]
period_effect <- mcmcout$summary$all.chains[grep("sus_period_effect",rownames(mcmcout$summary$all.chains)),1]
sus_beta0 <- mcmcout$summary$all.chains[grep("sus_beta0",rownames(mcmcout$summary$all.chains)),1]
sus_beta_sex <- mcmcout$summary$all.chains[grep("sus_beta_sex",rownames(mcmcout$summary$all.chains)),1]
inf_beta0 <- mcmcout$summary$all.chains[grep("inf_beta0",rownames(mcmcout$summary$all.chains)),1]
inf_beta_sex <- mcmcout$summary$all.chains[grep("inf_beta_sex",rownames(mcmcout$summary$all.chains)),1]
age_effect <- unname(age_effect)
period_effect <- unname(period_effect)
sus_beta0 <- unname(sus_beta0)
sus_beta_sex <- unname(sus_beta_sex)
inf_beta0 <- unname(inf_beta0)
inf_beta_sex <- unname(inf_beta_sex)
nT_age <- length(age_effect)
nT_period <- length(period_effect)
