###############################################################################################
###
### Post processing
###
###############################################################################################

# source("summarize.R")

# load("mcmcout.Rdata")
# load("runtime.Rdata")

# out <- mcmc.list(mcmcout)
# fit_sum <- summarize(out)

# save(results/fit_sum,file="fit_sum.Rdata")
# load("results/mcmcout.Rdata")
# load("results/runperiod.Rdata")
out <- mcmcout$samples
# fit_sum <- mcmcout$summary
fit_sum <- mcmcout$summary$all.chains

# load("results/fit_sum.Rdata")
# load("results/gd.Rdata")
# load("results/ess.Rdata")
parameters
gd <- gelman.diag(out)
gd
save(gd, file = "results/gd.Rdata")

es <- effectiveSize(out)
save(es,file="results/es.Rdata")
gd
es
pdf("figures/traceplots.pdf")
traceplot(out[, "beta0"], ylab = "beta0")
traceplot(out[, "beta0_male"], ylab = "beta0_male")
traceplot(out[, "beta_gun"], ylab = "beta_gun")
traceplot(out[, "beta_ng"], ylab = "beta_ng")
traceplot(out[, "beta_maleage"], ylab = "beta_maleage")
traceplot(out[, "p_hunt[1, 1, 1]"])
dev.off()



###############################################
###
### Plots of age effects for mortality hazard
###
###############################################

p_hunt_indx <- grep("p_hunt",rownames(fit_sum))
length(p_hunt_indx)/2
p_hunt_mean_f <- fit_sum[p_hunt_indx[1:(length(p_hunt_indx)/2)],1]
p_hunt_mean_m <- fit_sum[p_hunt_indx[(length(p_hunt_indx)/2 + 1):length(p_hunt_indx)],1]

tail(p_hunt_mean_m)



te_indx <- grep("age_effect",rownames(fit_sum))
head(fit_sum[te_indx,])

age_effect_mean <- fit_sum[te_indx,1]
age_effect_lower <- fit_sum[te_indx,4]
age_effect_upper <- fit_sum[te_indx,5]

# sus_nT_age <- length(age_effect_mean)
weeks <- 1:length(age_effect_mean)

out_age_effect <- data.frame(weeks,age_effect_mean,age_effect_lower,age_effect_upper)

age_effect_plot <- ggplot(data =out_age_effect,aes(x = weeks))+
  geom_line(aes(x = weeks,y=age_effect_mean),size=1)+
  geom_ribbon(aes(ymin=age_effect_lower,ymax=age_effect_upper),alpha=.2,linetype=0)+
  ggtitle("Age Effect Posterior")+xlab("Age (Years)")+ylab("Effect Size")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,nT_age_surv,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("Year",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("Year",values = met.brewer("Kandinsky", 2))
  
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

age_effect_plot
 
ggsave("figures/age_effect.pdf",age_effect_plot)
ggsave("figures/age_effect.png",age_effect_plot)


###############################################
###
### Plots of period effects for mortality hazard
###
###############################################

te_indx <- grep("period_effect",rownames(fit_sum))
head(fit_sum[te_indx,])

period_effect_mean <- fit_sum[te_indx,1]
period_effect_lower <- fit_sum[te_indx,4]
period_effect_upper <- fit_sum[te_indx,5]

weeks <- 1:nT_period_surv

out_period_effect <- data.frame(weeks,period_effect_mean,period_effect_lower,period_effect_upper)

period_effect_plot <- ggplot(data =out_period_effect,aes(x = weeks))+
  geom_line(aes(x = weeks,y=period_effect_mean),size=1)+
  geom_ribbon(aes(ymin=period_effect_lower,ymax=period_effect_upper),alpha=.2,linetype=0)+
  ggtitle("Period Effect Posterior")+xlab("Period (Weeks)")+ylab("Effect Size")+
  theme_bw()+
  # scale_x_continuous(breaks = seq(0,sus_nT_period,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2))
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

period_effect_plot
 
ggsave("figures/period_effect.pdf",period_effect_plot)
ggsave("figures/period_effect.png",period_effect_plot)


############################
###
### plotting betas
###
############################
#for multiple chains
# beta_df <- do.call(rbind,out[,grep('beta',rownames(fit_sum))])
#for 1 chain
beta_df <- out[,grep('beta',rownames(fit_sum))]

beta0_df <- pivot_longer(data.frame(beta_df[,1:2]),cols=c("beta0_inf","beta0_sus"))

names(beta0_df) <- c("disease","value")
# levels(beta0_df$disease) <- c("Infected","Susceptible")

beta0_plot_combo <- ggplot(data =beta0_df,aes(color=disease,fill=disease))+
  geom_density(aes(x = value),size=1,alpha = .6) +  
  ggtitle("Intercept (beta0) Posterior") +
  xlab(expression(beta[0])) +
  ylab("Density")+
  theme_bw()+
  # scale_x_continuous(breaks = seq(0,inf_nT_age,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2))
beta0_plot_combo
ggsave("figures/beta0_plot_combo.pdf",beta0_plot_combo,height=5,width=6)
ggsave("figures/beta0_plot_combo.png",beta0_plot_combo,height=5,width=6)

beta_sex_df <- pivot_longer(data.frame(beta_df[,3]),cols=c("var1"))
names(beta_sex_df) <- c("disease","value")

beta_sex_plot_combo <- ggplot(data =beta_sex_df)+
  geom_density(aes(x = value),size=1,alpha=.6) +  
  ggtitle("Sex Effect (beta_sex) Posterior") +
  xlab(paste0(expression(beta),"_sex")) +
  ylab("Density")+
  theme_bw()+
  # scale_x_continuous(breaks = seq(0,inf_nT_age,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2))
beta_sex_plot_combo
ggsave("figures/beta_sex_plot_combo.pdf",beta_sex_plot_combo,height=5,width=6)
ggsave("figures/beta_sex_plot_combo.png",beta_sex_plot_combo,height=5,width=6)


######################################################################
###
### Including intercept in the above plots
###
#######################################################################


###############################################
###
### Plots of age effects for mortality hazard
###
###############################################
beta0mn_inf <- apply(beta_df,2,mean)[1]
beta0mn_inf

te_indx <- grep("age_effect",rownames(fit_sum))

age_effect_mean <- fit_sum[te_indx,1] + beta0mn_inf
age_effect_lower <- fit_sum[te_indx,4] + beta0mn_inf
age_effect_upper <- fit_sum[te_indx,5] + beta0mn_inf

weeks <- 1:nT_age

out_age_effect_inf_int <- data.frame(weeks,age_effect_mean,age_effect_lower,age_effect_upper)
out_age_effect_inf_int$disease = "Infected"

age_effect_plot_inf_int <- ggplot(data =out_age_effect_inf_int,aes(x = weeks))+
  geom_line(aes(x = weeks,y=age_effect_mean),size=1)+
  geom_ribbon(aes(ymin=age_effect_lower,ymax=age_effect_upper),alpha=.2,linetype=0)+
  ggtitle("Infected Age Effect Posterior")+xlab("Age (Years)")+ylab("Effect Size")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,nT_age,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("Year",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("Year",values = met.brewer("Kandinsky", 2))
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

age_effect_plot_inf_int

ggsave("figures/age_effect_inf_int.pdf",age_effect_plot_inf_int)
ggsave("figures/age_effect_inf_int.png",age_effect_plot_inf_int)

###############################################
###
### Plots of age effects for mortality hazard
###
###############################################
beta0mn_sus <- apply(beta_df,2,mean)[3]
beta0mn_sus

te_indx <- grep("age_effect",rownames(fit_sum))

age_effect_mean <- fit_sum[te_indx,1] + beta0mn_sus
age_effect_lower <- fit_sum[te_indx,4] + beta0mn_sus
age_effect_upper <- fit_sum[te_indx,5] + beta0mn_sus

weeks <- 1:nT_age

out_age_effect_sus_int <- data.frame(weeks,age_effect_mean,age_effect_lower,age_effect_upper)
out_age_effect_sus_int$disease = "Susceptible"

age_effect_plot_sus_int <- ggplot(data =out_age_effect_sus_int,aes(x = weeks))+
  geom_line(aes(x = weeks,y=age_effect_mean),size=1)+
  geom_ribbon(aes(ymin=age_effect_lower,ymax=age_effect_upper),alpha=.2,linetype=0)+
  ggtitle("Susceptible Age Effect Posterior")+xlab("Age (Years)")+ylab("Effect Size")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,nT_age,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("Year",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("Year",values = met.brewer("Kandinsky", 2))
  
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

age_effect_plot_sus_int
 
ggsave("figures/age_effect_sus_int.pdf",age_effect_plot_sus_int)
ggsave("figures/age_effect_sus_int.png",age_effect_plot_sus_int)

###
### combo plot of age effects for infected/susceptibles
###

out_age_effect_int <- rbind(out_age_effect_sus_int,out_age_effect_inf_int)
out_age_effect_int$disease <- factor(out_age_effect_int$disease,levels=c("Susceptible","Infected"))

age_effect_plot_combo_int <- ggplot(data =out_age_effect_int,aes(x = weeks,color=disease,fill=disease))+
  geom_line(aes(x = weeks,y=age_effect_mean),size=1)+
  geom_ribbon(aes(ymin=age_effect_lower,ymax=age_effect_upper),alpha=.2,linetype=0)+
  ggtitle("Age Effect Posterior")+xlab("Age (Years)")+ylab("Effect Size")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,nT_age,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("Year",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("Year",values = met.brewer("Kandinsky", 2))+
  facet_wrap(.~disease)
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

age_effect_plot_combo_int
 
ggsave("figures/age_effect_combo_int.pdf",age_effect_plot_combo_int,height=4,width=8)
ggsave("figures/age_effect_combo_int.png",age_effect_plot_combo_int,height=4,width=8)



###############################################
###
### Plots of period effects for mortality hazard
###
###############################################

te_indx <- grep("period_effect",rownames(fit_sum))

period_effect_mean <- fit_sum[te_indx,1] + beta0mn_inf
period_effect_lower <- fit_sum[te_indx,4] + beta0mn_inf
period_effect_upper <- fit_sum[te_indx,5] + beta0mn_inf

weeks <- 1:nT_period

out_period_effect_inf_int <- data.frame(weeks,period_effect_mean,period_effect_lower,period_effect_upper)
out_period_effect_inf_int$disease <- "Infected"

period_effect_plot_inf_int <- ggplot(data =out_period_effect_inf_int,aes(x = weeks))+
  geom_line(aes(x = weeks,y=period_effect_mean),size=1)+
  geom_ribbon(aes(ymin=period_effect_lower,ymax=period_effect_upper),alpha=.2,linetype=0)+
  ggtitle("Infected Period Effect Posterior")+xlab("Period (Weeks)")+ylab("Effect Size")+
  theme_bw()+
  # scale_x_continuous(breaks = seq(0,inf_nT_period,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2))
  
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

period_effect_plot_inf_int

ggsave("figures/period_effect_inf_int.pdf",period_effect_plot_inf_int)
ggsave("figures/period_effect_inf_int.png",period_effect_plot_inf_int)

###############################################
###
### Plots of period effects for mortality hazard
###
###############################################

te_indx <- grep("period_effect",rownames(fit_sum))
tail(fit_sum[te_indx,])

period_effect_mean <- fit_sum[te_indx,1] + beta0mn_sus
period_effect_lower <- fit_sum[te_indx,4] + beta0mn_sus
period_effect_upper <- fit_sum[te_indx,5] + beta0mn_sus

weeks <- 1:nT_period

out_period_effect_sus_int <- data.frame(weeks,period_effect_mean,period_effect_lower,period_effect_upper)
out_period_effect_sus_int$disease <- "Susceptible"

period_effect_plot_sus_int <- ggplot(data =out_period_effect_sus_int,aes(x = weeks))+
  geom_line(aes(x = weeks,y=period_effect_mean),size=1)+
  geom_ribbon(aes(ymin=period_effect_lower,ymax=period_effect_upper),alpha=.2,linetype=0)+
  ggtitle("Susceptible Period Effect Posterior")+xlab("Period (Weeks)")+ylab("Effect Size")+
  theme_bw()+
  # scale_x_continuous(breaks = seq(0,sus_nT_period,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2))
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

period_effect_plot_sus_int
 
ggsave("figures/period_effect_sus_int.pdf",period_effect_plot_sus_int)
ggsave("figures/period_effect_sus_int.png",period_effect_plot_sus_int)

###
### combo plot of period effects for infected/susceptibles
###

out_period_effect_int <- rbind(out_period_effect_sus_int,out_period_effect_inf_int)
out_period_effect_int$disease <- factor(out_period_effect_int$disease,levels=c("Susceptible","Infected"))

period_effect_plot_combo_int <- ggplot(data =out_period_effect_int,aes(x = weeks,color=disease,fill=disease))+
  geom_line(aes(x = weeks,y=period_effect_mean),size=1)+
  geom_ribbon(aes(ymin=period_effect_lower,ymax=period_effect_upper),alpha=.2,linetype=0)+
  ggtitle("Period Effect Posterior")+xlab("Period (Weeks)")+ylab("Effect Size")+
  theme_bw()+
  # scale_x_continuous(breaks = seq(0,inf_nT_age,by=104),labels=seq(0,18,by=2))+
  scale_color_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2)) +
  scale_fill_manual("CWD\nStatus",values = met.brewer("Kandinsky", 2))+
  facet_wrap(.~disease)
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

period_effect_plot_combo_int
 
ggsave("figures/period_effect_combo_int.pdf",period_effect_plot_combo_int,height=4,width=8)
ggsave("figures/period_effect_combo_int.png",period_effect_plot_combo_int,height=4,width=8)



#############################################
###
### Plots of age effects for infection hazard
###
#############################################

pdf("figures/27_urw2_nsp_periodRW1_age.pdf")
plot(1:n_age,fit_sum[grep("f_age",rownames(fit_sum)),1][1:n_age],type="o",ylim=c(-23,0))
points(1:n_age,fit_sum[grep("f_age",rownames(fit_sum)),4][1:n_age],lty="dashed",type="o")
points(1:n_age,fit_sum[grep("f_age",rownames(fit_sum)),5][1:n_age],lty="dashed",type="o")
plot(1:n_age,fit_sum[grep("m_age",rownames(fit_sum)),1][1:n_age],type="o",ylim=c(-23,0))
points(1:n_age,fit_sum[grep("m_age",rownames(fit_sum)),4][1:n_age],lty="dashed",type="o")
points(1:n_age,fit_sum[grep("m_age",rownames(fit_sum)),5][1:n_age],lty="dashed",type="o")
dev.off()

Agegroups=1:n_age
haz.mean = fit_sum[grep("f_age",rownames(fit_sum)),1][1:n_age]
haz.lower = fit_sum[grep("f_age",rownames(fit_sum)),4][1:n_age]
haz.upper = fit_sum[grep("f_age",rownames(fit_sum)),5][1:n_age]

temp = data.frame(Agegroups,haz.mean,haz.lower,haz.upper)

fage.plot=ggplot(data=temp)+geom_step(aes(x=Agegroups,y=haz.mean),direction="hv")+
  geom_step(aes(x=Agegroups,y=haz.lower),direction="hv",linetype=3)+
  geom_step(aes(x=Agegroups,y=haz.upper),direction="hv",linetype=3)+
  theme_bw()
ggsave(fage.plot,file="figures/27_urw2_nsp_periodRW1_fage_stepplot.pdf")


Agegroups=1:n_age
haz.mean = fit_sum[grep("m_age",rownames(fit_sum)),1][1:n_age]
haz.lower = fit_sum[grep("m_age",rownames(fit_sum)),4][1:n_age]
haz.upper = fit_sum[grep("m_age",rownames(fit_sum)),5][1:n_age]

temp = data.frame(Agegroups,haz.mean,haz.lower,haz.upper)
# temp$Agegroups=as.factor(temp$Agegroups)
# .out=melt(temp,ids.var=Agegroups)

mage.plot=ggplot(data=temp)+geom_step(aes(x=Agegroups,y=haz.mean),direction="hv")+
  geom_step(aes(x=Agegroups,y=haz.lower),direction="hv",linetype=3)+
  geom_step(aes(x=Agegroups,y=haz.upper),direction="hv",linetype=3)+
  theme_bw()
ggsave(mage.plot,file="figures/27_urw2_nsp_periodRW1_mage_stepplot.pdf")


###################################################
###
### period plots
###
##################################################



pdf("figures/27_urw2_periodEX_nsp_period.pdf")
plot(1:n_period,fit_sum[grep("f_period",rownames(fit_sum)),1],type="o",ylim=c(-2,2))
points(1:n_period,fit_sum[grep("f_period",rownames(fit_sum)),4],lty="dashed",type="o")
points(1:n_period,fit_sum[grep("f_period",rownames(fit_sum)),5],lty="dashed",type="o")
plot(1:n_period,fit_sum[grep("m_period",rownames(fit_sum)),1],type="o",ylim=c(-2,2))
points(1:n_period,fit_sum[grep("m_period",rownames(fit_sum)),4],lty="dashed",type="o")
points(1:n_period,fit_sum[grep("m_period",rownames(fit_sum)),5],lty="dashed",type="o")
dev.off()

periodgroups=1:n_period
haz.mean = fit_sum[grep("f_period",rownames(fit_sum)),1]
haz.lower = fit_sum[grep("f_period",rownames(fit_sum)),4]
haz.upper = fit_sum[grep("f_period",rownames(fit_sum)),5]

temp = data.frame(periodgroups,haz.mean,haz.lower,haz.upper)

fperiod.plot=ggplot(data=temp)+geom_step(aes(x=periodgroups,y=haz.mean),direction="hv")+
  geom_step(aes(x=periodgroups,y=haz.lower),direction="hv",linetype=3)+
  geom_step(aes(x=periodgroups,y=haz.upper),direction="hv",linetype=3)+
  theme_bw()
ggsave(fperiod.plot,file="figures/27_urw2_nsp_periodRW1_fperiod_stepplot.pdf")


periodgroups=1:n_period
haz.mean = fit_sum[grep("m_period",rownames(fit_sum)),1]
haz.lower = fit_sum[grep("m_period",rownames(fit_sum)),4]
haz.upper = fit_sum[grep("m_period",rownames(fit_sum)),5]

temp = data.frame(periodgroups,haz.mean,haz.lower,haz.upper)
# temp$periodgroups=as.factor(temp$periodgroups)
# .out=melt(temp,ids.var=periodgroups)

mperiod.plot=ggplot(data=temp)+geom_step(aes(x=periodgroups,y=haz.mean),direction="hv")+
  geom_step(aes(x=periodgroups,y=haz.lower),direction="hv",linetype=3)+
  geom_step(aes(x=periodgroups,y=haz.upper),direction="hv",linetype=3)+
  theme_bw()
ggsave(mperiod.plot,file="figures/27_urw2_nsp_periodRW1_mperiod_stepplot.pdf")


###################################################
###
### write results
###
##################################################


sink('results_27_urw2_nsp_periodRW1.txt')
fit_sum[c(grep("age",rownames(fit_sum)),grep("period",rownames(fit_sum)),grep("prec",rownames(fit_sum))),]
gd
es
reps
runtime
sink()

fit_sum[c(grep("age",rownames(fit_sum)),grep("period",rownames(fit_sum)),grep("prec",rownames(fit_sum))),]
gd
es
reps
runtime



##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
###
### Plots for final output
###
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

Set2b=brewer.pal(n = 8, name = "YlGnBu")



fage.effects=do.call(rbind,out[,grep('f_age',rownames(fit_sum))])
fmu.age=apply(fage.effects,1,mean)
fage.effects.center=fage.effects-fmu.age
fage.effects.center.sum=cbind(apply(fage.effects.center,2,mean),apply(fage.effects.center,2,sd),t(apply(fage.effects.center,2,quantile,c(.5,.025,.975))))

mage.effects=do.call(rbind,out[,grep('m_age',rownames(fit_sum))])
mmu.age=apply(mage.effects,1,mean)
mage.effects.center=mage.effects-mmu.age
mage.effects.center.sum=cbind(apply(mage.effects.center,2,mean),apply(mage.effects.center,2,sd),t(apply(mage.effects.center,2,quantile,c(.5,.025,.975))))


mmu.sum=c(mean(mmu.age),sd(mmu.age),quantile(mmu.age,c(.5,.025,.975)))
mmu.sum
fmu.sum=c(mean(fmu.age),sd(fmu.age),quantile(fmu.age,c(.5,.025,.975)))
fmu.sum
baseline.haz.sum<-data.frame(rbind(fmu.sum,mmu.sum))
names(baseline.haz.sum)=c("Mean","SD","Median","Lower","Upper")
baseline.haz.sum$Sex <- c("Female","Male")
baseline.haz.sum <- baseline.haz.sum[,c(6,1:5)]

write.csv(baseline.haz.sum,file="baseline_hazard.csv")

print(xtable(baseline.haz.sum),include.rownames=FALSE)






###################################################
###
### Age plots that look good 
###
##################################################
Agegroups=1:n_age
mage.mn = fit_sum[grep("m_age",rownames(fit_sum)),1][1:n_age]
mage.l = fit_sum[grep("m_age",rownames(fit_sum)),4][1:n_age]
mage.u = fit_sum[grep("m_age",rownames(fit_sum)),5][1:n_age]
fage.mn = fit_sum[grep("f_age",rownames(fit_sum)),1][1:n_age]
fage.l = fit_sum[grep("f_age",rownames(fit_sum)),4][1:n_age]
fage.u = fit_sum[grep("f_age",rownames(fit_sum)),5][1:n_age]


#### Females
agegroups=as.factor(1:n_age)
sex=rep("Female",n_age)
age.df.f = data.frame(agegroups,fage.mn,fage.l,fage.u,sex)

fage.plot=ggplot(data=age.df.f,aes(x=agegroups,y=fage.mn,group=1))+
  geom_ribbon(aes(ymin=fage.l,ymax=fage.u),alpha=.95,linetype=0,fill=Set2b[3])+
  geom_line(size=1.5)+
  # ylim(-1.4,2.5)+
  theme_bw()+xlab("Year")+ylab("Period Log Hazard")+
  scale_x_discrete(limit = factor(1:n_age),labels =c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"),
        title =element_text(size=14, face='bold')
  )

fage.plot

#females age step plot
agegroups=1:n_age
sex=rep("Female",n_age)
age.df.f = data.frame(agegroups,fage.mn,fage.l,fage.u,sex)

fage.plot.step=ggplot(data=age.df.f)+
  geom_rect(aes(xmin=agegroups,
                xmax=lead(agegroups),
                ymin=fage.l,
                ymax=fage.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=agegroups,y=fage.mn),size=1.2)+
  ylim(-12,-4)+
  ggtitle("Female")+
  theme_bw()+xlab("Age Class")+ylab("Age Effects Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = 1:n_age,labels =c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fage.plot.step
ggsave(fage.plot.step,file="figures/fage_stepplot.pdf",height=7,width=7)
 
# ###Males
sex=rep("Male",n_age)
age.df.m = data.frame(agegroups,mage.mn,mage.l,mage.u,sex)
age.df.m$agegroups=as.factor(age.df.m$agegroups)
# .out=melt(temp,ids.var=agegroups)

mage.plot=ggplot(data=age.df.m,aes(x=agegroups,y=mage.mn,group=1))+
  geom_ribbon(aes(ymin=mage.l,ymax=mage.u),alpha=.95,linetype=0,fill=Set2b[5])+
  geom_line(size=1.5)+
  # ylim(-1.4,2.5)+
  theme_bw()+xlab("Year")+ylab("Age Log Hazard")+
  scale_x_discrete(limit = factor(1:n_age),labels =c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"),
        title =element_text(size=14, face='bold')
  )
mage.plot

#male age step plot
agegroups=1:n_age
sex=rep("Male",n_age)
age.df.m = data.frame(agegroups,mage.mn,mage.l,mage.u,sex)

mage.plot.step=ggplot(data=age.df.m)+
  geom_rect(aes(xmin=agegroups,
                xmax=lead(agegroups),
                ymin=mage.l,
                ymax=mage.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=agegroups,y=mage.mn),size=1.2)+
  ggtitle("Male")+
  ylim(-12,-4)+
  theme_bw()+xlab("Age Class")+ylab("Age Effects Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = 1:n_age,labels =c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mage.plot.step
ggsave(mage.plot.step,file="figures/mage_stepplot.pdf",height=7,width=7)
 
#combo step age plot
 
names(age.df.f)=c("agegroups","age.mn","age.l","age.u","sex")
names(age.df.m)=c("agegroups","age.mn","age.l","age.u","sex")

age.df=rbind(age.df.f,age.df.m)
age.df$agegroups=as.factor(age.df$agegroups)

age.plot=ggplot(data=age.df,aes(x=agegroups,y=age.mn,group=1))+
  geom_ribbon(aes(ymin=age.l,ymax=age.u,fill=sex),alpha=.95,linetype=0)+
  geom_line(size=1.5)+
  scale_fill_manual(values=Set2b[c(3,5)])+
  # ylim(1,3.28)+
  theme_bw()+ggtitle("Change in Force of Infection Over Age Groups")+xlab("Year")+ylab("Age effect (log scale)")+
  scale_x_discrete(limit = factor(1:n_age),labels =c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        legend.position="none",
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~sex)
age.plot

ggsave(age.plot,file="figures/FOI_age_plot.pdf",height=5,width=9)






library(gridExtra)
combo_age_step_plot <- grid.arrange(fage.plot.step,mage.plot.step,ncol=2)
ggsave("figures/combo_age_step_plot.pdf",combo_age_step_plot,height=5,width=10)

 



###################################################
###
### Age - mean centered 
###
##################################################

Agegroups=1:n_age

mage.mn = mage.effects.center.sum[1:n_age,1]
mage.l = mage.effects.center.sum[1:n_age,4]
mage.u = mage.effects.center.sum[1:n_age,5]
fage.mn =fage.effects.center.sum[1:n_age,1]
fage.l = fage.effects.center.sum[1:n_age,4]
fage.u = fage.effects.center.sum[1:n_age,5]


#### Females
agegroups=as.factor(1:n_age)
sex=rep("Female",n_age)
age.df.f = data.frame(agegroups,fage.mn,fage.l,fage.u,sex)

fage.plot=ggplot(data=age.df.f,aes(x=agegroups,y=fage.mn,group=1))+
  geom_ribbon(aes(ymin=fage.l,ymax=fage.u),alpha=.95,linetype=0,fill=Set2b[3])+
  geom_line(size=1.5)+
  # ylim(-1.4,2.5)+
  theme_bw()+xlab("Year")+ylab("Period Log Hazard")+
  scale_x_discrete(limit = factor(1:n_age),labels =c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"),
        title =element_text(size=14, face='bold')
  )
fage.plot
# ggsave(fage.plot,file="figures/40_urw2_sp_ageExp_fage_stepplot.pdf")

# ###Males
sex=rep("Male",n_age)
age.df.m = data.frame(agegroups,mage.mn,mage.l,mage.u,sex)
age.df.m$agegroups=as.factor(age.df.m$agegroups)
# .out=melt(temp,ids.var=agegroups)

mage.plot=ggplot(data=age.df.m,aes(x=agegroups,y=mage.mn,group=1))+
  geom_ribbon(aes(ymin=mage.l,ymax=mage.u),alpha=.95,linetype=0,fill=Set2b[5])+
  geom_line(size=1.5)+
  # ylim(-1.4,2.5)+
  theme_bw()+xlab("Year")+ylab("Age Log Hazard")+
  scale_x_discrete(limit = factor(1:n_age),labels =c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"),
        title =element_text(size=14, face='bold')
  )
mage.plot

names(age.df.f)=c("agegroups","age.mn","age.l","age.u","sex")
names(age.df.m)=c("agegroups","age.mn","age.l","age.u","sex")

age.df=rbind(age.df.f,age.df.m)


age.plot=ggplot(data=age.df,aes(x=agegroups,y=age.mn,group=1))+
  geom_ribbon(aes(ymin=age.l,ymax=age.u,fill=sex),alpha=.95,linetype=0)+
  geom_line(size=1.5)+
  scale_fill_manual(values=Set2b[c(3,5)])+
  # ylim(1,3.28)+
  theme_bw()+ggtitle("Change in Force of Infection Over Age Groups")+xlab("Year")+ylab("Age effect (log scale)")+
  scale_x_discrete(limit = factor(1:n_age),labels =c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        legend.position="none",
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~sex)
age.plot

ggsave(age.plot,file="figures/FOI_age_plot_mu.pdf",height=5,width=9)


####################################################################################
###
### Apparent prevalence over period
###
#######################################################################################

names(cwd.df)
head(cwd.df$age)
levels(cwd.df$age)=c("F","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")
table(cwd.df$age)
cwd.df[cwd.df$age == 2 & cwd.df$sex == 1,]



apparent.temp = cwd.df %>% group_by(sex,age,kill.year) %>% summarise(n=n(),positive = sum(teststatus), prevalence = sum(teststatus)/n())
# apparent.temp
# apparent.prev=apparent.temp %>% filter(prevalence !=1)
apparent.prev=apparent.temp
apparent.prev$sex=as.factor(apparent.prev$sex)
levels(apparent.prev$sex)=c("Male","Female")
apparent.prev$sex = factor(apparent.prev$sex,levels=c("Female","Male"))

class(apparent.prev$age)
levels(apparent.prev$age)
apparent.prev$age <- factor(apparent.prev$age,levels=c("0","1","2","3","4","6","9")) 
levels(apparent.prev$age) <- c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")

# ggplot(apparent.prev,aes(fill=sex,y=prevalence,x=kill.year))+
#   geom_bar(position="dodge",stat="identity")+
#   scale_fill_manual(values=Set2b[c(3,5)])+
#   theme_bw()


prev.plot = ggplot(apparent.prev,aes(fill=age,y=prevalence,x=kill.year))+
  geom_bar(position="dodge",stat="identity")+
  scale_fill_manual("Age Class",values=rev(Set2b[2:8]),labels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme_bw()+facet_wrap(~sex)+
  ylab("Apparent Prevalence")+xlab("Year")+labs("Age Class")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        strip.text.x = element_text(size = 12),
        legend.title = element_text(size=12))#+
  # ggtitle("Core Area - Apparent Prevalence")
prev.plot

ggsave(prev.plot,file="figures/prev_plot.pdf",height=5,width=8)
ggsave(prev.plot,file="figures/prev_plot.png",height=5,width=8)


library(ggh4x)

prev.plot.sex = ggplot(apparent.prev,aes(fill=sex,y=prevalence,x=kill.year))+
  geom_bar(position="dodge",stat="identity")+
  scale_fill_manual("Sex",values=rev(Set2b[c(5,3)]),labels=c("Female","Male"))+
  # scale_fill_manual("Age Class",values=rev(Set2b[2:8]),labels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme_bw()+facet_wrap(~sex)+
  ylab("Apparent Prevalence")+xlab("Year")+labs("Age Class")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        strip.text.x = element_text(size = 12),
        legend.title = element_text(size=12))#+
  # ggtitle("Core Area - Apparent Prevalence")
prev.plot.sex
ggsave(prev.plot.sex,file="figures/prev_plot_sex.pdf",height=5,width=8)
ggsave(prev.plot.sex,file="figures/prev_plot_sex.png",height=5,width=8)


apparent.prev.f <- apparent.prev[apparent.prev$sex == "Female",]

names(apparent.prev)
prev.plot.sex.f = ggplot(apparent.prev.f,aes(fill=sex,y=prevalence,x=kill.year))+
  geom_bar(position="dodge",stat="identity")+
  scale_fill_manual(values=rev(Set2b[c(3)]))+
  # scale_fill_manual("Age Class",values=rev(Set2b[2:8]),labels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme_bw()+facet_wrap(~age,ncol=1)+
  ylim(0,1)+
  ylab("Apparent Prevalence")+xlab("Year")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        strip.text.x = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.position="none")+
  ggtitle("Female")
prev.plot.sex.f

ggsave(prev.plot.sex.f,file="figures/prev_plot_sex_female.pdf",height=7,width=3.5)
ggsave(prev.plot.sex.f,file="figures/prev_plot_sex_female.png",height=7,width=3.5)

apparent.prev.m <- apparent.prev[apparent.prev$sex == "Male",]

names(apparent.prev)
prev.plot.sex.m <- ggplot(apparent.prev.m,aes(fill=sex,y=prevalence,x=kill.year))+
  geom_bar(position="dodge",stat="identity")+
  scale_fill_manual(values=rev(Set2b[c(5)]))+
  # scale_fill_manual("Age Class",values=rev(Set2b[2:8]),labels=c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))+
  theme_bw()+facet_wrap(~age,ncol=1)+
  ylim(0,1)+
  ylab("Apparent Prevalence")+xlab("Year")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        strip.text.x = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.position="none")+
  ggtitle("Male")
prev.plot.sex.m

ggsave(prev.plot.sex.m,file="figures/prev_plot_sex_male.pdf",height=7,width=3.5)
ggsave(prev.plot.sex.m,file="figures/prev_plot_sex_male.png",height=7,width=3.5)


prev.plot.sex.m <- prev.plot.sex.m + theme(axis.text.y=element_blank(), axis.title.y=element_blank())

combo_sex_age_grid <- grid.arrange(prev.plot.sex.f,prev.plot.sex.m,ncol=2,widths=c(1.15,1))
combo_sex_age_grid
ggsave(combo_sex_age_grid,file="figures/combo_prev_sex_age_grid.pdf",height=9,width=6)
ggsave(combo_sex_age_grid,file="figures/combo_prev_sex_age_grid.png",height=9,width=6,dpi=600)



####################################################################################
###
### Annual FOI period effects
###
#######################################################################################

periods=1:n_period
mperiod.mn = fit_sum[grep("m_period",rownames(fit_sum)),1]
mperiod.l = fit_sum[grep("m_period",rownames(fit_sum)),4]
mperiod.u = fit_sum[grep("m_period",rownames(fit_sum)),5]
fperiod.mn = fit_sum[grep("f_period",rownames(fit_sum)),1]
fperiod.l = fit_sum[grep("f_period",rownames(fit_sum)),4]
fperiod.u = fit_sum[grep("f_period",rownames(fit_sum)),5]


#### Females
periodgroups=as.factor(1:n_period)
sex=rep("Female",n_period)
period.df.f = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)

fperiod.plot=ggplot(data=period.df.f,aes(x=periodgroups,y=fperiod.mn,group=1))+
  geom_ribbon(aes(ymin=fperiod.l,ymax=fperiod.u),alpha=.95,linetype=0,fill=Set2b[3])+
  geom_line(size=1.5)+
  ylim(-2,1.7)+
  theme_bw()+xlab("Year")+ylab("Period Log Hazard")+
  scale_x_discrete(limit = factor(seq(1,20,by=2)),labels =seq(2002,2020,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"),
        title =element_text(size=14, face='bold')
  )
fperiod.plot

ggsave(fperiod.plot,file="figures/female_period_plot.pdf",height=5,width=6)


#step plot over period

period.df.f = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)
period.df.f$periodgroups <- as.numeric(as.character(period.df.f$periodgroups))
fperiod.plot.step=ggplot(data=period.df.f)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=fperiod.l,
                ymax=fperiod.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=periodgroups,y=fperiod.mn),size=1.2)+
  ggtitle("Female")+
      ylim(-2,1.7)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fperiod.plot.step

ggsave(fperiod.plot.step,file="figures/fperiod_stepplot.pdf",height=7,width=7)
 


#### Males
periodgroups=as.factor(1:n_period)
sex=rep("Male",n_period)
period.df.m = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)

mperiod.plot=ggplot(data=period.df.m,aes(x=periodgroups,y=mperiod.mn,group=1))+
  geom_ribbon(aes(ymin=mperiod.l,ymax=mperiod.u),alpha=.95,linetype=0,fill=Set2b[5])+
  geom_line(size=1.5)+
  # ylim(-1.4,2.5)+
  theme_bw()+xlab("Year")+ylab("Period Log Hazard")+
  scale_x_discrete(limit = factor(seq(1,20,by=2)),labels =seq(2002,2020,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"),
        title =element_text(size=14, face='bold')
  )
mperiod.plot
ggsave(mperiod.plot,file="figures/male_period_plot.pdf",height=5,width=6)


#step plot over period

period.df.m = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)
period.df.m$periodgroups <- as.numeric(as.character(period.df.m$periodgroups))
mperiod.plot.step=ggplot(data=period.df.m)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=mperiod.l,
                ymax=mperiod.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=periodgroups,y=mperiod.mn),size=1.2)+
  ggtitle("Male")+
    ylim(-2,1.7)+
  theme_bw()+xlab("Year")+ylab("Annual Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mperiod.plot.step

ggsave(mperiod.plot.step,file="figures/mperiod_stepplot.pdf",height=7,width=7)
 



#sexes combined

names(period.df.f)=c("periodgroups","period.mn","period.l","period.u","sex")
names(period.df.m)=c("periodgroups","period.mn","period.l","period.u","sex")
period.df=rbind(period.df.f,period.df.m)
period.df$periodgroups <- as.factor(period.df$periodgroups)

period.plot=ggplot(data=period.df,aes(x=periodgroups,y=period.mn,group=1))+
  geom_ribbon(aes(ymin=period.l,ymax=period.u,fill=sex),alpha=.95,linetype=0)+
  geom_line(size=1.5)+
  scale_fill_manual(values=Set2b[c(3,5)])+
  # ylim(-1.2,1.3)+
  theme_bw()+ggtitle("Annual Change in Force of Infection")+xlab("Year")+ylab("Period effect (log scale)")+
  scale_x_discrete(limit = factor(seq(1,20,by=2)),labels =seq(2002,2020,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        legend.position="none",
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~sex)
period.plot

ggsave(period.plot,file="figures/FOI_period_plot.pdf",height=5,width=9)

library(gridExtra)

combo_period_step_plot <- grid.arrange(fperiod.plot.step,mperiod.plot.step,ncol=2)
ggsave("figures/combo_period_step_plot.pdf",combo_period_step_plot,height=5,width=10)

####################################################################################
###
### period plot including intercept
###
#######################################################################################
#step plot over period
baseline.haz.sum <- baseline.haz.sum[,c(2:6,1)]
baseline.haz.sum <- t(baseline.haz.sum)
sex=rep("Female",19)
period.df.f = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)
period.df.f$periodgroups <- as.numeric(as.character(period.df.f$periodgroups))
period.df.f$fperiod.mn
period.df.f$fperiod.mn <- period.df.f$fperiod.mn + baseline.haz.sum[1,1]
period.df.f$fperiod.l <- period.df.f$fperiod.l + baseline.haz.sum[1,1]
period.df.f$fperiod.u <- period.df.f$fperiod.u + baseline.haz.sum[1,1]

fperiod.plot.step.int=ggplot(data=period.df.f)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=fperiod.l,
                ymax=fperiod.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=periodgroups,y=fperiod.mn),size=1.2)+
  ggtitle("Female")+
    ylim(-9.2,-5.3)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fperiod.plot.step.int

ggsave(fperiod.plot.step.int,file="figures/fperiod_stepplot_int.pdf",height=7,width=7)
 


#step plot over period

period.df.m = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)
period.df.m$periodgroups <- as.numeric(as.character(period.df.m$periodgroups))
period.df.m$mperiod.mn <- period.df.m$mperiod.mn + baseline.haz.sum[2,1]
period.df.m$mperiod.l <- period.df.m$mperiod.l + baseline.haz.sum[2,1]
period.df.m$mperiod.u <- period.df.m$mperiod.u + baseline.haz.sum[2,1]


mperiod.plot.step.int=ggplot(data=period.df.m)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=mperiod.l,
                ymax=mperiod.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=periodgroups,y=mperiod.mn),size=1.2)+
  ggtitle("Male")+
    ylim(-9.2,-5.3)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mperiod.plot.step.int

ggsave(mperiod.plot.step.int,file="figures/mperiod_stepplot_int.pdf",height=7,width=7)


mperiod.plot.step.int <- mperiod.plot.step.int+ylab("")+theme(axis.text.y=element_blank())

combo_period_step_plot_int <- grid.arrange(fperiod.plot.step.int,mperiod.plot.step.int,ncol=2)
ggsave("figures/combo_period_step_plot_int.pdf",combo_period_step_plot_int,height=5,width=10)

####################################################################################
###
### period plots by sex and age groups
###
#######################################################################################
#step plot over period
age.df <- as.data.frame(age.df)


period.df.f.fawn = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)
period.df.f.fawn$periodgroups <- as.numeric(as.character(period.df.f.fawn$periodgroups))
period.df.f.fawn$fperiod.mn
period.df.f.fawn$fperiod.mn <- period.df.f.fawn$fperiod.mn + age.df$age.mn[1]+ baseline.haz.sum[1,2]
period.df.f.fawn$fperiod.l <- period.df.f.fawn$fperiod.l + age.df$age.mn[1]+ baseline.haz.sum[1,2]
period.df.f.fawn$fperiod.u <- period.df.f.fawn$fperiod.u + age.df$age.mn[1]+ baseline.haz.sum[1,2]

fperiod.plot.step.fawn=ggplot(data=period.df.f.fawn)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=fperiod.l,
                ymax=fperiod.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=periodgroups,y=fperiod.mn),size=1.2)+
  ggtitle("Female")+
    # ylim(-8,-3.5)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fperiod.plot.step.fawn

ggsave(fperiod.plot.step.fawn,file="figures/fperiod_stepplot_fawn.pdf",height=7,width=7)
 


#step plot over period

period.df.m.fawn = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)
period.df.m.fawn$periodgroups <- as.numeric(as.character(period.df.m.fawn$periodgroups))
period.df.m.fawn$mperiod.mn <- period.df.m.fawn$mperiod.mn +age.df$age.mn[8]+ baseline.haz.sum[2,2]
period.df.m.fawn$mperiod.l <- period.df.m.fawn$mperiod.l + age.df$age.mn[8]+baseline.haz.sum[2,2]
period.df.m.fawn$mperiod.u <- period.df.m.fawn$mperiod.u + age.df$age.mn[8]+baseline.haz.sum[2,2]


mperiod.plot.step.fawn =ggplot(data=period.df.m.fawn)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=mperiod.l,
                ymax=mperiod.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=periodgroups,y=mperiod.mn),size=1.2)+
  ggtitle("Male")+
    # ylim(-8,-3.5)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mperiod.plot.step.fawn 

ggsave(mperiod.plot.step.fawn ,file="figures/mperiod_stepplot_Fawn.pdf",height=7,width=7)



period.df.f.1 = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)
period.df.f.1$periodgroups <- as.numeric(as.character(period.df.f.1$periodgroups))
period.df.f.1$fperiod.mn
period.df.f.1$fperiod.mn <- period.df.f.1$fperiod.mn + age.df$age.mn[2]+ baseline.haz.sum[1,2]
period.df.f.1$fperiod.l <- period.df.f.1$fperiod.l + age.df$age.mn[2]+ baseline.haz.sum[1,2]
period.df.f.1$fperiod.u <- period.df.f.1$fperiod.u + age.df$age.mn[2]+ baseline.haz.sum[1,2]

fperiod.plot.step.1=ggplot(data=period.df.f.1)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=fperiod.l,
                ymax=fperiod.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=periodgroups,y=fperiod.mn),size=1.2)+
  ggtitle("Female")+
    # ylim(-8,-3.5)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fperiod.plot.step.1

ggsave(fperiod.plot.step.1,file="figures/fperiod_stepplot_1.pdf",height=7,width=7)
 


#step plot over period

period.df.m.1 = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)
period.df.m.1$periodgroups <- as.numeric(as.character(period.df.m.1$periodgroups))
period.df.m.1$mperiod.mn <- period.df.m.1$mperiod.mn +age.df$age.mn[9]+ baseline.haz.sum[2,2]
period.df.m.1$mperiod.l <- period.df.m.1$mperiod.l + age.df$age.mn[9]+baseline.haz.sum[2,2]
period.df.m.1$mperiod.u <- period.df.m.1$mperiod.u + age.df$age.mn[9]+baseline.haz.sum[2,2]


mperiod.plot.step.1 =ggplot(data=period.df.m.1)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=mperiod.l,
                ymax=mperiod.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=periodgroups,y=mperiod.mn),size=1.2)+
  ggtitle("Male")+
    # ylim(-8,-3.5)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mperiod.plot.step.1 

ggsave(mperiod.plot.step.1 ,file="figures/mperiod_stepplot_1.pdf",height=7,width=7)




period.df.f.2 = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)
period.df.f.2$periodgroups <- as.numeric(as.character(period.df.f.2$periodgroups))
period.df.f.2$fperiod.mn
period.df.f.2$fperiod.mn <- period.df.f.2$fperiod.mn + age.df$age.mn[3]+ baseline.haz.sum[1,2]
period.df.f.2$fperiod.l <- period.df.f.2$fperiod.l + age.df$age.mn[3]+ baseline.haz.sum[1,2]
period.df.f.2$fperiod.u <- period.df.f.2$fperiod.u + age.df$age.mn[3]+ baseline.haz.sum[1,2]

fperiod.plot.step.2=ggplot(data=period.df.f.2)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=fperiod.l,
                ymax=fperiod.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=periodgroups,y=fperiod.mn),size=1.2)+
  ggtitle("Female")+
    # ylim(-8,-3.5)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fperiod.plot.step.2

ggsave(fperiod.plot.step.2,file="figures/fperiod_stepplot_2.pdf",height=7,width=7)
 


#step plot over period

period.df.m.2 = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)
period.df.m.2$periodgroups <- as.numeric(as.character(period.df.m.2$periodgroups))
period.df.m.2$mperiod.mn <- period.df.m.2$mperiod.mn +age.df$age.mn[10]+ baseline.haz.sum[2,2]
period.df.m.2$mperiod.l <- period.df.m.2$mperiod.l + age.df$age.mn[10]+baseline.haz.sum[2,2]
period.df.m.2$mperiod.u <- period.df.m.2$mperiod.u + age.df$age.mn[10]+baseline.haz.sum[2,2]


mperiod.plot.step.2 =ggplot(data=period.df.m.2)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=mperiod.l,
                ymax=mperiod.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=periodgroups,y=mperiod.mn),size=1.2)+
  ggtitle("Male")+
    # ylim(-8,-3.5)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mperiod.plot.step.2 

ggsave(mperiod.plot.step.2 ,file="figures/mperiod_stepplot_2.pdf",height=7,width=7)



fperiod.plot.step.1 <- fperiod.plot.step.1+ylab("")+theme(axis.text.y=element_blank())+ggtitle("")
fperiod.plot.step.2 <- fperiod.plot.step.2+ylab("")+theme(axis.text.y=element_blank())+ggtitle("")

mperiod.plot.step.1 <- mperiod.plot.step.1+ylab("")+theme(axis.text.y=element_blank())+ggtitle("")
mperiod.plot.step.2 <- mperiod.plot.step.2+ylab("")+theme(axis.text.y=element_blank())+ggtitle("")


combo_period_step_plot_female <- grid.arrange(fperiod.plot.step.fawn,fperiod.plot.step.1,fperiod.plot.step.2,ncol=3)
ggsave("figures/combo_period_step_plot_female.pdf",combo_period_step_plot_female,height=4,width=10)

combo_period_step_plot_male <- grid.arrange(mperiod.plot.step.fawn,mperiod.plot.step.1,mperiod.plot.step.2,ncol=3)
ggsave("figures/combo_period_step_plot_male.pdf",combo_period_step_plot_male,height=4,width=10)


combo_period_step_plot_all_sex_young <- grid.arrange(fperiod.plot.step.fawn,
                                                   fperiod.plot.step.1,
                                                   fperiod.plot.step.2,
                                                   mperiod.plot.step.fawn,
                                                   mperiod.plot.step.1,
                                                   mperiod.plot.step.2,
                                                   ncol=3)
ggsave("figures/combo_period_step_plot_all.pdf",combo_period_step_plot_all_sex_young,height=7,width=10)



# mperiod.plot.step.int <- mperiod.plot.step.int+ylab("")+theme(axis.text.y=element_blank())

# combo_period_step_plot_int <- grid.arrange(fperiod.plot.step.int,mperiod.plot.step.int,ncol=2)
# ggsave("figures/combo_period_step_plot_int.pdf",combo_period_step_plot_int,height=5,width=10)

#################3
###
### no baseline added
###
###################




period.df.f.fawn = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)
period.df.f.fawn$periodgroups <- as.numeric(as.character(period.df.f.fawn$periodgroups))
period.df.f.fawn$fperiod.mn
period.df.f.fawn$fperiod.mn <- period.df.f.fawn$fperiod.mn + age.df$age.mn[1]#+ baseline.haz.sum[1,2]
period.df.f.fawn$fperiod.l <- period.df.f.fawn$fperiod.l + age.df$age.mn[1]#+ baseline.haz.sum[1,2]
period.df.f.fawn$fperiod.u <- period.df.f.fawn$fperiod.u + age.df$age.mn[1]#+ baseline.haz.sum[1,2]

fperiod.plot.step.fawn=ggplot(data=period.df.f.fawn)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=fperiod.l,
                ymax=fperiod.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=periodgroups,y=fperiod.mn),size=1.2)+
  ggtitle("Female")+
    # ylim(-8.7,-6.1)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fperiod.plot.step.fawn

ggsave(fperiod.plot.step.fawn,file="figures/fperiod_stepplot_fawn.pdf",height=7,width=7)
 


#step plot over period

period.df.m.fawn = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)
period.df.m.fawn$periodgroups <- as.numeric(as.character(period.df.m.fawn$periodgroups))
period.df.m.fawn$mperiod.mn <- period.df.m.fawn$mperiod.mn +age.df$age.mn[8]#+ baseline.haz.sum[2,2]
period.df.m.fawn$mperiod.l <- period.df.m.fawn$mperiod.l + age.df$age.mn[8]#+baseline.haz.sum[2,2]
period.df.m.fawn$mperiod.u <- period.df.m.fawn$mperiod.u + age.df$age.mn[8]#+baseline.haz.sum[2,2]


mperiod.plot.step.fawn =ggplot(data=period.df.m.fawn)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=mperiod.l,
                ymax=mperiod.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=periodgroups,y=mperiod.mn),size=1.2)+
  ggtitle("Male")+
    # ylim(-8.7,-6.1)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mperiod.plot.step.fawn 

ggsave(mperiod.plot.step.fawn ,file="figures/mperiod_stepplot_Fawn.pdf",height=7,width=7)



period.df.f.1 = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)
period.df.f.1$periodgroups <- as.numeric(as.character(period.df.f.1$periodgroups))
period.df.f.1$fperiod.mn
period.df.f.1$fperiod.mn <- period.df.f.1$fperiod.mn + age.df$age.mn[2]#+ baseline.haz.sum[1,2]
period.df.f.1$fperiod.l <- period.df.f.1$fperiod.l + age.df$age.mn[2]#+ baseline.haz.sum[1,2]
period.df.f.1$fperiod.u <- period.df.f.1$fperiod.u + age.df$age.mn[2]#+ baseline.haz.sum[1,2]

fperiod.plot.step.1=ggplot(data=period.df.f.1)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=fperiod.l,
                ymax=fperiod.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=periodgroups,y=fperiod.mn),size=1.2)+
  ggtitle("Female")+
    # ylim(-8.7,-6.1)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fperiod.plot.step.1

ggsave(fperiod.plot.step.1,file="figures/fperiod_stepplot_1.pdf",height=7,width=7)
 


#step plot over period

period.df.m.1 = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)
period.df.m.1$periodgroups <- as.numeric(as.character(period.df.m.1$periodgroups))
period.df.m.1$mperiod.mn <- period.df.m.1$mperiod.mn +age.df$age.mn[9]#+ baseline.haz.sum[2,2]
period.df.m.1$mperiod.l <- period.df.m.1$mperiod.l + age.df$age.mn[9]#+baseline.haz.sum[2,2]
period.df.m.1$mperiod.u <- period.df.m.1$mperiod.u + age.df$age.mn[9]#+baseline.haz.sum[2,2]


mperiod.plot.step.1 =ggplot(data=period.df.m.1)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=mperiod.l,
                ymax=mperiod.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=periodgroups,y=mperiod.mn),size=1.2)+
  ggtitle("Male")+
    # ylim(-8.7,-6.1)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mperiod.plot.step.1 

ggsave(mperiod.plot.step.1 ,file="figures/mperiod_stepplot_1.pdf",height=7,width=7)




period.df.f.2 = data.frame(periodgroups,fperiod.mn,fperiod.l,fperiod.u,sex)
period.df.f.2$periodgroups <- as.numeric(as.character(period.df.f.2$periodgroups))
period.df.f.2$fperiod.mn
period.df.f.2$fperiod.mn <- period.df.f.2$fperiod.mn + age.df$age.mn[3]#+ baseline.haz.sum[1,2]
period.df.f.2$fperiod.l <- period.df.f.2$fperiod.l + age.df$age.mn[3]#+ baseline.haz.sum[1,2]
period.df.f.2$fperiod.u <- period.df.f.2$fperiod.u + age.df$age.mn[3]#+ baseline.haz.sum[1,2]

fperiod.plot.step.2=ggplot(data=period.df.f.2)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=fperiod.l,
                ymax=fperiod.u),
            fill=Set2b[3],alpha=.8)+
  geom_step(aes(x=periodgroups,y=fperiod.mn),size=1.2)+
  ggtitle("Female")+
    # ylim(-8.7,-6.1)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

fperiod.plot.step.2

ggsave(fperiod.plot.step.2,file="figures/fperiod_stepplot_2.pdf",height=7,width=7)
 


#step plot over period

period.df.m.2 = data.frame(periodgroups,mperiod.mn,mperiod.l,mperiod.u,sex)
period.df.m.2$periodgroups <- as.numeric(as.character(period.df.m.2$periodgroups))
period.df.m.2$mperiod.mn <- period.df.m.2$mperiod.mn +age.df$age.mn[10]#+ baseline.haz.sum[2,2]
period.df.m.2$mperiod.l <- period.df.m.2$mperiod.l + age.df$age.mn[10]#+baseline.haz.sum[2,2]
period.df.m.2$mperiod.u <- period.df.m.2$mperiod.u + age.df$age.mn[10]#+baseline.haz.sum[2,2]


mperiod.plot.step.2 =ggplot(data=period.df.m.2)+
  geom_rect(aes(xmin=periodgroups,
                xmax=lead(periodgroups),
                ymin=mperiod.l,
                ymax=mperiod.u),
            fill=Set2b[5],alpha=.8)+
  geom_step(aes(x=periodgroups,y=mperiod.mn),size=1.2)+
  ggtitle("Male")+
    # ylim(-8.7,-6.1)+
  theme_bw()+xlab("Year")+ylab("Period Effect Monthly Conversion Hazard (Log)")+
  scale_x_continuous(breaks = seq(1,19,by=2),labels =seq(2002,2021,by=2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        title =element_text(size=14)
  )

mperiod.plot.step.2 

ggsave(mperiod.plot.step.2 ,file="figures/mperiod_stepplot_2.pdf",height=7,width=7)



# mperiod.plot.step.int <- mperiod.plot.step.int+ylab("")+theme(axis.text.y=element_blank())

# combo_period_step_plot_int <- grid.arrange(fperiod.plot.step.int,mperiod.plot.step.int,ncol=2)
# ggsave("figures/combo_period_step_plot_int.pdf",combo_period_step_plot_int,height=5,width=10)






####################################################################################
###
### Spatial FOI effects
###
#######################################################################################

space.mn=fit_sum[grep("space",rownames(fit_sum)),1]
space.l=fit_sum[grep("space",rownames(fit_sum)),4]
space.u=fit_sum[grep("space",rownames(fit_sum)),5]

space.mn


shape <-study.df 

shape$sect <- sect.num
shape$space.mn <- space.mn[sect.num]

space_plot <- ggplot(shape) +
                geom_sf(aes(fill=space.mn)) +
                scale_fill_viridis()+theme_bw()+
                # ggtitle("Spatial Effects")+
                theme(
                  # axis.text=element_blank(),
                # axis.title=element_blank(),
                # title = element_text(size=12, face='bold'),
                strip.text.x = element_text(size = 10),
                legend.title = element_text(size=12),
                legend.text = element_text(size=12))
ggsave(space_plot, file = "figures/space_plot.pdf")

#####################
###
### space + basemap
###
#####################
# library(ggmap)
# plot(shape$space.mn)
# plot(shape["space.mn"])

# st_crs(shape$geometry) <-  "EPSG:4326"
# shape_proj <- shape %>% st_transform("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000+y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# nn3 <- data.frame(df3proj)
# nn3 <- nn3 %>%
#   mutate(X = unlist(map(nn3$geometry,1)),
#          Y = unlist(map(nn3$geometry,2)))


# foi_basemap <- get_map(location=c(lon =, lat = ), zoom=11, maptype = 'terrain-background', source = 'stamen')


#  ggplot(shape) +
#                 geom_sf(aes(fill=space.mn)) +
#                 scale_fill_viridis()+theme_bw()+
#                 # ggtitle("Spatial Effects")+
#                 theme(
#                     # axis.text=element_blank(),
#                   # axis.title=element_blank(),
#                   # title = element_text(size=12, face='bold'),
#                   strip.text.x = element_text(size = 10),
#                   legend.title = element_text(size=12),
#                   legend.text = element_text(size=12)) 



#####################
###
### period + space
###
#####################

# shape$hetero.mn=hetero.mn
# shape$space.combo=shape$space.mn+shape$hetero.mn

# shape.ls=vector(mode="list",length=16)
# yrs=2002:2020
# for (i in 1:19){
#     shape.ls[[i]] <- shape %>% mutate(FOI= space.mn,year=yrs[i])
# }
# shape.out=do.call(rbind,shape.ls)

# space_plot <- ggplot(shape.out) +
#                 geom_sf(aes(fill=FOI)) +
#                 scale_fill_viridis()+theme_bw()+
#                 ggtitle("Spatial Effects")+
#                 theme(axis.text=element_blank(),
#                 axis.title=element_blank(),
#                 title =element_text(size=12, face='bold'),
#                 strip.text.x = element_text(size = 10),
#                 legend.title = element_text(size=12),
#                 legend.text = element_text(size=12))
# space_plot


# shape.ls=vector(mode="list",length=16)
# yrs=2002:2020
# for (i in 1:19){
#     shape.ls[[i]] <- shape %>% mutate(FOI= space.mn+mperiod.mn[i],year=yrs[i])
# }
# shape.out=do.call(rbind,shape.ls)


# foi.facet.plot=ggplot(shape.out)+geom_sf(aes(fill=FOI))+scale_fill_viridis()+theme_bw()+facet_wrap(~year)+theme_bw()+
#     ggtitle("Spatial Effects")+
#     theme(axis.text=element_blank(),
#         axis.title=element_blank(),
#         title =element_text(size=12, face='bold'),
#         strip.text.x = element_text(size = 10),
#         legend.title = element_text(size=12),
#         legend.text = element_text(size=12))
#         # legend.key.size = unit(1, 'in'), #change legend key size
#         # legend.key.height = unit(1, 'in'), #change legend key height
#         # legend.key.width = unit(1, 'in')) #change legend key width
# foi.facet.plot

# ggsave(foi.facet.plot,file="figures/foi_facet.pdf",height=9,width=8)
# ggsave(space_plot,file="figures/space_effects.pdf",height=7,width=7)

# library(gganimate)
# library(animation)
# foi.animated = ggplot(shape.out,aes(fill=FOI,frame=year))+geom_sf()+scale_fill_viridis(option="mako")+theme_bw()+
#   transition_manual(frames=year)+
# #   theme(legend.key.size = unit(2, 'cm'), #change legend key size
# #         legend.key.height = unit(2, 'cm'), #change legend key height
# #         legend.key.width = unit(2, 'cm'),
# #         legend.text=element_text(size=18),
#         theme(legend.text=element_text(size=12),
#         legend.title=element_text(size=12),
#         plot.title=element_text(size=12,face="bold")
#         )+labs(title='{current_frame}')
# foi.animated

# #forposter:
# # foi_anime <- animate(plot = foi.animated,renderer=gifski_renderer(),height=2400,width=2400)
# foi_anime <- animate(plot = foi.animated,renderer=gifski_renderer(),height=800,width=900)

# anim_save("foi_anime_forppt.gif")



# bins=seq(min(shape$yr_2002),max(shape$yr_2017),length=12)

##########################################################################################
############################################################################################3
##########################################################################################
###
### Incidence
###
##########################################################################################
##########################################################################################
##########################################################################################

# shape=st_transform(shape,crs=st_crs("+proj=longlat +datum=WGS84"))
# 
# names(shape)
# leaflet(shape)%>% addTiles %>% setView(lat=43.1,lng=-89.85,zoom=11)%>%  
#   addPolygons(fillColor = space.mn,weight=2,opacity=1,fillOpacity=.7,color="gray")
# 
# library(leaflet)
# 
# 
# names(shape)
# 
# pal<- colorBin("YlGnBu",domain=shape$yr_2002,bins=bins)
# leaflet(shape)%>% addTiles %>% setView(lat=43.1,lng=-89.85,zoom=11)%>% 
#   addPolygons(fillColor = ~pal(yr_2002),weight=2,opacity=1,fillOpacity=.7,color="gray")
# 
# leaflet(shape)%>% addTiles %>% setView(lat=43.1,lng=-89.85,zoom=11)%>% 
#   addPolygons(fillColor = ~pal(yr_2010),weight=2,opacity=1,fillOpacity=.7,color="gray")
# 
# leaflet(shape)%>% addTiles %>% setView(lat=43.1,lng=-89.85,zoom=11)%>% 
#   addPolygons(fillColor = ~pal(yr_2017),weight=2,opacity=1,fillOpacity=.7,color="gray")
# 
# 
# plot(shape)
# library(reshape2)
# melt(shape, id=paste0("yr_",2002:2017))
# name
# names(shape)
# pivot_longer(shape,!(names(shape)[1:15]))
# 
# gather(
#   shape,
#   key=paste0("yr_",2002:2017))
# 
# 
# %>% setView(lat=43.1,lng=-89.85,zoom=11)%>% 
#   addPolygons(fillColor = ~pal(),weight=2,opacity=1,fillOpacity=.7,color="gray")

Agegroups=1:n_age
mage.mn = fit_sum[grep("m_age",rownames(fit_sum)),1][1:n_age]
mage.l = fit_sum[grep("m_age",rownames(fit_sum)),4][1:n_age]
mage.u = fit_sum[grep("m_age",rownames(fit_sum)),5][1:n_age]
fage.mn = fit_sum[grep("f_age",rownames(fit_sum)),1][1:n_age]
fage.l = fit_sum[grep("f_age",rownames(fit_sum)),4][1:n_age]
fage.u = fit_sum[grep("f_age",rownames(fit_sum)),5][1:n_age]

###
### Calculating FOI quantities
###
###Monthly Version
foi.state.transition <- nimbleFunction(
  run = function(age_lookup=double(1),
                 Nage_lookup=double(0),
                 fcar_age=double(1),
                 mcar_age=double(1),
                 fcar_period=double(1),
                 mcar_period=double(1),
                 n_period=double(0)) {
    p <- nimArray(value = 0, c(Nage_lookup,n_period,2))
    gam <-nimArray(value = 0, c(Nage_lookup,n_period,2))

    for (t in 1:n_period){
      for (k in 1:Nage_lookup){
        gam[k,t,1] <- fcar_age[age_lookup[k]]+fcar_period[t]
        gam[k,t,2] <- mcar_age[age_lookup[k]]+mcar_period[t]
        # lam[k,t,1] <- sum(gam[1:k,t,1])
        # lam[k,t,2] <- sum(gam[1:k,t,2])
        p[k,t,1] <- exp(-sum(exp(gam[1:k,t,1])))
        p[k,t,2] <- exp(-sum(exp(gam[1:k,t,2])))
    }
    # for(j in 1:n_age){

    }
    # delta[j,t,1]  lam[1:,1,1]

    # }
    returnType(double(3))
    return(p[1:Nage_lookup,1:n_period,1:2])
  })

#testing state.transition function as R function
prob.infect<-foi.state.transition(age_lookup=age_lookup,
                 Nage_lookup=Nage_lookup,
                 fcar_age=fage.mn,
                 mcar_age=mage.mn,
                 fcar_period=fperiod.mn,
                 mcar_period=mperiod.mn,
                 n_period=n_period)

prob.infect.l<-foi.state.transition(age_lookup=age_lookup,
                 Nage_lookup=Nage_lookup,
                 fcar_age=fage.l,
                 mcar_age=mage.l,
                 fcar_period=fperiod.l,
                 mcar_period=mperiod.l,
                 n_period=n_period)

prob.infect.u<-foi.state.transition(age_lookup=age_lookup,
                 Nage_lookup=Nage_lookup,
                 fcar_age=fage.u,
                 mcar_age=mage.u,
                 fcar_period=fperiod.u,
                 mcar_period=mperiod.u,
                 n_period=n_period)

inc_f_mn <- c()
inc_m_mn <- c()
inc_f_l <- c()
inc_m_l <- c()
inc_f_u <- c()
inc_m_u <- c()

for(i in 1:n_period){
  inc_f_mn<-c(inc_f_mn,c(1-prob.infect[12,i,1],
            1-(prob.infect[24,i,1]/prob.infect[12,i,1]),# yearling female 2
            1-(prob.infect[36,i,1]/prob.infect[24,i,1]), ## 3
            1-(prob.infect[48,i,1]/prob.infect[36,i,1]),#4
            1-(prob.infect[72,i,1]/prob.infect[48,i,1]),#5
            1-(prob.infect[107,i,1]/prob.infect[72,i,1]),#9
            1-(prob.infect[108,i,1]/prob.infect[107,i,1])
            ))
 
  inc_m_mn<-c(inc_m_mn,c(1-prob.infect[12,i,2],
            1-(prob.infect[24,i,2]/prob.infect[12,i,2]),# yearling female 2
            1-(prob.infect[36,i,2]/prob.infect[24,i,2]), ## 3
            1-(prob.infect[48,i,2]/prob.infect[36,i,2]),#4
            1-(prob.infect[72,i,2]/prob.infect[48,i,2]),#5
            1-(prob.infect[107,i,2]/prob.infect[72,i,2]),#9
            1-(prob.infect[108,i,2]/prob.infect[107,i,2])
            ))
  inc_f_l<-c(inc_f_l,c(1-prob.infect.l[12,i,1],
            1-(prob.infect.l[24,i,1]/prob.infect.l[12,i,1]),# yearling female 2
            1-(prob.infect.l[36,i,1]/prob.infect.l[24,i,1]), ## 3
            1-(prob.infect.l[48,i,1]/prob.infect.l[36,i,1]),#4
            1-(prob.infect.l[72,i,1]/prob.infect.l[48,i,1]),#5
            1-(prob.infect.l[107,i,1]/prob.infect.l[72,i,1]),#9
            1-(prob.infect.l[108,i,1]/prob.infect.l[107,i,1])
            ))
    inc_m_l<-c(inc_m_l,c(1-prob.infect.l[12,i,2],
            1-(prob.infect.l[24,i,2]/prob.infect.l[12,i,2]),# yearling female 2
            1-(prob.infect.l[36,i,2]/prob.infect.l[24,i,2]), ## 3
            1-(prob.infect.l[48,i,2]/prob.infect.l[36,i,2]),#4
            1-(prob.infect.l[72,i,2]/prob.infect.l[48,i,2]),#5
            1-(prob.infect.l[107,i,2]/prob.infect.l[72,i,2]),#9
            1-(prob.infect.l[108,i,2]/prob.infect.l[107,i,2])
            ))
    inc_f_u<-c(inc_f_u,c(1-prob.infect.u[12,i,1],
            1-(prob.infect.u[24,i,1]/prob.infect.u[12,i,1]),# yearling female 2
            1-(prob.infect.u[36,i,1]/prob.infect.u[24,i,1]), ## 3
            1-(prob.infect.u[48,i,1]/prob.infect.u[36,i,1]),#4
            1-(prob.infect.u[72,i,1]/prob.infect.u[48,i,1]),#5
            1-(prob.infect.u[107,i,1]/prob.infect.u[72,i,1]),#9
            1-(prob.infect.u[108,i,1]/prob.infect.u[107,i,1])
            ))
    inc_m_u<-c(inc_m_u,c(1-prob.infect.u[12,i,2],
            1-(prob.infect.u[24,i,2]/prob.infect.u[12,i,2]),# yearling female 2
            1-(prob.infect.u[36,i,2]/prob.infect.u[24,i,2]), ## 3
            1-(prob.infect.u[48,i,2]/prob.infect.u[36,i,2]),#4
            1-(prob.infect.u[72,i,2]/prob.infect.u[48,i,2]),#5
            1-(prob.infect.u[107,i,2]/prob.infect.u[72,i,2]),#9
            1-(prob.infect.u[108,i,2]/prob.infect.u[107,i,2])
            ))
}

ages <-rep(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"),n_period)
year <-sort(rep(2002:2020,n_age))
prob_inf_female <- data.frame(ages,year,inc_f_mn,inc_f_l,inc_f_u)
prob_inf_male <- data.frame(ages,year,inc_m_mn,inc_m_l,inc_m_u)
names(prob_inf_female) <-names(prob_inf_male) <- c("ages","year","mean","lower","upper")
prob_inf_female$inc_mn <- prob_inf_female$mean*1000
prob_inf_female$inc_l <- prob_inf_female$lower*1000
prob_inf_female$inc_u <- prob_inf_female$upper*1000

prob_inf_male$inc_mn <- prob_inf_male$mean*1000
prob_inf_male$inc_l <- prob_inf_male$lower*1000
prob_inf_male$inc_u <- prob_inf_male$upper*1000


prob_inf_female$ages <- as.factor(prob_inf_female$ages)
prob_inf_female$ages <- factor(prob_inf_female$ages, 
                                levels = c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
prob_inf_male$ages <- as.factor(prob_inf_male$ages)
prob_inf_male$ages <- factor(prob_inf_male$ages, 
                                levels = c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
ages <-rep(c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"),n_period)
year <-sort(rep(2002:2020,n_age))

prob_inf_female <- data.frame(ages,year,inc_f_mn,inc_f_l,inc_f_u)
prob_inf_male <- data.frame(ages,year,inc_m_mn,inc_m_l,inc_m_u)
names(prob_inf_female) <-names(prob_inf_male) <- c("ages","year","mean","lower","upper")
prob_inf_female$inc_mn <- prob_inf_female$mean*1000
prob_inf_female$inc_l <- prob_inf_female$lower*1000
prob_inf_female$inc_u <- prob_inf_female$upper*1000

prob_inf_male$inc_mn <- prob_inf_male$mean*1000
prob_inf_male$inc_l <- prob_inf_male$lower*1000
prob_inf_male$inc_u <- prob_inf_male$upper*1000

prob_inf_female$ages <- as.factor(prob_inf_female$ages)
prob_inf_female$ages <- factor(prob_inf_female$ages, 
                                levels = c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))


prob_inf_male$ages <- as.factor(prob_inf_male$ages)
prob_inf_male$ages <- factor(prob_inf_male$ages, 
                                levels = c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))

library(RColorBrewer)
display.brewer.all(colorblindFriendly = T)
ryb_pal=brewer.pal(n = 8, name = "RdYlBu")[c(1:3,5:8)]

incidence_female_plot <- ggplot(prob_inf_female)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
  # scale_color_manual(values = ryb_pal)+
  scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Female incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
    ylim(0,205)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )
incidence_female_plot 
ggsave("figures/incidence_female_plot.pdf",incidence_female_plot,height=7,width=7)

  # geom_errorbar(aes(x=year,ymin=inc_l,ymax=inc_u,color=ages),width=.2)
  

incidence_male_plot <- ggplot(prob_inf_male)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
   scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Male incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
  ylim(0,205)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )
incidence_male_plot  
ggsave("figures/incidence_male_plot.pdf",incidence_male_plot,height=7,width=7)

prob_inf_female$sex <- "Female"
prob_inf_male$sex <- "Male"
inc_df <- rbind(prob_inf_female,prob_inf_male)
head(inc_df)
#combined
incidence_combo_plot <- ggplot(inc_df)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
    scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~sex)
incidence_combo_plot  


ggsave("figures/incidence_combo_plot.pdf",incidence_combo_plot,height=5,width=10)
ggsave("figures/incidence_combo_plot.png",incidence_combo_plot,height=4,width=7)

###
### adding error bars and making rows by age class
###
# prob_inf_female$ages <- as.factor(prob_inf_female$ages)
prob_inf_female$ages <- factor(prob_inf_female$ages, 
                                levels = c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+"))
inc_female_plot <- ggplot(prob_inf_female)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
  scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Female incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
    ylim(0,205)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~ages,ncol=1)


inc_female_plot 
ggsave("figures/incidence_bars_female_plot.pdf",inc_female_plot,height=7,width=7)




inc_F_plot <- ggplot(prob_inf_female)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
  geom_errorbar(aes(x=year,ymin=inc_l, ymax=inc_u,color=ages), width=.2)+
  scale_color_brewer("Ages",palette = "Dark2")+
  ylim(0,358)+
  ggtitle("Female")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        # axis.text.y=element_blank(),
        # axis.title.y=element_blank(),
        axis.title=element_text(size=14),
        title =element_text(size=12, face='bold'),
        legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~ages,nrow=7,ncol=1,scales="fixed")



inc_M_plot <- ggplot(prob_inf_male)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
  geom_errorbar(aes(x=year,ymin=inc_l, ymax=inc_u,color=ages), width=.2)+
      scale_color_brewer("Ages",palette = "Dark2")+
  ylim(0,358)+
  ggtitle("Male")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
  theme(axis.text=element_text(size=12),
  axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~ages,nrow=7,ncol=1,scales="fixed")



incidence_combo_eb <-grid.arrange(inc_F_plot,inc_M_plot,ncol=2)

ggsave("figures/incidence_combo_eb.pdf",incidence_combo_eb,height=10,width=8)


############################################################################
#####################################################
###
### incidence plots only 1.5 and 2.5 yr olds
###
#####################################################


library(RColorBrewer)
display.brewer.all(colorblindFriendly = T)
ryb_pal=brewer.pal(n = 8, name = "RdYlBu")[c(1:3,5:8)]

prob_inf_female_young12 <- prob_inf_female[prob_inf_female$ages %in% c("1.5","2.5"),]

incidence_female_plot_young12 <- ggplot(prob_inf_female_young12)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
  # scale_color_manual(values = ryb_pal)+
  scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Female incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
    ylim(0,205)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )
incidence_female_plot_young12
ggsave("figures/incidence_female_plot_young12.pdf",incidence_female_plot_young12,height=7,width=7)

  # geom_errorbar(aes(x=year,ymin=inc_l,ymax=inc_u,color=ages),width=.2)
  
prob_inf_male_young12 <- prob_inf_male[prob_inf_male$ages %in% c("1.5","2.5"),]

incidence_male_plot_young12 <- ggplot(prob_inf_male_young12)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
   scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Male incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
  ylim(0,205)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )
incidence_male_plot_young12  
ggsave("figures/incidence_male_plot_young12.pdf",incidence_male_plot_young12,height=7,width=7)

prob_inf_female_young12$sex <- "Female"
prob_inf_male_young12$sex <- "Male"
inc_df_young12 <- rbind(prob_inf_female_young12,prob_inf_male_young12)
head(inc_df_young12)
#combined
incidence_combo_plot_young12 <- ggplot(inc_df_young12)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
    scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~sex)
incidence_combo_plot_young12  


ggsave("figures/incidence_combo_plot_young12.pdf",incidence_combo_plot_young12,height=5,width=10)
ggsave("figures/incidence_combo_plot_young12.png",incidence_combo_plot_young12,height=4,width=7)


# #combined with age on axis
# library(ggh4x)
inc_df$age_num <- as.factor(inc_df$age)
levels(inc_df$age_num)=c(1.5,2.5,3.5,4.5,6.5,9.5,0.5)
inc_df$age_num <- as.numeric(as.character(inc_df$age_num))
inc_df$sex <-as.factor(inc_df$sex)
names(inc_df)[2]<- "Year"
#combined
incidence_combo_plot_age <- ggplot(inc_df)+
  geom_point(aes(x=age_num,y=inc_mn,color=Year)) +
  # geom_line(aes(x=age_num,y=inc_mn)) +
  ggtitle("Mean incidence for each age class")+
  xlab("Age")+
  ylab("New Cases per 1000 individuals")+
  facet_wrap(inc_df$sex,ncol=2,scales="fixed")+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )
incidence_combo_plot_age


ggsave("figures/incidence_combo_plot_age.pdf",incidence_combo_plot_age,height=5,width=10)
ggsave("figures/incidence_combo_plot_age.png",incidence_combo_plot_age,height=5,width=10)


#####################################################
###
### incidence plots only fawns, 1.5 and 2.5 yr olds
###
#####################################################


library(RColorBrewer)
display.brewer.all(colorblindFriendly = T)
ryb_pal=brewer.pal(n = 8, name = "RdYlBu")[c(1:3,5:8)]

prob_inf_female_young <- prob_inf_female[prob_inf_female$ages %in% c("Fawn","1.5","2.5"),]

incidence_female_plot_young <- ggplot(prob_inf_female_young)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
  # scale_color_manual(values = ryb_pal)+
  scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Female incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
    ylim(0,205)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )
incidence_female_plot_young
ggsave("figures/incidence_female_plot_young.pdf",incidence_female_plot_young,height=7,width=7)

  # geom_errorbar(aes(x=year,ymin=inc_l,ymax=inc_u,color=ages),width=.2)
  
prob_inf_male_young <- prob_inf_male[prob_inf_male$ages %in% c("Fawn","1.5","2.5"),]

incidence_male_plot_young <- ggplot(prob_inf_male_young)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
   scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Male incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
  ylim(0,205)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )
incidence_male_plot_young  
ggsave("figures/incidence_male_plot_young.pdf",incidence_male_plot_young,height=7,width=7)

prob_inf_female_young$sex <- "Female"
prob_inf_male_young$sex <- "Male"
inc_df_young <- rbind(prob_inf_female_young,prob_inf_male_young)
head(inc_df_young)
#combined
incidence_combo_plot_young <- ggplot(inc_df_young)+
  geom_point(aes(x=year,y=inc_mn,color=ages)) +
  geom_line(aes(x=year,y=inc_mn,color=ages)) +
    scale_color_brewer("Ages",palette = "Dark2")+
  ggtitle("Incidence for each age class")+
  xlab("Year")+
  ylab("New Cases per 1000 individuals")+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )+facet_wrap(~sex)
incidence_combo_plot_young  


ggsave("figures/incidence_combo_plot_young.pdf",incidence_combo_plot_young,height=5,width=10)
ggsave("figures/incidence_combo_plot_young.png",incidence_combo_plot_young,height=4,width=7)


# #combined with age on axis
# library(ggh4x)
inc_df$age_num <- as.factor(inc_df$age)
levels(inc_df$age_num)=c(1.5,2.5,3.5,4.5,6.5,9.5,0.5)
inc_df$age_num <- as.numeric(as.character(inc_df$age_num))
inc_df$sex <-as.factor(inc_df$sex)
names(inc_df)[2]<- "Year"
#combined
incidence_combo_plot_age <- ggplot(inc_df)+
  geom_point(aes(x=age_num,y=inc_mn,color=Year)) +
  # geom_line(aes(x=age_num,y=inc_mn)) +
  ggtitle("Mean incidence for each age class")+
  xlab("Age")+
  ylab("New Cases per 1000 individuals")+
  facet_wrap(inc_df$sex,ncol=2,scales="fixed")+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        # legend.position="none",
        strip.text.x = element_text(size = 12)
        # axis.text.x = element_text(angle = 45,vjust=.6)
  )
incidence_combo_plot_age


ggsave("figures/incidence_combo_plot_age.pdf",incidence_combo_plot_age,height=5,width=10)
ggsave("figures/incidence_combo_plot_age.png",incidence_combo_plot_age,height=5,width=10)



############################################################################


# seq(3,42,by=3)
# seq(1,40,by=3)
# inc_df=inc_df[order(inc_df$sex,inc_df$age_num),]

# inc_df %>% group_by(ages,year,)

# inc_df[inc_df$year==2002,]
prob_inf_female[prob_inf_female$year==2002,]
prob_inf_male[prob_inf_male$year==2002,]

prob_inf_female[prob_inf_female$year==2010,]
prob_inf_male[prob_inf_male$year==2010,]

prob_inf_female[prob_inf_female$year==2020,]
prob_inf_male[prob_inf_male$year==2020,]

prob_inf_female$inc_mn[prob_inf_female$year==2002]
prob_inf_female$inc_mn[prob_inf_female$year==2010]

percent_incr_female_2002_2020 <- 100*(prob_inf_female$inc_mn[prob_inf_female$year==2020] - prob_inf_female$inc_mn[prob_inf_female$year==2002])/prob_inf_female$inc_mn[prob_inf_female$year==2002]
percent_incr_female_2010_2020 <- 100*(prob_inf_female$inc_mn[prob_inf_female$year==2020] - prob_inf_female$inc_mn[prob_inf_female$year==2010])/prob_inf_female$inc_mn[prob_inf_female$year==2010]
percent_incr_female_2002_2010 <- 100*(prob_inf_female$inc_mn[prob_inf_female$year==2010] - prob_inf_female$inc_mn[prob_inf_female$year==2002])/prob_inf_female$inc_mn[prob_inf_female$year==2002]

percent_incr_male_2002_2020 <- 100*(prob_inf_male$inc_mn[prob_inf_male$year==2020] - prob_inf_male$inc_mn[prob_inf_male$year==2002])/prob_inf_male$inc_mn[prob_inf_male$year==2002]
percent_incr_male_2010_2020 <- 100*(prob_inf_male$inc_mn[prob_inf_male$year==2020] - prob_inf_male$inc_mn[prob_inf_male$year==2010])/prob_inf_male$inc_mn[prob_inf_male$year==2010]
percent_incr_male_2002_2010 <- 100*(prob_inf_male$inc_mn[prob_inf_male$year==2010] - prob_inf_male$inc_mn[prob_inf_male$year==2002])/prob_inf_male$inc_mn[prob_inf_male$year==2002]


percent_incr_female_2002_2020_l <- 100*(prob_inf_female$inc_l[prob_inf_female$year==2020] - prob_inf_female$inc_l[prob_inf_female$year==2002])/prob_inf_female$inc_l[prob_inf_female$year==2002]
percent_incr_female_2010_2020_l <- 100*(prob_inf_female$inc_l[prob_inf_female$year==2020] - prob_inf_female$inc_l[prob_inf_female$year==2010])/prob_inf_female$inc_l[prob_inf_female$year==2010]
percent_incr_female_2002_2010_l <- 100*(prob_inf_female$inc_l[prob_inf_female$year==2010] - prob_inf_female$inc_l[prob_inf_female$year==2002])/prob_inf_female$inc_l[prob_inf_female$year==2002]

percent_incr_male_2002_2020_l <- 100*(prob_inf_male$inc_l[prob_inf_male$year==2020] - prob_inf_male$inc_l[prob_inf_male$year==2002])/prob_inf_male$inc_l[prob_inf_male$year==2002]
percent_incr_male_2010_2020_l <- 100*(prob_inf_male$inc_l[prob_inf_male$year==2020] - prob_inf_male$inc_l[prob_inf_male$year==2010])/prob_inf_male$inc_l[prob_inf_male$year==2010]
percent_incr_male_2002_2010_l <- 100*(prob_inf_male$inc_l[prob_inf_male$year==2010] - prob_inf_male$inc_l[prob_inf_male$year==2002])/prob_inf_male$inc_l[prob_inf_male$year==2002]


percent_incr_female_2002_2020_u <- 100*(prob_inf_female$inc_u[prob_inf_female$year==2020] - prob_inf_female$inc_u[prob_inf_female$year==2002])/prob_inf_female$inc_u[prob_inf_female$year==2002]
percent_incr_female_2010_2020_u <- 100*(prob_inf_female$inc_u[prob_inf_female$year==2020] - prob_inf_female$inc_u[prob_inf_female$year==2010])/prob_inf_female$inc_u[prob_inf_female$year==2010]
percent_incr_female_2002_2010_u <- 100*(prob_inf_female$inc_u[prob_inf_female$year==2010] - prob_inf_female$inc_u[prob_inf_female$year==2002])/prob_inf_female$inc_u[prob_inf_female$year==2002]

percent_incr_male_2002_2020_u <- 100*(prob_inf_male$inc_u[prob_inf_male$year==2020] - prob_inf_male$inc_u[prob_inf_male$year==2002])/prob_inf_male$inc_u[prob_inf_male$year==2002]
percent_incr_male_2010_2020_u <- 100*(prob_inf_male$inc_u[prob_inf_male$year==2020] - prob_inf_male$inc_u[prob_inf_male$year==2010])/prob_inf_male$inc_u[prob_inf_male$year==2010]
percent_incr_male_2002_2010_u <- 100*(prob_inf_male$inc_u[prob_inf_male$year==2010] - prob_inf_male$inc_u[prob_inf_male$year==2002])/prob_inf_male$inc_u[prob_inf_male$year==2002]

per_f_2002_2020<-paste0(
round(percent_incr_female_2002_2020,2),"(",
round(percent_incr_female_2002_2020_l,2),",",
round(percent_incr_female_2002_2020_u,2),")")


per_f_2002_2010<-paste0(
round(percent_incr_female_2002_2010,2),"(",
round(percent_incr_female_2002_2010_l,2),",",
round(percent_incr_female_2002_2010_u,2),")")


per_f_2010_2020<-paste0(
round(percent_incr_female_2010_2020,2),"(",
round(percent_incr_female_2010_2020_l,2),",",
round(percent_incr_female_2010_2020_u,2),")")

per_m_2002_2010<-paste0(
round(percent_incr_male_2002_2010,2),"(",
round(percent_incr_male_2002_2010_l,2),",",
round(percent_incr_male_2002_2010_u,2),")")



per_m_2010_2020<-paste0(
round(percent_incr_male_2010_2020,2),"(",
round(percent_incr_male_2010_2020_l,2),",",
round(percent_incr_male_2010_2020_u,2),")")


per_m_2002_2020<-paste0(
round(percent_incr_male_2002_2020,2),"(",
round(percent_incr_male_2002_2020_l,2),",",
round(percent_incr_male_2002_2020_u,2),")")

Age_class <- c("Fawn","1.5","2.5","3.5","4.5-5.5","6.5-8.5","9.5+")

head(inc_df)

inc_female_2002 <-paste0(
  round(inc_df$inc_mn[inc_df$year==2002 & inc_df$sex=="Female"],2),"(",
  round(inc_df$inc_l[inc_df$year==2002 & inc_df$sex=="Female"],2),",",
  round(inc_df$inc_u[inc_df$year==2002 & inc_df$sex=="Female"],2),")")

inc_female_2010 <-paste0(
  round(inc_df$inc_mn[inc_df$year==2010 & inc_df$sex=="Female"],2),"(",
  round(inc_df$inc_l[inc_df$year==2010 & inc_df$sex=="Female"],2),",",
  round(inc_df$inc_u[inc_df$year==2010 & inc_df$sex=="Female"],2),")")

inc_female_2020 <-paste0(
  round(inc_df$inc_mn[inc_df$year==2020 & inc_df$sex=="Female"],2),"(",
  round(inc_df$inc_l[inc_df$year==2020 & inc_df$sex=="Female"],2),",",
  round(inc_df$inc_u[inc_df$year==2020 & inc_df$sex=="Female"],2),")")


inc_male_2002 <-paste0(
  round(inc_df$inc_mn[inc_df$year==2002 & inc_df$sex=="Male"],2),"(",
  round(inc_df$inc_l[inc_df$year==2002 & inc_df$sex=="Male"],2),",",
  round(inc_df$inc_u[inc_df$year==2002 & inc_df$sex=="Male"],2),")")

inc_male_2010 <-paste0(
  round(inc_df$inc_mn[inc_df$year==2010 & inc_df$sex=="Male"],2),"(",
  round(inc_df$inc_l[inc_df$year==2010 & inc_df$sex=="Male"],2),",",
  round(inc_df$inc_u[inc_df$year==2010 & inc_df$sex=="Male"],2),")")

inc_male_2020 <-paste0(
  round(inc_df$inc_mn[inc_df$year==2020 & inc_df$sex=="Male"],2),"(",
  round(inc_df$inc_l[inc_df$year==2020 & inc_df$sex=="Male"],2),",",
  round(inc_df$inc_u[inc_df$year==2020 & inc_df$sex=="Male"],2),")")



inc_f_out <- data.frame(Age_class,
                        inc_female_2002,
                        inc_female_2010,
                        inc_female_2020,
                        per_f_2002_2010,
                        per_f_2010_2020,
                        per_f_2002_2020)

inc_m_out <- data.frame(Age_class,
                        inc_male_2002,
                        inc_male_2010,
                        inc_male_2020,
                        per_m_2002_2010,
                        per_m_2010_2020,
                        per_m_2002_2020)

names(inc_f_out)<-names(inc_m_out)<-c("Age_class","inc2002","inc2010","inc2020","per2002_2010","per2010_2020","per2002_2020")

Sex <- c(rep("",3),"Female",rep("",3),
        rep("",3),"Male",rep("",3))

inc_tab <- data.frame(Sex,rbind(inc_f_out,inc_m_out))
print(xtable(inc_tab),include.rownames=FALSE)

##########################################################################################
##########################################################################################
###
### Fawn prevalence by year and space
###
##########################################################################################
##########################################################################################


################################################################3
###
### spatial prevalence plot
###
#################################################################

N.sect <- length(unique(cwd.df$sectionid))
# plot(core.df)
levels(cwd.df$age)=c("F","1.5","9.5+","2.5","3.5","4.5-5.5","6.5-8.5")
table(cwd.df$age)

#restrict to fawns only
fawn.df <-cwd.df %>% filter(age=="F")
apparent.temp <- fawn.df %>% group_by(sectionid) %>% summarise(n=n(),positive = sum(teststatus), prevalence = sum(teststatus)/n())
apparent.temp
# apparent.prev=apparent.temp %>% filter(prevalence !=1)
apparent.prev<-apparent.temp
apparent.prev


# study.df <- sf::st_read("~/Documents/Data/Core_Area/core_sections.shp")
# study.df=study.df[-8,]

#creating sections that account for range.direction first
study.df$dsection <- paste0(study.df$dir,"-",study.df$sectionid)

fawn.df$dsection <- do.call(paste, c(fawn.df[c("range.dir","range", "town", "sect")], sep = "-"))

fawn.df <- fawn.df[fawn.df$dsection %in% study.df$dsection,]
dim(fawn.df)




study.df$prevalence<-c(0)
study.df$n<-c(0)
study.df$positive<-c(0)

head(study.df)

for (i in 1:dim(apparent.prev)[1]) {
    for (j in 1:dim(study.df)[1]) {
        if (apparent.prev$sectionid[i] == study.df$sectionid[j]) {
            study.df$prevalence[j] <- apparent.prev$prevalence[i]
            study.df$n[j] <- apparent.prev$n[i]
            study.df$positive[j] <- apparent.prev$positive[i]
        }
    }
}
names(study.df)
head(study.df)
# save(study.df,file="study.df.Rdata")



# study.df <- study.df %>% left_join(apparent.prev, by = c("sectionid","year")) 
# names(study.df)
space_plot_prev<-ggplot(study.df)+geom_sf(aes(fill=prevalence))+theme_bw()+
    scale_fill_viridis()+
    ggtitle("Spatial Prevalence")+
    theme(axis.text=element_blank(),
        axis.title=element_blank(),
        title =element_text(size=12, face='bold'),
        strip.text.x = element_text(size = 10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))
space_plot_prev
# ggsave(space_plot_prev,file="figures/Prevalence_space_plot.pdf",height=4,width=8)

#######################
###
### year effect
###
##########################
fawn.df.female <-fawn.df[fawn.df$sex==1,]
fawn.df.male <-fawn.df[fawn.df$sex==0,]

#female
apparent.temp <- fawn.df.female %>% group_by(sectionid,kill.year) %>% summarise(n=n(),positive = sum(teststatus), prevalence = sum(teststatus)/n())
apparent.prev.female<-apparent.temp
names(apparent.prev.female)[2]="year"
study.df.yr.female <- study.df[rep(1:nrow(study.df),19),]
study.df.yr.female$year <- rep(2002:2020,each=N.sect)
study.df.yr.female$prevalence<-c(0)
study.df.yr.female$n<-c(0)
study.df.yr.female$positive<-c(0)
for (i in 1:dim(apparent.prev.female)[1]) {
    for(j in 1:dim(study.df.yr.female)[1]) {
        if (apparent.prev.female$sectionid[i] == study.df.yr.female$sectionid[j] & apparent.prev.female$year[i] == study.df.yr.female$year[j]) {
            study.df.yr.female$prevalence[j] <- apparent.prev.female$prevalence[i]
            study.df.yr.female$n[j] <- apparent.prev.female$n[i]
            study.df.yr.female$positive[j] <- apparent.prev.female$positive[i]
        }
    }
}
space_plot_prev_yr_female<-ggplot(study.df.yr.female)+geom_sf(aes(fill=prevalence))+theme_bw()+
    scale_fill_viridis()+
    ggtitle("Spatial Prevalence")+
    theme(axis.text=element_blank(),
        axis.title=element_blank(),
        title =element_text(size=12, face='bold'),
        strip.text.x = element_text(size = 10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))+facet_wrap(~year)
space_plot_prev_yr_female
ggsave(space_plot_prev_yr_female,file="figures/Prevalence_space_plot_year_female_fawn.pdf",height=6,width=8)
    
#male
apparent.temp <- fawn.df.male %>% group_by(sectionid,kill.year) %>% summarise(n=n(),positive = sum(teststatus), prevalence = sum(teststatus)/n())
apparent.prev.male<-apparent.temp
names(apparent.prev.male)[2]="year"
study.df.yr.male <- study.df[rep(1:nrow(study.df),19),]
study.df.yr.male$year <- rep(2002:2020,each=N.sect)
study.df.yr.male$prevalence<-c(0)
study.df.yr.male$n<-c(0)
study.df.yr.male$positive<-c(0)
study.df.yr.male$sex<-c(0)
for (i in 1:dim(apparent.prev.male)[1]) {
    for(j in 1:dim(study.df.yr.male)[1]) {
        if (apparent.prev.male$sectionid[i] == study.df.yr.male$sectionid[j] & apparent.prev.male$year[i] == study.df.yr.male$year[j]) {
            study.df.yr.male$prevalence[j] <- apparent.prev.male$prevalence[i]
            study.df.yr.male$n[j] <- apparent.prev.male$n[i]
            study.df.yr.male$positive[j] <- apparent.prev.male$positive[i]
        }
    }
}
space_plot_prev_yr_male<-ggplot(study.df.yr.male)+geom_sf(aes(fill=prevalence))+theme_bw()+
    scale_fill_viridis()+
    ggtitle("Spatial Prevalence")+
    theme(axis.text=element_blank(),
        axis.title=element_blank(),
        title =element_text(size=12, face='bold'),
        strip.text.x = element_text(size = 10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))+facet_wrap(~year)
space_plot_prev_yr_male
ggsave(space_plot_prev_yr_male,file="figures/Prevalence_space_plot_year_male_fawn.pdf",height=6,width=8)
    




apparent.temp = fawn.df %>% group_by(sex,kill.year) %>% summarise(n=n(),positive = sum(teststatus), prevalence = sum(teststatus)/n())
# apparent.temp
# apparent.prev=apparent.temp %>% filter(prevalence !=1)
apparent.prev=apparent.temp
apparent.prev$sex=as.factor(apparent.prev$sex)
levels(apparent.prev$sex)=c("Male","Female")
apparent.prev$sex <- factor(apparent.prev$sex,levels=c("Female","Male"))
# ggplot(apparent.prev,aes(fill=sex,y=prevalence,x=kill.year))+
#   geom_bar(position="dodge",stat="identity")+
#   scale_fill_manual(values=Set2b[c(3,5)])+
#   theme_bw()

apparent.prev


prev.plot.fawn = ggplot(apparent.prev,aes(fill=sex,y=prevalence,x=kill.year))+
  geom_bar(position="dodge",stat="identity")+
   scale_fill_manual("Sex",values=Set2b[c(3,5)],labels=c("Male","Female"))+
  theme_bw()+facet_wrap(~sex)+
  ylab("Apparent Prevalence Fawns")+xlab("Year")+labs("Age Class")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title =element_text(size=12, face='bold'),
        strip.text.x = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.position="none")#+
  # ggtitle("Core Area - Apparent Prevalence Fawns")
prev.plot.fawn

ggsave(prev.plot.fawn,file="figures/prev_plot_fawn.pdf",height=5,width=8)

ggsave(prev.plot.fawn,file="figures/prev_plot_fawn.png",height=5,width=8)






# apparent.temp <- fawn.df %>% group_by(sectionid,kill.year) %>% summarise(n=n(),positive = sum(teststatus), prevalence = sum(teststatus)/n())
# apparent.temp
# # apparent.prev=apparent.temp %>% filter(prevalence !=1)
# apparent.prev<-apparent.temp[apparent.temp$kill.year %in% 2017:2020,]

# apparent.prev
# names(apparent.prev)[2]="year"

# study.df <- rbind(study.df,study.df,study.df,study.df)
# study.df$year <- rep(2017:2020,each=N.sect)

# study.df$prevalence<-c(0)
# study.df$n<-c(0)
# study.df$positive<-c(0)

# head(study.df)

# for (i in 1:dim(apparent.prev)[1]) {
#     for(j in 1:dim(study.df)[1]) {
#         if (apparent.prev$sectionid[i] == study.df$sectionid[j] & apparent.prev$year[i] == study.df$year[j]) {
#             study.df$prevalence[j] <- apparent.prev$prevalence[i]
#             study.df$n[j] <- apparent.prev$n[i]
#             study.df$positive[j] <- apparent.prev$positive[i]
#         }
#     }
# }
# names(study.df)
# head(study.df)
# study.df.yr<-study.df
# save(study.df.yr,file="study.df.yr.Rdata")



# # study.df <- study.df %>% left_join(apparent.prev, by = c("sectionid","year")) 
# # names(study.df)
# space_plot_prev_yr<-ggplot(study.df.yr)+geom_sf(aes(fill=prevalence))+theme_bw()+
#     scale_fill_viridis()+
#     ggtitle("Spatial Prevalence")+
#     theme(axis.text=element_blank(),
#         axis.title=element_blank(),
#         title =element_text(size=12, face='bold'),
#         strip.text.x = element_text(size = 10),
#         legend.title = element_text(size=12),
#         legend.text = element_text(size=12))+facet_wrap(~year)
# space_plot_prev_yr
# ggsave(space_plot_prev_yr,file="figures/Prevalence_space_plot_year.pdf",height=4,width=8)


