require(lme4)
require(robustlmm)
require(ggplot2)
require(tidyr)
library(nlme)
parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="AH",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
sims=100
correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
correlation_df$combination<-as.factor(correlation_df$combination)
correlation_df$region<-cut(correlation_df$lat,breaks=c(min(correlation_df$lat)-1,-23.5,23.5,max(correlation_df$lat)+1), labels=c("Southern","Tropics","Northern"))
correlation_df$Region_Combined<-cut(abs(correlation_df$lat),breaks=c(0,23.5,max(abs(correlation_df$lat)+1)), labels=c("Tropics","Temperate"))


a<-lmer(data=correlation_df,corsI~abs(lat)*mismatch*long*log(pop)+(1|combination))
summary(a)
plot(correlation_df$corsI,resid(a)) #too weird?
summary(a)[[10]]
cbind(summary(a)[[10]][,c(1)]+summary(a)[[10]][,c(2)],summary(a)[[10]][,c(1)]-summary(a)[[10]][,c(2)])

a<-lmer(data=correlation_df,corsI~abs(lat)*mismatch*long+log(pop)+(1|combination))
summary(a)
plot(correlation_df$corsI,resid(a)) #too weird?
summary(a)[[10]]
cbind(summary(a)[[10]][,c(1)]+summary(a)[[10]][,c(2)],summary(a)[[10]][,c(1)]-summary(a)[[10]][,c(2)])
#rem long due to small effect sizes
a<-lmer(data=correlation_df,corsI~abs(lat)*mismatch+log(pop)+(1|combination))
summary(a)
plot(correlation_df$corsI,resid(a)) #too weird?
summary(a)[[10]]
cbind(summary(a)[[10]][,c(1)]+summary(a)[[10]][,c(2)],summary(a)[[10]][,c(1)]-summary(a)[[10]][,c(2)])
correlation_df$pred<-predict(a)
correlation_df$res<-resid(a)
ggplot(correlation_df,aes(abs(lat),corsI,col=mismatch))+geom_point()
ggplot(correlation_df,aes(abs(lat),pred,col=mismatch))+geom_point()

#mismatch as factor
e<-lmer(data=correlation_df,corsI~abs(lat)*as.factor(mismatch)+(1|combination))
#e<-lmer(data=correlation_df,corsI~(maxs-mins)*as.factor(mismatch)+log(pop)+(1|combination))
plot(correlation_df$corsI,resid(e)) #too weird?
correlation_df$pred<-predict(e)
correlation_df$res<-resid(e)
summary(e)
ggplot(correlation_df,aes(abs(lat),corsI,col=as.factor(mismatch)))+geom_point()
ggplot(correlation_df,aes(abs(lat),pred,col=as.factor(mismatch)))+geom_point()
 

#correlation as factor, diff and means instead of abs
correlation_df$diff<-correlation_df$maxs-correlation_df$mins
f<-lmer(data=correlation_df,corsI~means*diff*as.factor(mismatch)+(1|combination))
#e<-lmer(data=correlation_df,corsI~(maxs-mins)*as.factor(mismatch)+log(pop)+(1|combination))
plot(correlation_df$corsI,resid(f)) #too weird?
correlation_df$pred<-predict(f)
correlation_df$res<-resid(f)
summary(f)
ggplot(correlation_df,aes(diff,corsI,col=as.factor(mismatch)))+geom_point()
ggplot(data=correlation_df,aes(diff,pred,col=as.factor(mismatch)))+geom_point()
ggplot(data=correlation_df,aes(means,pred,col=as.factor(mismatch)))+geom_point()
ggplot(data=correlation_df,aes(diff,corsI,col=as.factor(mismatch)))+geom_point()
ggplot(data=correlation_df,aes(means,corsI,col=as.factor(mismatch)))+geom_point()

anova(e,f)

#using protocol
#mod<-lm(corsI~abs(lat)*mismatch*log(pop)*long,data=correlation_df)
plot(resid(mod),correlation_df$corsI)
mod<-gls(corsI~abs(lat)*mismatch*log(pop)*long,data=correlation_df,method = "REML")

#1minlim
vf2<- varIdent(form =~abs(lat)) 
mod2<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf2,data=correlation_df,method = "REML")

vf4<-varIdent(~abs(lat)*1|combination)
mod4<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf4,data=correlation_df,method = "REML")
vf5<- varIdent(form =~1|Region_Combined) 
mod5<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf5,data=correlation_df,method = "REML")
r5<-resid(mod5,type = "normalized")
coplot(r5~corsI|Region_Combined,data=correlation_df)
vf6<-varIdent(~1|combination*Region_Combined)
mod6<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf6,data=correlation_df,method = "REML")
#anova(mod,mod2,mod3,mod4,mod5,mod6)
vf7<- varIdent(form =~1|mismatch )
mod7<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf7,data=correlation_df,method = "REML")
vf10<- varIdent(form =~1|mismatch*Region_Combined )
mod10<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf10,data=correlation_df,method = "REML")
r10<-resid(mod10,type = "normalized")
coplot(r10~corsI|(mismatch*Region_Combined),data=correlation_df)
vf8<- varIdent(form =~abs(lat)* 1|mismatch)
mod8<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf8,data=correlation_df,method = "REML")
anova(mod,mod2,mod4,mod5,mod6,mod7,mod8,mod10,mod13)
vf13<- varIdent(form =~long) 
mod13<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf13,data=correlation_df,method = "REML")

#suggests that just added the region_combined is the onyly thing that has an effect
#10minlim
vf9<- varIdent(form =~abs(lat)*1|mismatch*combination )
mod9<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf9,data=correlation_df,method = "REML")
vf11<- varIdent(form =~1|mismatch*Region_Combined*combination )
mod11<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf11,data=correlation_df,method = "REML")
vf12<- varIdent(form =~1|mismatch*combination)
mod12<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf12,data=correlation_df,method = "REML")
vf3<- varIdent(form =~1|combination)
mod3<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf3,data=correlation_df,method = "REML")



