require(lme4)
require(robustlmm)
require(ggplot2)

library(nlme)
parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
sims=100
correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
correlation_df$combination<-as.factor(correlation_df$combination)
correlation_df$region<-cut(correlation_df$lat,breaks=c(min(correlation_df$lat)-1,-23.5,23.5,max(correlation_df$lat)+1), labels=c("Southern","Tropics","Northern"))
correlation_df$Region_Combined<-cut(abs(correlation_df$lat),breaks=c(0,23.5,max(abs(correlation_df$lat)+1)), labels=c("Tropics","Temperate"))


#head(correlation_df)
a<-lm(data=correlation_df,corsI~abs(lat)*mismatch)

a<-lmer(data=correlation_df,corsI~abs(lat)+long+mismatch+(1|combination))
a<-lmer(data=correlation_df,corsI~lat*mismatch+(1|combination))
a<-lmer(data=correlation_df,corsI~lat*mismatch+long+log(pop)+(1|combination))
a<-lmer(data=correlation_df,corsI~abs(lat)*mismatch+log(pop)+(1|combination))
#a<-rlmer(data=correlation_df,formula=corsI~abs(lat)*mismatch+long+(1|combination))
#a<-rlmer(data=correlation_df,formula=corsI~mismatch+(1|combination))

summary(a)
summary(a)
plot(correlation_df$corsI,resid(a)) #too weird?

correlation_df$pred<-predict(a)
correlation_df$res<-resid(a)
correlation_df$absres<-abs(correlation_df$res)
correlation_df$absres2<-correlation_df$absres^2
Levene.Model.F <- lm(absres2 ~ combination, data=correlation_df) #ANOVA of the squared residuals
anova(Levene.Model.F) 


plot(resid(a),x=correlation_df$corsI)
ggplot(data=correlation_df,aes(x=corsI,y=resid(a),colour=abs(lat)))+geom_point()
print(p)
#p+geom_point(aes(lat,pred,col=as.factor(mismatch)))

plot(correlation_df$pred,x=correlation_df$res)


a<-lmer(data=correlation_df,corsI~mismatch+(1|combination))
a<-lmer(data=correlation_df,corsI~region_cmismatch+(1|combination))

summary(a)
a<-lmer(data=correlation_df,corsI~(1|combination))
a<-lm(data = correlation_df,corsI~combination)
plot(b,correlation_df$mismatch)


a<-lmer(data=correlation_df,corsI~abs(lat)*mismatch+(1|combination))

a<-lmer(data=correlation_df,corsI~abs(lat)*as.factor(mismatch)+(1|combination))
a<-lmer(data=correlation_df,corsI~Region_Combined*as.factor(mismatch)+(1|combination))

summary(a)

plot(correlation_df$corsI,resid(a)) #too weird?






# scaled<-correlation_df
# scaled$pop<-scale(scaled$pop)
# scaled$lat<-scale(scaled$lat)
# scaled$long<-scale(scaled$long)
# scaled$corsI<-scale(scaled$corsI)
# scaled$combination<-as.factor(scaled$combination)
# scaleA<-lmer(data=scaled,corsI~lat+long+mismatch+(1|combination))
# scaleA<-lmer(data=correlation_df,corsI~lat+long+mismatch+log(pop)+(1|combination))
# scaleA<-lmer(data=correlation_df,corsI~mismatch+(1|combination))


summary(a)
b<-resid(a)  
plot(correlation_df$lat,b)
hist(b)

a<-glm(data=correlation_df,corsI~mismatch+lat+long+pop)

#option 1- mismatch and cluster analysis on residuals
a<-lm(data=correlation_df,corsI~as.factor(mismatch))
summary(a)



plot(correlation_df$lat,a$residuals)
res<-a$residuals
lat<-correlation_df$lat
require(mclust)
shorter<-correlation_df[,c(4,5,11)]
model1<-mclustBIC(shorter)
plot(model1)
summary(model1)
model1a<-Mclust(shorter,x=model1)
summary(model1a,parameters=TRUE)


shorter<-correlation_df[,c(4,5)]
model1<-mclustBIC(shorter)
plot(model1)
summary(model1)
model1a<-Mclust(shorter,x=model1)
summary(model1a,parameters=TRUE)

shorter<-cbind(res,lat)
model1<-mclustBIC(shorter)
plot(model1)
summary(model1)
model1a<-Mclust(shorter,x=model1)
summary(model1a,parameters=TRUE)
#ggplot(data=correlation_df,aes(abs(lat),corsI,col=mismatch))+geom_point()
#plot(correlation_df$mismatch,correlation_df$corsI)

#resid(a)

#option 2: split by latitude.
sims=100
correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
correlation_df$combination<-as.factor(correlation_df$combination)

a<-lmer(data=correlation_df,corsI~Region_Combined*mismatch+(1|combination))

summary(a)
a<-lmer(data=correlation_df,corsI~region*mismatch+(1|combination))
a<-lmer(data=correlation_df,corsI~region*mismatch+(1|combination)+(0+region|combination))



summary(a)
plot(a)
AIC(a)
hist(resid(a))
plot(a)
a<-lm(data=correlation_df,corsI~combination)
a<-lm(data=correlation_df,corsI~Region_Combined*mismatch)
summary(a)
b<-rbind(correlation_df$mismatch,correlation_df$Region_Combined)
plot(resid(a),predict(a))
AIC(a)
par(mfrow=c(2,2))
for(i in unique(correlation_df$region)){
  print(i)
  short<-correlation_df[which(correlation_df$region==i),]
  plot(as.factor(short$mismatch),short$corsI,ylim=c(-1,1))
}
#combine


#lmer test
 #singular fit due to lCK OF EFFECT?
mismatch<-c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
combination<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
corsI<-1*mismatch+combination
a<-lmer(corsI~mismatch+(1|combination))
summary(a)


bests<-correlation_df %>% group_by(Region_Combined,country,combination) %>% summarise(best=mismatch[which.max(corsI)],.groups="keep")
cont<-table(bests$Region_Combined,bests$best,bests$combination)
cont<-table(bests$Region_Combined,bests$best)
chisq.test(cont)
mantelhaen.test(cont)


a<-lmer(correlation_df$corsI~abs(correlation_df$lat)*as.factor(correlation_df$mismatch)+(1|correlation_df$combination))
summary(a)
plot(a)



vf<- varComb(varIdent(form=~1|combination ) ,  varIdent(form =~1|Region_Combined)) 
mod<-gls(corsI~abs(lat)*mismatch,weights=vf,data=correlation_df)
vf<- varComb(varIdent(form=~1|combination ) ,  varIdent(form =~abs(lat))) 
mod2<-gls(corsI~abs(lat)*mismatch,weights=vf,data=correlation_df)
vf<- varIdent(form =~abs(lat)) 
mod3<-gls(corsI~abs(lat)*mismatch,weights=vf,data=correlation_df)
vf<- varIdent(form =~1|combination * factor(Region_Combined))
mod4<-gls(corsI~abs(lat)*mismatch,weights=vf,data=correlation_df)
summary(mod4)



parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
sims=1
correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
correlation_df$combination<-as.factor(correlation_df$combination)
correlation_df$region<-cut(correlation_df$lat,breaks=c(min(correlation_df$lat)-1,-23.5,23.5,max(correlation_df$lat)+1), labels=c("Southern","Tropics","Northern"))
correlation_df$Region_Combined<-cut(abs(correlation_df$lat),breaks=c(0,23.5,max(abs(correlation_df$lat)+1)), labels=c("Tropics","Temperate"))

vf<- varIdent(form =~abs(lat)) 
vf<- varIdent(form =1|Region_Combined)
mod3<-gls(corsI~abs(lat)*mismatch,weights=vf,data=correlation_df)

mod4<-gls(corsI~abs(lat)*mismatch,data=correlation_df)
VF=varFixed(~abs(lat))
mod5<-gls(corsI~abs(lat)*mismatch,data=correlation_df,weights = VF)
VF=varIdent(~abs(lat))
mod6<-gls(corsI~abs(lat)*mismatch,data=correlation_df,weights = VF)
VF=varIdent(~1|Region_Combined)
mod7<-gls(corsI~abs(lat)*mismatch,data=correlation_df,weights = VF)
anova(mod4,mod5,mod6,mod7)
#so ignore model 5 and 7
parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
sims=100
correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
correlation_df$combination<-as.factor(correlation_df$combination)
correlation_df$region<-cut(correlation_df$lat,breaks=c(min(correlation_df$lat)-1,-23.5,23.5,max(correlation_df$lat)+1), labels=c("Southern","Tropics","Northern"))
correlation_df$Region_Combined<-cut(abs(correlation_df$lat),breaks=c(0,23.5,max(abs(correlation_df$lat)+1)), labels=c("Tropics","Temperate"))
mod<-gls(corsI~abs(lat)*mismatch,data=correlation_df)
vf<- varIdent(form =~abs(lat)) 
mod8<-gls(corsI~abs(lat)*mismatch,weights=vf,data=correlation_df)
vf<- varIdent(form =~1|factor(combination) )
mod9<-gls(corsI~abs(lat)*mismatch,data=correlation_df,weights = vf)
VF<-varIdent(~abs(lat)*1|factor(combination))
mod10<-gls(corsI~abs(lat)*mismatch,data=correlation_df,weights = VF)

anova(mod8,mod9,mod10,mod)

mod11<-gls(corsI~abs(lat)*mismatch,data=correlation_df)
vf<- varIdent(form =~1|Region_Combined) 
mod12<-gls(corsI~abs(lat)*mismatch,weights=vf,data=correlation_df)
vf<- varIdent(form =~1|factor(combination) )
mod13<-gls(corsI~abs(lat)*mismatch,data=correlation_df,weights = vf)
VF<-varIdent(~1|Region_Combined*1|factor(combination))
mod14<-gls(corsI~abs(lat)*mismatch,data=correlation_df,weights = VF)

anova(mod11,mod12,mod13,mod14)


mod15<-gls(corsI~abs(lat)*mismatch+combination,data=correlation_df)
vf<- varIdent(form =~1|Region_Combined) 
mod16<-gls(corsI~abs(lat)*mismatch+combination,weights=vf,data=correlation_df)
vf<- varIdent(form =~1|factor(combination) )
mod17<-gls(corsI~abs(lat)*mismatch+combination,data=correlation_df,weights = vf)
VF<-varIdent(~1|Region_Combined*1|factor(combination))
mod18<-gls(corsI~abs(lat)*mismatch+combination,data=correlation_df,weights = VF)
anova(mod15,mod16,mod17,mod18)

mod<-gls(corsI~abs(lat)*mismatch*log(pop)*long,data=correlation_df,method = "REML")

plot(resid(mod),correlation_df$corsI)
correlation_df$pred<-predict(mod)
correlation_df$res<-resid(mod)
correlation_df$absres<-abs(correlation_df$res)
correlation_df$absres2<-correlation_df$absres^2
Levene.Model.F <- lm(absres2 ~ combination, data=correlation_df) #ANOVA of the squared residuals
anova(Levene.Model.F) 
ggplot(data=correlation_df,aes(x=corsI,y=res,col=mismatch,size=log(pop)))+geom_point()
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






vf2<- varIdent(form =~abs(lat)) 
mod2<-gls(corsI~abmod11<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf11,data=correlation_df,method = "REML")
          vf12<- varIdent(form =~mismatch*cos(lat)*mismatch*log(pop)*long,weights = vf2,data=correlation_df,method = "REML")

vf4<-varIdent(~combination*abs(lat))
mod4<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf4,data=correlation_df,method = "REML")
vf5<- varIdent(form =~Region_Combined) 
mod5<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf5,data=correlation_df,method = "REML")
r5<-resid(mod5,type = "normalized")
coplot(r5~corsI|Region_Combined,data=correlation_df)
vf6<-varIdent(~combination*Region_Combined)
mod6<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf6,data=correlation_df,method = "REML")
#anova(mod,mod2,mod3,mod4,mod5,mod6)
vf7<- varIdent(form =~mismatch )
mod7<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf7,data=correlation_df,method = "REML")

vf10<- varIdent(form =~mismatch*Region_Combined )
mod10<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf10,data=correlation_df,method = "REML")

vf3<- varIdent(form =~combination)
mod3<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf3,data=correlation_df,method = "REML")
vf8<- varIdent(form =~mismatch*abs(lat) )
mod8<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf8,data=correlation_df,method = "REML")
vf9<- varIdent(form =~mismatch*abs(lat)*combination )
mod9<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf9,data=correlation_df,method = "REML")
vf11<- varIdent(form =~mismatch*Region_Combined*combination )
mod11<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf11,data=correlation_df,method = "REML")
vf12<- varIdent(form =~mismatch*combination)
mod12<-gls(corsI~abs(lat)*mismatch*log(pop)*long,weights = vf12,data=correlation_df,method = "REML")
anova(mod,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12)









