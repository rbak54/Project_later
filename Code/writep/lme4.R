require(lme4)
parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
sims=160
correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
correlation_df$combination<-as.factor(correlation_df$combination)
#head(correlation_df)
a<-lmer(data=correlation_df,corsI~lat+long+mismatch+log(pop)+(1|combination))

a<-lmer(data=correlation_df,corsI~mismatch+(1|combination))
summary(a)
a<-lmer(data=correlation_df,corsI~(1|combination))
a<-lm(data = correlation_df,corsI~combination)
plot(b,correlation_df$mismatch)
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
sims=160
correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
correlation_df$combination<-as.factor(correlation_df$combination)

correlation_df$region<-cut(correlation_df$lat,breaks=c(min(correlation_df$lat)-1,-23.5,23.5,max(correlation_df$lat)+1), labels=c("Southern","Tropics","Northern"))
a<-lmer(data=correlation_df,corsI~region*mismatch+(1|combination))
a<-lmer(data=correlation_df,corsI~region*mismatch+(1|combination)+(0+region|combination))

summary(a)
AIC(a)
hist(resid(a))
plot(a)
a<-lm(data=correlation_df,corsI~combination)
a<-lm(data=correlation_df,corsI~region*mismatch)
summary(a)
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
