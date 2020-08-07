source("../Model/modelling_functions.R")
require("tidyverse")
require("ggplot2")

plot4<-function(parms){
  correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
  bests<-correlation_df %>% group_by(country) %>% summarise(best=mismatch[which.max(corsI)],.groups="keep")
  
  correlation_df_means<-as_tibble(correlation_df) %>% group_by(mismatch) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  correlation_df_means_country<-as_tibble(correlation_df) %>% group_by(mismatch,lat,maxs,mins,time_max,pop) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  
  pdf(paste0("../../Writeup/draft plots/",sims,parms[["climate_label"]],"rangelat.pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= abs(lat) ,y=maxs-mins))+geom_point()+xlab("Absolute Value of Latitude")+theme_bw()+ylab(paste0(parms[["climate_label_long"]]," Range"))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  
  pdf(paste0("../../Writeup/draft plots/boxandwhisker",sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=correlation_df, aes(x= as.factor(mismatch),y= corsI)) +geom_boxplot()  +theme_bw()+xlab("Mismatch") +ylab("Mean Correlation")+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  
  pdf(paste0("../../Writeup/draft plots/erros",sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=correlation_df_means, aes(x= as.factor(mismatch),y= means)) +geom_point()+theme_bw()+
          geom_errorbar(aes(ymin=means+errors,ymax=means-errors))+xlab("Mismatch") +ylab("Mean Correlation")+ 
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  
  graphics.off()
  
  pdf(paste0("../../Writeup/draft plots/lats",sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= abs(lat), col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
          geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab("Absolute Value of Latitude") +ylab("Mean Correlation") +
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) + 
          geom_vline(xintercept=23.5)
  
  graphics.off()
  
  
}

sims=160

parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
plot4(parms)


parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="RH",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Relative Humidity")

plot4(parms)





