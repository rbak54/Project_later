source("../Model/modelling_functions.R")
require("tidyverse")
require("ggplot2")
data_wider_means_summ<-read.csv("../../Data/data_wider_means_summ_POP.csv")
latlong=read.csv("../../Data/latlong/latlong_sel_short.csv")

plot4<-function(parms){
  correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
  correlation_df$region<-cut(correlation_df$lat,breaks=c(min(correlation_df$lat)-1,-23.5,23.5,max(correlation_df$lat)+1), labels=c("Southern","Tropics","Northern"))
  
  bests<-correlation_df %>% group_by(country,lat,maxs,mins) %>% summarise(best=mismatch[which.max(corsI)],.groups="keep")
  
  correlation_df_means<-as_tibble(correlation_df) %>% group_by(mismatch) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  correlation_df_means_country<-as_tibble(correlation_df) %>% group_by(mismatch,lat,maxs,mins,time_max,pop) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
 #notpos correlation_df_means$region<-cut(correlation_df_means$lat,breaks=c(min(correlation_df_means$lat)-1,-23.5,23.5,max(correlation_df_means$lat)+1), labels=c("Southern","Tropics","Northern"))
  correlation_df_means_country$region<-cut(correlation_df_means_country$lat,breaks=c(min(correlation_df_means_country$lat)-1,-23.5,23.5,max(correlation_df_means_country$lat)+1), labels=c("Southern","Tropics","Northern"))
  
  pdf(paste0("../../Writeup/draft plots/",sims,parms[["climate_label"]],"rangelat.pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= abs(lat) ,y=maxs-mins))+geom_point()+xlab("Absolute Value of Latitude")+theme_bw()+ylab(paste0(parms[["climate_label_long"]]," Range"))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  
  #interesting but not good enough
  pdf(paste0("../../Writeup/draft plots/range_best",sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=bests, aes(y= best ,x=maxs-mins))+geom_point()+ylab("Best Mismatch")+theme_bw()+xlab(paste0(parms[["climate_label_long"]]," Range"))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  
  #about the same as latitude. so its difficult to know which causes (covary). model both and pick best???
  pdf(paste0("../../Writeup/draft plots/rangecorr",sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= maxs-mins, col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
          geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab(paste0(parms[["climate_label_long"]]," Range")) +ylab("Mean Correlation") +
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) + 
    geom_vline(xintercept=23.5)
  
  
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
  
  

  correlation_df$combination<-as.factor(correlation_df$combination)
 
  for(i in unique(correlation_df$region)){
    short<-correlation_df[which(correlation_df$region==i),]
    pdf(paste0("../../Writeup/draft plots/boxandwhisker_just",i,sims,parms[["climate_label"]],".pdf"))
    print(ggplot(data=short, aes(x= as.factor(mismatch),y= corsI)) +geom_boxplot()  +theme_bw()+xlab("Mismatch") +ylab("Mean Correlation")+
            theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
    graphics.off() 
    ##all below does is tell me how few southern there are. not clear trends within region. so may be better to combine
    short<-correlation_df_means_country[which(correlation_df_means_country$region==i),]
    pdf(paste0("../../Writeup/draft plots/just_",i,sims,parms[["climate_label"]],"rangelat.pdf"))
    print(ggplot(data=short, aes(x= lat ,y=maxs-mins))+geom_point()+xlab(" Latitude")+theme_bw()+ylab(paste0(parms[["climate_label_long"]]," Range"))+
            theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
    graphics.off()
    
    
  
    }
  
  model_means<-read.csv(paste0("../../Results/fromfunction/",sims,parms[["climate_label"]],".csv"))
  model_means$lat<-latlong[model_means$country,"V2"]
  
  model_means$country<-data_wider_means_summ[model_means$country,"country"]
 # for (j in 1:nrow(model_means)){
  #model_means$lat[j]<-latlong[which(latlong$V1==model_means$country[j]),"V2"]
  #}#
  model_means$region<-cut(model_means$lat,breaks=c(min(model_means$lat)-1,-23.5,23.5,max(model_means$lat)+1), labels=c("Southern","Tropics","Northern"))
  
  m<-model_means %>% group_by(country,mismatch,region) %>% summarise(meanmeanR0=mean(meanR0),meanmeanI=mean(meanI),maxmeanR0=max(meanR0),maxmeanI=max(meanI),.groups="keep")
  
  pdf(paste0("../../Writeup/draft plots/mismatchseverity_meanmeanI",i,sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=m,aes(mismatch,meanmeanI,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Mismatch")+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  pdf(paste0("../../Writeup/draft plots/mismatchseverity_meanmeanro",i,sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=m,aes(mismatch,meanmeanR0,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Mean R0")+xlab("Mismatch")+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  pdf(paste0("../../Writeup/draft plots/mismatchseverity_maxmeanI",i,sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=m,aes(mismatch,maxmeanI,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Maximum Proportion Infected")+xlab("Mismatch")+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  pdf(paste0("../../Writeup/draft plots/mismatchseverity_maxmeanro",i,sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=m,aes(mismatch,maxmeanR0,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Maximum R0")+xlab("Mismatch")+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
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

parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
sims=1
covid_means<-read.csv("../../Results/fromfunction/covid/1temperature_shift.csv")
covid_means$lat<-latlong[covid_means$country,"V2"]
covid_means$country<-data_wider_means_summ[covid_means$country,"country"]
covid_means$region<-cut(covid_means$lat,breaks=c(min(covid_means$lat)-1,-23.5,23.5,max(covid_means$lat)+1), labels=c("Southern","Tropics","Northern"))
#need to turn this into prediction! 
for (each in unique(covid_means$shift)){
    shift_subset<-covid_means[which(covid_means$shift==each),]
  pdf(paste0("../../Writeup/draft plots/covidtime_I_shift=",each,sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=shift_subset,aes(week,meanI,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Infected Proportion")+xlab("Week")+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()

pdf(paste0("../../Writeup/draft plots/covidtime_I_shift=",each,sims,parms[["climate_label"]],".pdf"))
print(ggplot(data=shift_subset,aes(week,meanR0,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("R0")+xlab("Week")+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()
}
#how about plotting mean R0 and mean I and maxes vs shift

m<-covid_means %>% group_by(country,shift,region) %>% summarise(meanmeanR0=mean(meanR0),meanmeanI=mean(meanI),maxmeanR0=max(meanR0),maxmeanI=max(meanI),.groups="keep")

pdf(paste0("../../Writeup/draft plots/shiftseverity_meanmeanI",i,sims,parms[["climate_label"]],".pdf"))
print(ggplot(data=m,aes(shift,meanmeanI,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Shift")+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()
pdf(paste0("../../Writeup/draft plots/shiftseverity_meanmeanro",i,sims,parms[["climate_label"]],".pdf"))
print(ggplot(data=m,aes(shift,meanmeanR0,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Mean R0")+xlab("Shift")+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()
pdf(paste0("../../Writeup/draft plots/shiftseverity_maxmeanI",i,sims,parms[["climate_label"]],".pdf"))
print(ggplot(data=m,aes(shift,maxmeanI,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Maximum Proportion Infected")+xlab("Shift")+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()
pdf(paste0("../../Writeup/draft plots/shiftseverity_maxmeanro",i,sims,parms[["climate_label"]],".pdf"))
print(ggplot(data=m,aes(shift,maxmeanR0,group=country, colour = region))+geom_point()+geom_line()+theme_bw()+ylab("Maximum R0")+xlab("Shift")+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()



#}
