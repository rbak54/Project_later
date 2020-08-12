

source("../Model/modelling_functions.R"
       )
require("tidyverse")
require("ggplot2")
data_wider_means_summ<-read.csv("../../Data/data_wider_means_summ_POP.csv")
data_wider_means<-read.csv("../../Data/data_wider_means_POP.csv")

latlong=read.csv("../../Data/latlong/latlong_sel_short.csv")

plot_influenza<-function(parms){
  correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
  correlation_df$Region<-cut(correlation_df$lat,breaks=c(min(correlation_df$lat)-1,-23.5,23.5,max(correlation_df$lat)+1), labels=c("Southern","Tropics","Northern"))
  correlation_df$Region_Combined<-cut(abs(correlation_df$lat),breaks=c(0,23.5,max(abs(correlation_df$lat)+1)), labels=c("Tropics","Temperate"))
  correlation_df$combination<-as.factor(correlation_df$combination)
  correlation_df$mismatch<-as.factor(correlation_df$mismatch)
  bests<-correlation_df %>% group_by(country,lat,maxs,mins,Region,Region_Combined) %>% summarise(best=mismatch[which.max(corsI)],.groups="keep")
  
  meanAHs<-data_wider_means_summ$meanAH
  meanAHs<-rep(meanAHs,each=5)
  correlation_df$meanAH<-meanAHs
  correlation_df$meanT<-rep(data_wider_means_summ$meanT,each=5)
  #correlation_df_means<-as_tibble(correlation_df) %>% group_by(mismatch) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  correlation_df_means_country<-as_tibble(correlation_df) %>% group_by(mismatch,lat,maxs,mins,time_max,pop,country,Region_Combined,Region,meanAH,meanT,combination) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  
  #to get weekly values 
  model_means<-read.csv(paste0("../../Results/fromfunction/",sims,parms[["climate_label"]],".csv"))
  model_means$lat<-latlong[model_means$country,"V2"]
  model_means$country<-data_wider_means_summ[model_means$country,"country"]
  model_means$Region<-cut(model_means$lat,breaks=c(min(model_means$lat)-1,-23.5,23.5,max(model_means$lat)+1), labels=c("Southern","Tropics","Northern"))
  model_means$Region_Combined<-cut(abs(model_means$lat),breaks=c(0,23.5,max(abs(model_means$lat)+1)), labels=c("Tropics","Temperate"))
  #model_means$mismatch<-as.factor(model_means$mismatch)
  
 
  model_means_summary<-model_means %>% group_by(country,Region_Combined,mismatch,combination) %>% summarise(meanmeanR0=mean(meanR0),meanmeanI=mean(meanI),maxmeanR0=max(meanR0),maxmeanI=max(meanI))
  
  pdf(paste0("../../Writeup/draft plots/favs/",sims,parms[["climate_label"]],"rangelat.pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= abs(lat) ,y=maxs-mins))+geom_point()+xlab("Absolute Value of Latitude")+theme_bw()+ylab(paste0(parms[["climate_label_long"]]," Range"))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  
  pdf(paste0("../../Writeup/draft plots/favs/lats",sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= abs(lat), col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
          geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab("Absolute Value of Latitude") +ylab("Mean Correlation") +
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) + 
    geom_vline(xintercept=23.5)
  
  graphics.off()
  
  
  pdf(paste0("../../Writeup/draft plots/favs/boxandwhisker_facet",sims,parms[["climate_label"]],".pdf"),width = 10,height = 5)
  print(ggplot(data=correlation_df, aes(x= as.factor(mismatch),y= corsI)) +geom_boxplot()  +theme_bw()+xlab("Mismatch") +ylab("Mean Correlation")+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) +facet_wrap(~Region_Combined))
  graphics.off() 
  
  m<-model_means_summary %>%  group_by(Region_Combined,mismatch) %>% summarise(mI=mean(meanmeanI),mR=mean(meanmeanR0),maI=mean(maxmeanI),maR=mean(maxmeanR0), smI=std(meanmeanI),smR=std(meanmeanR0),smaI=std(maxmeanI),smaR=std(maxmeanR0))
  #correlation_df_means<-as_tibble(correlation_df) %>% group_by(mismatch) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  maxR<-max(m$maR)
  maxI<-max(m$maI)
  
  pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanI",parms[["climate_label"]],sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=m,aes(mismatch,mI,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Mismatch")+
          geom_errorbar(aes(ymin=mI-smI,ymax=mI+smI))+scale_y_continuous(limits = c(0,maxI), breaks = seq(0,maxI,by=0.1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanro",parms[["climate_label"]],sims,parms[["climate_label"]],".pdf"),width=7.5,height = 5)
  print(ggplot(data=m,aes(mismatch,mR,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean R0 Infected")+xlab("Mismatch")+
          geom_errorbar(aes(ymin=mR-smR,ymax=mR+smR))+scale_y_continuous(limits = c(0,maxR), breaks = seq(0,maxR,by=1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_maxmeanI",parms[["climate_label"]],sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=m,aes(mismatch,maI,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Maximum Proportion Infected")+xlab("Mismatch")+
          geom_errorbar(aes(ymin=maI-smaI,ymax=maI+smaI))+scale_y_continuous(limits = c(0,maxI), breaks = seq(0,maxI,by=0.1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  
    pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_maxmeanro",parms[["climate_label"]],sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=m,aes(mismatch,maR,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Maximum R0 Infected")+xlab("Mismatch")+
          geom_errorbar(aes(ymin=maR-smaR,ymax=maR+smaR))+scale_y_continuous(limits = c(0,maxR), breaks = seq(0,maxR,by=1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  
  # 
  # pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanI",i,sims,parms[["climate_label"]],".pdf"))
  # print(ggplot(data=m,aes(mismatch,meanmeanI,group=country, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Mismatch")+
  #         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  # graphics.off()
  # pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanro",i,sims,parms[["climate_label"]],".pdf"),width=7.5,height = 5)
  # print(ggplot(data=m,aes(mismatch,meanmeanR0,group=country, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean R0")+xlab("Mismatch")+
  #         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  # graphics.off()
  # pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_maxmeanI",i,sims,parms[["climate_label"]],".pdf"))
  # print(ggplot(data=m,aes(mismatch,maxmeanI,group=country, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Maximum Proportion Infected")+xlab("Mismatch")+
  #         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  # graphics.off()
  # pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_maxmeanro",i,sims,parms[["climate_label"]],".pdf"))
  # print(ggplot(data=m,aes(mismatch,maxmeanR0,group=country, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Maximum R0")+xlab("Mismatch")+
  #         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  # graphics.off()
  
  # for (j in 1:nrow(model_means)){
  # head(model_means)
  #  model_means$best<-bests[which(bests$country==model_means$country),"best"]
  
  # 4for (f in 1:nrow(model_means)){
  #  model_means$best[f]<-bests[which(bests$country==model_means$country[f]),"best"]
  # }
  #  model_means<-as.tibble(model_means) %>% mutate(best=)
  #  
  
  #   for (country in unique(model_means$country)){
  #     country_model_means<-model_means[which(model_means$country==country),]
  #     country_model_means$best<-rep(bests[which(bests$country==country),"best"],each=nrow(country_model_means))
  #     best_mismatch<-country_model_means[which(country_model_means$mismatch==country_model_means$best[[1]]),]
  #     country_data_wider_means<-data_wider_means[which(data_wider_means$country==country),]
  #     print(ggplot(best_mismatch,aes(week,meanI) )+geom_line()+geom_line(data = country_data_wider_means,aes(week,meanflu)))
  # }
  #   
  #   
  #   dummy_dataset<-country_model_means[,c(2,3,4,5)]
  #   dummy_dataset$meanI<-NA
  # 
  #   ggplot(best_mismatch, aes(x = week,meanI,group=mismatch,col=mismatch))  + geom_line(aes(week,meanI,col=mismatch,group=mismatch))  +geom_point(data=dummy_dataset, aes(week,meanI, group=mismatch,col=mismatch))
  # 
  #   #+geom_line(data=dummy_dataset,aes(meanI,group=mismatch,col=mismatch))
  # 
  #     
#  p <- ggplot(best_mismatch, aes(x = week))
  #  p <- p + geom_line(aes(y = meanI, group=mismatch,colour = mismatch))+ylab("Model I")
  #  p<- p+geom_point(data=dummy_dataset, aes(week,col=mismatch))
  #   
  #   # adding the relative humidity data, transformed to match roughly the range of the temperature
  #   p <- p + geom_line(data=country_data_wider_means,aes(y = 100*meanflu, colour = "Data"))
  #   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  #   # and, very important, reverting the above transformation
  #   p <- p + scale_y_continuous(sec.axis = sec_axis(~./100, name = "Data Flu Cases"))
  #    print(p)
  
  # modifying colours and theme options
  # p <- p + scale_colour_manual(values = c("blue][", "red"))
  # p <- p + labs(y = "Air temperature [°C]",
  #               x = "Date and time",
  #               colour = "Parameter")
  # #p <- p + theme(legend.position = c(0.8, 0.9))
  # p
  # 
  # 
  
  
    
  
  
  #model_means$lat[j]<-latlong[which(latlong$V1==model_means$country[j]),"V2"]
  #model_means_mean<-model_means %>% group_by(country,mismatch,Region,Region_Combined) %>% summarise(meanmeanR0=mean(meanR0),meanmeanI=mean(meanI),maxmeanR0=max(meanR0),maxmeanI=max(meanI),.groups="keep")
  #notpos correlation_df_means$Region<-cut(correlation_df_means$lat,breaks=c(min(correlation_df_means$lat)-1,-23.5,23.5,max(correlation_df_means$lat)+1), labels=c("Southern","Tropics","Northern"))
  #correlation_df_means_country$Region<-cut(correlation_df_means_country$lat,breaks=c(min(correlation_df_means_country$lat)-1,-23.5,23.5,max(correlation_df_means_country$lat)+1), labels=c("Southern","Tropics","Northern"))
    
  #a<-lm((correlation_df_means_country$maxs-correlation_df_means_country$mins)~abs(correlation_df_means_country$lat))
  #print(summary(a))
  #a$coefficients[1]+a$coefficients[2]*23.5
  #11.205 
  
  #interesting but not good enough
  # pdf(paste0("../../Writeup/draft plots/range_best",sims,parms[["climate_label"]],".pdf"))
  # print(ggplot(data=bests, aes(y= best ,x=maxs-mins))+geom_point()+ylab("Best Mismatch")+theme_bw()+xlab(paste0(parms[["climate_label_long"]]," Range"))+
  #         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  # graphics.off()
  # 
#   #about the same as latitude. so its difficult to know which causes (covary). model both and pick best???
#   print(ggplot(data=correlation_df_means_country, aes(x= maxs-mins, col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
#           geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab(paste0(parms[["climate_label_long"]]," Range")) +ylab("Mean Correlation") +
#           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) + 
#     geom_vline(xintercept=11.205)
#   print(ggplot(data=correlation_df_means_country, aes(x= abs(lat), col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
#           geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab(paste0(parms[["climate_label_long"]]," Range")) +ylab("Mean Correlation") +
#           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) + 
#     geom_vline(xintercept=11.205)
# #  print(
# 
# 
#   
#   #for some reason mean AH is a good predictor of mismatch????????    
#     ggplot(data=correlation_df_means_country, aes(x= meanAH, col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
#           geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab(paste0(parms[["climate_label_long"]]," Range")) +ylab("Mean Correlation") +
#           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) + 
#     geom_vline(xintercept=11.205)
#     ggplot(data=correlation_df_means_country, aes(x= meanT, col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
#       geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab(paste0(parms[["climate_label_long"]]," Range")) +ylab("Mean Correlation") +
#       theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) + 
#       geom_vline(xintercept=11.205)
#     #)
#   
  #for ah see above
  # graphics.off()
  # pdf(paste0("../../Writeup/draft plots/favs/boxandwhisker",sims,parms[["climate_label"]],".pdf"))
  # print(ggplot(data=correlation_df, aes(x= as.factor(mismatch),y= corsI)) +geom_boxplot()  +theme_bw()+xlab("Mismatch") +ylab("Mean Correlation")+
  #         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  # graphics.off()
  # 
  # pdf(paste0("../../Writeup/draft plots/erros",sims,parms[["climate_label"]],".pdf"))
  # print(ggplot(data=correlation_df_means, aes(x= as.factor(mismatch),y= means)) +geom_point()+theme_bw()+
  #         geom_errorbar(aes(ymin=means+errors,ymax=means-errors))+xlab("Mismatch") +ylab("Mean Correlation")+ 
  #         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  # 
  # graphics.off()
  # 
 
  
  
  # 
  # 
  # for(i in unique(correlation_df$Region)){
  #   short<-correlation_df[which(correlation_df$Region==i),]
  #   pdf(paste0("../../Writeup/draft plots/boxandwhisker_just",i,sims,parms[["climate_label"]],".pdf"))
  #   print(ggplot(data=short, aes(x= as.factor(mismatch),y= corsI)) +geom_boxplot()  +theme_bw()+xlab("Mismatch") +ylab("Mean Correlation")+
  #           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  #   graphics.off() 
  #   ##all below does is tell me how few southern there are. not clear trends within Region. so may be better to combine
  #   short<-correlation_df_means_country[which(correlation_df_means_country$Region==i),]
  #   print(i)
  #   print(length(unique(short$country)))
  #   pdf(paste0("../../Writeup/draft plots/just_",i,sims,parms[["climate_label"]],"rangelat.pdf"))
  #   print(ggplot(data=short, aes(x= lat ,y=maxs-mins))+geom_point()+xlab(" Latitude")+theme_bw()+ylab(paste0(parms[["climate_label_long"]]," Range"))+
  #           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  #   graphics.off()
  # }
  # 
  # 
  

  ##all below does is tell me how few southern there are. not clear trends within Region. so may be better to combine
  # short<-correlation_df_means_country[which(correlation_df_means_country$Region==i),]
  #  pdf(paste0("../../Writeup/draft plots/just_",i,sims,parms[["climate_label"]],"rangelat.pdf"))
  #  print(ggplot(data=short, aes(x= lat ,y=maxs-mins))+geom_point()+xlab(" Latitude")+theme_bw()+ylab(paste0(parms[["climate_label_long"]]," Range"))+
  #          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  #  graphics.off()
  

  
  
}



sims=1

#sims=160

parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
plot_influenza(parms)

#covid_plots(parms)
parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="RH",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Relative Humidity")

#plot_influenza(parms)
#need t oredo rh with proportion
sims=1
parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="AH",extra="Absolute Humidity",
              g=0.062,q0=-30.162,Climate_Variables=NA)
plot_influenza(parms)

pdf(paste0("../../Writeup/draft plots/favs/AHtemp.pdf"))
ggplot(data_wider_means,aes(meantemp,meanAH ))+geom_point()+theme_bw()+ylab("Absolute Humidity")+xlab("Temperature")+
         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) 
graphics.off()#covid_plots(parms)

#COVID
covid_means<-read.csv(paste0("../../Results/fromfunction/covid/1temperature_shift.csv"))
covid_means$lat<-latlong[covid_means$country,"V2"]
covid_means$country<-data_wider_means_summ[covid_means$country,"country"]
covid_means$Region<-cut(covid_means$lat,breaks=c(min(covid_means$lat)-1,-23.5,23.5,max(covid_means$lat)+1), labels=c("Southern","Tropics","Northern"))
shift_vec<-c(0,0.25,0.5,0.75,1)
covid_means$shift<-shift_vec[covid_means$shift+1]
covid_means$Region_Combined<-cut(abs(covid_means$lat),breaks=c(0,23.5,max(abs(covid_means$lat)+1)), labels=c("Tropics","Temperate"))
covid_means$mismatch<-as.factor(covid_means$mismatch)


covid_means_summary<-covid_means %>% group_by(country,Region_Combined,combination,shift) %>% summarise(meanmeanR0=mean(meanR0),meanmeanI=mean(meanI),maxmeanR0=max(meanR0),maxmeanI=max(meanI))



m<-covid_means_summary %>%  group_by(Region_Combined,shift) %>% summarise(mI=mean(meanmeanI),mR=mean(meanmeanR0),maI=mean(maxmeanI),maR=mean(maxmeanR0), smI=std(meanmeanI),smR=std(meanmeanR0),smaI=std(maxmeanI),smaR=std(maxmeanR0))
#correlation_df_means<-as_tibble(correlation_df) %>% group_by(mismatch) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
maxR<-max(m$maR)
maxI<-max(m$maI)

pdf(paste0("../../Writeup/draft plots/favs/shiftseverity_meanmeanI",sims,".pdf"))
print(ggplot(data=m,aes(shift,mI,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Shift")+
        geom_errorbar(aes(ymin=mI-smI,ymax=mI+smI),width=0.25)+scale_y_continuous(limits = c(0,maxI), breaks = seq(0,maxI,by=0.1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()
pdf(paste0("../../Writeup/draft plots/favs/shiftseverity_meanmeanro",sims,".pdf"),width=7.5,height = 5)
print(ggplot(data=m,aes(shift,mR,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean R0")+xlab("Shift")+
        geom_errorbar(aes(ymin=mR-smR,ymax=mR+smR))+scale_y_continuous(limits = c(0,maxR), breaks = seq(0,maxR,by=1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()
pdf(paste0("../../Writeup/draft plots/favs/shiftseverity_maxmeanI",sims,".pdf"))
print(ggplot(data=m,aes(shift,maI,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Maximum Proportion Infected")+xlab("Shift")+
        geom_errorbar(aes(ymin=maI-smaI,ymax=maI+smaI))+scale_y_continuous(limits = c(0,maxI), breaks = seq(0,maxI,by=0.1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()

pdf(paste0("../../Writeup/draft plots/favs/shiftseverity_maxmeanro",sims,".pdf"))
print(ggplot(data=m,aes(shift,maR,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Maximum R0")+xlab("Shift")+
        geom_errorbar(aes(ymin=maR-smaR,ymax=maR+smaR))+scale_y_continuous(limits = c(0,maxR), breaks = seq(0,maxR,by=1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()

#need to turn this into prediction! 
# for (each in unique(covid_means$shift)){
#   shift_subset<-covid_means[which(covid_means$shift==each),]
# #  pdf(paste0("../../Writeup/draft plots/covidtime_I_shift=",each,sims,parms[["climate_label"]],".pdf"))
#   print(ggplot(data=shift_subset,aes(week,meanI,group=country, colour = Region))+geom_point()+geom_line()+theme_bw()+ylab("Infected Proportion")+xlab("Week")+
#           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
#  # graphics.off()
#   
#   #pdf(paste0("../../Writeup/draft plots/favs/covidtime_R0_shift=",each,sims,parms[["climate_label"]],".pdf"))
#   print(ggplot(data=shift_subset,aes(week,meanR0,group=country, colour = Region))+geom_point()+geom_line()+theme_bw()+ylab("R0")+xlab("Week")+
#           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# #  graphics.off()
#   #pdf(paste0("../../Writeup/draft plots/covidtime_R0_facet_shift=",each,sims,parms[["climate_label"]],".pdf"))
#   print(ggplot(data=shift_subset,aes(week,meanR0,group=country))+geom_line()+theme_bw()+ylab("R0")+xlab("Week")+
#           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))+facet_wrap(~Region)) 
#   #graphics.off()
#   #pdf(paste0("../../Writeup/draft plots/covidtime_I_facet_shift=",each,sims,parms[["climate_label"]],".pdf"))
#   print(ggplot(data=shift_subset,aes(week,meanI,group=country))+geom_line()+theme_bw()+ylab("I")+xlab("Week")+
#           theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))+facet_wrap(~Region)) 
#   #graphics.off()
# }

trop<-covid_means[which(covid_means$Region=="Southern"),]
trop<-trop[which(trop$shift==0),]
#cambodia, india, niger, Australia, united Kingdom
# for (i in unique(trop$country)){
# trop2<-trop[which(trop$country==i),]
# png(paste0("../../Sandbox/COVIDTROPICS/",i,".png"))
# print(ggplot(data=trop2,aes(week,meanI))+geom_line()+theme_bw()+ylab("I")+xlab("Week")+
#         theme(legend.position = "none",text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)))
# graphics.off()
# }
noshift<-covid_means[which(covid_means$shift==0),]
noshift$Country<-noshift$country
selected_countries<-noshift[which(noshift$country %in% c("United Kingdom", "Cambodia","India","Australia","Niger")),]
pdf(paste0("../../Writeup/draft plots/favs/selectedseriesR0",".pdf"),width=10,height=5)
print(ggplot(data=selected_countries,aes(week,meanR0,group=Country,color=Region))+geom_line()+theme_bw()+ylab("R0")+xlab("Week")+
        theme(legend.position="bottom",text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))+facet_wrap(c("Country"),nrow=1)+
        theme(strip.background = element_blank(), strip.text.x = element_blank()) )
graphics.off()



covid_means_summ<-covid_means %>% group_by(Region,country,shift,combination) %>% summarise(peakweek=which.max(meanR0),peakI=which.max(meanI))
covid_means_summ<-covid_means_summ[which(covid_means_summ$shift==0),]
pdf(paste0("../../Writeup/draft plots/favs/covidtime_R0_peekweek_",sims,".pdf"),width=10,height=5)
print(ggplot(data=covid_means_summ,aes(y=peakweek,x=Region))+geom_boxplot()+theme_bw()+ylab("Week of Maximal R0")+xlab("Region")+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()
pdf(paste0("../../Writeup/draft plots/covidtime_I_peekweek_",sims,".pdf"))
print(ggplot(data=covid_means_summ,aes(y=peakI,x=Region))+geom_boxplot()+theme_bw()+ylab("Week of Maximal I")+xlab("Region")+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
graphics.off()
#how about plotting mean R0 and mean I and maxes vs shift
# 
# m<-covid_means %>% group_by(country,shift,Region) %>% summarise(meanmeanR0=mean(meanR0),meanmeanI=mean(meanI),maxmeanR0=max(meanR0),maxmeanI=max(meanI),.groups="keep")
# 
# m<-model_means_summary %>%  group_by(Region_Combined,mismatch) %>% summarise(mI=mean(meanmeanI),mR=mean(meanmeanR0),maI=mean(maxmeanI),maR=mean(maxmeanR0), smI=std(meanmeanI),smR=std(meanmeanR0),smaI=std(maxmeanI),smaR=std(maxmeanR0))
# #correlation_df_means<-as_tibble(correlation_df) %>% group_by(mismatch) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
# maxR<-max(m$maR)
# maxI<-max(m$maI)
# 
# pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanI",parms[["climate_label"]],i,sims,parms[["climate_label"]],".pdf"))
# print(ggplot(data=m,aes(mismatch,mI,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Mismatch")+
#         geom_errorbar(aes(ymin=mI-smI,ymax=mI+smI))+ylim(0,maxI)+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanro",parms[["climate_label"]],i,sims,parms[["climate_label"]],".pdf"),width=7.5,height = 5)
# print(ggplot(data=m,aes(mismatch,mR,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean R0 Infected")+xlab("Mismatch")+
#         geom_errorbar(aes(ymin=mR-smR,ymax=mR+smR))+ylim(0,maxR)+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_maxmeanI",parms[["climate_label"]],i,sims,parms[["climate_label"]],".pdf"))
# print(ggplot(data=m,aes(mismatch,maI,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Maximum Proportion Infected")+xlab("Mismatch")+
#         geom_errorbar(aes(ymin=maI-smaI,ymax=maI+smaI))+ylim(0,maxI)+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# 
# pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_maxmeanro",parms[["climate_label"]],i,sims,parms[["climate_label"]],".pdf"))
# print(ggplot(data=m,aes(mismatch,maR,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Maximum R0 Infected")+xlab("Mismatch")+
#         geom_errorbar(aes(ymin=maR-smaR,ymax=maR+smaR))+ylim(0,maxR)+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# 
# pdf(paste0("../../Writeup/draft plots/shiftseverity_meanmeanI",i,sims,parms[["climate_label"]],".pdf"))
# print(ggplot(data=m,aes(shift,meanmeanI,group=country, colour = Region))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Shift")+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# pdf(paste0("../../Writeup/draft plots/favs/shiftseverity_meanmeanro",i,sims,parms[["climate_label"]],".pdf"),width = 7.5,height = 5)
# print(ggplot(data=m,aes(shift,meanmeanR0,group=country, colour = Region))+geom_point()+geom_line()+theme_bw()+ylab("Mean R0")+xlab("Shift")+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# pdf(paste0("../../Writeup/draft plots/shiftseverity_maxmeanI",i,sims,parms[["climate_label"]],".pdf"))
# print(ggplot(data=m,aes(shift,maxmeanI,group=country, colour = Region))+geom_point()+geom_line()+theme_bw()+ylab("Maximum Proportion Infected")+xlab("Shift")+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# pdf(paste0("../../Writeup/draft plots/shiftseverity_maxmeanro",i,sims,parms[["climate_label"]],".pdf"))
# print(ggplot(data=m,aes(shift,maxmeanR0,group=country, colour = Region))+geom_point()+geom_line()+theme_bw()+ylab("Maximum R0")+xlab("Shift")+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# 
# data_with_lat<-cbind(data_wider_means_summ,latlong[,c(2,3)])
# 
# 
# 
# 
# 
# #this is absolutely not what I was expecting!!
# ggplot(data=data_with_lat,aes(V2,meanvartemp))+geom_point()
# ggplot(data=data_with_lat,aes(V2,varvartemp))+geom_point()
# #tropics temp varies less which makes sense
# ggplot(data=data_with_lat,aes(V2,meanvarflu))+geom_point()
# ggplot(data=data_with_lat,aes(V2,varvarflu))+geom_point()
# 
# 
# hist(data_wider_means_summ$meanvartemp)
# #}
# data_wider<-read.csv("../../Data/data_wider_means_POP.csv")
# pdf(paste0("../../Writeup/draft plots/favs/AHTEMPDATA.pdf"))
# print(ggplot(data=data_wider,aes(y=AH,x=T))+geom_point()+theme_bw()+ylab("Absolute Humidty")+xlab("Temperature")+
#         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
# graphics.off()
# 
# 
# 
# 
