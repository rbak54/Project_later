

source("../Model/modelling_functions.R"
       )
require("tidyverse")
require("ggplot2")


which_best<-function(country){
  return(data_wider_means[which(data_wider_means$country==country),"meanflu"])
}  

which_function<-function(country){
  return(bests$best[which(bests$country==country)])
}

data_wider_means_summ<-read.csv("../../Data/data_wider_means_summ_POP.csv")
data_wider_means<-read.csv("../../Data/data_wider_means_POP.csv")

latlong=read.csv("../../Data/latlong/latlong_sel_short.csv")

plot_influenza<-function(parms){
  correlation_df<-read.csv(paste0("../../Results/fromfunction/cors/",sims,parms[["climate_label"]],"correlation_dataframe.csv"))
  correlation_df$Region<-cut(correlation_df$lat,breaks=c(min(correlation_df$lat)-1,-23.5,23.5,max(correlation_df$lat)+1), labels=c("Southern","Tropics","Northern"))
  correlation_df$Region_Combined<-cut(abs(correlation_df$lat),breaks=c(0,23.5,max(abs(correlation_df$lat)+1)), labels=c("Tropics","Temperate"))
  correlation_df$combination<-as.factor(correlation_df$combination)
  correlation_df$mismatch<-as.factor(correlation_df$mismatch)
  if (parms[["climate_label"]]=="Temperature"){
  correlation_df$meanCl<-rep(data_wider_means_summ$meanT,each=5*sims)
  }
  if (parms[["climate_label"]]=="AH"){
    correlation_df$meanCl<-rep(data_wider_means_summ$meanAH,each=5*sims)
  }
  if (parms[["climate_label"]]=="RH"){
    correlation_df$meanCl<-rep(data_wider_means_summ$meanRH,each=5*sims)
  }
  #correlation_df_means<-as_tibble(correlation_df) %>% group_by(mismatch) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  correlation_df_means_country<-as_tibble(correlation_df) %>% group_by(mismatch,lat,country,maxs,mins,time_max,pop,Region_Combined,Region,meanCl) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  #to get weekly values 
  
 if(sims<2){
   bests<-correlation_df %>% group_by(country,lat,maxs,mins,Region,Region_Combined,meanCl) %>% summarise(best=mismatch[which.max(corsI)],.groups="keep")
   
   model_means<-read.csv(paste0("../../Results/fromfunction/",sims,parms[["climate_label"]],".csv"))
  model_means_combined<-model_means %>% group_by(country,mismatch,week) %>%  summarise(meanI=mean(meanI),meanR0=mean(meanR0),meanCl=mean(meanCl))
  model_means$lat<-latlong[model_means$country,"V2"]
  model_means$country<-data_wider_means_summ[model_means$country,"country"]
  model_means$Region<-cut(model_means$lat,breaks=c(min(model_means$lat)-1,-23.5,23.5,max(model_means$lat)+1), labels=c("Southern","Tropics","Northern"))
  model_means$Region_Combined<-cut(abs(model_means$lat),breaks=c(0,23.5,max(abs(model_means$lat)+1)), labels=c("Tropics","Temperate"))
  #model_means$mismatch<-as.factor(model_means$mismatch)


  model_means$best<-sapply(model_means$country,which_function)
  model_means_summary<-model_means %>% group_by(country,Region_Combined,mismatch) %>% summarise(meanmeanR0=mean(meanR0),meanmeanI=mean(meanI),maxmeanR0=max(meanR0),maxmeanI=max(meanI))
  
 
  m<-model_means_summary %>%  group_by(Region_Combined,mismatch) %>% summarise(mI=mean(meanmeanI),mR=mean(meanmeanR0),maI=mean(maxmeanI),maR=mean(maxmeanR0), smI=std(meanmeanI),smR=std(meanmeanR0),smaI=std(maxmeanI),smaR=std(maxmeanR0))
  #correlation_df_means<-as_tibble(correlation_df) %>% group_by(mismatch) %>% summarise(means=mean(corsI),errors=std(corsI),.groups="keep")
  maxR<-max(m$maR)
  maxI<-max(m$maI)
  
  pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanI",parms[["climate_label"]],sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=m,aes(mismatch,mI,group=Region_Combined, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Mismatch")+
          geom_errorbar(aes(ymin=mI-smI,ymax=mI+smI))+scale_y_continuous(limits = c(0,maxI), breaks = seq(0,maxI,by=0.1))+scale_x_continuous(limits = c(-0.125,1.125), breaks = seq(0,1,by=0.25))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanro",parms[["climate_label"]],sims,parms[["climate_label"]],".pdf"),width=7.5,height= 5)
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
  #less differencein R0 at higher meanAHs
  #mismathch of 0- increasing AH decreases R0
  #mismatch of 1 - increases AH increases R0
  
  #lit suggests hump shaped relationship with AH 
  
  # so at low humidities results consistent with increasing humdities decreasing R0
  #at high humidities results consistent with increasing humidities increasing R0
  # at intermediate humidities increase then decrease
  
  #could I plot cr at different rhs?
  
  
  
  ## which_best-function(mismatch){
  # return(model_means[which(bests$country==country)])
  #}
  
  model_means$mismatch<-as.factor(model_means$mismatch)
  #model_means_2<-model_means[which(model_means$mismatch==1),] 
  model_means_2<-model_means[which(model_means$mismatch==model_means$best),] 
  
  #a<-lm(meancr ~ poly(meanAH, degree=2), data=model_means_2)# 
  #summary(a)
  #could the reason be incorreact asumptions? 
  #get AH and contact rates for each country

  ggplot(model_means_2,aes(meanCl,meanR0,group=country))+geom_line() # 
  ggplot(model_means_2,aes(meanCl,meancr,group=country,col=best))+geom_line() # 
  ggplot(model_means_2,aes(meanCl,meanq,group=country,col=best))+geom_line() # 

  model_means_2$data=NA
  for (coun in unique(model_means_2$country)){
    data=which_best(coun)
    data_space=which(model_means_2$country==coun)
    data_space=data_space[1:length(data)]
    model_means_2[data_space,"data"]<-data
  }
  cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7","#000000")
  
  #make a function that finds how much to shift 
  # do for seperate then pic best
  #then go back to hump thing- look for temp= nothing
  #for (country in unique(model_means_2$country)){
  pdf(paste0("../../Writeup/draft plots/favs/",parms[["climate_label_long"]],"sel_I_TIME_DATA.pdf"),width=15,height=5)

  model_means_3<-model_means_2[which(model_means_2$mismatch==1),]
  if (parms[["climate_label"]]=="AH"){
      model_means_3<-model_means_2[which(model_means_2$country %in% c("El Salvador", "France", "Costa Rica",  "Panama","Paraguay")),] 
      model_means_3$c_order = factor(model_means_3$country, levels=c( "France","Paraguay","El Salvador","Costa Rica", "Panama"))  
        mult<-50

  }else{  model_means_3<-model_means_2[which(model_means_2$country %in% c("El Salvador", "France", "Dominican Republic",  "Senegal","Thailand")),] 
  model_means_3$c_order = factor(model_means_3$country, levels=c( "France", "Senegal","Thailand", "Dominican Republic","El Salvador"))  
  }
  mult=50
   #    mult<-max(model_means_3$meanI)/max(model_means_3$data,na.rm = T)
 # p<-ggplot(data=model_means_3, aes(week, meanI,group=mismatch,col=mismatch))+geom_line()+facet_wrap(~country)
  
  p<-ggplot(data=model_means_3, aes(week, meanI,group=mismatch,col=mismatch))+geom_line()+facet_wrap(~c_order,nrow=1)
  p <- p + geom_line(aes(y = mult*data, colour = "Data"))
  
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./mult, name = "Data Flu Cases"))
  p <- p + labs(x = "Week",
                y = "Model Number of Infectious", colour="Mismatch") +theme_bw()
  p<-p+  scale_colour_manual(values=cbPalette)
  p<-p+theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) 
  
  print(p)
  graphics.off()
  for (country in unique(model_means_2$country)){
    model_means_4<-model_means_2[which(model_means_2$country==country),]
    png(paste0("../../Writeup/draft plots/loop",country,parms[["climate_label_long"]],"sel_I_TIME_DATA.png"),width=50*10,height=50*5)
    mult<-max(model_means_4$meanI)/max(model_means_4$data,na.rm = T)
    
    p<-ggplot(data=model_means_4, aes(week, meanI,group=mismatch,col=mismatch))+geom_line()
    p <- p + geom_line(aes(y = mult*data, colour = "Data"))
    p <- p + scale_y_continuous(sec.axis = sec_axis(~./mult, name = "Data Flu Cases"))
    p <- p + labs(x = "Week",
                  y = "Model Number of Infectious", colour="Mismatch") +theme_bw()
    p<-p+  scale_colour_manual(values=cbPalette)
    p<-p+theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) 
    print(p)
    graphics.off()
  }

 }
  
  
   

  pdf(paste0("../../Writeup/draft plots/favs/",sims,parms[["climate_label"]],"rangelat.pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= abs(lat) ,y=maxs-mins))+geom_point()+xlab("Absolute Value of Latitude")+theme_bw()+ylab(paste0(parms[["climate_label_long"]]," Range"))+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  graphics.off()
  
  pdf(paste0("../../Writeup/draft plots/favs/lats",sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= abs(lat), col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
          geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab("Absolute Value of Latitude") +ylab("Mean Correlation") +
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) + 
    geom_vline(xintercept=23.5))
  
  graphics.off()
  
  
  pdf(paste0("../../Writeup/draft plots/lats",sims,parms[["climate_label"]],".pdf"))
  print(ggplot(data=correlation_df_means_country, aes(x= abs(lat), col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
          geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab("Absolute Value of Latitude") +ylab("Mean Correlation") +
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) + 
    geom_vline(xintercept=23.5))
  
  graphics.off()
  
  
  pdf(paste0("../../Writeup/draft plots/favs/boxandwhisker_facet",sims,parms[["climate_label"]],".pdf"),width= 10,height= 5)
  print(ggplot(data=correlation_df, aes(x= as.factor(mismatch),y= corsI)) +geom_boxplot()  +theme_bw()+xlab("Mismatch") +ylab("Mean Correlation")+
          theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)) +facet_wrap(~Region_Combined))
  graphics.off() 
  
 

#   #for some reason mean AH is a good predictor of mismatch????????    


pdf(paste0("../../Writeup/draft plots/favs/bestmean",parms[["climate_label"]],"_MISMATCH.pdf"))

print(ggplot(data=bests, aes(x= meanCl, y=best))+geom_point()+theme_bw() +geom_boxplot() +xlab(paste0("Mean Absolute Humidity")) +ylab("Best Mismatch") +
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)))
graphics.off()

pdf(paste0("../../Writeup/draft plots/favs/bestrange",parms[["climate_label"]],"_MISMATCH.pdf"))

print(ggplot(data=bests, aes(x= maxs-mins, y=best))+geom_point()+theme_bw() +geom_boxplot() +xlab(paste0("Best ",parms[["climate_label_long"]]," Range")) +ylab("Best Mismatch") +
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)))
graphics.off()


pdf(paste0("../../Writeup/draft plots/favs/alltemp",parms[["climate_label"]],"_MISMATCH.pdf"))

print(ggplot(data=correlation_df_means_country, aes(x= meanCl, col=as.factor(mismatch),y= means))+geom_point()+theme_bw()+labs(col="Mismatch") +
  geom_errorbar(aes(ymin=means+errors,ymax=means-errors)) +xlab(paste0("Mean",parms[["climate_label_long"]])) +ylab("Mean Correlation") +
  theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)))
graphics.off()




}  
#   
#p<-ggplot(data=model_means_2, aes(week, meanI,group=mismatch,col=mismatch))+geom_line()+facet_wrap(~country)
   #mult=10
   #  p <- ggplot(best_mismatch, aes(x = week))
   #  p <- p + geom_line(aes(y = meanI, group=mismatch,colour = mismatch))+ylab("Model I")
   #  p<- p+geom_point(data=dummy_dataset, aes(week,col=mismatch))
   #   
   #   # adding the relative humidity data, transformed to match roughly the range of the temperature
   #p <- p + geom_line(aes(y = mult*data, colour = "Data"))
   #   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
   #   # and, very important, reverting the above transformation
  # p <- p + scale_y_continuous(sec.axis = sec_axis(~./mult, name = "Data Flu Cases"))
  # p <- p + labs(x = "Week",
   #              x = "Model Number of Infectious") +theme_bw()
   #p#<-p+  scale_colour_manual(values=cbPalette)
   #p<-p+theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))
#print(p)

  
# 
# 
#data_wider_means[["country"]]

  # pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanI",i,sims,parms[["climate_label"]],".pdf"))
  # print(ggplot(data=m,aes(mismatch,meanmeanI,group=country, colour = Region_Combined))+geom_point()+geom_line()+theme_bw()+ylab("Mean Proportion Infected")+xlab("Mismatch")+
  #         theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))) 
  # graphics.off()
  # pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanro",i,sims,parms[["climate_label"]],".pdf"),width=7.5,height= 5)
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
  

  
  


sims=1

sims=160

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
pdf(paste0("../../Writeup/draft plots/favs/shiftseverity_meanmeanro",sims,".pdf"),width=7.5,height= 5)
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
        theme(strip.background = element_blank(), strip.text.x = element_blank())+
        theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15)))
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
# pdf(paste0("../../Writeup/draft plots/favs/mismatchseverity_meanmeanro",parms[["climate_label"]],i,sims,parms[["climate_label"]],".pdf"),width=7.5,height= 5)
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
# pdf(paste0("../../Writeup/draft plots/favs/shiftseverity_meanmeanro",i,sims,parms[["climate_label"]],".pdf"),width= 7.5,height= 5)
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
#general climate - showing mismatch
parms = list( mu = 2.06e-5,sigma = 0.68 ,p = 0.001, gamma =0.25,f=0.1,
              N = NA, nu = 5.07e-5, h=0.25 / 24 ,epsilon= 0.05, d=4/24,Max_cr=29.97,climate_label="Temperature",
              g=0.085,q0=-9.079,Climate_Variables=NA,climate_label_long="Temperature")
plot_influenza(parms)




# 
# for (max_cr in c(min(C),(min(C)+max(C))/2,(min(C)+max(C))/2,max(C))){
# CR<-cr_climate(c(max_cr,30),range_C = c(min(C),max(C)), Climate = C)
# q<-q_climate(g=parms[["g"]],q0=parms[["q0"]],Climate = C)
# dummy_dataframe<-as.data.frame(matrix(ncol=3,data=c(C,CR,q),byrow = F))
# pdf(paste0("../../Writeup/draft plots/favs/mismatch",max_cr,".pdf"))
# p<-ggplot(data=dummy_dataframe, aes(Temperature,Contact_Rate,col="Contact Rate"))+geom_line()
# p <- p + geom_line(aes(y = Survival_Rate+add, colour = "Survival Rate"))
#  p <- p + scale_y_continuous(sec.axis = sec_axis(~.-add, name = "Virus Growth Rate"))
# p <- p + scale_colour_manual(values = c("blue", "red"))
# p<-p+    theme_bw()    +theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))
# print(p)
# graphics.off()
# }
# }
# mismatch_temp(C)

#  p <- ggplot(best_mismatch, aes(x = week))
#  p <- p + geom_line(aes(y = meanI, group=mismatch,colour = mismatch))+ylab("Model I")
#  p<- p+geom_point(data=dummy_dataset, aes(week,col=mismatch))
#   
#   # adding the relative humidity data, transformed to match roughly the range of the temperature
#   # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#   # and, very important, reverting the above transformation
# p <- p + labs(x = "W",
#               x = "Model Number of Infectious") +theme_bw()

#p <- p + labs(y = "Air temperature [°C]",
 #             x = "Date and time",
  #            colour = "Parameter")
#p <- p + theme(legend.position = c(0.8, 0.9))

##this isn't working
#p<-p+  scale_colour_manual(values=cbPalette)
#pick a few of these and plot?(

C<-seq(5,25,length.out = 1000)
dummy_dataframe<-as.data.frame(matrix(ncol=5,nrow=length(C)*5))
colnames(dummy_dataframe)<-c("Climate","Contact_Rate","Survival_Rate","Max_cr","Mismatch")
dummy_dataframe$Climate<-rep(C,5)
range<-seq(min(C),max(C),length.out = 5)
dummy_dataframe$Max_cr<-rep(range,each=length(C))
dummy_dataframe$Mismatch<-(dummy_dataframe$Max_cr-min(C))/(max(C)-min(C))
for (k in unique(dummy_dataframe$Max_cr)){
  dummy_dataframe[which(dummy_dataframe$Max_cr==k),"Contact_Rate"]<-cr_climate(Max_Coordinates_cr = c(k,30),range_C=c(min(C),max(C)),Climate =C)
}
dummy_dataframe$Survival_Rate<-q_climate(q0 = parms[["q0"]],g=parms[["g"]],Climate = dummy_dataframe$Climate)

add<-30-max(dummy_dataframe$Survival_Rate,na.rm = T)
pdf(paste0("../../Writeup/draft plots/favs/",parms[["climate_label_long"]],"mismatch_vis.pdf"),width=15,height=5)

p<-ggplot(data=dummy_dataframe, aes(Climate,Contact_Rate,col="Contact Rate"))+geom_line()+
  geom_line(aes(y = Survival_Rate+add, colour = "Survival Rate"))+
  scale_y_continuous(sec.axis = sec_axis(~.-add, name = "Virus Survival Rate"))+ ylab("Human Contact Rate")+
  scale_colour_manual(values = c("blue", "red"))+facet_wrap(~Mismatch,nrow=1)+
  theme_bw()    +theme(text = element_text(size = 15),axis.text = element_text(size=15),axis.title = element_text(size=15))

print(p)
graphics.off()





ggplot(data=data_wider_means,aes(y=meanAH,meantemp,group=country, col=country))+geom_point()



ggplot(data=data_wider_means_summ,aes(x=meanT,y=meanvartemp))+geom_point()



ggplot(data=data_wider_means_summ,aes(x=meanT,y=meanvarflu))+geom_point()

ggplot(data=data_wider_means_summ,aes(x=maxT-minT,y=meanvartemp))+geom_point()


ggplot(data=data_wider_means_summ,aes(x=maxT-minT,y=(meanvarflu))/meanmeanflu)+geom_point()


cor.test(data_wider_means_summ$meanvarflu,data_wider_means_summ$maxT-data_wider_means_summ$minT)
#the question is how does variance account for scale 

  