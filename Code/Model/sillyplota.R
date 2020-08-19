t<-read.csv("../../Results/fromfunction/cors/1Temperaturecorrelation_dataframe.csv")
t<-read.csv("../../Results/fromfunction/cors/1AHcorrelation_dataframe.csv")

head(t)
require(tidyverse)
require(ggplot2)

t2 <- t %>% group_by(country,time_max,means,lat,long,pop) %>% summarise(best=mismatch[which.max(corsI)])
head(t2)

ggplot(data=t2,aes(means,best))+geom_point()+geom_jitter()

ggplot(data=t,aes(means,corsI,col=as.factor(mismatch)))+geom_point()
