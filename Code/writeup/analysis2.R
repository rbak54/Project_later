TEMP<-read.csv("../../Results/fromfunction/cors/100Temperaturecorrelation_dataframe.csv")

RH<-read.csv("../../Results/fromfunction/cors/100RHcorrelation_dataframe.csv")
AH<-read.csv("../../Results/fromfunction/cors/100AHcorrelation_dataframe.csv")
source("../Model/modelling_functions.R")
require(tidyverse)
TEMPS<-TEMP %>% group_by(combination,country) %>% summarise(bestcor=max(corsI))
RHS<-RH %>% group_by(combination,country) %>% summarise(bestcor=max(corsI))
AHS<-AH %>% group_by(combination,country) %>% summarise(bestcor=max(corsI))


mean(TEMPS$bestcor)
sd(TEMPS$bestcor)
TEMPSS<-TEMPS %>% group_by(country) %>% summarise(comvar=sd(bestcor))
mean(TEMPSS$comvar)
TEMPSS<-TEMPS %>% group_by(combination) %>% summarise(comvar=sd(bestcor))
mean(TEMPSS$comvar)

mean(TEMPS$bestcor)
std(TEMPS$bestcor)
TEMPSS<-TEMPS %>% group_by(country) %>% summarise(comvar=std(bestcor))
mean(TEMPSS$comvar)
TEMPSS<-TEMPS %>% group_by(combination) %>% summarise(comvar=std(bestcor))
mean(TEMPSS$comvar)





mean(AHS$bestcor)
sd(AHS$bestcor)
mean(RHS$bestcor)
sd(RHS$bestcor)


#AHSS<-AHS %>% group_by(country) %>% summarise(comvar=sd(bestcor))
#mean(AHSS$comvar)
#RHSS<-RHS %>% group_by(country) %>% summarise(comvar=sd(bestcor))
#mean(RHSS$comvar)
