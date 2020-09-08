rm(list=ls(all=TRUE))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(1234)

#Save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep="/")
DataDir       <- paste(BaseDir, "Data", sep="/")
FiguresDir    <- paste(BaseDir, "Figures", sep="/")
ResultsDir    <- paste(BaseDir, "Results", sep="/")

#Load Packages
setwd(CodesDir)
source("checkpackages.R")

packages<-c("caret","caretEnsemble","Metrics","xtable","tidyverse","cowplot","plyr","dplyr",
            "mboost","normtest","foreach","iterators","doParallel","hht",
            "pbapply","nortest","aTSA","MCDM","mco","forecast","scmamp","pracma","dplR","RWeka","tcltk")

sapply(packages,packs)

library(extrafont)
windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)

#Check how many cores there are
ncl<-detectCores();ncl

#Records the clusters to be used
cl <- makeCluster(ncl);registerDoParallel(cl)
###########################################################################
setwd(DataDir)
#Reading the data
dados_all     <-read.table(file="Meningite_Plot.csv",header=TRUE,sep=";",dec=".")
#Add Date
dados_all$date<-rep(seq(from=as.Date("2007-01-01"), to=as.Date("2018-12-31"), by="month"),times=4)

#Divide por Estado
dados_state   <-split(dados_all,dados_all$State)
#1- Minas Gerais, 2 - Paraná, 3 - Rio de Janeiro, 4 - São Paulo

#############################Plots

setwd(CodesDir)
source("PO_PLOTS.R")
source("PO_KSA.R")
#MG
setwd(ResultsDir)
load("Results_Ensemble_2019-08-29_State_MG1SA.RData")
load("Results_Ensemble_2019-08-29_State_MG2SA.RData")
load("Results_Ensemble_2019-08-28_State_MG3SA.RData")

mg1<-PO_kSA(dados_state[[1]][5:144,1],
            c(Results1SA[[3]][,6],Results1SA[[4]][,6]),
            c(Results2SA[[3]][1:128,6],Results2SA[[4]][1:12,6]),
            c(Results3SA[[3]][1:128,6],Results3SA[[4]][1:12,6]))


PLOTS(dados_state[[1]][5:144,1],             #Observações
      rbind(Results1SA[[3]],Results1SA[[4]]),#Previsões
      Results1SA[[6]],                       #Erros
      data.frame(Results1SA[[11]][[6]]),     #ParetoFront
      1,                                     #Passos a Frente
      "MG")                                  #Estado

PLOTS(dados_state[[1]][5:144,1],             #Observações
      rbind(Results2SA[[3]][1:128,],Results2SA[[4]][1:12,]),#Previsões
      Results2SA[[6]],                       #Erros
      data.frame(Results2SA[[11]][[6]]),     #ParetoFront
      2,                                     #Passos a Frente
      "MG")                                  #Estado

PLOTS(dados_state[[1]][5:144,1],             #Observações
      rbind(Results3SA[[3]][1:128,],Results3SA[[4]][1:12,]),#Previsões
      Results3SA[[6]],                       #Erros
      data.frame(Results3SA[[11]][[6]]),     #ParetoFront
      3,                                     #Passos a Frente
      "MG")                                  #Estado

#SP
setwd(ResultsDir)
load("Results_Ensemble_2019-08-30_State_SP1SA.RData")
load("Results_Ensemble_2019-08-29_State_SP2SA.RData")
load("Results_Ensemble_2019-08-28_State_SP3SA.RData")

sp1<-PO_kSA(dados_state[[4]][5:144,1],
            c(Results1SA[[3]][,6],Results1SA[[4]][,6]),
            c(Results2SA[[3]][1:128,6],Results2SA[[4]][1:12,6]),
            c(Results3SA[[3]][1:128,6],Results3SA[[4]][1:12,6]))


PLOTS(dados_state[[4]][5:144,1],             #Observações
      rbind(Results1SA[[3]],Results1SA[[4]]),#Previsões
      Results1SA[[6]],                       #Erros
      data.frame(Results1SA[[11]][[6]]),     #ParetoFront
      1,                                     #Passos a Frente
      "SP")                                  #Estado

PLOTS(dados_state[[4]][5:144,1],             #Observações
      rbind(Results2SA[[3]][1:128,],Results2SA[[4]][1:12,]),#Previsões
      Results2SA[[6]],                       #Erros
      data.frame(Results2SA[[11]][[6]]),     #ParetoFront
      2,                                     #Passos a Frente
      "SP")                                  #Estado

PLOTS(dados_state[[4]][5:144,1],             #Observações
      rbind(Results3SA[[3]][1:128,],Results3SA[[4]][1:12,]),#Previsões
      Results3SA[[6]],                       #Erros
      data.frame(Results3SA[[11]][[6]]),     #ParetoFront
      3,                                     #Passos a Frente
      "SP")                                  #Estado

#PR
setwd(ResultsDir)
load("Results_Ensemble_2019-09-01_State_PR1SA.RData")
load("Results_Ensemble_2019-08-29_State_PR2SA.RData")
load("Results_Ensemble_2019-08-29_State_PR3SA.RData")

pr1<-PO_kSA(dados_state[[2]][5:144,1],
            c(Results1SA[[3]][,6],Results1SA[[4]][,6]),
            c(Results2SA[[3]][1:128,6],Results2SA[[4]][1:12,6]),
            c(Results3SA[[3]][1:128,6],Results3SA[[4]][1:12,6]))

PLOTS(dados_state[[2]][5:144,1],             #Observações
      rbind(Results1SA[[3]],Results1SA[[4]]),#Previsões
      Results1SA[[6]],                       #Erros
      data.frame(Results1SA[[11]][[6]]),     #ParetoFront
      1,                                     #Passos a Frente
      "PR")                                  #Estado

PLOTS(dados_state[[2]][5:144,1],             #Observações
      rbind(Results2SA[[3]][1:128,],Results2SA[[4]][1:12,]),#Previsões
      Results2SA[[6]],                       #Erros
      data.frame(Results2SA[[11]][[6]]),     #ParetoFront
      2,                                     #Passos a Frente
      "PR")                                  #Estado

PLOTS(dados_state[[2]][5:144,1],             #Observações
      rbind(Results3SA[[3]][1:128,],Results3SA[[4]][1:12,]),#Previsões
      Results3SA[[6]],                       #Erros
      data.frame(Results3SA[[11]][[6]]),     #ParetoFront
      3,                                     #Passos a Frente
      "PR")                                  #Estado
#RJ
setwd(ResultsDir)
load("Results_Ensemble_2019-08-30_State_RJ1SA.RData")
load("Results_Ensemble_2019-08-29_State_RJ2SA.RData")
load("Results_Ensemble_2019-08-29_State_RJ3SA.RData")

rj1<-PO_kSA(dados_state[[3]][5:144,1],
            c(Results1SA[[3]][,6],Results1SA[[4]][,6]),
            c(Results2SA[[3]][1:128,6],Results2SA[[4]][1:12,6]),
            c(Results3SA[[3]][1:128,6],Results3SA[[4]][1:12,6]))

PLOTS(dados_state[[3]][5:144,1],             #Observações
      rbind(Results1SA[[3]],Results1SA[[4]]),#Previsões
      Results1SA[[6]],                       #Erros
      data.frame(Results1SA[[11]][[6]]),     #ParetoFront
      1,                                     #Passos a Frente
      "RJ")                                  #Estado

PLOTS(dados_state[[3]][5:144,1],             #Observações
      rbind(Results2SA[[3]][1:128,],Results2SA[[4]][1:12,]),#Previsões
      Results2SA[[6]],                       #Erros
      data.frame(Results2SA[[11]][[6]]),     #ParetoFront
      2,                                     #Passos a Frente
      "RJ")                                  #Estado

PLOTS(dados_state[[3]][5:144,1],             #Observações
      rbind(Results3SA[[3]][1:128,],Results3SA[[4]][1:12,]),#Previsões
      Results3SA[[6]],                       #Erros
      data.frame(Results3SA[[11]][[6]]),     #ParetoFront
      3,                                     #Passos a Frente
      "RJ")                                  #Estado

plot_grid(mg1,sp1,pr1,rj1,labels=c("A","B","C","D"),nrow=4)