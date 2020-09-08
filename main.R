rm(list=ls(all=TRUE))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(1234)

#Save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep="/")
DataDir       <- paste(BaseDir, "Data", sep="/")
#FiguresDir    <- paste(BaseDir, "Figures", sep="/")
#ResultsDir    <- paste(BaseDir, "Results", sep="/")

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

######################################################################
#Test de Stacionaridade
lapply(dados_state,function(x){adf.test(x[,1])})

#Seasonality test
#Ref: https://rstudio-pubs-static.s3.amazonaws.com/133842_3f1f6c0abf8942098fb90c322d9dd18b.html
#H0: There is no Seasonality
#H1: There is Seasonality

lapply(dados_state,function(x){kruskal.test(x[,1]~x[,3])})

lapply(dados_state,function(x){auto.arima(x[,1])})

#ACF plots

setwd(CodesDir)
source("ACF.R")

MG<-ACF(dados_state[[1]][,1]);PR<-ACF(dados_state[[2]][,1]);
RJ<-ACF(dados_state[[3]][,1]);SP<-ACF(dados_state[[4]][,1]);

plot_grid(MG,PR,RJ,SP, labels = c('A', 'B','C','D'), label_size = 18,ncol=4)

setwd(FiguresDir)
ggsave("ACFs_MGPRRJSP.eps", device=cairo_ps,width = 10,height = 5,dpi = 1200)
#####################################
setwd(CodesDir)
#1) Data;
#2) Pre-Processing avaiable in; https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/preProcess
#4) Crossover Probability;
#5) Mutation Probability;
#6) Adopt Models;
#7) Seeds.

models<-c("qrf","pls","brnn","gbm","cubist");
#One-month-ahead
source('MOO-1SA-ALL.R') 
MG-OSA<-PD_ENSEMBLE(dados_state[[1]],c("center","scale"),0.9,0.1,models,12,12)
PR-OSA<-PD_ENSEMBLE(dados_state[[2]],c("center","scale"),0.9,0.1,models,12,12)
RJ-OSA<-PD_ENSEMBLE(dados_state[[3]],c("center","scale"),0.9,0.1,models,12,12)
SP-OSA<-PD_ENSEMBLE(dados_state[[4]],c("center","scale"),0.9,0.1,models,12,12)

#Two-months-ahead
source('MOO-2SA-ALL.R') 

MG-DSA<-PD_ENSEMBLE(dados_state[[1]],c("center","scale"),0.9,0.1,models,12,12)
PR-DSA<-PD_ENSEMBLE(dados_state[[2]],c("center","scale"),0.9,0.1,models,12,12)
RJ-DSA<-PD_ENSEMBLE(dados_state[[3]],c("center","scale"),0.9,0.1,models,12,12)
SP-DSA<-PD_ENSEMBLE(dados_state[[4]],c("center","scale"),0.9,0.1,models,12,12)

#Three-months-ahead
source('MOO-3SA-ALL.R') 
MG-TSA<-PD_ENSEMBLE(dados_state[[1]],c("center","scale"),0.9,0.1,models,12,12)
SP-TSA<-PD_ENSEMBLE(dados_state[[4]],c("center","scale"),0.9,0.1,models,12,12)
PR-TSA<-PD_ENSEMBLE(dados_state[[2]],c("center","scale"),0.9,0.1,models,12,12)
RJ-TSA<-PD_ENSEMBLE(dados_state[[3]],c("center","scale"),0.9,0.1,models,12,12)

