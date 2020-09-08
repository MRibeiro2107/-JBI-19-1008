PD<-function(data,model,pp,cp,mp,seed1,seed2)
{

  options(warn=-1)
  set.seed(seed1)
  setwd(CodesDir)
  
  fun<-c('Decomposition_EEMD.R','Feature_Engineering.R',"Feature_Importance.R","Metricas.R")
  sapply(fun,function(x){source(x)})

  data_dec   <-Decomposition_EEMD(data)
  data_dec1  <-list()
  cat("\014")
 #Objetos para salvar os resultados
  Models <-list();  
  Params <-list();
  PTrOSA <-matrix(nrow=128,ncol=dim(data_dec)[2]+3); 
  PTrKSA <-matrix(nrow=128,ncol=dim(data_dec)[2]+3)
  PTeOSA <-matrix(nrow=12 ,ncol=dim(data_dec)[2]+3);
  PTeKSA <-matrix(nrow=12 ,ncol=dim(data_dec)[2]+3)
  ETrOSA <-matrix(nrow=128 ,ncol=3);
  ETrKSA <-matrix(nrow=128 ,ncol=3);
  ETeOSA <-matrix(nrow=12 ,ncol=3);
  ETeKSA <-matrix(nrow=12 ,ncol=3);
  P      <-matrix(nrow=4, ncol=6)
  row.names(P)<-c("RMSE","R2","MAE","SMAPE")
  colnames(P) <-c("Tr-WO","Te-WO","Tr-MO","Te-MO","Tr-SD","Te-SD")
  thetas  <-matrix(nrow=1,ncol=dim(data_dec)[2],byrow=FALSE) 
  ParetoFront<-list()
  
  pb <- tkProgressBar(title = "Please wait...", min = 0, max = dim(data_dec)[2], width = 300)
  
 for(i in 1:dim(data_dec)[2])
 {
  #--------------------------Construindo Conjuno-----------------------------#
   data_dec1[[i]]<-data.frame(data_dec[5:144,i],   #Component
                             data_dec[4:143,i],   #Lag1
                             data_dec[3:142,i],   #Lag2
                             data_dec[2:141,i],   #Lag3
                             data_dec[1:140,i])   #Lag4
  colnames(data_dec1[[i]])<-c(names(data_dec[i]),paste("Lag",1:4,sep=""))
  
  # Var<-match(names(data_dec[i]),names(data_dec1[[i]]));
  # 
  # #--------------------------Feature Engineering----------------------------#
  # Features<-FE(as.data.frame(data_dec1[[i]][,-Var]))
  # cat("\014")
  # data_dec1[[i]]<-data.frame(data_dec1[[i]],Features)

  #----------------------Divisão em treinamento e teste---------------------#
  n      <-140#dim(data_dec1[[i]])[1]
  cut    <-128#ceiling(0.7*n)
  
  train  <-data_dec1[[i]][1:cut,];
  Y_train<-train[,1];
  X_train<-train[,-1]
  test   <-tail(data_dec1[[i]],n-cut)
  Y_test <-test[,1];
  X_test <-test[,-1]
  
 #----------------------Divisões para Treinamento----------------------------#
  fitControl<- trainControl(method         = "timeslice",
                            initialWindow  = floor(cut*0.7), #69% 
                            horizon        = 3,    #3% 
                            fixedWindow    = TRUE,
                            allowParallel  = TRUE,
                            savePredictions="final")

  #-----------------------Treinamento----------------------------------------#
  Models[[i]]<-train(X_train,Y_train,
                     method=model,
                     preProcess = pp,
                     trControl = fitControl,verbose=FALSE)
  #-----------------------Salvando Parâmetros--------------------------------#
  Params[[i]]<-Models[[i]]$bestTune
  
  #---------------------------OSA---------------------------------------------#
  colnames(PTrOSA)<-c(names(data_dec),"EEMD","EEMD-MO","WEEMD")
  colnames(PTeOSA)<-c(names(data_dec),"EEMD","EEMD-MO","WEEMD")

  #-------------------------Predições-----------------------------------------#
  PTrOSA[,i]<-predict(Models[[i]],newdata=X_train)
  PTeOSA[,i]<-predict(Models[[i]],newdata=X_test)

  cat('Trainning process for',names(data_dec[i]),'done!\n')
  
  percentage <- (i/dim(data_dec)[2])
  if(percentage %% 0.01 == 0) {
    setTkProgressBar(pb, i, label=paste(round(percentage*100, 0),"% done"))
  }
}
  close(pb)
  #-----------------------Otimização Multi-Objetivo---------------------------------#
  ws_fit<-function(x) 
  {J<-numeric(2)
  
  w1<-x[1];w2<-x[2];w3<-x[3];w4<-x[4];w5<-x[5];
  P1<-PTrOSA[,1]*w1+PTrOSA[,2]*w2+PTrOSA[,3]*w3+PTrOSA[,4]*w4+PTrOSA[,5]*w5
  
  Y <- P1
  J[1] <- mse(data[5:132,1],Y)
  J[2] <- var(data[5:132,1]-Y)
  return(J)}
  
  #----------------------------Bounds-----------------------------------------------#
  theta_min <-c(w1=-2,w2=-2,w3=-2,w4=-2,w5=-2);
  theta_max <-c(w1=2,w2=2,w3=2,w4=2,w5=2);

  #-------------------Run the genetic algorithm---------------------------------#
  set.seed(seed2)

  res <- nsga2(ws_fit, dim(data_dec)[2], 2,
               lower.bounds=theta_min,
               upper.bounds=theta_max,
               popsize = 100, generations = 100,
               cprob = cp, cdist = 5,
               mprob = mp, mdist = 10,
               constraints=NULL)

  ParetoFront[[1]]<-res$value

  plot(ParetoFront[[1]],xlab='J1',ylab='J2',main=paste("Pareto Front Model"))
  
  d <- as.matrix(res$value)
  w <- c(0.5,0.5)
  cb <- c('min','min')
  top<-TOPSISVector(d,w,cb)
  
  thetas[1,]<-res$par[top[which.max(top$R),]$Ranking,]

  cat("Multi-objective Done!\n")
  
  PTrOSA[,dim(data_dec)[2]+1]<-floor(apply(PTrOSA[,1:dim(data_dec)[2]],1,sum))
  PTeOSA[,dim(data_dec)[2]+1]<-floor(apply(PTeOSA[,1:dim(data_dec)[2]],1,sum))
  PTrOSA[,dim(data_dec)[2]+2]<-floor((PTrOSA[,1]*thetas[1]+PTrOSA[,2]*thetas[2]+PTrOSA[,3]*thetas[3]+PTrOSA[,4]*thetas[4]+PTrOSA[,5]*thetas[5]))
  PTeOSA[,dim(data_dec)[2]+2]<-floor((PTeOSA[,1]*thetas[1]+PTeOSA[,2]*thetas[2]+PTeOSA[,3]*thetas[3]+PTeOSA[,4]*thetas[4]+PTeOSA[,5]*thetas[5]))

  #------------------------------------------Errors------------------------------------------------------#
  ETrOSA[,1]<-data[5:132,1]-PTrOSA[,dim(data_dec)[2]+1]
  ETeOSA[,1]<-data[133:144,1]-PTeOSA[,dim(data_dec)[2]+1]
  ETrOSA[,2]<-data[5:132,1]-PTrOSA[1:128,dim(data_dec)[2]+2]
  ETeOSA[,2]<-data[133:144,1]-PTeOSA[1:12,dim(data_dec)[2]+2]
  
  #------------------------       Modelo sem decomposição-----------------------------------------#
  data_all<-data.frame(data[5:144,1],   #Componente
                       data[4:143,1],   #Lag1
                       data[3:142,1],   #Lag2
                       data[2:141,1],   #Lag3
                       data[1:140,1])   #Lag4
  colnames(data_all)<-c("OBS",paste("Lag",1:4,sep=""))
  
  # Var<-match("OBS",names(data_all));
  # 
  # #----------------------------Feature Engineering------------------------------------------------#
  # Features<-FE(as.data.frame(data_all[,-Var]))
  # 
  # data_all<-data.frame(data_all,Features)
  # 
  #------------------------------Divisão para Treino e Teste--------------------------#
  train  <-data_all[1:cut,];
  Y_train<-train[,1];
  X_train<-train[,-1]
  test   <-tail(data_all,n-cut)
  Y_test <-test[,1];
  X_test <-test[,-1]
  
  #------------------------------Treinando o modelo-----------------------------------#
  Models[[i+1]]<-train(X_train,Y_train,
                       method=model,
                       preProcess = pp,
                       trControl = fitControl,verbose=FALSE)
  
  #------------------------------Salvando os parâmetros--------------------------------#
  Params[[i+1]]<-Models[[i+1]]$bestTune
  
  #-----------------------------Predições----------------------------------------------#
  PTrOSA[,dim(data_dec)[2]+3]<-predict(Models[[i+1]],newdata=X_train)
  PTeOSA[,dim(data_dec)[2]+3]<-predict(Models[[i+1]],newdata=X_test)
  
  ETrOSA[,3]<-data[5:132,1]-PTrOSA[,dim(data_dec)[2]+3]
  ETeOSA[,3]<-data[133:144,1]-PTeOSA[,dim(data_dec)[2]+3]
  
  P[,1]<-PM(data[5:132,1],PTrOSA[,dim(data_dec)[2]+1])
  P[,2]<-PM(data[133:144,1],PTeOSA[,dim(data_dec)[2]+1])
  P[,3]<-PM(data[5:132,1],PTrOSA[1:128,dim(data_dec)[2]+2])
  P[,4]<-PM(data[133:144,1],PTeOSA[1:12,dim(data_dec)[2]+2])
  P[,5]<-PM(data[5:132,1],PTrOSA[1:128,dim(data_dec)[2]+3])
  P[,6]<-PM(data[133:144,1],PTeOSA[1:12,dim(data_dec)[2]+3])
  P
  
  #--------------------------Salvando os resultados------------------------------------#
  cat('Metrics for',model, "!\n")
  Results<-list(PTrOSA,PTeOSA,ETrOSA,ETeOSA,P,thetas,Params,res,Models,data_dec)
  
  form2   <-paste("Results",Sys.Date(),"_",model,"_",data[1,2],"OSA.RData", sep='')

  setwd(ResultsDir)
  save(Results,file=form2)

  setwd(BaseDir)
  Results<-Results
  print(Results)
}
  
  
  
  
  
  
   
  
  
    
    
  
  
  
