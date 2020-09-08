PD_ENSEMBLE<-function(data,pp,cp,mp,models,seed1,seed2)
{
 
  number<-seq(1,length(models),1)
  #Grid of models
  combs <-expand.grid(number,number,number,number,number)
   
 #Find the approaches that considers same models for all components
  r<-c()
  for(l in 1:5)
  {
    r[l]<- rownames(combs)[combs$Var1 == l & combs$Var2 == l & 
                             combs$Var3 == l & combs$Var4 == l & 
                             combs$Var5 == l]
  }
  
  
  options(warn=-1)
  set.seed(seed1)
  setwd(CodesDir)
  
  fun<-c('Decomposition_EEMD.R',"Metricas.R")
  sapply(fun,function(x){source(x)})
  
  data_dec   <-Decomposition_EEMD(data)
  data_dec1  <-list()
  cat("\014")
  #Objects
  Models <-list();    Params <-list();  PTR    <-list(); 
  PTE    <-list();    ParetoFront<-list()
  
  PTRF   <-matrix(nrow=128,ncol=2*length(models)+1);
  ETRF   <-matrix(nrow=128,ncol=2*length(models)+1);
  PTEF   <-matrix(nrow=12,ncol=2*length(models)+1);
  ETEF   <-matrix(nrow=12,ncol=2*length(models)+1);
  PTRmo  <-matrix(nrow=128,ncol=length(models));
  PTEmo  <-matrix(nrow=12,ncol=length(models));
  
  PTrOSA <-matrix(nrow=128,ncol=dim(combs)[1]+length(models)+1); 
  PTeOSA <-matrix(nrow=12 ,ncol=dim(combs)[1]+length(models)+1);
  
  ETrOSA <-matrix(nrow=128 ,ncol=dim(combs)[1]+length(models)+1);
  ETeOSA <-matrix(nrow=12  ,ncol=dim(combs)[1]+length(models)+1);
  P      <-matrix(nrow=dim(combs)[1]+length(models)+1, ncol=11)
  k      <-1;
  
  row.names(P)    <-c(paste("Model",1:dim(combs[1])),models,"EEMD-SS")
  colnames(P)     <-c(rep(c("RMSE","R2","MAE","SMAPE","RRMSE"),times=2),"Model")
  colnames(ETrOSA)<-c(paste("Model",1:dim(combs[1])),models,"EEMD-SS")
  colnames(ETeOSA)<-c(paste("Model",1:dim(combs[1])),models,"EEMD-SS")
  
  thetas          <-matrix(nrow=dim(combs)[1],ncol=dim(data_dec)[2],byrow=FALSE) 
  
  pb <- txtProgressBar(title = "progress bar", min = 0,
                       max = dim(data_dec)[2], width = 30, style = 3)
  
  for(i in 1:dim(data_dec)[2])
  {
    #--------------------------Inputs and outputs-----------------------------#
    data_dec1[[i]]<-data.frame(data_dec[5:144,i],   #Component
                               data_dec[4:143,i],   #Lag1
                               data_dec[3:142,i],   #Lag2
                               data_dec[2:141,i],   #Lag3
                               data_dec[1:140,i])   #Lag4
    colnames(data_dec1[[i]])<-c(names(data_dec[i]),paste("Lag",1:4,sep=""))
    
    #----------------------Split---------------------#
    n      <-140
    cut    <-128
    
    train  <-data_dec1[[i]][1:cut,];
    Y_train<-train[,1];X_train<-train[,-1]
    test   <-tail(data_dec1[[i]],n-cut)
    Y_test <-test[,1]; X_test <-test[,-1]
    
    #----------------------Controls----------------------------#
    fitControl<- trainControl(method         = "timeslice",
                              initialWindow  = floor(cut*0.7),  
                              horizon        = 3,              #max horizon in the paper 
                              fixedWindow    = TRUE,
                              allowParallel  = TRUE,
                              savePredictions="final")
    #Train and predict each component with different models - 5 components x 6 models 
    for(m in 1:length(models))
    {
      X_trainm<-train[,-1]; X_testm <-test[,-1]
      
      #-----------------------Training----------------------------------------#
      Models[[k]]<-train(X_trainm,Y_train,
                         method=models[m],
                         preProcess = pp,
                         trControl = fitControl,verbose=FALSE)
      #-----------------------Hyperparameters--------------------------------#
      Params[[k]]<-Models[[k]]$bestTune
      
      #---------------------------Names-----------------------------------------#
      Lag1<-match("Lag1",names(X_testm))
      Lag2<-match("Lag2",names(X_testm))
      
      
      #------------------Predictions for train and test sets-----------
      PTRmo[,m]<-predict(Models[[k]],newdata=X_train)
      PTEmo[,m]<-predict(Models[[k]],newdata=X_test)
      
      k<-k+1
    }
    PTR[[i]]<-PTRmo[1:128,];PTE[[i]]<-PTEmo[1:12,] 
    
    percentage <- (i/dim(data_dec)[2])
    Sys.sleep(0.01)
    setTxtProgressBar(pb, i, title=paste(round(percentage*100, 0),"% done"))
  }
  close(pb)
  #-----------------------Multi-Objective Optimization---------------------------------#
  
  for(c in 1:dim(combs)[1])
  {
    #Cost function
    ws_fit<-function(x) 
    {J<-numeric(2)
    
    w1<-x[1];w2<-x[2];w3<-x[3];w4<-x[4];w5<-x[5];
    P1<-PTR[[1]][,combs[c,1]]*w1+PTR[[2]][,combs[c,2]]*w2+
      PTR[[3]][,combs[c,3]]*w3+PTR[[4]][,combs[c,4]]*w4+
      PTR[[5]][,combs[c,5]]*w5
    
    Y <- P1
    J[1] <- mse(data[5:132,1],Y) #Objective 1
    J[2] <- var(data[5:132,1]-Y) #Objective 2
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
    
    ParetoFront[[c]]<-res$value
    
    #------------------------MCDM--------------------------------------#
    d <- as.matrix(res$value)
    w <- c(0.5,0.5)
    cb <- c('min','min')
    top<-TOPSISVector(d,w,cb)
    
    thetas[c,]<-res$par[top[which.max(top$R),]$Ranking,]
    
    #------------------Predictions for combined model------------------#
    PTrOSA[1:128,c]<-floor((PTR[[1]][1:128,combs[c,1]]*thetas[c,1]+
                              PTR[[2]][1:128,combs[c,2]]*thetas[c,2]+
                              PTR[[3]][1:128,combs[c,3]]*thetas[c,3]+
                              PTR[[4]][1:128,combs[c,4]]*thetas[c,4]+
                              PTR[[5]][1:128,combs[c,5]]*thetas[c,5]))
    
    PTeOSA[,c]<-floor((PTE[[1]][,combs[c,1]]*thetas[c,1]+
                         PTE[[2]][,combs[c,2]]*thetas[c,2]+
                         PTE[[3]][,combs[c,3]]*thetas[c,3]+
                         PTE[[4]][,combs[c,4]]*thetas[c,4]+
                         PTE[[5]][,combs[c,5]]*thetas[c,5]))
    #------------------------------------------Errors------------------------------------------------------#
    ETrOSA[,c]<-as.vector(data[5:132,1] -PTrOSA[1:128,c])
    ETeOSA[,c]<-as.vector(data[133:144,1]-PTeOSA[1:12,c])
    
    #Performance Measures
    P[c,1:4]<-PM(data[5:132,1]  ,PTrOSA[1:128,c])
    P[c,6:9]<-PM(data[133:144,1],PTeOSA[1:12,c])
    P[c,5]  <-P[c,1]/mean(data[5:132,1])
    P[c,10] <-P[c,6]/mean(data[133:144,1])
    P[c,11]  <-c
    
    cat('Model:',c,sprintf('RMSE: %0.4f',P[c,6]),
        sprintf('R2: %0.4f',P[c,7]),
        sprintf('MAE: %0.4f',P[c,8]),
        sprintf('SMAPE: %0.4f',P[c,9]),
        sprintf('RRMSE: %0.4f\n',P[c,10]))
  }
  
  ##################################################################################################
  #------------------------      Models without decomposition-----------------------------------------#
  data_all<-data.frame(data[5:144,1],   #Component
                       data[4:143,1],   #Lag1
                       data[3:142,1],   #Lag2
                       data[2:141,1],   #Lag3
                       data[1:140,1])   #Lag4
  
  #------------------------------Split--------------------------#
  train  <-data_all[1:cut,];
  Y_train<-train[,1]; X_train<-train[,-1]
  test   <-tail(data_all,n-cut)
  Y_test <-test[,1];  X_test <-test[,-1]
  # 
  #------------------------------Train-----------------------------------#
  for(m in 1:length(models))
  {
    X_trainm   <-train[,-1]
    X_testm    <-test[,-1]
    Models[[k]]<-train(X_trainm,Y_train,
                       method=models[m],
                       preProcess = pp,
                       trControl = fitControl,verbose=FALSE)
    
    #------------------------------Hyperparameters--------------------------------#
    Params[[k]]<-Models[[k]]$bestTune
    
    #------------------Predictions for train and test sets-----------
    PTrOSA[,c+m]<-predict(Models[[k]],newdata=X_train)
    PTeOSA[,c+m]<-predict(Models[[k]],newdata=X_test)
    
    #-----------------------------Errors----------------------------------------------#
    
    ETrOSA[,c+m]<-data[5:132,1]-PTrOSA[1:128,c+m]
    ETeOSA[,c+m]<-data[133:144,1]-PTeOSA[1:12,c+m]
    
    #-----------------------------Performance----------------------------------------#
    P[c+m,1:4]<-PM(data[5:132,1]  ,PTrOSA[1:128,c+m])
    P[c+m,6:9]<-PM(data[133:144,1],PTeOSA[1:12,c+m])
    P[c+m,5]  <-P[c+m,1]/mean(data[5:132,1])
    P[c+m,10]<-P[c+m,6]/mean(data[133:144,1])
    P[c+m,11] <-c+m
    
    cat('Model:',c+m,sprintf(',RMSE: %0.4f',P[c+m,6]),
        sprintf(',R2: %0.4f',P[c+m,7]),
        sprintf(',MAE: %0.4f',P[c+m,8]),
        sprintf(',SMAPE: %0.4f',P[c+m,9]),
        sprintf(',RRMSE: %0.4f\n',P[c+m,10]))
    
    k<-k+1
  }
  ######################################################################## 
  #Final Objects
  
  #Find the minimum smape
  P[which.min(P[,6]),];P[which.max(P[,7]),]
  P[which.min(P[,8]),];P[which.min(P[,9]),]
  
  ms<-P[which.min(P[,6]),11]
  
  MF<-c(combs[P[which.min(P[,6]),11],])
  
  #Simple Sum
  
  #------------------Predictions for combined model------------------#
  PTrOSA[1:128,c+m+1]<-as.vector(floor((PTR[[1]][1:128,combs[MF$Var1,1]]+
                                          PTR[[2]][1:128,combs[MF$Var2,2]]+
                                          PTR[[3]][1:128,combs[MF$Var3,3]]+
                                          PTR[[4]][1:128,combs[MF$Var4,4]]+
                                          PTR[[5]][1:128,combs[MF$Var5,5]])))
  
  PTeOSA[1:12,c+m+1]<-as.vector(floor((PTE[[1]][,combs[MF$Var1,1]]+
                                         PTE[[2]][,combs[MF$Var2,2]]+
                                         PTE[[3]][,combs[MF$Var3,3]]+
                                         PTE[[4]][,combs[MF$Var4,4]]+
                                         PTE[[5]][,combs[MF$Var5,5]])))
  
  ETrOSA[,c+m+1]<-data[5:132,1]-PTrOSA[1:128,c+m+1]
  ETeOSA[,c+m+1]<-data[133:144,1]-PTeOSA[1:12,c+m+1]
  
  P[c+m+1,1:4]<-PM(data[5:132,1]  ,PTrOSA[1:128,c+m+1])
  P[c+m+1,6:9]<-PM(data[133:144,1],   PTeOSA[1:12,c+m+1])
  P[c+m+1,5]  <-P[c+m+1,1]/mean(data[5:132,1])
  P[c+m+1,10] <-P[c+m+1,6]/mean(data[133:144,1])
  P[c+m+1,11] <-c+m+1
  
  #---------------------------------Final Remarks------------------------------# 
  lines  <-c(as.numeric(r),P[which.min(P[,6]),11],seq(c+1,c+length(models)+1,1))
  PTRF   <-PTrOSA[,lines];  PTEF   <-PTeOSA[,lines]
  ETRF   <-ETrOSA[,lines];  ETEF   <-ETeOSA[,lines];  PF     <-P[lines,]
  
  names        <-c(paste("EEMD-MOO",models,sep="-"))
  row.names(PF)<-c(names,"Proposed",models,"EEMD-SS")
  
  PFS    <-PF[order(PF[,9],decreasing=FALSE),]
  WF     <-thetas[c(as.numeric(r),P[which.min(P[,6]),11]),]
  ParetoF<-ParetoFront[c(as.numeric(r),P[which.min(P[,6]),11])]
  colnames(WF)  <-c(paste("Theta",1:dim(data_dec)[2]))
  row.names(WF) <-c(names,"Proposed")
  colnames(PTRF)<-row.names(PF)
  colnames(PTEF)<-row.names(PF)
  colnames(ETEF)<-row.names(PF)
  colnames(ETRF)<-row.names(PF)
  
  #------------------Results---------------------------------------#
  cat("Results: \n")
  print(PFS)
  #--------------------------Save the results------------------------------------#
  Results1SA<-list(PTrOSA,PTeOSA,PTRF,PTEF,ETRF,ETEF,MF,Params,Models,WF,ParetoF,data_dec,PFS,P)
  form2   <-paste("Results_Ensemble_",Sys.Date(),"_State_",data[1,2],"1SA.RData", sep='')
  
  setwd(ResultsDir)
  save(Results1SA,file=form2)
  
  setwd(BaseDir)
  Results<-Results1SA
  print(Results1SA)
}














