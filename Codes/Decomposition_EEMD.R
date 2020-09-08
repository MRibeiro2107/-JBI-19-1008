Decomposition_EEMD<-function(data)
{
  library(hht)
  
  trials <- 100;nimf <- 4;noise.amp <- 6.4e-07;trials.dir <- "testtrain"

  EEMD(data[,1], time(data[,1]), noise.amp, trials, nimf, trials.dir = trials.dir)
  result<- EEMDCompile(trials.dir, trials, nimf)

  data_final<-as.data.frame(cbind(result$averaged.imfs,result$averaged.residue))

  colnames(data_final)<-c(paste("IMF",1:(dim(data_final)[2]-1),sep=""),"Residual")

  time.span <- c(1, 30)
  imf.list <- 1:(dim(data_final)[2]-1)
  os       <- TRUE
  res      <- TRUE
  PlotIMFs(result, time.span, imf.list, os, res)
  return(data_final)
}
