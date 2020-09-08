PLOTS<-function(observed,predicted,erros,paretos,step,state)
{
  
  all          <-cbind(observed,predicted)
  
  colnames(all)<-c("Observed","EEMD-MOO-QRF" ,"EEMD-MOO-PLS"  ,  "EEMD-MOO-BRNN"  
                       ,"EEMD-MOO-GBM","EEMD-MOO-CUBIST", "Proposed"       
                       ,"QRF"         ,"PLS"            , "BRNN"           
                       ,"GBM"         ,"CUBIST"        ,  "EEMD-ENSEMBLE")
   M1     <-c("Observed", "Proposed")
  
  Modelos1<-match(M1,colnames(all))
    
  Long    <-data.frame(as.vector(unlist(all[,Modelos1])),
                       rep(M1,each=140),
                       rep(c(rep(c("Training"),each=128),rep(c("Test"),each=12)),times=length(M1)),
                       rep(seq(from=as.Date("2007-05-01"), to=as.Date("2018-12-31"), by="month"),times=length(M1)))
  
  colnames(Long)<-c("Notifications","Models","Set","Date")
  
  name1eps<-paste("LinePO",state,step,"SA",".eps",sep="")
  name1png<-paste("LinePO",state,step,"SA",".png",sep="")
  
  Lplot<- ggplot(Long, aes(x = Date, y = Notifications)) + 
    geom_line(aes(linetype=Models,colour=Models),size=1) +
    scale_color_manual(values=c("#000000","#FF0000")) +
    xlab("Date") +  ylab("Notifications Cases Number")+
    theme_bw(base_size = 18)+
    theme(#legend.position = "bottom", 
      #legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.position = c(0.6, 0.95),
      legend.background = element_rect(fill="transparent",colour=NA),
      legend.text = element_text(size=30),
      axis.text=element_text(size=30),
      text=element_text(family="Times New Roman"),
      axis.title=element_text(size=30))+
    geom_vline(aes(xintercept=as.numeric(Date[128])))+
    annotate("text", x = Long$Date[48], y = min(round(Long[,1],2)), label = "Training ",size=8)+
    annotate("text", x = Long$Date[138], y = min(round(Long[,1],2)), label = "Test",size=8)+
    scale_x_date(date_labels = "%Y")
  
  setwd(FiguresDir)
  
  ggsave(name1eps, device=cairo_ps,width = 10,height = 10,dpi = 1200)
  ggsave(name1png, width = 10,height = 10,dpi = 1200)
  # 
  name2eps<-paste("PointPO",state,step,"SA",".eps",sep="")
  name2png<-paste("PointPO",state,step,"SA",".png",sep="")
  
  LongPoint    <-data.frame(all[,Modelos1[1]],all[,Modelos1[2]],
                            rep(c(rep(c("Training"),each=128),rep(c("Test"),each=12)),times=1))
  
  colnames(LongPoint)<-c("Observed","Notifications","Set")
  
  PointPlot<-ggplot(LongPoint, aes(x=Observed, y=Notifications)) +
             geom_point(aes(shape=Set, color = Set),size=3) +
             geom_abline(slope = 1, intercept = 0)+
             scale_color_manual(values=c("#000000","#FF0000"))+
             xlab("Observed") + ylab("Predicted") + ggtitle("") + 
            theme_bw(base_size = 18)+
            theme(legend.position = "bottom", 
            legend.title = element_blank(),
            axis.text=element_text(size=30),
            text=element_text(family="Times New Roman"),
            axis.title=element_text(size=30))
  
  ggsave(name2eps, device=cairo_ps,width = 10,height = 10,dpi = 1200)
  ggsave(name2png, width = 10,height = 10,dpi = 600)
  # 
  name3eps<-paste("ACF",state,step,"SA",".eps",sep="")
  name3png<-paste("ACF",state,step,"SA",".png",sep="")
  
  AcfPlot<-ggAcf(erros[,Modelos1[2]],lag.max=25, ci = 0.95)+
           theme_bw(base_size = 18)+ggtitle("")+
           theme(legend.position = "bottom", 
            axis.text=element_text(size=30),
            legend.text=element_text(size=rel(1)),
            plot.title = element_text(hjust=0.5),
            text=element_text(family="Times New Roman"),
            axis.title=element_text(size=30))
  
   ggsave(name3eps, device=cairo_ps,width = 10,height = 10,dpi = 1200)
   ggsave(name3png, width = 10,height = 10,dpi = 600)
  # 
  #Pareto Plot
  name4eps<-paste("Pareto",state,step,"SA",".eps",sep="")
  name4png<-paste("Pareto",state,step,"SA",".png",sep="")
  
  colnames(paretos)<-c("J1","J2")
  
  ParetoPlot<-ggplot(paretos, aes(x=J1, y=J2)) + 
    geom_point() +
    xlab(expression(italic('J'[1]))) +ylab(expression(italic('J'[2])))+
    theme_bw(base_size = 18)+
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          axis.text=element_text(size=30),
          legend.text=element_text(size=rel(1)),
          plot.title = element_text(hjust=0.5),
          text=element_text(family="Times New Roman"),
          axis.title=element_text(size=30))
                      
   ggsave(name4eps, device=cairo_ps,width = 10,height = 10,dpi = 1200)
   ggsave(name4png, width = 10,height = 10,dpi = 600)
  # 
  # name5eps<-paste("ALL",state,step,"SA",".eps",sep="")
  # name5png<-paste("ALL",state,step,"SA",".png",sep="")
  # 
  # plot_grid(ParetoPlot,Lplot,AcfPlot,labels=c("A","B","C"),nrow=1)
  # 
  # ggsave(name5eps, device=cairo_ps,width = 10,height = 10,dpi = 1200)
  # ggsave(name5png, width = 10,height = 10,dpi = 600)
  
  
}