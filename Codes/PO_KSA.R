PO_kSA<-function(OBS,OSA,DSA,TSA)
{
  all<-data.frame(OBS,OSA,DSA,TSA)
  
  
  Long<-data.frame(as.vector(unlist(all)),
                   rep(c("Observed","1-Month-Ahead","2-Months-Ahead","3-Months-Ahead"),each=140),
                   rep(c(rep(c("Training"),each=128),rep(c("Test"),each=12)),times=4),
                   rep(seq(from=as.Date("2007-05-01"), to=as.Date("2018-12-31"), by="month"),times=4))
  
  colnames(Long)<-c("Notifications","Models","Set","Date")
  
  ggplot(Long, aes(x = Date, y = Notifications)) + 
    geom_line(aes(linetype=Models,colour=Models),size=1) +
    scale_color_manual(values=c("#FF0000","#0033FF","#00FF00","#000000")) + 
    scale_linetype_manual(values=c("longdash", "dotted","dashed","solid"))+
    xlab("Date") +  ylab("Notifications Cases Number")+
    theme_bw(base_size = 18)+
    theme(#legend.position = "bottom", 
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.position = c(0.5, 0.95),
      legend.background = element_rect(fill="transparent",colour=NA),
      legend.text = element_text(size=18),
      axis.text=element_text(size=18),
      text=element_text(family="Times New Roman"),
      axis.title=element_text(size=18))+
    geom_vline(aes(xintercept=as.numeric(Date[128])))+
    annotate("text", x = Long$Date[48], y = min(round(Long[,1],2)), label = "Training ",size=8)+
    annotate("text", x = Long$Date[138], y = min(round(Long[,1],2)), label = "Test",size=8)+
    scale_x_date(date_labels = "%Y")
}

