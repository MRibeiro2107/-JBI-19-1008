ACF<-function(data)
{
  plot<-ggAcf(data,lag.max=25, ci = 0.95)+
    theme_bw(base_size = 18)+ggtitle("")+
    theme(legend.position = "bottom", 
          axis.text=element_text(size=18),
          legend.text=element_text(size=rel(1)),
          plot.title = element_text(hjust=0.5),
          text=element_text(family="Times New Roman"),
          axis.title=element_text(size=20))
  print(plot)
}
