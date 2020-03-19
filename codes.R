library(ggplot2)
library(ggpubr)
library(hrbrthemes)

cariler$tarih <- as.Date(cariler$tarih,format = "%d.%m.%Y")
cariler$trimports<- as.integer(cariler$trimports)
cariler$trexports<- as.integer(cariler$trexports)
cariler[6]<-usdtry$V1 #USDTRY values from other graph
cariler[7]<-cariler$usimports/cariler$usexports#usimport/usexport
cariler[8]<-cariler$trimports/cariler$trexports#trimport/trexport
cariler[9]<-cariler$tarih.1/cariler$V8#(usimport/usexport)/(trimport/trexport)
colnames(cariler) <- c("tarih","usimport","usexports","trexports",
                       "trimports","usdtry","usimpexp","trimpexp","oran")
  plot1<-ggplot(data=cariler)+
  geom_line(aes(x=cariler$tarih, y=cariler$usexports),color="darkred",size=1.3)+
  geom_line(aes(x=cariler$tarih, y=cariler$usimport),color="darkblue",size=1.3)+
  xlab("U.S")+
  ylab("")+
  ggtitle("TOTAL EXPORTS&IMPORTS")+
  theme_bw()+
  labs(caption="Red=Import")
  
  plot2<- ggplot(data=cariler)+
  geom_line(aes(x=cariler$tarih, y=cariler$trexports),color="darkred",size=1)+
  geom_line(aes(x=cariler$tarih, y=cariler$trimports),color="darkblue",size=1)+
  xlab("Turkey")+
  ylab("")+
  ggtitle("")+
  theme_bw()+
  labs(caption="Blue=Export")
  
  ggarrange(plot1,plot2)

  ggplot(data=cariler)+
    geom_line(aes(x=cariler$tarih, y=cariler$usimpexp),color="darkred",size=1.2)+
    geom_line(aes(x=cariler$tarih, y=cariler$trimpexp),color="darkblue",size=1.2)+
    theme_gray()+
    labs(caption="Red=U.S Blue=Turkey",x=" ",y=" ",title="IMPORT/EXPORT RATE")
    
regresyon <- lm(cariler$oran~cariler$usdtry)
summary(regresyon)  #Multiple R-squared:  0.3239


  ggplot(data=cariler)+
    geom_smooth(aes(x=cariler$oran, y=cariler$usdtry),color="darkred",size=1.2)+
    labs(caption="Red=U.S Blue=Turkey",x="Current Account Deficit Rating(U.S Rating/TR Rating",y=" USDTRY")
    theme_gray()
