##Fig.2a-xiamen Preciptation & Sea surface Temperature
install.packages("zoo")
install.packages("base")
library(zoo)
library(base)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
setwd("DataAnalysis/R_Code")
getwd()
JLRain = read.csv('JiulongRain.csv', header = TRUE) 

head(JLRain)
dim(JLRain)  # [1] 1686    5
head(JLRain)
tail(JLRain)
Rain24 <- JLRain[457:480,1:2]
Rain24
####Extract sea surface temperature
XMTemp = read.csv('SeaSurfaceTemp.csv', header = TRUE) 
SeaSurTemp = XMTemp[1554:1577,1:2]
head(SeaSurTemp)


par(mar = c(5, 4.5, 3,4.5))

dates = seq(as.Date("01-01-2017",  format = "%d-%m-%Y"), length.out = 24, by = "month")
length(dates)

Preciptition = JLRain[457:480,2]
length(Preciptition)
Temp <- SeaSurTemp[,2]


df <- data.frame(dates, Preciptition, Temp)  
head(df)


ggplot(df, aes(x=dates)) + 
  geom_line(aes(y=Preciptition),linetype=1, size=1,color = '#762A83',show.legend = FALSE) + 
  geom_line(aes(y=Temp-20),linetype=1, size=1,color = '#69b3a2',show.legend = TRUE)+ 
  scale_x_date(date_labels = "%b-%Y", date_breaks = "4 month") +
  geom_vline(xintercept = as.Date("01-11-2017",  format = "%d-%m-%Y"), linetype = 'dotted', color = 'blue', size=1)+
  geom_vline(xintercept = as.Date("01-07-2018",  format = "%d-%m-%Y"), linetype = 'dotted', color = 'blue', size=1)+
  geom_point(x = as.Date("01-11-2017",format = "%d-%m-%Y"), y=3.5, shape=10,color = 'Salmon', cex=10)+
  geom_point(x = as.Date("01-01-2018",format = "%d-%m-%Y"), y=1.0, shape=10,color = 'Salmon', cex=10)+
  geom_point(x = as.Date("01-04-2018",format = "%d-%m-%Y"), y=2.2, shape=10,color = 'Salmon', cex=10)+
  geom_point(x = as.Date("01-07-2018",format = "%d-%m-%Y"), y=7.5, shape=10,color = 'Salmon', cex=10)+
  labs(x="Date")+  scale_y_continuous(
    # Features of the first axis
    name = "Mean Precipitation [mm/day]",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.*0.5)+20, name="Temp ["~degree~"C]")) + 
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(color ='#762A83' , size=13),
      axis.title.y.right = element_text(color = '#69b3a2', size=13),
      axis.text.y =element_text(color ='#762A83' , size=13),
      axis.text.y.right =element_text(color = '#69b3a2', size=13),
      axis.text.x =element_text(color ='grey30' , size=13),
      panel.background = element_rect(fill='grey100'),
      legend.position = "top",
      legend.key.size = unit(0.6, 'cm'), #change legend key size
      legend.key.height = unit(0.6, 'cm'), #change legend key height
      legend.key.width = unit(0.8, 'cm'), #change legend key width
      legend.title = element_text(size=12), #change legend title font size
      legend.text = element_text(size=10)) #change legend text font size


plot(dates,Preciptition, type ='l',col = 'blue',
     cex.lab = 1.2,
     xlab = 'Date',
     ylab = 'Precipitatipm [mm/day]',
     main = 'Jiulong Percipitaion')
legend(1978, 16, col = c('blue'),
       lty = 1, lwd = 1.5,
       legend = c('Precipitation'),
       bty = 'n',text.font = 2, cex =1.1)
#linear regression: y = a +b X
#(Intercept)  realda[, 1]  
# a = -9.548981   b = 0.004887  
#0.48 deg/100a
Tempda = XMT [,2]
XMT = read.csv('xmtemp.csv', header = TRUE) 


XMT[1:5,]
dim(XMT)#[1] 481   5
tail(XMT) #481 1979-01 2019-01
n1 = which(XMT == "1979-01") #1189 
XMT[1189,]
n2 = which(XMT == "2019-01") #[1] 1669
Tempda = XMT[n1:n2,2]
Tempda
#length(Tempda)
par(new=TRUE)#Add new plot

plot(time, Tempda, type="s",col="red",
     axes=FALSE,xlab="",ylab="")

axis(4, col="red", col.axis ='red', cex.axis =1)
mtext("Temp anomaly [deg C]",
      col="red", cex =1.1, side = 4, line = 3)
legend(1978, 5.99, col = c('red'),
       lty = 1, lwd = 1.5,
       legend = c('Temperture'),
       bty = 'n',text.font = 2, cex =1.1)

dev.off()