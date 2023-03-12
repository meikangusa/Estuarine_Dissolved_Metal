#Fig.3 distribution of Mn and Cu 
library(ggmap)
library(usethis)
library(ggplot2)
library(devtools)
library(maps)
library(chinamap)
library(sf)
library(tidyverse)
library(sp)
library(rgdal)
library(ggspatial)
library(mapproj)
library(ggnewscale)
library(patchwork)

install.packages("patchwork") 
library(patchwork) 

devtools::install_github("eliocamp/ggnewscale@dev")
library(ggnewscale)

setwd("/Users/Mike/R_Programming/2022/ggplot")
#Set google map account
register_google(key = "AIzaSyAGbxNeUY2MfS8BJlv-dSUGKi4zCz_JItw")
register_google(key = "AIzaSyAGbxNeUY2MfS8BJlv-dSUGKi4zCz_JItw", write = TRUE)
google_key()

##plot 
Xiamen <- get_map(c(118.007,24.498013), zoom = 11, scale = 2, maptype ='terrain-background',color = "color", language = "en-EN")
BaseMap <- ggmap(Xiamen,darken = c(0.05, "navy"))
BaseMap  

coords <- read.table("Fig3.csv", header = TRUE, sep = ",")


############Fig.3b. Mn distribbution ###############
coords_Mn <- read.table("Fig3.csv", header = TRUE, sep = ",")

AutumnMn <- BaseMap + #ggplot(coords_JLR[coords_JLR$AutumnMn != 0,], aes(x=lon, y=lat))+
  geom_point(data=coords_Mn, aes(x = lon,y = lat, fill=AutumnMn), size = 8,alpha=0.9, shape=21, colour="NA")+
  scale_fill_gradient2("Mn [μM]", limits = c(1, 6), low = "#1B7837", mid = "white", high = "#762A83", na.value = "NA")+
  #low = "#762A83", mid = "white", high = "#1B7837")+
  new_scale("fill") +
  geom_point(data=coords_Mn, aes(x = lon,y = lat, fill=AutumnMn1), size = 8,alpha=0.9, shape=24, colour="NA")+
  scale_fill_gradient2("Mn [nM]", limits = c(0, 1000),
                       low="#56B1F7", mid="white", high="Salmon",na.value = "NA", midpoint = 600) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.key.size = unit(0.8, 'cm'), #change legend key size
    legend.key.height = unit(0.6, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=10)) #change legend text font size

AutumnMn
###########
WinterMn <- BaseMap + #ggplot(coords_JLR[coords_JLR$AutumnMn != 0,], aes(x=lon, y=lat))+
  geom_point(data=coords_Mn, aes(x = lon,y = lat, fill=WinterMn), size = 8,alpha=0.9, shape=21, colour="NA")+
  scale_fill_gradient2("Mn [μM]", limits = c(1, 6), low = "#1B7837", mid = "white", high = "#762A83", na.value = "NA")+
  #low = "#762A83", mid = "white", high = "#1B7837")+
  new_scale("fill") +
  geom_point(data=coords_Mn, aes(x = lon,y = lat, fill=WinterMn1), size = 8,alpha=0.9, shape=24, colour="NA")+
  scale_fill_gradient2("Mn [nM]", limits = c(0, 1000),
                       low="#56B1F7", mid="white", high="Salmon",na.value = "NA", midpoint = 600) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "top",
    legend.key.size = unit(2, 'cm'), #change legend key size
    legend.key.height = unit(2, 'cm'), #change legend key height
    legend.key.width = unit(2, 'cm'), #change legend key width
    legend.title = element_text(size=18), #change legend title font size
    legend.text = element_text(size=16)) #change legend text font size

WinterMn

#######
SpringMn <- BaseMap + #ggplot(coords_JLR[coords_JLR$AutumnMn != 0,], aes(x=lon, y=lat))+
  geom_point(data=coords_Mn, aes(x = lon,y = lat, fill=SpringMn), size = 8,alpha=0.9, shape=21, colour="NA")+
  scale_fill_gradient2("Mn [μM]", limits = c(1, 6), low = "#1B7837", mid = "white", high = "#762A83", na.value = "NA")+
  #low = "#762A83", mid = "white", high = "#1B7837")+
  new_scale("fill") +
  geom_point(data=coords_Mn, aes(x = lon,y = lat, fill=SpringMn1), size = 8,alpha=0.9, shape=24, colour="NA")+
  scale_fill_gradient2("Mn [nM]", limits = c(0, 1000),
                       low="#56B1F7", mid="white", high="Salmon",na.value = "NA", midpoint = 600) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.key.size = unit(0.8, 'cm'), #change legend key size
    legend.key.height = unit(0.6, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=10)) #change legend text font size

SpringMn

########
SummerMn <- BaseMap + #ggplot(coords_JLR[coords_JLR$AutumnMn != 0,], aes(x=lon, y=lat))+
  geom_point(data=coords_Mn, aes(x = lon,y = lat, fill=SummerMn), size = 8,alpha=0.9, shape=21, colour="NA")+
  scale_fill_gradient2("Mn [μM]", limits = c(1, 6), low = "#1B7837", mid = "white", high = "#762A83", na.value = "NA")+
  #low = "#762A83", mid = "white", high = "#1B7837")+
  new_scale("fill") +
  geom_point(data=coords_Mn, aes(x = lon,y = lat, fill=SummerMn1), size = 8,alpha=0.9, shape=24, colour="NA")+
  scale_fill_gradient2("Mn [nM]", limits = c(0, 1000),
                       low="#56B1F7", mid="white", high="Salmon",na.value = "NA", midpoint = 600) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.key.size = unit(2, 'cm'), #change legend key size
    legend.key.height = unit(2, 'cm'), #change legend key height
    legend.key.width = unit(2, 'cm'), #change legend key width
    legend.title = element_text(size=18), #change legend title font size
    legend.text = element_text(size=16)) #change legend text font size

SummerMn

#### Mn patchwork for Fig.3a

(AutumnMn|WinterMn)/(SpringMn|SummerMn)


##############Fig.3b. Cu distribution ##################
coords_Cu <- read.table("Fig3.csv", header = TRUE, sep = ",")
AutumnCu <- BaseMap + labs(fill='Cu [nM]') + #aes(x=lon, y=lat) +
  #geom_polygon(fill="grey95", colour="grey60") + 
  geom_point(data=coords_Cu, aes(x = lon,y = lat, fill=AutumnCu), size = 8,alpha=0.9, shape=21, colour="grey60")+ 
  #  scale_size_area(max_size=4)+         
  scale_fill_gradient2(limits = c(0,60),low="#56B1F7", mid="white", high="Salmon", na.value = "NA", midpoint=30)+         
  #coord_map("polyconic") +ggtitle("Annual temperature of meteorological stations in Tibet Plateau")+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=10)) #change legend text font size
AutumnCu

WinterCu <- BaseMap + labs(fill='Cu [nM]') + #aes(x=lon, y=lat) +
  #geom_polygon(fill="grey95", colour="grey60") + 
  geom_point(data=coords, aes(x = lon,y = lat, fill=WinterCu), size = 8,alpha=0.9, shape=21, colour="grey60")+ 
  scale_fill_gradient2(limits = c(0,60),low="#56B1F7", mid="white", high="Salmon",na.value = "NA", midpoint=30)+         
  #coord_map("polyconic") +ggtitle("Annual temperature of meteorological stations in Tibet Plateau")+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "top",
    legend.key.size = unit(2, 'cm'), #change legend key size
    legend.key.height = unit(2, 'cm'), #change legend key height
    legend.key.width = unit(2, 'cm'), #change legend key width
    legend.title = element_text(size=18), #change legend title font size
    legend.text = element_text(size=16) #change legend text font size
  )
WinterCu

SpringCu <- BaseMap + labs(fill='Cu [nM]') + #aes(x=lon, y=lat) +
  #geom_polygon(fill="grey95", colour="grey60") + 
  geom_point(data=coords, aes(x = lon,y = lat, fill=SpringCu), size = 8,alpha=0.9, shape=21, colour="grey60")+ 
  #scale_fill_gradientn()+
  scale_fill_gradient2(limits = c(0,60),low="#56B1F7", mid="white", high="Salmon", na.value = "NA",midpoint=30)+  
  #coord_map("polyconic") +ggtitle("Annual temperature of meteorological stations in Tibet Plateau")+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=14), #change legend title font size
    legend.text = element_text(size=10) #change legend text font size
  )
SpringCu

SummerCu <- BaseMap + labs(fill='Cu [nM]') + #aes(x=lon, y=lat) +
  #geom_polygon(fill="grey95", colour="grey60") + 
  geom_point(data=coords, aes(x = lon,y = lat, fill=SummerCu), size = 8,alpha=0.9, shape=21, colour="grey60")+ 
  #scale_fill_gradientn()+
  scale_fill_gradient2(limits = c(0,60),low="#56B1F7", mid="white", high="Salmon", na.value = "NA",midpoint=30)+  
  #coord_map("polyconic") +ggtitle("Annual temperature of meteorological stations in Tibet Plateau")+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.key.size = unit(0.6, 'cm'), #change legend key size
    legend.key.height = unit(0.6, 'cm'), #change legend key height
    legend.key.width = unit(0.8, 'cm'), #change legend key width
    legend.title = element_text(size=12), #change legend title font size
    legend.text = element_text(size=10)) #change legend text font size

SummerCu

#### Cu patchwork for Fig.3b

(AutumnCu|WinterCu)/(SpringCu|SummerCu)

dev.off()