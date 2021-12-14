#PROGETTO NON PARAMETRIC
library(sf)
library(dplyr)
library(gstat)
library(ggplot2)
library(maps)
library(maptools)
library(rgdal)
library(sp)
library(shp2graph)
library(lubridate)
setwd("C:/Users/User/OneDrive/Documenti/GitHub/ERICA/Municipi_RDN2008")
dt<-read.table("DATASET/dataset.csv",header=T,sep=",")

#MUNICIPI------
municipi_Milano <- st_read("Municipi.shp")
municipi_Milano <-st_transform(municipi_Milano, crs=4326)


x11()
plot(st_geometry(municipi_Milano), col = sf.colors(9, categorical = TRUE), border = 'grey',  axes = TRUE)
#plot(45.47,9.19, pch = 3, col = 'red', add = TRUE) non credo di poter plottare punti dopo aver usato st_geometry
PM10_1<-read.table("RW_20211125182304_77016_10273_1.csv",header=T,sep=",")
head(PM10_1)
x=PM10_1[,2]
media1<-mean(x)
media1  #media annuale deve stare sotto i 40,
#giornalmente non posso superare i 50 per più di 35 giorni di fila solo che noi non abbiamo dati su così tanti giorni
PM10_2<-read.table("RW_20211125182304_77014_6956_1.csv",header=T,sep=",")
head(PM10_2)
x=PM10_2[,2]
media2<-mean(x)
media2  #sopra 40

PM10_3<-read.table("RW_20211125182303_77011_20429_1.csv",header=T,sep=",")
head(PM10_3)
x=PM10_3[,2]
media3<-mean(x)
media3 #sopra 50!!!

PM10_4<-read.table("RW_20211125182304_77015_10320_1.csv",header=T,sep=",")
head(PM10_4)
x=PM10_4[,2]
media4<-mean(x)
media4   #sopra 50!!!

#tutte hanno il minimo il 20 Febbraio (sui 20/25)
