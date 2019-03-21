#Extract Island Time Series from SPI grids (output = 1 time series per island, spatially avg all pixels)

rm(list=ls())

library('raster')
library('RColorBrewer')
library('classInt')
library('rgdal')
library("maptools")
library("latticeExtra")
library(rasterVis)
library(sp)


#Set working directory based on SPI time period
#open Rainfall Atlas grid to mask:
setwd("C:\\AbbyF\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
rfa_mask<-raster("staterf_inann")
P4S.latlon <- CRS(projection(rfa_mask))
rm(rfa_mask)
#open coast file:
setwd("C:\\AbbyF\\GIS_Layers\\coast_n83.shp")
coast<-readShapePoly("coast_geo",proj4string=P4S.latlon)

#Get extent for each island:
#Kauai:
wdpath2<-"C:/AbbyF/GIS_Layers/RainfallAtlas/RF_Inches/KauaiRFGrids_inches"
setwd(wdpath2)
rfa_ext<-raster("rf_in_ka_ann")
eKa<-extent(rfa_ext)
rm(rfa_ext)
#OAHU:
wdpath2<-"C:/AbbyF/GIS_Layers/RainfallAtlas/RF_Inches/OahuRFGrids_inches"
setwd(wdpath2)
rfa_ext<-raster("rf_in_oa_ann")
eOa<-extent(rfa_ext)
rm(rfa_ext)
#MAUI:
wdpath2<-"C:/AbbyF/GIS_Layers/RainfallAtlas/RF_Inches/MauiRFGrids_inches"
setwd(wdpath2)
rfa_ext<-raster("rf_in_ma_ann")
eMa<-extent(rfa_ext)
rm(rfa_ext)
#BigIsland:
wdpath2<-"C:/AbbyF/GIS_Layers/RainfallAtlas/RF_Inches/HawaiiRFGrids_inches"
setwd(wdpath2)
rfa_ext<-raster("rf_in_bi_ann")
eBi<-extent(rfa_ext)
rm(rfa_ext)

month_list<-c("Jan_","Feb_","Mar_","Apr_","May_","Jun_","Jul_","Aug_","Sep_","Oct_","Nov_","Dec_")

spi_list<-c(1,3,6,9,12,18,24,36,48,60)



for (i in 1:10) {
#Set SPI time period (3-month, 6-month, etc.)
spitime<-spi_list[i]

#open SPI grids one year at a time, append mean vals to "mn_ka", starting cell = 0
mn_ka=0
mn_oa=0
mn_ma=0
mn_bi=0
mn_st=0


for (yr in 1920:2012) {
  s=stack()
  #Set working directory based on SPI time period
  wdpath<-paste("F:/WORKING/SPI_Grid_Analysis/SPI_Grids/HI_SPI_Outputs/250m/",spitime,"mo_SPI",sep="")
  setwd(wdpath)
  
  #Stack all files in year:
  for (j in 1:12) {
    mon=month_list[j]
    #open raster:
    rname<-paste(mon,yr,'_spi',spitime,'_HI_statewide.tif',sep="")
    spi_rast<-raster(rname)
    #add to stack:
    s<-stack(s,spi_rast)
  }
  #Crop by island
  tt_ka <- crop(s,eKa)
  tt_oa <- crop(s,eOa)
  tt_ma <- crop(s,eMa)
  tt_bi <- crop(s,eBi)
  
  mn_ka <- c(mn_ka,cellStats(tt_ka,stat='mean',na.rm=TRUE))
  mn_oa <- c(mn_oa,cellStats(tt_oa,stat='mean',na.rm=TRUE))
  mn_ma <- c(mn_ma,cellStats(tt_ma,stat='mean',na.rm=TRUE))
  mn_bi <- c(mn_bi,cellStats(tt_bi,stat='mean',na.rm=TRUE))
  mn_st <- c(mn_st,cellStats(s,stat='mean',na.rm=TRUE))
  
}

setwd("F:/WORKING/SPI_Grid_Analysis/SPI_MapTimeSeries/IslandTimeSeries")
write.table(mn_ka,file=paste("Ka_AvgSPI_",spitime,".txt",sep=""),sep="\t",col.names=TRUE)
write.table(mn_oa,file=paste("Oa_AvgSPI_",spitime,".txt",sep=""),sep="\t",col.names=TRUE)
write.table(mn_ma,file=paste("Ma_AvgSPI_",spitime,".txt",sep=""),sep="\t",col.names=TRUE)
write.table(mn_bi,file=paste("Bi_AvgSPI_",spitime,".txt",sep=""),sep="\t",col.names=TRUE)
write.table(mn_st,file=paste("State_AvgSPI_",spitime,".txt",sep=""),sep="\t",col.names=TRUE)

}