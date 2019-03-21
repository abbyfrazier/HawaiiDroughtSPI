#Open SPI rasters from Matt Lucas
#Stack by decade (all months), calculate mean SPI by decade
#Save mean raster

rm(list=ls())

library('raster')
library('RColorBrewer')
library('classInt')
library("maptools")

#Set SPI time period (3-month, 6-month, etc.)
#spitime=6
spi_list<-c(1,3,6,9,12,18,24,36,48,60) #10 spi time periods


#open Rainfall Atlas grid and Coastline layer for plots:
setwd("C:\\AbbyF\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
rfa_mask<-raster("staterf_inann")
P4S.latlon <- CRS(projection(rfa_mask))
rm(rfa_mask)
#open coast file:
setwd("C:\\AbbyF\\GIS_Layers\\coast_n83.shp")
coast<-readShapePoly("coast_geo",proj4string=P4S.latlon)


for (i in 1:10) { #loop through all 10 SPI time periods
  #Set SPI time period (3-month, 6-month, etc.)
  spitime<-spi_list[i]
  
  #Set working directory based on SPI time period
  wdpath<-paste("F:/WORKING/SPI_Grid_Analysis/SPI_Grids/HI_SPI_Outputs/250m/",spitime,"mo_SPI",sep="")
  setwd(wdpath)
  
  #set variables for month & year, then open files
  month_list<-c("Jan_","Feb_","Mar_","Apr_","May_","Jun_","Jul_","Aug_","Sep_","Oct_","Nov_","Dec_")
  dec_start<-c(1920,1930,1940,1950,1960,1970,1980,1990,2000)
  dec_end<-c(1929,1939,1949,1959,1969,1979,1989,1999,2012)
  p=10 #num yrs in decade - use if statement to change it to 13 for the 2000-2012 period
  
  for (k in 1:9){
    #****RERUN FOR ALL DECADES*****
    setwd(wdpath)
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    yr_list<-seq(from=dec_start[k],to=dec_end[k],by=1)
    s=stack()
    
    #2000-2012 decade is longer than 10
    if (k == 9) {
      p=13
    }
    
    #take mean of entire decade (all months):
    for (i in 1:p) {
      yr=yr_list[i]
      
      for (j in 1:12) {
        mon=month_list[j]
        #open raster:
        rname<-paste(mon,yr,'_spi',spitime,'_HI_statewide.tif',sep="")
        spi_rast<-raster(rname)
        #add to stack:
        s<-stack(s,spi_rast)
        
      }
    }
    #Calculate MEAN of all 120 rasters (this step takes a while)
    spi_dec_mean<-mean(s,na.rm=TRUE)
    
    #save mean output
    mnpath<-paste("F:/WORKING/SPI_Grid_Analysis/SPI_Mean_Grids/",spitime,"mo_SPI_mean",sep="")
    setwd(mnpath)
    fname<-paste("SPI",spitime,"Mn_",decade,sep="")
    writeRaster(spi_dec_mean, filename=fname)
  
  }
  
  
  #Plot mean maps (open them up and plot on one figure):
  #spitime=6
  mnpath<-paste("F:/WORKING/SPI_Grid_Analysis/SPI_Mean_Grids/",spitime,"mo_SPI_mean",sep="")
  setwd(mnpath)
  r1<-raster(paste("SPI",spitime,"Mn_20-29",sep=""))
  r2<-raster(paste("SPI",spitime,"Mn_30-39",sep=""))
  r3<-raster(paste("SPI",spitime,"Mn_40-49",sep=""))
  r4<-raster(paste("SPI",spitime,"Mn_50-59",sep=""))
  r5<-raster(paste("SPI",spitime,"Mn_60-69",sep=""))
  r6<-raster(paste("SPI",spitime,"Mn_70-79",sep=""))
  r7<-raster(paste("SPI",spitime,"Mn_80-89",sep=""))
  r8<-raster(paste("SPI",spitime,"Mn_90-99",sep=""))
  r9<-raster(paste("SPI",spitime,"Mn_00-12",sep=""))
  s4<-stack(r1,r2,r3,r4,r5,r6,r7,r8,r9)
  
  pal5 <- colorRampPalette(brewer.pal(11,"Spectral"))(100) #set color palette
  brks1<-c(-2,-1.5,-1, -0.75, -0.5, -0.3, -0.1,  0.1,  0.3,  0.5, 0.75, 1,1.5,2) #set value breaks for colors
  
  #set file name for output file
  imgpath<-file.path("F:","WORKING","SPI_Grid_Analysis","SPI_Mean_Grids","MeanSPI_Decade_Images",paste("mean SPI-",spitime,"_bydecade.png",sep=""))
  dpi=300
  png(file=imgpath,width=5.5*dpi,height=4*dpi,res=dpi)
  
  spplot(s4,col.regions=pal5,equal=TRUE,at=brks1,main=paste("Mean SPI-",spitime," By Decade",sep=""),names.attr=c("1920-1929","1930-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989","1990-1999","2000-2012"))+layer(sp.polygons(coast))
  dev.off()
  
  rm(r1,r2,r3,r4,r5,r6,r7,r8,r9,s4) #clean up
  
} #next SPI time period

