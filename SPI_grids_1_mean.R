#RERUN 2000-2012 DECADE - CHANGE P VARIABLE MANUALLY

#Open SPI rasters from Matt Lucas
#Stack by decade (all months), calculate mean SPI by decade
#Save mean raster

#Right now set up to run SPI-3 & 6 only:

rm(list=ls())

library('raster')
library('RColorBrewer')
library('classInt')

#Set SPI time period (3-month, 6-month, etc.)
spitime=6

#Set working directory based on SPI time period
wdpath<-paste("C:/AbbyF/SPI_Grid_Analysis/SPI_Grids/HI_SPI_Outputs/250m/",spitime,"mo_SPI",sep="")
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
  #if (k = 9) {
  #  p=13
  #}
  
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
  mnpath<-paste("C:/AbbyF/SPI_Grid_Analysis/SPI_Mean_Grids/",spitime,"mo_SPI_mean",sep="")
  setwd(mnpath)
  fname<-paste("SPI",spitime,"Mn_",decade,sep="")
  writeRaster(spi_dec_mean, filename=fname)

}


#Plot mean maps:
spitime=3
mnpath<-paste("C:/AbbyF/SPI_Grid_Analysis/SPI_Mean_Grids/",spitime,"mo_SPI_mean",sep="")
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

pal5 <- colorRampPalette(brewer.pal(9,"Spectral"))(100)

#brks4<-classIntervals(na.omit(values(r3)),n=5,style="equal")
#brks4<-brks4$brks
#brks4

brks1<-c(-1, -0.75, -0.5, -0.3, -0.1,  0.1,  0.3,  0.5, 0.75, 1)

spplot(s4,col.regions=pal5,equal=TRUE,at=brks1,main=paste("avg SPI-",spitime," by decade",sep=""),names.attr=c("1920-1929","1930-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989","1990-1999","2000-2012"))
