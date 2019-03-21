#Open SPI rasters from Matt Lucas
#Stack by decade (all months), calculate "Frequency" by decade
#Save "freq" raster
#mask out ocean using rainfall atlas grid

#Define "frequency": # months in drought
#  at each pixel, count number of months with SPI < -1


rm(list=ls())

library('raster')
library('RColorBrewer')
library('classInt')

#Set SPI time period (3-month, 6-month, etc.)
spitime=6
#Months with SPI < which value? 0? -1? -1.5? -2?
negspi=-2

#open Rainfall Atlas grid to mask:
setwd("C:\\AbbyF\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
rfa_mask<-raster("staterf_inann")

#Set working directory based on SPI time period
wdpath<-paste("C:/AbbyF/SPI_Grid_Analysis/SPI_Grids/HI_SPI_Outputs/250m/",spitime,"mo_SPI",sep="")
setwd(wdpath)

#set variables for month & year, then open files
month_list<-c("Jan_","Feb_","Mar_","Apr_","May_","Jun_","Jul_","Aug_","Sep_","Oct_","Nov_","Dec_")
dec_start<-c(1920,1930,1940,1950,1960,1970,1980,1990,2000)
dec_end<-c(1929,1939,1949,1959,1969,1979,1989,1999,2012)
p=10 #num yrs in decade - use if statement to change it from 10 to 13 for the 2000-2012 period
#k=9
#p=13

for (k in 1:8){
  #****RERUN FOR ALL DECADES*****
  setwd(wdpath)
  decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
  yr_list<-seq(from=dec_start[k],to=dec_end[k],by=1)
  s=stack()
  
  #2000-2012 decade is longer than 10
  #if (k = 9) {(p=13)}
  
  #take mean of entire decade (all months):
  for (i in 1:p) {
    yr=yr_list[i]
    
    for (j in 1:12) {
      mon=month_list[j]
      #open raster:
      rname<-paste(mon,yr,'_spi',spitime,'_HI_statewide.tif',sep="")
      spi_rast<-raster(rname)
      
      #If greater than -1 (or whatever "negspi" value is), set = NA
      spi_rast[spi_rast>=negspi]=NA
      #add to stack:
      s<-stack(s,spi_rast)
      
    }
  }
  #Calculate SUM of non-NAs
  rNA <- sum(!is.na(s))
  
  #Make sure extents match between rainfall atlas grid & spi grid
  #SPI maps are smaller extent than RFA maps
  #need same extent.  If you use extent command, it will squish map
  #need to use CROP instead - make RFA map smaller, now it matches SPI
  newextent<-extent(rNA)
  rfa_mask2<-crop(rfa_mask,newextent)
  
  #save rasters of counts (number of drought months)
  #Set up output path and filename:
  outpath<-paste("C:/AbbyF/SPI_Grid_Analysis/SPI_Freq_Grids/",spitime,"mo_SPI_freq",sep="")
  setwd(outpath)
  fname<-paste("SPI",spitime,negspi,decade,sep="")  #For -1.5, make raster name -15 instead of negspi
  
  #MASK: (save as grid, overwrite old file)
  rNA<-mask(rNA,rfa_mask2,filename=fname,overwrite=TRUE)
  
  #Then save as TIF file - they don't open in Arc as-is
  fname2<-paste("SPI",spitime,negspi,decade,".tif",sep="")
  writeRaster(rNA,filename=fname2,overwrite=TRUE)
  
}





#MASK OCEAN FROM ALL FILES:
#USE RAINFALL ATLAS GRID
rm(list=ls())
library('raster')
setwd("C:\\AbbyF\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
rfa_mask<-raster("staterf_inann")

spitime=6
negspi=0
month_list<-c("Jan_","Feb_","Mar_","Apr_","May_","Jun_","Jul_","Aug_","Sep_","Oct_","Nov_","Dec_")
dec_start<-c(1920,1930,1940,1950,1960,1970,1980,1990,2000)
dec_end<-c(1929,1939,1949,1959,1969,1979,1989,1999,2012)

for (k in 1:9){  #1st decade: 1920-1929; 9th decade: 2000-2012
  decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
  outpath<-paste("C:/AbbyF/SPI_Grid_Analysis/SPI_Freq_Grids/",spitime,"mo_SPI_freq",sep="")
  setwd(outpath)
  #OPEN RASTER:
  fname<-paste("SPI",spitime,negspi,decade,sep="")
  r1<-raster(fname)
  #SPI maps are smaller extent than RFA maps
  #need same extent.  If you use extent command, it will squish map
  #need to use CROP instead - make RFA map smaller, now it matches SPI
  newextent<-extent(r1)
  rfa_mask2<-crop(rfa_mask,newextent)
  #MASK: (overwrite old file)
  r1<-mask(r1,rfa_mask2,filename=fname,overwrite=TRUE)
  #Then save as TIF file - they don't open in Arc as-is
  fname2<-paste("SPI",spitime,negspi,decade,".tif",sep="")
  writeRaster(r1,filename=fname2,overwrite=TRUE)
}



#calculate as proportion
r1d<-r1/120




#Open Coastline Shapefile
library("maptools")
library("latticeExtra")
library(rasterVis)
library(sp)
#Set working directory based on SPI time period
setwd("C:\\AbbyF\\GIS_Layers\\coast_n83.shp")
P4S.latlon <- CRS(projection(r1))
coast<-readShapePoly("coast_geo",proj4string=P4S.latlon)
plot(coast)


#open results & plot:
#brks1<-c(0, 20, 40, 60, 80,  100,  120, 140)
brks2<-c(0.01, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
pal5 <- colorRampPalette(brewer.pal(9,"YlOrRd"))(100)
negspi=-1

k=9 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
outpath<-paste("C:/AbbyF/SPI_Grid_Analysis/SPI_Freq_Grids/",spitime,"mo_SPI_freq",sep="")
setwd(outpath)
fname<-paste("SPI",spitime,negspi,decade,sep="")
r1<-raster(fname)
r1d<-r1/156

k=8 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
fname<-paste("SPI",spitime,negspi,decade,sep="")
r2<-raster(fname)
r2d<-r2/120

k=7 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
fname<-paste("SPI",spitime,negspi,decade,sep="")
r3<-raster(fname)
r3d<-r3/120

k=6 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
fname<-paste("SPI",spitime,negspi,decade,sep="")
r4<-raster(fname)
r4d<-r4/120

k=5 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
fname<-paste("SPI",spitime,negspi,decade,sep="")
r5<-raster(fname)
r5d<-r5/120

k=4 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
fname<-paste("SPI",spitime,negspi,decade,sep="")
r6<-raster(fname)
r6d<-r6/120

k=3 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
fname<-paste("SPI",spitime,negspi,decade,sep="")
r7<-raster(fname)
r7d<-r7/120

k=2 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
fname<-paste("SPI",spitime,negspi,decade,sep="")
r8<-raster(fname)
r8d<-r8/120

k=1 #1st decade: 1920-1929; 9th decade: 2000-2012
decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
fname<-paste("SPI",spitime,negspi,decade,sep="")
r9<-raster(fname)
r9d<-r9/120

levelplot(r1d,col.regions=pal5,at=brks2,equal=TRUE,main="Proportion of months SPI < 0, 2000-2012")+layer(sp.polygons(coast))
spplot(r1d,col.regions=pal5,at=brks2,equal=TRUE,main="Proportion of months SPI < 0, 2000-2012")
spplot(r2d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI < 0, 1990-1999")
spplot(r3d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI < 0, 1980-1989")
spplot(r4d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI < 0, 1970-1979")
spplot(r5d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI < 0, 1960-1969")
spplot(r6d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI < 0, 1950-1959")
spplot(r7d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI < 0, 1940-1949")
spplot(r8d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI < 0, 1930-1939")
spplot(r9d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI < 0, 1920-1929")


s_d<-stack(r9d,r8d,r7d,r6d,r5d,r4d,r3d,r2d,r1d)
spplot(s_d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI-6 < -1 by Decade",names.attr=c("1920-1929","1930-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989","1990-1999","2000-2012"))+layer(sp.polygons(coast))

levelplot(s_d,col.regions=pal5,equal=TRUE,at=brks2,main="Proportion of months SPI-6 < 0 by Decade",names.attr=c("1920-1929","1930-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989","1990-1999","2000-2012"))+layer(sp.polygons(coast))


