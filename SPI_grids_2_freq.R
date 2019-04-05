#Open SPI rasters from Matt Lucas
#Stack by decade (all months), calculate "Frequency" by decade
#Save "freq" raster
#mask out ocean using rainfall atlas grid

#Define "frequency": # months in drought
#  at each pixel, count number of months with SPI < -1

rm(list=ls())

memory.limit(56000)

library('raster')
library('RColorBrewer')
library('classInt')
library("latticeExtra")
library(rasterVis)
library(sp)
library("maptools")


#Set SPI time period (3-month, 6-month, etc.)
#spitime=6
#Months with SPI < which value? 0? -1? -1.5? -2?
#negspi=-2

spi_list<-c(1,3,6,9,12,18,24,36,48,60)
negspi_list1<-c(0,-1,-1.5,-2) 
negspi_list2<-c("_0_","_neg1_","_neg15_","_neg2_")
negspi_list3<-c(0,-1,-15,-2) 


#open Rainfall Atlas grid and Coastline layer for plots:
setwd("C:\\AbbyF\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
rfa_mask<-raster("staterf_inann")
P4S.latlon <- CRS(projection(rfa_mask))
#open coast file:
setwd("C:\\AbbyF\\GIS_Layers\\coast_n83.shp")
coast<-readShapePoly("coast_geo",proj4string=P4S.latlon)


for (i in 1:length(spi_list)) { #loop through all 10 SPI time periods
  #Set SPI time period (3-month, 6-month, etc.)
  spitime<-spi_list[i]
  
  #Set working directory based on SPI time period
  wdpath<-paste("F:/WORKING/SPI_Grid_Analysis/SPI_Grids/HI_SPI_Outputs/250m/",spitime,"mo_SPI",sep="")
  setwd(wdpath)
  
  #set variables for month & year, then open files
  month_list<-c("Jan_","Feb_","Mar_","Apr_","May_","Jun_","Jul_","Aug_","Sep_","Oct_","Nov_","Dec_")
  dec_start<-c(1920,1930,1940,1950,1960,1970,1980,1990,2000)
  dec_end<-c(1929,1939,1949,1959,1969,1979,1989,1999,2012)
  p=10 #num yrs in decade - use if statement to change it from 10 to 13 for the 2000-2012 period
  #k=9
  #p=13
  
  for (k in 1:9){
    #****RERUN FOR ALL DECADES*****
    setwd(wdpath)
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    yr_list<-seq(from=dec_start[k],to=dec_end[k],by=1)
    s0=stack()
    s1=stack()
    s15=stack()
    s2=stack()
    
    #2000-2012 decade is longer than 10
    if (k == 9) {
      p=13
    }
    
    #take mean of entire decade (all months):
    for (m in 1:p) {
      yr=yr_list[m]
      
      for (j in 1:12) {
        mon=month_list[j]
        #open raster:
        rname<-paste(mon,yr,'_spi',spitime,'_HI_statewide.tif',sep="")
        spi_rast<-raster(rname)
        
        #If greater than -1 (or whatever "negspi" value is), set = NA
        #instead of looping through "negspi" values, add explicit code for each level: 0, -1, -1.5, -2
        spi_rast[spi_rast>=0]=NA
        #add to stack:
        s0<-stack(s0,spi_rast)
        
        #next negspi: -1
        spi_rast<-raster(rname)
        spi_rast[spi_rast>=-1]=NA
        s1<-stack(s1,spi_rast)
        #next negspi: -1.5
        spi_rast<-raster(rname)
        spi_rast[spi_rast>=-1.5]=NA
        s15<-stack(s15,spi_rast)
        #next negspi: -2
        spi_rast<-raster(rname)
        spi_rast[spi_rast>=-2]=NA
        s2<-stack(s2,spi_rast)
      }
    }
    #Calculate SUM of non-NAs
    rNA0 <- sum(!is.na(s0))
    rNA1 <- sum(!is.na(s1))
    rNA15 <- sum(!is.na(s15))
    rNA2 <- sum(!is.na(s2))
    
    #Make sure extents match between rainfall atlas grid & spi grid
    #SPI maps are smaller extent than RFA maps
    #need same extent.  If you use extent command, it will squish map
    #need to use CROP instead - make RFA map smaller, now it matches SPI
    newextent<-extent(rNA0)
    rfa_mask2<-crop(rfa_mask,newextent)
    
    #save rasters of counts (number of drought months)
    #Set up output path and filename:
    outpath<-paste("F:/WORKING/SPI_Grid_Analysis/SPI_Freq_Grids/",spitime,"mo_SPI_freq",sep="")
    setwd(outpath)
    fname0<-paste("SPI",spitime,"_0_",decade,sep="")  
    fname1<-paste("SPI",spitime,"_neg1_",decade,sep="")  
    fname15<-paste("SPI",spitime,"_neg15_",decade,sep="")  #For -1.5, make raster name -15 instead of negspi
    fname2<-paste("SPI",spitime,"_neg2_",decade,sep="")  
    
    
    #MASK: (save as grid, overwrite old file)
    rNA0<-mask(rNA0,rfa_mask2,filename=fname0,overwrite=TRUE)
    rNA1<-mask(rNA1,rfa_mask2,filename=fname1,overwrite=TRUE)
    rNA15<-mask(rNA15,rfa_mask2,filename=fname15,overwrite=TRUE)
    rNA2<-mask(rNA2,rfa_mask2,filename=fname2,overwrite=TRUE)
    
    #Then save as TIF file - they don't open in Arc as-is
    fname0.t<-paste("SPI",spitime,"_0_",decade,".tif",sep="")
    writeRaster(rNA0,filename=fname0.t,overwrite=TRUE)
    
    fname1.t<-paste("SPI",spitime,"_neg1_",decade,".tif",sep="")
    writeRaster(rNA1,filename=fname1.t,overwrite=TRUE)
    
    fname15.t<-paste("SPI",spitime,"_neg15_",decade,".tif",sep="")
    writeRaster(rNA15,filename=fname15.t,overwrite=TRUE)
    
    fname2.t<-paste("SPI",spitime,"_neg2_",decade,".tif",sep="")
    writeRaster(rNA2,filename=fname2.t,overwrite=TRUE)
  }
  
  
  #Calculate as proportion of total time (10 years = divide by 120 months, 13 years = divide by 156)
  #open results & plot:
  brks2<-c(0.01, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
  pal5 <- colorRampPalette(brewer.pal(9,"YlOrRd"))(100)
  
  #LOOP THROUGH NEGSPI LIST:
  for (y in 1:4) { 
    negspi1<-negspi_list1[y] #numbers
    negspi2<-negspi_list2[y] #file names
    negspi3<-negspi_list3[y] #file names output = -1.5 = 15
    
    outpath<-paste("F:/WORKING/SPI_Grid_Analysis/SPI_Freq_Grids/",spitime,"mo_SPI_freq",sep="")
    setwd(outpath)
    
    
    k=9 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r1<-raster(fname)
    r1d<-r1/156
    
    k=8 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r2<-raster(fname)
    r2d<-r2/120
    
    k=7 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r3<-raster(fname)
    r3d<-r3/120
    
    k=6 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r4<-raster(fname)
    r4d<-r4/120
    
    k=5 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r5<-raster(fname)
    r5d<-r5/120
    
    k=4 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r6<-raster(fname)
    r6d<-r6/120
    
    k=3 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r7<-raster(fname)
    r7d<-r7/120
    
    k=2 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r8<-raster(fname)
    r8d<-r8/120
    
    k=1 #1st decade: 1920-1929; 9th decade: 2000-2012
    decade=paste(substr(dec_start[k],3,4),substr(dec_end[k],3,4),sep="-")
    fname<-paste("SPI",spitime,negspi2,decade,sep="")
    r9<-raster(fname)
    r9d<-r9/120
    
    #stack all proportion maps:
    s_d<-stack(r9d,r8d,r7d,r6d,r5d,r4d,r3d,r2d,r1d)
    
    #PLOT and save image:
    #set file name for output file
    imgpath<-file.path("F:","WORKING","SPI_Grid_Analysis","SPI_Freq_Grids","FreqSPI_Decade_Images",paste("Freq SPI-",spitime,"_LessThan_",negspi3,"_ByDecade.png",sep=""))
    dpi=300
    png(file=imgpath,width=5.5*dpi,height=4*dpi,res=dpi)
    
    plt<-spplot(s_d,col.regions=pal5,equal=TRUE,at=brks2,main=paste("Proportion of months SPI-",spitime," < ",negspi1," by Decade",sep=""),names.attr=c("1920-1929","1930-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989","1990-1999","2000-2012"))+layer(sp.polygons(coast))
    print(plt)
    #OR:
    #levelplot(s_d,col.regions=pal5,equal=TRUE,at=brks2,main=paste("Proportion of months SPI-",spitime," < ",negspi1," by Decade",sep=""),names.attr=c("1920-1929","1930-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989","1990-1999","2000-2012"))+layer(sp.polygons(coast))
    dev.off()
    
    rm(r1,r2,r3,r4,r5,r6,r7,r8,r9,r1d,r2d,r3d,r4d,r5d,r6d,r7d,r8d,r9d,s_d) #clean up
    
  } #next negspi 
  
}  #next SPI time period

