library(ncdf4)
library(sp)
library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
require(doParallel)
library(gridExtra)

source('WQ_functions.R')

retrieveData = FALSE

if (retrieveData) {
    ## Download the catalog of available data
    download.file(paste0('http://dapds00.nci.org.au/thredds/catalog/fx3/gbr4_bgc_926/catalog.html'),paste0('data/ereefs_926_data.xml'))
    ## extract the names of the available netcdf files
    fs=system("sed -n 's/<a href.*\\/\\(gbr.*nc\\).*>/\\1/pg' data/ereefs_926_data.xml", intern=TRUE)
    fs = gsub('\\r','',fs)
    
    numberofdays = function(dt) {
        m = format(dt,'%m')
        while(format(dt,'%m')==m) dt <- dt+1
        return(as.integer(format(dt-1,format='%d')))
    }
    registerDoParallel(cores=20)
    
    ## Download the data into data/eReefs926
    foreach(i=fs) %dopar% {
        d=gsub('gbr4\\_bgc\\_simple\\_([0-9]{4}-[0-9]{2}).*','\\1',i)
        nd=numberofdays(as.Date(paste0(d,'-01')))
        download.file(paste0('http://dapds00.nci.org.au/thredds/ncss/fx3/gbr4_bgc_926/',i,'?var=Chl_a_sum&var=EFI&var=Kd_490&var=NO3&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=',d,'-01&time_end=',d,'-',nd,'&timeStride=1&vertCoord=44&addLatLon=true'),paste0('data/eReefs926/',i))
                                        #nc <- nc_open(paste0('data/eReefs926/',i))
    }
    
}


load('data/GIS/Polys.RData')
load('data/GIS/layers.RData')
plot(Polys)
Polys.df = tidy(Polys)
qld.df = tidy(qld)


## Get the 2014-2017 reanalysis data=====================================================================
flist <- list.files(path = "data/eReefs926", pattern = "^.*\\.(nc|NC|Nc|Nc)$")
flist

##Chl: Chl_a_sum, Nap: EFI, SD: 1.7/KD490, NO3
meas.ereef = c('Chl_a_sum','EFI','Kd_490','NO3')
meas = c('chl','nap','sd','NOx')
registerDoParallel(cores=20)
foreach(i=1:length(flist)) %dopar% {
    year = gsub('.*_simple_([0-9]{4}).*','\\1',flist[i])
    month = gsub('.*_simple_[0-9]{4}-([0-9]{2}).*','\\1',flist[i])
    nc <- nc_open(paste0("data/eReefs926/", flist[i]))
    foreach(m=meas) %do% {
        m.e = meas.ereef[which(meas==m)]
        dat = data.frame(Latitude=as.vector(ncvar_get(nc,'latitude', start=c(1,1),count=c(-1,-1))),
                         Longitude=as.vector(ncvar_get(nc,'longitude', start=c(1,1),count=c(-1,-1))))
        tm=ncvar_get(nc,'time', start=c(1),count=c(-1))
        tm = as.Date(tm, origin='1990-01-01')
        dat = dat %>% expand(tm,dat)
        dat = dat %>% mutate(Measure=m,Value=as.vector(ncvar_get(nc,c(m.e), start=c(1,1,1,1),count=c(-1,-1,-1,-1))))
        if(m=='sd') dat = dat %>% mutate(Value=1.0/Value) #1.7/Value
        if(m=='nap') dat = dat %>% mutate(Value=Value*1000)
        dat = dat %>% filter(!is.na(Latitude),!is.na(Longitude))
        coordinates(dat) = ~Longitude+Latitude
        pts=sp:::over(dat,Polys)
                                        #pts = pts[!is.na(pts)]
        foreach (z=1:length(names(Polys))) %do% {
            pts.1 = if_else(is.na(pts),FALSE,if_else(pts==z,TRUE,FALSE))
            ereefs=as.data.frame(dat[pts.1,])
            save(ereefs,file=paste0('eReefs926/data/raw/ereefs_',m,'__',year,'__',month,'___',names(Polys)[[z]],'.RData'))
        }
    }
}
