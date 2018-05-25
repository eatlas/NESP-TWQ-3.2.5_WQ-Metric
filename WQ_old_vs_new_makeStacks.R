source('WQ_functions.R')

##make sure you have defined yr (in WQ_old_vs_new.R)
## I am going to restrict this to just the 2013-2014 water year from the very start

require(sqldf)
require(dplyr)
require(ggplot2)
require(gridExtra)
require(raster)
require(ncdf4)
require(tidyr)
require(broom)
require(lubridate)
require(foreach)
require(doParallel)

registerDoParallel(cores=10)

unlink('log/Old_vs_new.log', recursive=TRUE)

## Satellite data
source='Satellite'
yr=c(2013,2014)
WQ_tryCatch(
{
    fs = list.files(path=paste0('data/',yr), pattern=paste0('A',yr, collapse='|'),full.names = TRUE)
    ##fs = list.files(path=paste0('data/',2014), pattern=paste0('A',2014),full.names = TRUE)
    ##actually we actually want Oct 1 2013 -- Sept 30 2014
    dates = gsub('.*/A([0-9]{7,})\\..*','D\\1',fs)
    wch=which(dates %in% c('D20131001','D20140930'))
    fs=fs[wch[1]:wch[2]]
    ## only include those files that are larger than 0 in size
    fs = fs[file.size(fs)>0]
    dates = gsub('.*/A([0-9]{7,})\\..*','D\\1',fs)
}, paste0('Old_vs_new.log'), msg=paste0('Prepare for stacking rasters (',yr,')'), return=TRUE)
WQ_tryCatch(
{
    chl=stack(fs,varname='Chl_MIM')  #read in all .nc files and make a raster stack
    save(chl, file=paste0('data/Old_vs_new/',source,'/chl.RData'))
}, paste0('Old_vs_new.log'), msg=paste0('Stack chl (',yr,')'), return=TRUE)
WQ_tryCatch(
{
    nap=stack(fs,varname='Nap_MIM')  #read in all .nc files and make a raster stack
    save(nap, file=paste0('data/Old_vs_new/',source,'/nap.RData'))
}, paste0('Old_vs_new.log'), msg=paste0('Stack nap (',yr,')'), return=TRUE)
WQ_tryCatch(
{
    sd=stack(fs,varname='SD_MIM')  #read in all .nc files and make a raster stack
    save(sd, file=paste0('data/Old_vs_new/Satellite/sd.RData'))
}, paste0('Old_vs_new.log'), msg=paste0('Stack sd (',yr,')'), return=TRUE)

for (zd in c('oldzones','newzones')){
    eval(parse(text=paste('zones=',zd)))
    measure = c('chl')#,'nap','sd')
    xtra=ifelse(zd=='oldzones','old','')
    for (m in measure) {
        foreach(z=zones) %dopar% {
            print(paste0(zd,'_',m,'.',z))
            WQ_tryCatch(
            {
                eval(parse(text=paste0(m,'.',z,' = clip(',m,', ',xtra,'spatial.',z,')')))
                eval(parse(text=paste0("save(",m,".",z,", file='data/Old_vs_new/Stacks/",zd,"/",m,".",z,".RData')")))
            }, paste0('Old_vs_new.log'), msg=paste0('Clip stacks for ',zd,'_',m,' (Year=',yr,', Zone=',z,')'), return=TRUE)
            WQ_tryCatch(
            {
                eval(parse(text=paste0(m,'.',z,'.df = WQ_process_BOM(',m,'.',z,', dates)')))
                eval(parse(text=paste0('save(',m,'.',z,'.df, file="data/Old_vs_new/Stacks/',zd,'/',m,'.',z,'.df.RData")')))
            }, paste0('Old_vs_new.log'), msg=paste0('Process and convert clip stacks for ',zd,'_',m,' (Year=',yr,', Zone=',z,') into a data.frame'), return=TRUE)
            eval(parse(text=paste0('rm(',m,'.',z,', ',m,'.',z,'.df)')))
            gc()
        }
    }
}

## now go through each of the files and apply guidelines and zone info
## note for the oldzones, the names are different, so we will have to provide a conversion
wq.guidelines = read.csv('parameters/wq.guidelines.csv', strip.white=TRUE) %>%
    dplyr:::select(-Justification.Source) %>% dplyr:::rename(GL=Annual.guideline) %>% dplyr:::filter(!is.na(GL))

zd='newzones'
for (zd in c('oldzones','newzones')) {
    flist <- list.files(path = paste0("data/Old_vs_new/Stacks/",zd), pattern = "^chl.*df.RData", full.names=TRUE)
    foreach(f=flist) %dopar% {
        print(f)
        load(f)
        nm = gsub('.*(chl.*.df).RData','\\1',f)
        zone=gsub('.*chl.(.*).df.RData','\\1',f)
        eval(parse(text=paste0('satellite=',nm)))
        rm(nm)
        gc()
        spatial=read.csv('parameters/spatial.csv', strip.white=TRUE) %>%
            dplyr:::filter(GBRMPA_Zone==zone) %>% dplyr:::select(-GBRMPA_Zone)
        region=unique(spatial$Region)
        waterbody=unique(spatial$WaterBody)
        zone=unique(spatial$Zone)
        
        data = satellite %>%
            mutate(Measure='chl', Region=region,WaterBody=waterbody,Zone=zone) %>% 
                                        #left_join(measures %>% mutate(Measure = as.character(Measure))) %>%
            left_join(wq.guidelines %>% mutate(Measure = as.character(Measure))) %>%
            rename(Latitude=y, Longitude=x) %>% mutate(GL.seasonal=ifelse(Season=='Dry',Dry,Wet))
        save(data,file=paste0('data/Old_vs_new/processed/satellite_',zd,'__',region,'___',waterbody,'.RData'))
    }
}


## Finally, make a single file per zone domain
for (zd in c('oldzones','newzones')) {
    flist <- list.files(path = paste0("data/Old_vs_new/processed/"), pattern = paste0("^satellite_",zd,".*.RData"), full.names=TRUE)
    satellite = list()
    i = 0
    for (f in flist) {
        i = i + 1
        print(f)
        load(f)
        satellite[[i]]=data
        rm(data)
        gc()
    }
    data=do.call('rbind',satellite)
    save(data,file=paste0('data/Old_vs_new/processed/full_satellite_',zd,'.RData'))
    rm('satellite','data')
    gc()
}


## eReefs data=========================================================================================================
## Unfortunately, we need to re-extract the eReefs data as the process that did this step used the polygons..
20~flist <- list.files(path = "/export/databases2/ereefs/", pattern = "^.*\\.(nc|NC|Nc|Nc)$")
flist
flist = flist[5:16]
meas.ereef = c('Chl_a_sum')#,'EFI','Kd_490','NO3')
meas = c('chl')#,'nap','sd','NOx')
registerDoParallel(cores=10)

for (zd in c('oldzones','newzones')) {
    load('data/GIS/Polys.RData')
    if (zd == 'oldzones') load('data/GIS/old/Polys_old.RData')
    foreach(i=1:length(flist)) %dopar% {
        year = gsub('.*_all_([0-9]{4}).*','\\1',flist[i])
        month = gsub('.*_all_[0-9]{4}-([0-9]{2}).*','\\1',flist[i])
        
        nc <- nc_open(paste0("/export/databases2/ereefs/", flist[i]))
                                        #print(nc)
        
                                        #attributes(nc)$names
                                        #nc$nvars
        foreach(m=meas) %do% {
            m.e = meas.ereef[which(meas==m)]
            ## get lat and long
            dat = data.frame(Latitude=as.vector(ncvar_get(nc,'y_centre', start=c(1,1),count=c(-1,-1))),
                             Longitude=as.vector(ncvar_get(nc,'x_centre', start=c(1,1),count=c(-1,-1))))
            tm=ncvar_get(nc,'t', start=c(1),count=c(-1))
            tm = as.Date(tm, origin='1990-01-01')
            dat = dat %>% expand(tm,dat)
            dat = dat %>% mutate(Measure=m,Value=as.vector(ncvar_get(nc,c(m.e), start=c(1,1,44,1),count=c(-1,-1,1,-1))))
                                        #if (m=='sd') dat = dat %>% mutate(Value=1.0/Value)  #1.7/Value
                                        #if (m=='nap') dat = dat %>% mutate(Value=Value*1000)
            dat = dat %>% filter(!is.na(Latitude),!is.na(Longitude))
            coordinates(dat) = ~Longitude+Latitude
            pts=sp:::over(dat,Polys)
                                        #pts = pts[!is.na(pts)]
            foreach (z=1:length(names(Polys))) %do% {
                pts.1 = if_else(is.na(pts),FALSE,if_else(pts==z,TRUE,FALSE))
                ereefs=as.data.frame(dat[pts.1,])
                save(ereefs,file=paste0('data/Old_vs_new/Stacks/',zd,'/ereefs_',m,'__',year,'__',month,'___',names(Polys)[[z]],'.RData'))
            }
        }
    }
}

## consolidate the eReefs data into zone files (rather than zone/month)
## the resulting files are in data/Old_vs_new/Stacks/.*/eReefs_.*.RData
for (zd in c('oldzones','newzones')) {
    fs=list.files(paste0('data/Old_vs_new/Stacks/',zd), pattern='ereefs.*',full.names=TRUE)
    zones=gsub('.*ereefs_chl__201[34]__[0-9]{2}___(.*).RData','\\1',fs)
    for (z in unique(zones)) {
        wch=grep(z,fs)
        print(wch)
        eReefs=NULL
        for (f in wch) {
            load(fs[f])
            eReefs=rbind(eReefs,ereefs)
            rm('ereefs')
            gc()
        }
        save(eReefs, file=paste0('data/Old_vs_new/Stacks/',zd,'/eReefs_chl.',z,'.RData'))    
    }
}


## now go through each of the files and apply guidelines and zone info
## note for the oldzones, the names are different, so we will have to provide a conversion
wq.guidelines = read.csv('parameters/wq.guidelines.csv', strip.white=TRUE) %>%
    dplyr:::select(-Justification.Source) %>% dplyr:::rename(GL=Annual.guideline) %>% dplyr:::filter(!is.na(GL))

zd='oldzones'
for (zd in c('oldzones','newzones')) {
    flist <- list.files(path = paste0("data/Old_vs_new/Stacks/",zd), pattern = "^eReefs_chl.*.RData", full.names=TRUE)
    foreach(f=flist) %dopar% {
        print(f)
        load(f)
        nm = gsub('.*(chl.*).RData','\\1',f)
        zone=gsub('.*chl.(.*).RData','\\1',f)
        spatial=read.csv('parameters/spatial.csv', strip.white=TRUE) %>%
            dplyr:::filter(GBRMPA_Zone==zone) %>% dplyr:::select(-GBRMPA_Zone)
        region=unique(spatial$Region)
        waterbody=unique(spatial$WaterBody)
        zone=unique(spatial$Zone)
        
        data = eReefs %>%
            mutate(Season=WQ_season(tm)) %>%
            mutate(Measure='chl', Region=region,WaterBody=waterbody,Zone=zone) %>% 
                                        #left_join(measures %>% mutate(Measure = as.character(Measure))) %>%
            left_join(wq.guidelines %>% mutate(Measure = as.character(Measure))) %>%
            #rename(Latitude=y, Longitude=x) %>%
            mutate(GL.seasonal=ifelse(Season=='Dry',Dry,Wet))
        save(data,file=paste0('data/Old_vs_new/processed/eReefs_',zd,'__',region,'___',waterbody,'.RData'))
        rm('ereefs','eReefs','data')
        gc()
    }
}

## Finally, make a single file per zone domain
for (zd in c('oldzones','newzones')) {
    flist <- list.files(path = paste0("data/Old_vs_new/processed/"), pattern = paste0("^eReefs_",zd,".*.RData"), full.names=TRUE)
    ereefs = list()
    i = 0
    for (f in flist) {
        i = i + 1
        print(f)
        load(f)
        ereefs[[i]]=data
        rm(data)
        gc()
    }
    data=do.call('rbind',ereefs)
    save(data,file=paste0('data/Old_vs_new/processed/full_eReefs_',zd,'.RData'))
    rm('ereefs','data')
    gc()
}
