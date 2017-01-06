######################################################################
## The following module processes satellite data from BOM           ##
## Input:                                                           ##
##     data/GIS    Shapefiles                                       ##
##     data/YR/    *.nc (netCDF files)                              ##
## Output:                                                          ##
##     data/YR     *.RData (clipped raster stacks)                  ##
##                 *.df.RData (data.frames)                         ##
## Notes:          Perhaps it might also have been good to          ##
##                 add the Measure/Region/WaterBody info at         ##
##                 this stage.  However, it turns out to be         ##
##                 more computationally efficient to delay          ##
##                 this action to the next phase (WQ_consolidate.R) ##
######################################################################
source('WQ_functions.R')

require(sqldf)
require(dplyr)
require(raster)
require(ncdf4)
require(tidyr)
require(lubridate)
require(foreach)
require(doParallel)

unlink('log/processBOM.log', recursive=TRUE)


WQ_tryCatch(
  {
      fs = list.files(path='data/GIS', pattern='spatial.*', full.names = TRUE)
      for (i in fs) load(file=i)
  }, paste0('processBOM.log'), msg='Load shapefiles', return=TRUE)



registerDoParallel(cores=15)
zones = gsub('data.*spatial\\.(.*).RData','\\1',fs)
foreach(yr=as.character(seq(2002,2016,by=1))) %dopar% {
#    for (yr in as.character(seq(2002,2016,by=1))) {
    ## Check if the clipped (etc) versions exist
    ##if (!file.exists(paste0('data/',yr,'chl.RData'))) {
    WQ_tryCatch(
    {
        fs = list.files(path=paste0('data/',yr), pattern=paste0('A',yr),full.names = TRUE)
        ## only include those files that are larger than 0 in size
        fs = fs[file.size(fs)>0]
        dates = gsub('.*/A([0-9]{7,})\\..*','D\\1',fs)
    }, paste0('processBOM.log'), msg=paste0('Prepare for stacking rasters (',yr,')'), return=TRUE)
    WQ_tryCatch(
    {
        chl=stack(fs,varname='Chl_MIM')  #read in all .nc files and make a raster stack
        save(chl, file=paste0('data/',yr,'/chl.RData'))
    }, paste0('processBOM.log'), msg=paste0('Stack chl (',yr,')'), return=TRUE)
    WQ_tryCatch(
    {
        nap=stack(fs,varname='Nap_MIM')  #read in all .nc files and make a raster stack
        save(nap, file=paste0('data/',yr,'/nap.RData'))
    }, paste0('processBOM.log'), msg=paste0('Stack nap (',yr,')'), return=TRUE)
    WQ_tryCatch(
    {
        sd=stack(fs,varname='SD_MIM')  #read in all .nc files and make a raster stack
        save(sd, file=paste0('data/',yr,'/sd.RData'))
    }, paste0('processBOM.log'), msg=paste0('Stack sd (',yr,')'), return=TRUE)
    
    measure = c('chl','nap','sd')
    for (m in measure) {            
        for (z in zones) {
            WQ_tryCatch(
            {
                eval(parse(text=paste0(m,'.',z,' = clip(',m,', spatial.',z,')')))
                eval(parse(text=paste0("save(",m,".",z,", file='data/",yr,"/",m,".",z,".RData')")))
            }, paste0('processBOM.log'), msg=paste0('Clip stacks for ',m,' (Year=',yr,', Zone=',z,')'), return=TRUE)
            WQ_tryCatch(
            {
                eval(parse(text=paste0(m,'.',z,'.df = WQ_process_BOM(',m,'.',z,', dates)')))
                eval(parse(text=paste0('save(',m,'.',z,'.df, file="data/',yr,'/',m,'.',z,'.df.RData")')))
            }, paste0('processBOM.log'), msg=paste0('Process and convert clip stacks for ',m,' (Year=',yr,', Zone=',z,') into a data.frame'), return=TRUE)
            eval(parse(text=paste0('rm(',m,'.',z,', ',m,'.',z,'.df)')))
            gc()
        }
    }
    rm(chl,nap,sd)
    gc()
    
}

