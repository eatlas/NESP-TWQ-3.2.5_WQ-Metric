#####################################################################
## The following module can be considered the second phase         ##
## of data pre-processing.  It is responsible for adding spatial,  ##
## measure and guideline data to the Satellite data                ##
## Input:                                                          ##
##    data/YR         .*df.RData                                   ##
##    parameters      measures.txt      (Measure hierarchy)        ##
##                    wq.guidelines.csv (Water Quality Guidelines) ##
##                    spatial.csv       (Spatial hierarchy)        ##
## Output:                                                         ##
##    data/processed  data_.*RData                                 ##
#####################################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
require(foreach)
require(doParallel)


source('WQ_functions.R')

unlink('log/consolidateBOM.log', recursive=TRUE)


measures = read.table('parameters/measures.txt', strip.white=TRUE, sep=';', header=TRUE)
wq.guidelines = read.csv('parameters/wq.guidelines.csv', strip.white=TRUE) %>%
    dplyr:::select(-Justification.Source) %>% dplyr:::rename(GL=Annual.guideline) %>% dplyr:::filter(!is.na(GL))


registerDoParallel(cores=6)

fs = list.files(path='data/processed',pattern='^(chl|nap|sd)_.*.RData',full.names=TRUE)
foreach(z=fs) %dopar% {
    load(z)
    dat.str = gsub('data/processed/(.*).RData','\\1',z)
    eval(parse(text=paste0('data=',dat.str)))
    meas=gsub('([a-z]{2,3})\\_.*','\\1',dat.str)
    zone=gsub('[a-z]{2,3}\\_(.*)','\\1',dat.str)
    spatial=read.csv('parameters/spatial.csv', strip.white=TRUE) %>%
        dplyr:::filter(GBRMPA_Zone==zone) %>% dplyr:::select(-GBRMPA_Zone)
    region=unique(spatial$Region)
    waterbody=unique(spatial$WaterBody)
    zone=unique(spatial$Zone)

    WQ_tryCatch(
    {
        data = data %>%
            mutate(Measure=meas, Region=region,WaterBody=waterbody,Zone=zone) %>% 
            left_join(measures %>% mutate(Measure = as.character(Measure))) %>%
            left_join(wq.guidelines %>% mutate(Measure = as.character(Measure))) %>%
            rename(Latitude=y, Longitude=x) %>% mutate(GL.seasonal=ifelse(Season=='Dry',Dry,Wet))
        save(data,file=paste0('data/processed/data_',meas, '__',region,'___',waterbody,'.RData'))
    },paste0('consolidateBOM.log'), msg=paste0('Prepare data (Measure=',meas,', Region=',region,', Water Body=',waterbody,')'), return=TRUE)
    rm(data)
    eval(parse(text=paste0('rm(',dat.str,')')))
    gc()

}
    
