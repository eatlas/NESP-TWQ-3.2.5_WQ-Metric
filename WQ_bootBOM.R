library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
require(foreach)
require(doParallel)


source('WQ_functions.R')

unlink('log/bootBOM.log', recursive=TRUE)


Seed=123
Size=100
decimal.places=3

    ## Now try to bootstrapp------------------------------------------------
WQ_tryCatch(
{
    ## Weights
    weights.m <- read.csv('parameters/weights.m.csv')
    weights.s <- read.csv('parameters/weights.s.csv')
    weights.m = dplyr::arrange(weights.m, Source,Indicator,Subindicator,Measure,Weight)
    weights.s = dplyr::arrange(weights.s, Sources,Indicator,Subindicator,Measure,Weight)
    ## Overwrites
    grades2scores <- function(x) {
        ifelse(x=='A',0.925, ifelse(x=='B',0.75,ifelse(x=='C',0.575,ifelse(x=='D',0.375,ifelse(x=='E',0.125,NA)))))
    }
    
    overwrite <- read.csv('parameters/overwrites.csv', strip.white=TRUE)
    overwrite = overwrite %>% dplyr:::select(everything(),Grade= overwrittenGrade) 
    overwrite$Grade <- ifelse(is.na(overwrite$Grade),'-',as.character(overwrite$Grade))
    overwrite$Score <- grades2scores(overwrite$Grade)
    if (nrow(overwrite)>0) overwrite[overwrite=='']<-NA
    overwrite = dplyr::arrange(overwrite, Source,Indicator,Subindicator,Measure,Grade,Score)
},paste0('bootBOM.log'), msg=paste0('Load weights and overwrites'), return=TRUE)    
   

## Step 1. Bootstrap to water year level for each measure/site (and GL/index method)
### CURRENTLY ONLY fsMAMP and Annual GL used..
registerDoParallel(cores=6)
index=fsMAMP

if (1==2) {
#registerDoParallel(cores=6)
fs = list.files(path='data/indexed',pattern='^data.idx\\_',full.names=TRUE)
#fs = fs[c(45)]
foreach(z=fs) %dopar% {
    load(z)
    WQ_tryCatch(
    { 
        boot.site.measure.day = data.idx %>%
            dplyr:::select(Indicator,Subindicator,Date,waterYear,Region,WaterBody,Zone,Latitude,Longitude,Measure,Index=fsMAMP) %>%
            mutate(Boot=Index)
        meas=unique(boot.site.measure.day$Measure)
        region=unique(boot.site.measure.day$Region)
        waterbody=unique(boot.site.measure.day$WaterBody)
        save(boot.site.measure.day, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.measure.day__',meas,'___',region,'____',waterbody,'.RData'))
    },paste0('bootBOM.log'), msg=paste0('Prepare (Measure=',meas,', Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)    
    
    ## ## Start with aggregating over day for each site (Lat/Long)
    ## ## - to speed this up, dont perform the weight or overwrites at this level.
    ## ## - effectively, the distributions are generated from the days within years
    ## ## - also when calculating the bootstrap summaries, dont use unrtainty within (since
    ## ##   each observation (date) has only a single value..
    ## boot.site.measure.sum = (boot.site.measure = boot.site.measure.day %>% mutate(Weight=1) %>%
    ##                              dplyr:::mutate(Site=interaction(Latitude,Longitude)) %>% dplyr:::select(-Index) %>% ungroup %>%
    ##                                     #group_by(Indicator,Subindicator,Measure,Region,WaterBody,Latitude,Longitude,Site,waterYear) %>%
    ##                                     #do({WQ_weights(.,weights.m, lev='Date')}) %>%
    ##                                     #WQ_Overwrites(overwrite, recursive=TRUE) %>%
    ##                              group_by(Indicator,Subindicator,Measure,Region,WaterBody,Latitude,Longitude,Site,waterYear,Date) %>%
    ##                              filter(ifelse(!is.na(Boot) | row_number()==1,TRUE,FALSE))) %>%
    ##     group_by(Indicator,Subindicator,Measure,Region,WaterBody,Latitude,Longitude,Site,waterYear) %>%
    ##     MMP_bootStats(AggOver=NULL) %>% filter(!is.na(Measure) & !is.na(Site))

    WQ_tryCatch(
    { 
        boot.site.measure.sum = (boot.site.measure = boot.site.measure.day %>% mutate(Weight=1) %>%
                                     dplyr:::select(-Index) %>% ungroup %>%
                                     filter(ifelse(!is.na(Boot) | row_number()==1,TRUE,FALSE))) %>%
            group_by(Indicator,Subindicator,Measure,Region,WaterBody,Latitude,Longitude,waterYear) %>%
            MMP_bootStats(AggOver=NULL) %>% filter(!is.na(Measure) & !is.na(Latitude) & !is.na(Longitude))
        
        save(boot.site.measure.sum, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.measure.sum__',meas,'___',region,'____',waterbody,'.RData'))
        save(boot.site.measure, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.measure__',meas,'___',region,'____',waterbody,'.RData'))
        
        rm(boot.site.measure.sum, boot.site.measure, boot.site.measure.day,data.idx)
        gc()
    },paste0('bootBOM.log'), msg=paste0('Bootstrap to Measure/Site level (Measure=',meas,', Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)            
}
}
#z=fs[25]
#z=fs[49]

## Step 2. rbind each measured bootstrap from Step 1 above
if (1==1) {
fs = list.files(path='data/bootstrap/fsMAMP/Annual',pattern='fsMAMP\\_boot.site.measure\\_.*',full.names=TRUE)
## concatenate each of the measures for the different region/waterbodies
fs.str = unique(gsub('.*\\_{3}(.*)\\_{4}(.*).RData','\\1____\\2',fs))
foreach(f = fs.str) %dopar% {
    WQ_tryCatch(
    { 
        eval(parse(text=paste0('wch = grep("',f,'",fs)')))
        boot.site.measure.year = NULL
        for (w in wch) {
            load(fs[w])
            boot.site.measure.year = rbind(boot.site.measure.year, boot.site.measure)
        }
        region=unique(boot.site.measure.year$Region)
        waterbody=unique(boot.site.measure.year$WaterBody)
        save(boot.site.measure.year, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.measure.year__',region,'___',waterbody,'.RData'))
    },paste0('bootBOM.log'), msg=paste0('rbind measures for  Measure/Site level (Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)           
    
    ## ##Start with the Spatial-last pathway (Pathway A) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## ## Now bootstrap (site/subindicator/year)
    ## WQ_tryCatch(
    ## { 
    ##     load(file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.measure.year__',region,'___',waterbody,'.RData'))
    ##     boot.site.subindicator.sum = (boot.site.subindicator = boot.site.measure.year %>% ungroup %>% dplyr:::select(-Weight,-Date) %>% 
    ##                                       group_by(Indicator,Subindicator,Region,WaterBody,Latitude,Longitude,waterYear) %>%
    ##                                       do({WQ_weights(.,weights.m, lev='Measure')}) %>%
    ##                                       WQ_Overwrites(overwrite, recursive=TRUE) %>%
    ##                                       group_by(Indicator,Subindicator,Measure,Region,WaterBody,Latitude,Longitude,waterYear) %>%
    ##                                       filter(ifelse(!is.na(Boot) | row_number()==1,TRUE,FALSE))) %>%
    ##         MMP_bootStats(AggOver='Measure', Uncertainty.within=TRUE) %>% filter(!is.na(Subindicator) & !is.na(Latitude) & !is.na(Longitude))
    ##     save(boot.site.subindicator.sum, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.subindicator.sum___',region,'____',waterbody,'.RData'))
    ##     save(boot.site.subindicator, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.subindicator___',region,'____',waterbody,'.RData'))
        
    ##     rm(boot.site.subindicator.sum, boot.site.measure.year)
    ##     gc()
    ## },paste0('bootBOM.log'), msg=paste0('Bootstrap for Site/Subindicator level (Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)           

    ## ## Site/subindicator/year
    ## WQ_tryCatch(
    ## {
    ##     boot.site.indicator.sum = (boot.site.indicator = boot.site.subindicator %>% ungroup %>% dplyr:::select(-Weight,-Measure) %>% 
    ##                                    group_by(Indicator,Region,WaterBody,Latitude,Longitude,waterYear) %>%
    ##                                    do({WQ_weights(.,weights.m, lev='Subindicator')}) %>%
    ##                                    WQ_Overwrites(overwrite, recursive=TRUE) %>%
    ##                                    group_by(Indicator,Subindicator,Region,WaterBody,Latitude,Longitude,waterYear) %>%
    ##                                    filter(ifelse(!is.na(Boot) | row_number()==1,TRUE,FALSE))) %>%
    ##         MMP_bootStats(AggOver='Subindicator', Uncertainty.within=TRUE) %>% filter(!is.na(Indicator) & !is.na(Latitude) & !is.na(Longitude))
    ##     save(boot.site.indicator.sum, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.indicator.sum___',region,'____',waterbody,'.RData'))
    ##     save(boot.site.indicator, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.indicator___',region,'____',waterbody,'.RData'))
        
    ##     rm(boot.site.subindicator,boot.site.indicator.sum)
    ##     gc()
    ## },paste0('bootBOM.log'), msg=paste0('Bootstrap for Site/Indicator level (Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)           

    ## ## Zone/indicator/year
    ## WQ_tryCatch(
    ## {
    ##     boot.zone.indicator.sum = (boot.zone.indicator = boot.site.indicator %>% ungroup %>% dplyr:::select(-Weight,-Subindicator) %>%
    ##                                     #dplyr:::mutate(Zone=interaction(Region,WaterBody)) %>%
    ##                                    group_by(Indicator,Region,WaterBody,waterYear) %>%
    ##                                    do({d=.; d$Site=interaction(d$Latitude,d$Longitude);WQ_weights(d,weights.s, lev='Site');}) %>%
    ##                                    WQ_Overwrites(overwrite, recursive=TRUE) %>%
    ##                                    group_by(Indicator,Region,WaterBody,Site,waterYear) %>%
    ##                                    filter(ifelse(!is.na(Boot) | row_number()==1,TRUE,FALSE))) %>%
    ##         MMP_bootStats(AggOver='Site', Uncertainty.within=TRUE) %>% filter(!is.na(Indicator) & !is.na(Region) & !is.na(WaterBody))
    ##     save(boot.zone.indicator.sum, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.zone.indicator.sum___',region,'____',waterbody,'.RData'))
    ##     save(boot.zone.indicator, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.zone.indicator___',region,'____',waterbody,'.RData'))

    ##     #g=ggplot(boot.zone.indicator.sum, aes(y=Mean, x=waterYear)) + geom_line() + geom_pointrange(aes(ymin=lower, ymax=upper)) + facet_grid(Region~WaterBody) 
    ##     #ggsave(filename='A.pdf', g, width=10, height=10, units='in')
        
    ##     rm(boot.site.indicator,boot.zone.indicator.sum,boot.zone.indicator)
    ##     gs()
    ## },paste0('bootBOM.log'), msg=paste0('Bootstrap for Zone/Indicator (Pathway A) level (Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)           

    ## Now Pathway C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Zone/Measure
    WQ_tryCatch(
    {
        load(file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.site.measure.year__',region,'___',waterbody,'.RData'))
        ##Size=100
        ##Ideally we would allow this to run the full bootsrapp routine.  Unfortunately, this necessitates 10,000 reps per pixel per year.
        ## For many of the Region/Waterbodies (e.g. Cape York Offshore) this results in huge data that takes a very long time to aggregate etc.
        ## Given that each pixel/year has approx 365 (minus missed days) worth of data, we could instead just take a proportional quantity from each..
        boot.zone.measure.sum = (boot.zone.measure = boot.site.measure.year %>% ungroup %>% dplyr:::select(-Weight,-Date) %>% 
                                     group_by(Indicator,Subindicator,Measure,Region,WaterBody,waterYear) %>%
                                     do({d=.; d$Site=interaction(d$Latitude,d$Longitude);WQ_weights(d,weights.s, lev='Site');}) %>%
                                     WQ_Overwrites(overwrite, recursive=TRUE) %>%
                                     group_by(Indicator,Subindicator,Measure,Region,Site,WaterBody,waterYear) %>%
                                     filter(ifelse(!is.na(Boot) | row_number()==1,TRUE,FALSE))) %>%
#            MMP_bootStats(AggOver='Site', Uncertainty.within=TRUE) %>% filter(!is.na(Measure) & !is.na(Region) & !is.na(WaterBody))
        WQ_bootStats(AggOver='Site') %>% filter(!is.na(Measure) & !is.na(Region) & !is.na(WaterBody))
        save(boot.zone.measure.sum, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.zone.measure.sum___',region,'____',waterbody,'.RData'))
        save(boot.zone.measure, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.zone.measure___',region,'____',waterbody,'.RData'))
        
        rm(boot.zone.measure.sum, boot.site.measure.year)
        gc()
    },paste0('bootBOM.log'), msg=paste0('Bootstrap for Zone/Measure level (Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)           
        
    ## Zone/subindicator/year
    WQ_tryCatch(
    {
        boot.zone.subindicator.sum = (boot.zone.subindicator = boot.zone.measure %>% ungroup %>% dplyr:::select(-Weight,-Site,-Latitude,-Longitude) %>% 
                                          group_by(Indicator,Subindicator,Region,WaterBody,waterYear) %>%
                                          do({WQ_weights(.,weights.m, lev='Measure')}) %>%
                                          WQ_Overwrites(overwrite, recursive=TRUE) %>%
                                          group_by(Indicator,Subindicator,Measure,Region,WaterBody,waterYear) %>%
                                          filter(ifelse(!is.na(Boot) | row_number()==1,TRUE,FALSE))) %>%
#            MMP_bootStats(AggOver='Measure', Uncertainty.within=FALSE) %>% filter(!is.na(Subindicator) & !is.na(Region) & !is.na(WaterBody))
        WQ_bootStats(AggOver='Measure') %>% filter(!is.na(Subindicator) & !is.na(Region) & !is.na(WaterBody))
        save(boot.zone.subindicator.sum, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.zone.subindicator.sum___',region,'____',waterbody,'.RData'))
        save(boot.zone.subindicator, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.zone.subindicator___',region,'____',waterbody,'.RData'))
        
        rm(boot.zone.measure,boot.zone.subindicator.sum)
        gc()
    },paste0('bootBOM.log'), msg=paste0('Bootstrap for Zone/Subindicator level (Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)           

    ## Zone/indicator/year
    WQ_tryCatch(
    {
        boot.zone.indicator.sum = (boot.zone.indicator = boot.zone.subindicator %>% ungroup %>% dplyr:::select(-Weight,-Measure) %>% 
                                       group_by(Indicator,Region,WaterBody,waterYear) %>%
                                       do({WQ_weights(.,weights.m, lev='Subindicator')}) %>%
                                       WQ_Overwrites(overwrite, recursive=TRUE) %>%
                                       group_by(Indicator,Subindicator,Region,WaterBody,waterYear) %>%
                                       filter(ifelse(!is.na(Boot) | row_number()==1,TRUE,FALSE))) %>%
            MMP_bootStats(AggOver='Subindicator', Uncertainty.within=FALSE) %>% filter(!is.na(Indicator) & !is.na(Region) & !is.na(WaterBody))
        save(boot.zone.indicator.sum, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.zone.indicatorC.sum___',region,'____',waterbody,'.RData'))
        save(boot.zone.indicator, file=paste0('data/bootstrap/fsMAMP/Annual/fsMAMP_boot.zone.indicatorC___',region,'____',waterbody,'.RData'))
        g = ggplot(boot.zone.indicator.sum %>% mutate(lower=Mean-SD, upper=Mean+SD), aes(y=Mean, x=waterYear)) + geom_line() + geom_pointrange(aes(ymin=lower, ymax=upper))
        g
        rm(boot.zone.subindicator,boot.zone.indicator.sum, boot.zone.indicator)
        gc()
    },paste0('bootBOM.log'), msg=paste0('Bootstrap for Zone/Indicator (Pathway C) level (Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)           
    
}
}


## Now collect up all of the boot.zone.indicator.sum (from the various Zones) and rbind them
if(1==2){

    ## Pathway A
fs = list.files(path='data/bootstrap/fsMAMP/Annual',pattern='fsMAMP\\_boot.zone.indicator.sum\\_.*',full.names=TRUE)
temp=NULL
for (f in fs) {
    load(f)
    temp=rbind(temp,boot.zone.indicator.sum)
}
boot.zone.indicator.sum=temp
g1 = ggplot(boot.zone.indicator.sum, aes(y=mean,x=waterYear)) + geom_line() + geom_pointrange(aes(ymin=lower, ymax=upper)) +
    facet_grid(Region~WaterBody)
g1    
ggsave('A.pdf', g, width=10, height=10,units='in')

    #Pathway C
fs = list.files(path='data/bootstrap/fsMAMP/Annual',pattern='fsMAMP\\_boot.zone.indicatorC.sum.*',full.names=TRUE)
temp=NULL
for (f in fs) {
    load(f)
    temp=rbind(temp,boot.zone.indicator.sum)
}
boot.zone.indicator.sum=temp
g2 = ggplot(boot.zone.indicator.sum, aes(y=mean,x=waterYear)) + geom_line() + geom_pointrange(aes(ymin=lower, ymax=upper)) +
    facet_grid(Region~WaterBody)
g2


grid.arrange(g1,g2,nrow=2)    
}
