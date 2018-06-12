=#########################################################################
## The following script is intended to compare the old and new metrics ##
## This involves illustrating the differences between:                 ##
## - different sources (Satellite vs eReefs)                           ##
## - different thresholds                                              ##
## - different index calculations (sum of binary vs mean of fsmamp)    ##
## - different indicators                                              ##
## - different spatial boundaries (number of zones and water bodies)   ##
#########################################################################
   
source('WQ_functions.R')

## Just use a single year
yr=2014


require(sqldf)
require(dplyr)
require(ggplot2)
require(gridExtra)
require(raster)
require(ncdf4)
require(tidyr)
require(lubridate)
require(foreach)
require(doParallel)

unlink('log/Old_vs_new.log', recursive=TRUE)

##===============================STAGE 1==========================================================================================
##1. Start by getting the two different spatial domains for each data
## source
## This also produces a two map figure highlighting the difference between the old and new spatial zones.
source('WQ_old_vs_new_getSpatial.R')

## Create a stack for each variable (for each source)
## This also does the preprocessing
## Ultimately, data are in:
## data/Old_vs_new/processed/.* and internally as data
## and start with either full_satellite or full_ereefs
source('WQ_old_vs_new_makeStacks.R')

## Indexing

generateIndexes = function(filepath, filepattern) {
    fs = list.files(path=filepath,pattern=filepattern,full.names=TRUE)
    print(fs)
    foreach(f=fs) %dopar% {
        load(f)
        nm=gsub('data.*(full_.*).RData','\\1',f)
        data.idx = data %>% mutate(Binary=WQ_WQI_binary(.),
                                   MAMP=WQ_WQI_mamp(.,capped=FALSE,scaled=FALSE),
                                   fMAMP=WQ_WQI_mamp(.,capped=TRUE, scaled=FALSE),
                                   fsMAMP=WQ_WQI_mamp(.),
                                   fsMAMP4=WQ_WQI_mamp(.,fold=4)) %>%
            group_by(Region,WaterBody,Zone,Measure,Latitude,Longitude,GL,DirectionOfFailure) %>%
            summarise_at(vars(Binary,MAMP,fMAMP,fsMAMP,fsMAMP4), function(x) mean(x,na.rm=TRUE)) %>%
            ungroup %>%
            as.data.frame
        save(data.idx,file=paste0('data/Old_vs_new/indexed/',nm,'.RData'))

        ## old metric (binary on annual
        data.idx = data %>% group_by(Region,WaterBody,Zone,Measure,Latitude,Longitude,GL,DirectionOfFailure) %>%
            summarize(Value=mean(Value,na.rm=TRUE)) %>%
            ungroup %>%
            mutate(Binary=WQ_WQI_binary(.)) %>%
            as.data.frame
        save(data.idx,file=paste0('data/Old_vs_new/indexed/oldmetric_',nm,'.RData'))
        rm(data, data.idx)
        gc()
    }
}
registerDoParallel(cores=4)
generateIndexes(filepath='data/Old_vs_new/processed', filepattern='full.*.RData') 

aggregations = function(filepath,filepattern) {
    fs = list.files(path=filepath,pattern=filepattern,full.names=TRUE)
    print(fs)
    foreach(f=fs) %dopar% {
        load(f)
        nm=gsub('data.*indexed/(.*).RData','\\1',f)
        data.agg = data.idx %>% group_by(Region,WaterBody,Zone,Measure) %>%
            summarise_if(is.numeric, function(x) mean(x,na.rm=TRUE)) %>%
            ungroup %>%
            as.data.frame
        save(data.agg,file=paste0('data/Old_vs_new/aggregated/',nm,'.RData'))
        rm(data.agg, data.idx)
        gc()
    }
}
registerDoParallel(cores=8)
aggregations(filepath='data/Old_vs_new/indexed', filepattern='*.RData') 


## Can start from here

load('data/Old_vs_new/aggregated/full_eReefs_newzones.RData')
d1 = data.agg %>% mutate(Source='eReefs',Metric='New',Zones='New') %>%
    dplyr::select(Region,WaterBody,GL,Binary,fsMAMP,Source,Metric,Zones) %>%
    gather(key=Index, value=Value,Binary,fsMAMP)
load('data/Old_vs_new/aggregated/full_eReefs_oldzones.RData')
d2 = data.agg %>% mutate(Source='eReefs',Metric='New',Zones='Old') %>%
    dplyr::select(Region,WaterBody,GL,Binary,fsMAMP,Source,Metric,Zones) %>%
    gather(key=Index, value=Value,Binary,fsMAMP)
load('data/Old_vs_new/aggregated/full_satellite_newzones.RData')
d3 = data.agg %>% mutate(Source='Satellite',Metric='New',Zones='New') %>%
    dplyr::select(Region,WaterBody,GL,Binary,fsMAMP,Source,Metric,Zones) %>%
    gather(key=Index, value=Value,Binary,fsMAMP)
load('data/Old_vs_new/aggregated/full_satellite_oldzones.RData')
d4 = data.agg %>% mutate(Source='Satellite',Metric='New',Zones='Old') %>%
    dplyr::select(Region,WaterBody,GL,Binary,fsMAMP,Source,Metric,Zones) %>%
    gather(key=Index, value=Value,Binary,fsMAMP)

load('data/Old_vs_new/aggregated/oldmetric_full_eReefs_newzones.RData')
d1a = data.agg %>% mutate(Source='eReefs',Metric='Old',Zones='New') %>%
    dplyr::select(Region,WaterBody,GL,Binary,Source,Metric,Zones) %>%
    gather(key=Index, value=Value,Binary)
load('data/Old_vs_new/aggregated/oldmetric_full_eReefs_oldzones.RData')
d2a = data.agg %>% mutate(Source='eReefs',Metric='Old',Zones='Old') %>%
    dplyr::select(Region,WaterBody,GL,Binary,Source,Metric,Zones) %>%
    gather(key=Index, value=Value,Binary)
load('data/Old_vs_new/aggregated/oldmetric_full_satellite_newzones.RData')
d3a = data.agg %>% mutate(Source='Satellite',Metric='Old',Zones='New') %>%
    dplyr::select(Region,WaterBody,GL,Binary,Source,Metric,Zones) %>%
    gather(key=Index, value=Value,Binary)
load('data/Old_vs_new/aggregated/oldmetric_full_satellite_oldzones.RData')
d4a = data.agg %>% mutate(Source='Satellite',Metric='Old',Zones='Old') %>%
    dplyr::select(Region,WaterBody,GL,Binary,Source,Metric,Zones) %>%
    gather(key=Index, value=Value,Binary)

dat = rbind(d1,d2,d3,d4,d1a,d2a,d3a,d4a)



make.weights=function(Zones='New') {
    if (Zones=='New') {
        load('data/GIS/Polys.df.RData')
        load('data/GIS/Polys.RData')
    }else {
        load('data/GIS/old/Polys.df_old.RData')
        load('data/GIS/old/Polys_old.RData')
    }
    Polys.df = Polys.df %>% mutate(
                            WaterBody=case_when(stringr:::str_detect(.$id, 'Enc') ~ 'Enclosed Coastal',
                                                stringr:::str_detect(.$id, 'Open') ~ 'Open Coastal',
                                                stringr:::str_detect(.$id, 'Midshelf') ~ 'Midshelf',
                                                stringr:::str_detect(.$id, 'Offshore') ~ 'Offshore'),
                            Region=case_when(stringr:::str_detect(.$id, 'Cape') ~ 'Cape York',
                                             stringr:::str_detect(.$id, 'Terrain') ~ 'Wet Tropics',
                                             stringr:::str_detect(.$id, 'Burdekin') ~ 'Dry Tropics',
                                             stringr:::str_detect(.$id, 'Mackay') ~ 'Mackay Whitsunday',
                                             stringr:::str_detect(.$id, 'Fitzroy') ~ 'Fitzroy',
                                             stringr:::str_detect(.$id, 'Burnett') ~ 'Burnett Mary')
                        )
    GBRwide.weights = data.frame(Area=sapply(slot(Polys,'polygons'), slot, 'area'))
    rn=rownames(GBRwide.weights)
    grepl('^Open_Coastal',rn)
    grepl('Mackay',rn)

    GBRwide.weights$WaterBody=ifelse(grepl('^Enclosed_Coastal',rn),'Enclosed Coastal',
                              ifelse(grepl('^Open_Coastal',rn),'Open Coastal',
                              ifelse(grepl('^Midshelf',rn),'Midshelf','Offshore')))
    GBRwide.weights$Region=ifelse(grepl('Cape_York',rn),'Cape York',
                           ifelse(grepl('Terrain',rn),'Wet Tropics',
                           ifelse(grepl('Burdekin',rn),'Dry Tropics',
                           ifelse(grepl('Mackay',rn),'Mackay Whitsunday',
                           ifelse(grepl('Fitzroy',rn),'Fitzroy','Burnett Mary')))))
    GBRwide.weights = GBRwide.weights %>% mutate(Area=Area*1000000,Perc.Area=Area/sum(Area))
    ## Just the open coastal
    GBRwide.weights.oc = GBRwide.weights %>% filter(WaterBody=='Open Coastal') %>% mutate(Perc.Area=Area/sum(Area)*100) %>%
        mutate(Region=factor(Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))) %>%
        arrange(Region)
    GBRwide.weights.oc_old = GBRwide.weights %>% filter(WaterBody=='Open Coastal', Region %in% c('Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy')) %>%
        mutate(Perc.Area=Area/sum(Area)*100) %>%
        mutate(Region=factor(Region, levels=c('Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy'))) %>%
        arrange(Region)
   
    list(GBRwide.weights=GBRwide.weights,
         GBRwide.weights.oc = GBRwide.weights.oc,
         GBRwide.weights.oc_old = GBRwide.weights.oc_old)
}


old.weights=make.weights(Zones='Old')
new.weights=make.weights(Zones='New')

write.csv(old.weights[[1]], file='data/Old_vs_new/tables/old_weights.csv', quote=FALSE, row.names=FALSE)
write.csv(old.weights[[2]], file='data/Old_vs_new/tables/old_weights_oc.csv', quote=FALSE, row.names=FALSE)
write.csv(old.weights[[3]], file='data/Old_vs_new/tables/old_weights_oc_old.csv', quote=FALSE, row.names=FALSE)
write.csv(new.weights[[1]], file='data/Old_vs_new/tables/new_weights.csv', quote=FALSE, row.names=FALSE)
write.csv(new.weights[[2]], file='data/Old_vs_new/tables/new_weights_oc.csv', quote=FALSE, row.names=FALSE)
write.csv(new.weights[[3]], file='data/Old_vs_new/tables/new_weights_oc_old.csv', quote=FALSE, row.names=FALSE)


## Graphics ================================================================================================
### Compare Source changes

### newzones, new indices, new aggregations
dat1 = dat %>% dplyr::filter(Zones=='New', Metric=='New') %>%
    spread(key=Source, value=Value) %>%
    mutate(diff=eReefs-Satellite)
dat1=dat1 %>% dplyr::filter(WaterBody=='Open Coastal') %>% dplyr::select(-Metric,-Zones) %>%
    arrange(Index) %>%
    left_join(new.weights[[2]])
dat1a=dat1 %>% group_by(Index) %>%
    summarize(WaterBody=unique(WaterBody),
              GL=mean(GL),
              eReefs=weighted.mean(eReefs,w=Area),
              Satellite=weighted.mean(Satellite,w=Area),
              Area=sum(Area),
              Perc.Area=sum(Perc.Area)) %>%
    ungroup %>%
    mutate(Region='GBR wide',diff=eReefs-Satellite) %>%
    dplyr::select(Region,WaterBody,GL,Index,eReefs,Satellite,diff,Area,Perc.Area)
comp.data=dat1 %>% rbind(dat1a)
comp.data.sum = comp.data %>%
    dplyr::select(Region,WaterBody,Index,eReefs,Satellite) %>%
    gather(key=Source, value=Value,eReefs,Satellite) %>%
    mutate(Grade=WQ_generateGrades(Value,type='Uniform'),
           Region=factor(Region, levels=c('GBR wide','Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary')))

g = ggplot(comp.data.sum, aes(y=Value, x=Region)) +
    geom_blank() +
    geom_line(aes(x=as.numeric(Region),linetype=Source)) +
    geom_point(aes(fill=Grade), shape=21, size=3) +
    scale_fill_manual('Grade', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) +
    scale_linetype_manual('Source', breaks=c('eReefs','Satellite'), labels=c('eReefs','Remote sensing'), values=c('solid','dashed')) +
    scale_y_continuous('Score', limits=c(0,1)) + 
    facet_wrap(~Index,nrow=2) +
    theme_bw() + theme(axis.title.x=element_blank())
ggsave(filename='data/Old_vs_new/figures/Compare_sources.pdf', g, width=10, height=4)


### newzones, new indices, new aggregations, same as above, but swap facet with a legend
dat1 = dat %>% dplyr::filter(Zones=='New', Metric=='New') %>%
    spread(key=Source, value=Value) %>%
    mutate(diff=eReefs-Satellite)
dat1=dat1 %>% dplyr::filter(WaterBody=='Open Coastal') %>% dplyr::select(-Metric,-Zones) %>%
    arrange(Index) %>%
    left_join(new.weights[[2]])
dat1a=dat1 %>% group_by(Index) %>%
    summarize(WaterBody=unique(WaterBody),
              GL=mean(GL),
              eReefs=weighted.mean(eReefs,w=Area),
              Satellite=weighted.mean(Satellite,w=Area),
              Area=sum(Area),
              Perc.Area=sum(Perc.Area)) %>%
    ungroup %>%
    mutate(Region='GBR wide',diff=eReefs-Satellite) %>%
    dplyr::select(Region,WaterBody,GL,Index,eReefs,Satellite,diff,Area,Perc.Area)
comp.data=dat1 %>% rbind(dat1a)
comp.data.sum = comp.data %>%
    dplyr::select(Region,WaterBody,Index,eReefs,Satellite) %>%
    gather(key=Source, value=Value,eReefs,Satellite) %>%
    mutate(Grade=WQ_generateGrades(Value,type='Uniform'),
           Region=factor(Region, levels=c('GBR wide','Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary')))

g = ggplot(comp.data.sum, aes(y=Value, x=Region)) +
    geom_blank() +
    geom_line(aes(x=as.numeric(Region),linetype=Index)) +
    geom_point(aes(fill=Grade), shape=21, size=3) +
    scale_fill_manual('Grade', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) +
    scale_linetype_manual('Index', breaks=c('Binary','fsMAMP'), labels=c('Binary','fsMAMP'), values=c('solid','dashed')) +
    scale_y_continuous('Score', limits=c(0,1)) + 
    facet_wrap(~Source,nrow=2) +
    theme_bw() + theme(axis.title.x=element_blank())
ggsave(filename='data/Old_vs_new/figures/Compare_index.pdf', g, width=10, height=4)




## sources with newzones, old index, new aggregation
dat1 = dat %>% dplyr::filter(Zones=='New', Metric=='Old') %>%
    spread(key=Source, value=Value) %>%
    mutate(diff=eReefs-Satellite)
dat1=dat1 %>% dplyr::filter(WaterBody=='Open Coastal') %>% dplyr::select(-Metric,-Zones) %>%
    arrange(Index) %>%
    left_join(new.weights[[2]])
dat1a=dat1 %>% group_by(Index) %>%
    summarize(WaterBody=unique(WaterBody),
              GL=mean(GL),
              eReefs=weighted.mean(eReefs,w=Area),
              Satellite=weighted.mean(Satellite,w=Area),
              Area=sum(Area),
              Perc.Area=sum(Perc.Area)) %>%
    ungroup %>%
    mutate(Region='GBR wide',diff=eReefs-Satellite) %>%
    dplyr::select(Region,WaterBody,GL,Index,eReefs,Satellite,diff,Area,Perc.Area)
comp.data=dat1 %>% rbind(dat1a)
comp.data.sum = comp.data %>%
    dplyr::select(Region,WaterBody,Index,eReefs,Satellite) %>%
    gather(key=Source, value=Value,eReefs,Satellite) %>%
    mutate(Grade=WQ_generateGrades(Value,type='Uniform'),
           Region=factor(Region, levels=c('GBR wide','Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary')))

g = ggplot(comp.data.sum, aes(y=Value, x=Region)) +
    geom_blank() +
    geom_line(aes(x=as.numeric(Region),linetype=Source)) +
    geom_point(aes(fill=Grade), shape=21, size=3) +
    scale_fill_manual('Grade', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) +
    scale_linetype_manual('Source', breaks=c('eReefs','Satellite'), labels=c('eReefs','Remote sensing'), values=c('solid','dashed')) +
    scale_y_continuous('Score', limits=c(0,1)) + 
    facet_wrap(~Index,nrow=2) +
    theme_bw() + theme(axis.title.x=element_blank())
ggsave(filename='data/Old_vs_new/figures/Compare_sources_oldindices.pdf', g, width=10, height=3)


## Compare Old and new Binary
dat1 = dat %>% dplyr::filter(Zones=='New', Index=='Binary') %>%
    spread(key=Metric, value=Value) %>%
    mutate(diff=New-Old)
dat1=dat1 %>% dplyr::filter(WaterBody=='Open Coastal') %>% dplyr::select(-Index,-Zones) %>%
    arrange(Source) %>%
    left_join(new.weights[[2]])
dat1a=dat1 %>% group_by(Source) %>%
    summarize(WaterBody=unique(WaterBody),
              GL=mean(GL),
              New=weighted.mean(New,w=Area),
              Old=weighted.mean(Old,w=Area),
              Area=sum(Area),
              Perc.Area=sum(Perc.Area)) %>%
    ungroup %>%
    mutate(Region='GBR wide',diff=New-Old) %>%
    dplyr::select(Region,WaterBody,GL,Source,New,Old,diff,Area,Perc.Area)
comp.data=dat1 %>% rbind(dat1a)
comp.data.sum = comp.data %>%
    dplyr::select(Region,WaterBody,Source,New,Old) %>%
    gather(key=Metric, value=Value,New,Old) %>%
    mutate(Grade=WQ_generateGrades(Value,type='Uniform'),
           Region=factor(Region, levels=c('GBR wide','Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary')))

g = ggplot(comp.data.sum, aes(y=Value, x=Region)) +
    geom_blank() +
    geom_line(aes(x=as.numeric(Region),linetype=Metric)) +
    geom_point(aes(fill=Grade), shape=21, size=3) +
    scale_fill_manual('Grade', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) +
    scale_linetype_manual('Metric', breaks=c('New','Old'), labels=c('New','Old'), values=c('solid','dashed')) +
    scale_y_continuous('Score', limits=c(0,1)) + 
    facet_wrap(~Source,nrow=2) +
    theme_bw() + theme(axis.title.x=element_blank())
ggsave(filename='data/Old_vs_new/figures/Compare_metrics.pdf', g, width=10, height=3)



## Old zones vs new zones, eReefs data, new indices
dat1 = dat %>% dplyr::filter(Source=='eReefs', Metric=='New', WaterBody=='Open Coastal')
dat1 = dat1 %>% 
    group_by(Zones) %>%
    do({
        x=.
        if (unique(x$Zones)=='New'){
            x=x %>% left_join(new.weights[[2]]) %>%
                rename(Area.full=Area, Perc.Area.full=Perc.Area) %>%
                left_join(new.weights[[3]]) %>%
                rename(Area.red=Area, Perc.Area.red=Perc.Area)
        } else {
            x=x %>% left_join(old.weights[[2]]) %>%
                rename(Area.full=Area, Perc.Area.full=Perc.Area) %>%
                left_join(old.weights[[3]]) %>%
                rename(Area.red=Area, Perc.Area.red=Perc.Area)
        }
    }) %>% ungroup
comp.data.sum = dat1 %>%
    mutate(Grade=WQ_generateGrades(Value,type='Uniform'),
           Region=factor(Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary')))
g = ggplot(comp.data.sum, aes(y=Value, x=Region)) +
    geom_blank() +
    geom_line(aes(x=as.numeric(Region),linetype=Zones)) +
    geom_point(aes(fill=Grade), shape=21, size=3) +
    scale_fill_manual('Grade', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) +
    #scale_linetype_manual('Source', breaks=c('eReefs','Satellite'), labels=c('eReefs','Remote sensing'), values=c('solid','dashed')) +
    scale_y_continuous('Score', limits=c(0,1)) + 
    facet_wrap(~Index,nrow=2) +
    theme_bw() + theme(axis.title.x=element_blank())
ggsave(filename='data/Old_vs_new/figures/Compare_zones.pdf', g, width=10, height=4)


dat1a = dat1 %>%
    mutate(Area.red = ifelse(is.na(Area.red),1,Area.red)) %>%
    group_by(Zones,Index) %>%
    summarize(WaterBody=unique(WaterBody),
              GL=mean(GL),
              Source=unique(Source),
              Metric=unique(Metric),
              Value.full=weighted.mean(Value,w=Area.full),
              Value.red=weighted.mean(Value,w=Area.red, na.rm=TRUE),
              Area.full=sum(Area.full),
              Perc.Area.full=sum(Perc.Area.full),
              Area.red=sum(Area.red, na.rm=TRUE),
              Perc.Area.red=sum(Perc.Area.red, na.rm=TRUE))  %>%
    ungroup %>%
    mutate(Region='GBR wide') %>%
    dplyr::select(Region,WaterBody,GL,Source,Metric,Zones,Index,Value.full, Value.red, Area.full,Perc.Area.full,Area.red,Perc.Area.red) %>%
    gather(key=Aggregation,value=Value,Value.full,Value.red)

comp.data.sum=dat1a %>%
    mutate(Grade=WQ_generateGrades(Value,type='Uniform')) %>%
    dplyr::select(Zones,Index,Aggregation,Value,Grade)
comp.data.sum %>% as.data.frame
g = ggplot(comp.data.sum, aes(y=Value, x=Zones)) +
    geom_blank() +
    geom_line(aes(x=as.numeric(as.factor(Zones)),linetype=Aggregation)) +
    geom_point(aes(fill=Grade), shape=21, size=3) +
    scale_fill_manual('Grade', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) +
    scale_linetype_manual('GBR wide Aggregation', breaks=c('Value.full','Value.red'), labels=c('All regions','Excluding Cape York\nBurnett Mary'), values=c('solid','dashed')) +
    scale_y_continuous('Score', limits=c(0,1)) + 
    facet_wrap(~Index,nrow=2) +
    theme_bw() #+ theme(axis.title.x=element_blank())
ggsave(filename='data/Old_vs_new/figures/Compare_aggregation.pdf', g, width=10, height=4)
           
    
## Compare over time (eReefs/eReefs926/Remote sensing/Niskin)
add.str=''
index='fsMAMP'
temporal='Annual'
src=''
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
data.all <- NULL
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='Satellite', Index=index))
}
src='eReefs/'
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='eReefs',Index=index))
}
src='eReefs926/'
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='eReefs926',Index=index))
}
src='niskin/'
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='AIMS insitu', Index=index))
}
src='flntu/'
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='AIMS flntu',Index=index))
}        

index='Binary'
temporal='Annual'
src=''
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='Satellite', Index=index))
}
src='eReefs/'
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='eReefs',Index=index))
}
src='eReefs926/'
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='eReefs926',Index=index))
}
src='niskin/'
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='AIMS insitu', Index=index))
}
src='flntu/'
fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='AIMS flntu',Index=index))
}        


## get Binary




GradeType='Uniform'
data.all = data.all %>% mutate(Grade.MMP=WQ_generateGrades(Mean),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'),Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'))
if (GradeType=='MMP') data.all = data.all %>% mutate(Grade=Grade.MMP)
if (GradeType=='GHHP') data.all = data.all %>% mutate(Grade=Grade.GHHP)
if (GradeType=='Uniform') data.all = data.all %>% mutate(Grade=Grade.Uniform)

data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
data.all = data.all %>% filter(WaterBody=='Open Coastal', Measure=='chl') %>% droplevels

## Mark the first and last years of Satellite, eReefs and eReefs926 as an end
data.all = data.all %>% group_by(Source) %>% ungroup %>%
    mutate(Complete = ifelse(Source %in% c('Satellite','eReefs','eReefs926') & (waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
    ungroup
gradeMids = WQ_gradeMids(type=GradeType)
gradeBoundaries = WQ_gradeBoundaries(type=GradeType)

measure='chl'
unitLabel='Chlorophyll-a'
g=ggplot(data.all %>% filter(Measure==measure,Complete=='Full', Source %in% c('Satellite','eReefs','eReefs926'), Index=='fsMAMP'), aes(y=Mean, x=waterYear)) +
                                        #    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                                        #annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                                        #annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                                        #annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                                        #annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                                        #annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                                        #annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
    geom_line(aes(linetype=Source, color=Source)) +
    geom_point(aes(fill=Grade), shape=21, color='black', size=3) +
    scale_x_continuous('Water year') +
    scale_y_continuous('Index', limits=c(0,1)) +
    scale_fill_manual('Grade', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) +
    scale_color_manual('Source',breaks=c('Satellite','eReefs','eReefs926'), limits=c('Satellite','eReefs','eReefs926'), labels=c('Remote sensing','eReefs','eReefs926'),values=c('black','black','red')) +
        scale_linetype_manual('Source',breaks=c('Satellite','eReefs','eReefs926'), limits=c('Satellite','eReefs','eReefs926'), labels=c('Remote sensing','eReefs','eReefs926'),values=c('dashed','solid','solid')) +
    #scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), limits=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu','AIMS flntu'),values=c('green','red','blue','orange','purple')) +
    facet_grid(Region~WaterBody, scales='free_y') +
    theme_bw() +
    theme(strip.background=element_blank(), legend.key=element_blank(),
                                        #panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank()
          )#+
                                        #ggtitle(paste0('Measure=',unitLabel,', Index=',index,', Temporal=', temporal)) +
    #guides(fill=guide_legend(override.aes=list(color='black', size=5)))
ggsave(file=paste0('data/Old_vs_new/figures/Compare_years.pdf'), g, width=10, height=10, units='in')

## An alternative suggested by Cedric

g=data.all %>% filter(Measure==measure,Complete=='Full', Source %in% c('Satellite','eReefs'), waterYear>2012) %>%
    ggplot(aes(y=Mean, x=waterYear)) +
    geom_line(aes(linetype=Source, color=Source)) +
    geom_point(aes(fill=Grade), shape=21, color='black', size=3) +
    scale_x_continuous('Water year', limits=c(2012.5,2016.5)) +
    scale_y_continuous('Index', limits=c(0,1)) +
    scale_fill_manual('Grade', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) +
    scale_color_manual('Source',breaks=c('Satellite','eReefs'), limits=c('Satellite','eReefs'), labels=c('Remote sensing','eReefs'),values=c('black','black')) +
    scale_linetype_manual('Source',breaks=c('Satellite','eReefs'), limits=c('Satellite','eReefs'), labels=c('Remote sensing','eReefs'),values=c('dashed','solid')) +
    facet_grid(Index~Region, scales='free_y') +
    theme_bw(12) +
        theme(strip.background=element_blank(), legend.key=element_blank()
              #axis.text.x=element_text(angle=45,hjust=1)
                                        #panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank()
          )
ggsave(file=paste0('data/Old_vs_new/figures/Compare_years_alt.pdf'), g, width=13, height=5, units='in')
