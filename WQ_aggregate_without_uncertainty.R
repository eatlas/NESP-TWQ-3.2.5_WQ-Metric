library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
require(foreach)
require(doParallel)
require(broom)
require(sp)

source('WQ_functions.R')
getConfigs()

decimal_places=3

## A toggle to indicate whether to exclude Nap, SD and/or NOx as a Measure for any aggregations past Measurement level.
## So by that, it will be included in Measurement level outputs, yet not included in Sub-indicator aggregations.
if (!exists('include_sd')) include_sd = TRUE
if (!exists('include_NOx')) include_NOx=TRUE
if (!exists('include_nap')) include_nap=TRUE
if (!exists('include_enclosedcoastal')) include_enclosedcoastal = FALSE

add.str=''
if (!include_sd) add.str = paste0(add.str, 'noSD')
if (!include_NOx) add.str = paste0(add.str, 'noNOx')
if (!include_nap) add.str = paste0(add.str, 'nonap')
## A toggle to indicate whether or not to include Enclosed Coastal
ec.str='_with_enclosed_coastal'
if (!include_enclosedcoastal) ec.str='_without_enclosed_coastal'



index_type = rbind(
    expand.grid(pattern='data.idx\\_.*', index=c('Binary','MAMP','fsMAMP','fsMAMP4'), Temporal='Annual'),
    expand.grid(pattern='data.idx.seasonal\\_.*', index=c('Binary','MAMP','fsMAMP','fsMAMP4'), Temporal='Seasonal'),
    expand.grid(pattern='data.idx.exceed\\_.*', index=c('Exceed','Max_Duration'), Temporal='Annual'),
    expand.grid(pattern='data.idx.exceed.seasonal\\_.*', index='Exceed', Temporal='Seasonal')
)

registerDoParallel(cores=10)

unlink('log/aggregate_without_uncertainty.log', recursive=TRUE)
unlink('log/aggregate_without_uncertaintyBinary.log', recursive=TRUE)
unlink('log/aggregate_without_uncertaintyMAMP.log', recursive=TRUE)
unlink('log/aggregate_without_uncertaintyfsMAMP.log', recursive=TRUE)
unlink('log/aggregate_without_uncertaintyfsMAMP4.log', recursive=TRUE)
unlink('log/aggregate_without_uncertaintyExceed.log', recursive=TRUE)
unlink('log/aggregate_without_uncertaintyMax_Duration.log', recursive=TRUE)


load('data/GIS/Polys.RData')
Polys.df = tidy(Polys)

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

load('data/GIS/layers.RData')
qld.df=tidy(qld)
reefs.df=tidy(reefs)


GBRwide.weights = data.frame(Area=sapply(slot(Polys,'polygons'), slot, 'area'))
rn=rownames(GBRwide.weights)
grepl('^Open_Coastal',rn)*
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
write.csv(GBRwide.weights.oc, file='Report/Tables/GBRwide.weights.oc.csv', quote=FALSE, row.names=FALSE)
measures = read.table('parameters/measures.txt', strip.white=TRUE, sep=';', header=TRUE)
foreach(src=c('niskin/','flntu/','','eReefs/','eReefs926/')) %do% {
    print(src)
                                        #foreach(idx=1:nrow(index_type)) %do% {
    for (idx in c(1,2,3,4,9,10))
    {
        #idx=1
        pat = as.character(index_type$pattern[idx])
        index=as.character(index_type$index[idx])
        print(index)
        logfile=paste0('aggregate_without_uncertainty',index,'.log')
        temporal=as.character(index_type$Temporal[idx])
        fs = list.files(path=paste0(src,'data/indexed'),pattern=pat,full.names=TRUE)
        fs = fs[!grepl('_____',fs)]
        #if (1==2) {
        foreach(z=fs) %do% {
            load(z)
                                        #dat.str = gsub('data/processed/(.*).RData','\\1',z)
            meas=unique(data.idx$Measure)
            region=unique(data.idx$Region)
            waterbody=unique(data.idx$WaterBody)
            ## Aggregate over the observations within each water year
            WQ_tryCatch(
            {
                if (index %in% c('Exceed','Max_Duration')) data.idx = data.idx %>% mutate(Indicator='Water Quality', Subindicator=ifelse(Measure=='chl','Chlorophyll','Water Clarity'))
                data.av = data.idx %>% group_by(Longitude,Latitude,Region,WaterBody,Zone,waterYear,Indicator,Subindicator,Measure) %>%
                    summarize_(Mean=paste0("mean(",index,",na.rm=TRUE)"),Median=paste0("stats:::median(",index,",na.rm=TRUE)"), Var=paste0("stats:::var(",index,",na.rm=TRUE)")) %>%
                                        #summarize(Value=mean(fsMAMP, na.rm=TRUE),Median=median(fsMAMP,na.rm=TRUE), Var=var(fsMAMP,na.rm=TRUE)) %>%
                    ungroup %>% arrange(desc(Latitude),Longitude)
                save(data.av, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.av__',meas, '__',region,'___',waterbody,'.RData'))
            },logfile, msg=paste0('Generate annual averages (Source=',src,', Index=',index,', Temporal=',temporal,', Measure=',meas,', Region=',region,', Water Body=',waterbody,')'), return=TRUE)
            rm(data.idx,data.av)
                                        #eval(parse(text=paste0('rm(',z,')')))
            gc()
        }
        
        ## Concatenate the measures - so generate a Measure/Site/Year dataset for each Region/Waterbody
        ## This loop operates on the Zone level therefore it will only aggregate up to the level of Zone/Indicator
        ## For GBR wide aggregations, it is necessary to perform calculations in the proceding section that combines zones...
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal,'/'),pattern='data.av\\_',full.names=TRUE)
        fs.str = unique(gsub('.*\\_{2}.*\\_{2}(.*)\\_{3}(.*).RData','\\1___\\2',fs))
        foreach(f = fs.str) %dopar% {  #%dopar%
            WQ_tryCatch(
            { 
                eval(parse(text=paste0('wch = grep("',f,'",fs)')))
                data.av.measure.site.year = NULL
                for (w in wch) {
                    load(fs[w])
                    data.av.measure.site.year = rbind(data.av.measure.site.year, data.av)
                }
                region=unique(data.av.measure.site.year$Region)
                waterbody=unique(data.av.measure.site.year$WaterBody)
                save(data.av.measure.site.year, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.av.measure.site.year__',region,'___',waterbody,'.RData'))
            },logfile, msg=paste0('rbind measures for  Measure/Site/Year level (Source=',src,', Region=',region,', water body=',waterbody,', Index=',index,', Temporal=',temporal,')'), return=TRUE)
            rm(data.av)
            gc()

            if (!include_sd) data.av.measure.site.year = data.av.measure.site.year %>% filter(Measure!='sd') %>% droplevels
            if (!include_NOx) data.av.measure.site.year = data.av.measure.site.year %>% filter(Measure!='NOx') %>% droplevels
            if (!include_nap) data.av.measure.site.year = data.av.measure.site.year %>% filter(Measure!='nap') %>% droplevels
            
            ## Pathway A++++++++++++++++++++++++++++++++++++++++++++++++++++
            ## Subindicator/Site/Year
            print('Subindicator/Site/Year')
            data.av.subindicator.site.year = data.av.measure.site.year %>% ungroup %>%
                dplyr:::select(Indicator,Subindicator,Latitude,Longitude,Region,WaterBody,waterYear,Mean) %>%
                group_by(Indicator,Subindicator,Latitude,Longitude,Region,WaterBody,waterYear) %>%
                summarize(Mean=mean(Mean,na.rm=TRUE))
            save(data.av.subindicator.site.year, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.av.subindicator.site.year___',region,'____',waterbody,add.str,'.RData'))
            ## Indicator/Site/Year
            data.av.indicator.site.year = data.av.subindicator.site.year %>% ungroup %>%
                dplyr:::select(Indicator,Latitude,Longitude,Region,WaterBody,waterYear,Mean) %>%
                group_by(Indicator,Latitude,Longitude,Region,WaterBody,waterYear) %>%
                summarize(Mean=mean(Mean,na.rm=TRUE))
            save(data.av.indicator.site.year, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.av.indicator.site.year___',region,'____',waterbody,add.str,'.RData'))
            rm(data.av.subindicator.site.year)
            gc()
            ## Indicator/Zone/Year
            print('Indicator/Zone/Year')
            data.av.indicator.zone.year.A = data.av.indicator.site.year %>% ungroup %>%
                dplyr:::select(Indicator,Region,WaterBody,waterYear,Mean) %>%
                group_by(Indicator,Region,WaterBody,waterYear) %>%
                summarize(Mean=mean(Mean,na.rm=TRUE))
            save(data.av.indicator.zone.year.A, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.av.indicator.zone.year.A___',region,'____',waterbody,add.str,'.RData'))    
            rm(data.av.indicator.site.year,data.av.indicator.zone.year.A)
            gc()
            ## Pathway B++++++++++++++++++++++++++++++++++++++++++++++++++++
            ## Measure/Zone/Year
            data.av.measure.zone.year = data.av.measure.site.year %>% ungroup %>%
                dplyr:::select(Indicator,Subindicator,Measure,Region,WaterBody,waterYear,Mean) %>%
                group_by(Indicator,Subindicator,Measure,Region,WaterBody,waterYear) %>%
                summarize(Mean=mean(Mean,na.rm=TRUE))
            save(data.av.measure.zone.year, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.av.measure.zone.year___',region,'____',waterbody,add.str,'.RData'))
            rm(data.av.measure.site.year)
            gc()
            ## Subindicator/Zone/Year
            print('Subindicator/Zone/Year')
            data.av.subindicator.zone.year = data.av.measure.zone.year %>% ungroup %>%
                dplyr:::select(Indicator,Subindicator,Region,WaterBody,waterYear,Mean) %>%
                group_by(Indicator,Subindicator,Region,WaterBody,waterYear) %>%
                summarize(Mean=mean(Mean,na.rm=TRUE))
            save(data.av.subindicator.zone.year, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.av.subindicator.zone.year___',region,'____',waterbody,add.str,'.RData'))
            rm(data.av.measure.zone.year)
            gc()
            ## Indicator/Zone/Year
            print('Indicator/Zone/Year')
            data.av.indicator.zone.year.B = data.av.subindicator.zone.year %>% ungroup %>%
                dplyr:::select(Indicator,Region,WaterBody,waterYear,Mean) %>%
                group_by(Indicator,Region,WaterBody,waterYear) %>%
                summarize(Mean=mean(Mean,na.rm=TRUE))
            save(data.av.indicator.zone.year.B, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.av.indicator.zone.year.B___',region,'____',waterbody,add.str,'.RData'))
            rm(data.av.subindicator.zone.year,data.av.indicator.zone.year.B)
            gc()

        }  ## end of loop

        ## Prepare the aggregated summaries
        ## Make sure the Scores are rounded to three decimal places prior to generating grades...

        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        ##Site/Measure
        print('Site/Measure')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.site.year.*',full.names=TRUE)
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.measure.site.year)
        }
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places), Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'), Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        data.all = data.all %>% left_join(measures %>% dplyr:::select(Measure,UnitsLabel,Label))
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.site.year.RData'))

        ##Site/Subindicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('Site/Subindicator')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.subindicator.site.year.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            if (any(grepl('.*(noSD|nonap|noNOx).*',fs)))  fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            if (any(grepl('.*(noSDnoNOx).*',fs))) fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.subindicator.site.year)
        }
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        #data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.site.year',add.str,'.RData'))

        ##Site/Indicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('Site/Indicator')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.indicator.site.year.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            if (any(grepl('.*(noSD|nonap|noNOx).*',fs)))  fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            if (any(grepl('.*(noSDnoNOx).*',fs))) fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
                                        #fs.str = unique(gsub('.*\\_{2}.*\\_{2}(.*)\\_{3}(.*).RData','\\1___\\2',fs))
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.indicator.site.year)
        }
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        #data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.site.year',add.str,'.RData'))


        ##Zone/Measure====
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('Zone/Measure')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            if (any(grepl('.*(noSD|nonap|noNOx).*',fs))) fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            if (any(grepl('.*(noSDnoNOx).*',fs))) fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.measure.zone.year)
        }
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        #data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        data.all = data.all %>% left_join(measures %>% dplyr:::select(Measure,UnitsLabel,Label))
        if(!include_enclosedcoastal) data.all =data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
        if (stringr:::str_detect(src,'niskin')) {
            ll=c('chl','nap','sd','NOx')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            if (!include_NOx) ll = ll[ll!='NOx']
            data.all$Measure = factor(data.all$Measure, levels=ll)
#            if (include_sd & !include_NOx) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','sd'))
#            if (!include_sd & include_NOx) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','NOx'))
#            if (include_sd & include_NOx) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','sd','NOx'))
#            if (!include_sd & !include_NOx) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap'))
        }
        if (stringr:::str_detect(src,'flntu')) {
            ll=c('chl','nap')
            if (!include_nap) ll = ll[ll!='nap']
            data.all$Measure = factor(data.all$Measure, levels=ll)
            #data.all$Measure = factor(data.all$Measure, labels=c('chl','nap'))
        }
        if (stringr:::str_detect(src,'eReefs')) {
            ll=c('chl','nap','sd','NOx')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            if (!include_NOx) ll = ll[ll!='NOx']
            data.all$Measure = factor(data.all$Measure, levels=ll)
            ## if (include_sd & !include_NOx) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','sd'))
            ## if (!include_sd & include_NOx) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','NOx'))
            ## if (include_sd & include_NOx) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','sd','NOx'))
            ## if (!include_sd & !include_NOx) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap'))
        }
        if (src=='') {
            ll=c('chl','nap','sd')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            data.all$Measure = factor(data.all$Measure, levels=ll)
            #if (include_sd) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','sd'))
            #if (!include_sd) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap'))
        }
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.zone.year',add.str,ec.str,'.RData'))
        
        ##Zone/Subindicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('Zone/Subindicator')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.subindicator.zone.year.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            if (any(grepl('.*(noSD|nonap|noNOx).*',fs))) fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            if (any(grepl('.*(noSDnoNOx).*',fs))) fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.subindicator.zone.year)
        }
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'), Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        #data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        if(!include_enclosedcoastal) data.all =data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
        if (stringr:::str_detect(src,'niskin')) ll = c('Productivity','Water Clarity','Nutrients')
        if (stringr:::str_detect(src,'flntu')) ll=c('Productivity','Water Clarity')
        if (stringr:::str_detect(src,'eReefs')) ll=c('Productivity','Water Clarity','Nutrients')
        if (src=='') ll=c('Productivity','Water Clarity')
        wch=which(unique(data.all$Subindicator) %in% ll)
            data.all$Subindicator = factor(data.all$Subindicator, levels=ll[wch])
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.zone.year',add.str,ec.str,'.RData'))

        ##Zone/Indicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('Zone/Indicator')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.indicator.zone.year.A.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            if (any(grepl('.*(noSD|nonap|noNOx).*',fs)))  fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            if (any(grepl('.*(noSDnoNOx).*',fs))) fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
                                        #fs.str = unique(gsub('.*\\_{2}.*\\_{2}(.*)\\_{3}(.*).RData','\\1___\\2',fs))
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.indicator.zone.year.A)
        }
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'), Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        #data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        if(!include_enclosedcoastal) data.all =data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.zone.year',add.str,ec.str,'.RData'))


        ## WaterBody/Measure
        ## We cant just leverage above, since the scores are rounded..
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('WaterBody/Measure')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            if (any(grepl('.*(noSD|nonap|noNOx).*',fs))) fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            if (any(grepl('.*(noSDnoNOx).*',fs))) fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.measure.zone.year)
        }
        data.all = data.all %>% left_join(GBRwide.weights)
        data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        if(!include_enclosedcoastal) data.all =data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
        data.all = data.all %>% ungroup %>% group_by(Indicator,Subindicator,WaterBody,Measure,waterYear) %>%
            summarize(Mean=weighted.mean(Mean,w=Area))
        data.all.measure.waterbody.year = data.all
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        data.all = data.all %>% left_join(measures %>% dplyr:::select(Measure,UnitsLabel,Label))       
        if (stringr:::str_detect(src,'niskin')) {
            ll=c('chl','nap','sd','NOx')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            if (!include_NOx) ll = ll[ll!='NOx']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (stringr:::str_detect(src,'flntu')) {
            ll=c('chl','nap')
            if (!include_nap) ll = ll[ll!='nap']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (stringr:::str_detect(src,'eReefs')) {
            ll=c('chl','nap','sd','NOx')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            if (!include_NOx) ll = ll[ll!='NOx']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (src=='') {
            ll=c('chl','nap','sd')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.waterbody.year',add.str,ec.str,'.RData'))

        ## Open Coastal / Measure
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('Open Coastal/Measure')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            if (any(grepl('.*(noSD|nonap|noNOx).*',fs))) fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            if (any(grepl('.*(noSDnoNOx).*',fs))) fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.measure.zone.year)
        }
        data.all = data.all %>% filter(WaterBody=='Open Coastal') %>% left_join(GBRwide.weights.oc)
        data.all = data.all %>% ungroup %>% group_by(Indicator,Subindicator,WaterBody,Measure,waterYear) %>%
            summarize(Mean=weighted.mean(Mean,w=Area))
        data.all.measure.opencoastal.year = data.all
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        data.all = data.all %>% left_join(measures %>% dplyr:::select(Measure,UnitsLabel,Label))       
        if (stringr:::str_detect(src,'niskin')) {
            ll=c('chl','nap','sd','NOx')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            if (!include_NOx) ll = ll[ll!='NOx']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (stringr:::str_detect(src,'flntu')) {
            ll=c('chl','nap')
            if (!include_nap) ll = ll[ll!='nap']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (stringr:::str_detect(src,'eReefs')) {
            ll=c('chl','nap','sd','NOx')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            if (!include_NOx) ll = ll[ll!='NOx']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (src=='') {
            ll=c('chl','nap','sd')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.opencoastal.year',add.str,ec.str,'.RData'))

        
        ## WaterBody subindicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('WaterBody/Subindicator')
        data.all = data.all.measure.waterbody.year
        data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        data.all = data.all %>% ungroup %>% group_by(Indicator,Subindicator,WaterBody,waterYear) %>%
            summarize(Mean=mean(Mean))
        data.all.subindicator.waterbody.year = data.all
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        if (stringr:::str_detect(src,'niskin')) ll = c('Productivity','Water Clarity','Nutrients')
        if (stringr:::str_detect(src,'flntu')) ll=c('Productivity','Water Clarity')
        if (stringr:::str_detect(src,'eReefs')) ll=c('Productivity','Water Clarity','Nutrients')
        if (src=='') ll=c('Productivity','Water Clarity')
        wch=which(unique(data.all$Subindicator) %in% ll)
        data.all$Subindicator = factor(data.all$Subindicator, levels=ll[wch])        
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.waterbody.year',add.str,ec.str,'.RData'))

        ## Open Coastal subindicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('WaterBody/Subindicator')
        data.all = data.all.measure.opencoastal.year
        data.all = data.all %>% ungroup %>% group_by(Indicator,Subindicator,WaterBody,waterYear) %>%
            summarize(Mean=mean(Mean))
        data.all.subindicator.opencoastal.year = data.all
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        if (stringr:::str_detect(src,'niskin')) ll = c('Productivity','Water Clarity','Nutrients')
        if (stringr:::str_detect(src,'flntu')) ll=c('Productivity','Water Clarity')
        if (stringr:::str_detect(src,'eReefs')) ll=c('Productivity','Water Clarity','Nutrients')
        if (src=='') ll=c('Productivity','Water Clarity')
        wch=which(unique(data.all$Subindicator) %in% ll)
        data.all$Subindicator = factor(data.all$Subindicator, levels=ll[wch])        
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.opencoastal.year',add.str,ec.str,'.RData'))

        
        ##WaterBody/Indicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('WaterBody/Indicator')
        data.all = data.all.subindicator.waterbody.year
        data.all = data.all %>% ungroup %>% group_by(Indicator,WaterBody,waterYear) %>%
            summarize(Mean=mean(Mean))
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'), Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.waterbody.year',add.str,ec.str,'.RData'))

        ##Open Coastal/Indicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('Open Coastal/Indicator')
        data.all = data.all.subindicator.opencoastal.year
        data.all = data.all %>% ungroup %>% group_by(Indicator,WaterBody,waterYear) %>%
            summarize(Mean=mean(Mean))
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'), Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.waterbody.year',add.str,ec.str,'.RData'))


        ## GBR wide/Measure - this will employ GBRwide.weights
        ## We cant just leverage above, since the scores are rounded..
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('Zone/Measure')
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            if (any(grepl('.*(noSD|nonap|noNOx).*',fs))) fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            if (any(grepl('.*(noSDnoNOx).*',fs))) fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
        data.all <- NULL
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.av.measure.zone.year)
        }
        data.all = data.all %>% left_join(GBRwide.weights)
        if(!include_enclosedcoastal) data.all =data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
        data.all = data.all %>% ungroup %>% group_by(Indicator,Subindicator,Measure,waterYear) %>%
            summarize(Mean=weighted.mean(Mean,w=Area))
        data.all.measure.gbr.year = data.all
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        data.all = data.all %>% left_join(measures %>% dplyr:::select(Measure,UnitsLabel,Label))       
        if (stringr:::str_detect(src,'niskin')) {
            ll=c('chl','nap','sd','NOx')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            if (!include_NOx) ll = ll[ll!='NOx']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (stringr:::str_detect(src,'flntu')) {
            ll=c('chl','nap')
            if (!include_nap) ll = ll[ll!='nap']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (stringr:::str_detect(src,'eReefs')) {
            ll=c('chl','nap','sd','NOx')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            if (!include_NOx) ll = ll[ll!='NOx']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        if (src=='') {
            ll=c('chl','nap','sd')
            if (!include_sd) ll = ll[ll!='sd']
            if (!include_nap) ll = ll[ll!='nap']
            data.all$Measure = factor(data.all$Measure, levels=ll)
        }
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.gbr.year',add.str,ec.str,'.RData'))
        

        ## GBR subindicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('GBR/Subindicator')
        data.all = data.all.measure.gbr.year
        data.all = data.all %>% ungroup %>% group_by(Indicator,Subindicator,waterYear) %>%
            summarize(Mean=mean(Mean))
        data.all.subindicator.gbr.year = data.all
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        if (stringr:::str_detect(src,'niskin')) ll = c('Productivity','Water Clarity','Nutrients')
        if (stringr:::str_detect(src,'flntu')) ll=c('Productivity','Water Clarity')
        if (stringr:::str_detect(src,'eReefs')) ll=c('Productivity','Water Clarity','Nutrients')
        if (src=='') ll=c('Productivity','Water Clarity')
        wch=which(unique(data.all$Subindicator) %in% ll)
        data.all$Subindicator = factor(data.all$Subindicator, levels=ll[wch])        
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.gbr.year',add.str,ec.str,'.RData'))
        
        ##GBR/Indicator
        print(paste0('Src=',src,' Index=',index,' add.str=',add.str))
        print('GBR/Indicator')
        data.all = data.all.subindicator.gbr.year
        data.all = data.all %>% ungroup %>% group_by(Indicator,waterYear) %>%
            summarize(Mean=mean(Mean))
        data.all = data.all %>% mutate(Mean=round(Mean,decimal_places),Grade.mmp=WQ_generateGrades(Mean), Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'), Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
        save(data.all, file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.gbr.year',add.str,ec.str,'.RData'))

        
        ## report.card.colors = colorRampPalette(c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'))
        ## ## Map for Site/Measure
        ## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.site.year.*',full.names=TRUE)
        ## data.all <- NULL
        ## for (f in fs) {
        ##     load(f)
        ##     data.all <- rbind(data.all, data.av.measure.site.year)
        ## }
        ## data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        ## data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        ## data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        ## data.all = data.all %>% left_join(measures %>% dplyr:::select(Measure,UnitsLabel,Label))
        ## yl=range(data.all$Latitude)
        ## xl=range(data.all$Longitude)
        ## for (measure in unique(data.all$Measure))  {#c('chl','nap','sd','NOx')) {
        ##     unitsLabel = measures$Label[measures$Measure==measure]
        ##     g=ggplot(data.all %>% filter(Measure==measure), aes(y=Latitude,x=Longitude))
        ##     if (stringr:::str_detect(src,'eReefs')) {
        ##         g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn(paste0(unitsLabel,'\nIndex\n'),colors=rev(report.card.colors(10))) 
        ##     } else if (stringr:::str_detect(src,'niskin')) {
        ##         g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn(paste0(unitsLabel,'\nIndex\n'),colors=rev(report.card.colors(10)))
        ##     } else if (stringr:::str_detect(src,'flntu')) {
        ##         g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn(paste0(unitsLabel,'\nIndex\n'),colors=rev(report.card.colors(10))) 
        ##     } else {
        ##         g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn(paste0(unitsLabel,'\nIndex\n'),colors=rev(report.card.colors(10)))
        ##     }
        ##     g= g+geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', size=0.1,color='black') +
        ##         geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, size=0.1,color='black') +
        ##         facet_wrap(~waterYear) +
        ##         theme_classic() +
        ##         coord_equal(ylim=yl, xlim=xl) +
        ##         theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
        ##               strip.background=element_blank(), axis.text.y=element_text(size=10), axis.text.x=element_text(size=10),
        ##               legend.title=element_text(margin=c(1,1),'lines'))
        ##     ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_measure.',measure,'.site.pdf'), g, width=10, units='in', device=cairo_pdf)
        ##     ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_measure.',measure,'.site.png'), g, width=10, units='in',dpi=300)
        ## }
            
        ## ## Map for Site/Subindicator
        ## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.subindicator.site.year.*',full.names=TRUE)
        ## data.all <- NULL
        ## for (f in fs) {
        ##     load(f)
        ##     data.all <- rbind(data.all, data.av.subindicator.site.year)
        ## }
        ## data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        ## data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        ## data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        ## yl=range(data.all$Latitude)
        ## xl=range(data.all$Longitude)
        ## for (subindicator in unique(data.all$Subindicator)) {
        ##     g=ggplot(data.all %>% filter(Subindicator==subindicator), aes(y=Latitude,x=Longitude))
        ##     if (stringr:::str_detect(src,'eReefs')) {
        ##         g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(10))) 
        ##     } else if (stringr:::str_detect(src,'niskin')) {
        ##         g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(10)))
        ##     } else if (stringr:::str_detect(src,'flntu')) {
        ##         g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(10))) 
        ##     } else {
        ##         g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(10)))
        ##     }
        ##     g= g+geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', size=0.1,color='black') +
        ##         geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, size=0.1,color='black') +
        ##         facet_wrap(~waterYear) +
        ##         theme_classic() +
        ##         coord_equal(ylim=yl, xlim=xl) +
        ##         theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
        ##               strip.background=element_blank(), axis.text.y=element_text(size=10), axis.text.x=element_text(size=10),
        ##               legend.title=element_text(margin=c(1,1),'lines'))
        ##     ## gg=ggplotGrob(g)
        ##     ## nw=sum(grid:::convertWidth(gg$widths,'in',valueOnly=TRUE)) #sum(sapply(gg$widths,function(x) grid:::convertWidth(x,'in')))
        ##     ## nh=sum(grid:::convertHeight(gg$heights,'in',valueOnly=TRUE))
        ##     ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.',subindicator,'.site.pdf'), g, width=10, units='in', device=cairo_pdf)
        ##     ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.',subindicator,'.site.png'), g, width=10, units='in',dpi=300)
        ## }
        
        
        ## ## Map for Site/Indicator
        ## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.indicator.site.year.*',full.names=TRUE)
        ##                                 #fs.str = unique(gsub('.*\\_{2}.*\\_{2}(.*)\\_{3}(.*).RData','\\1___\\2',fs))
        ## data.all <- NULL
        ## for (f in fs) {
        ##     load(f)
        ##     data.all <- rbind(data.all, data.av.indicator.site.year)
        ## }
        ## data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        ## data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        ## data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        ## yl=range(data.all$Latitude)
        ## xl=range(data.all$Longitude)
        ## g=ggplot(data.all, aes(y=Latitude,x=Longitude))
        ## if (stringr:::str_detect(src,'eReefs')) {
        ##     g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(10))) 
        ## } else if (stringr:::str_detect(src,'niskin')) {
        ##     g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(10)))
        ## } else if (stringr:::str_detect(src,'flntu')) {
        ##     g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(10))) 
        ## } else {
        ##     g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(10)))
        ## }
        ## g= g+geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', size=0.1,color='black') +
        ##     geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, size=0.1,color='black') +
        ##     facet_wrap(~waterYear) +
        ##     theme_classic() +
        ##     coord_equal(ylim=yl, xlim=xl) +
        ##     theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
        ##           strip.background=element_blank(), axis.text.y=element_text(size=10), axis.text.x=element_text(size=10),
        ##           legend.title=element_text(margin=c(1,1),'lines'))
        ## ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone_A.pdf'), g, width=10, units='in',device=cairo_pdf)
        ## ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone_A.png'), g, width=10, units='in',dpi=300)
        
        ## ##Up to here
        ## ## Graphical summary for Zone/Measure
        ## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
        ##                                 #fs.str = unique(gsub('.*\\_{2}.*\\_{2}(.*)\\_{3}(.*).RData','\\1___\\2',fs))
        ## data.all <- NULL
        ## for (f in fs) {
        ##     load(f)
        ##     data.all <- rbind(data.all, data.av.measure.zone.year)
        ## }
        ## data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        ## data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        ## data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        ## data.all = data.all %>% left_join(measures %>% dplyr:::select(Measure,UnitsLabel,Label))
        ## if(!include_enclosedcoastal) data.all =data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
        ## if (stringr:::str_detect(src,'niskin')) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','sd','NOx'))
        ## if (stringr:::str_detect(src,'flntu')) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap'))
        ## if (stringr:::str_detect(src,'eReefs')) data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','sd','NOx'))
        ## if (src=='') data.all$Measure = factor(data.all$Measure, labels=c('chl','nap','sd'))
        ## g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
        ##     annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
        ## annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
        ## annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
        ## geom_line(aes(group=Measure)) + geom_point(aes(fill=Measure), shape=21, color='black', size=3) +
        ##     scale_x_continuous('Water year') +
        ##     scale_y_continuous('Index')
        ## if (stringr:::str_detect(src,'eReefs')) {
        ##     g=g+scale_fill_manual('Index\n',breaks=c('chl','nap','sd','NOx'), labels=c('Chlorophyll','TSS','Secchi','NOx'), values=c('green','blue','orange','red'))
        ## } else if (src==''){
        ##     g=g+scale_fill_manual('Index\n',breaks=c('chl','nap','sd'), labels=c('Chlorophyll','TSS','Secchi'),values=c('green','blue','orange'))
        ## } else if (stringr:::str_detect(src,'niskin')) {
        ##     g=g+scale_fill_manual('Index\n',breaks=c('chl','nap','sd','NOx'), labels=c('Chlorophyll','TSS','Secchi','NOx'),values=c('green','blue','orange','red'))
        ## } else if (stringr:::str_detect(src,'flntu')) {
        ##     g=g+scale_fill_manual('index\n',breaks=c('chl','nap'), labels=c('Chlorophyll','TSS'),values=c('green','blue'))
        ## }
        ## g=g+facet_grid(Region~WaterBody, scales='free_y') + theme_bw()+
        ##     theme(strip.background=element_blank())
        ## ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone.pdf'), g, width=10, height=10, units='in',device=cairo_pdf)
        ## ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone.png'), g, width=10, height=10, units='in',dpi=300)
        
        ## ## Flat map (Zone/Measure)
        ## data.map = data.all %>% full_join(Polys.df)
        ## yl=range(data.map$lat)
        ## xl=range(data.map$long)
        ## for (measure in unique(data.all$Measure)) {
        ##     g=ggplot(data.map %>% filter(Measure==measure), aes(y=lat, x=long)) +
        ##         geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey',size=0.1, color='black') +
        ##         geom_polygon(aes(fill=Grade,group=group), color='black',size=0.1) +
        ##         geom_polygon(data=Polys.df, aes(group=group), fill=NA, color='black', size=0.1) +
        ##         facet_wrap(~waterYear) +
        ##         scale_fill_manual('Grade\n', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) + 
        ##     theme_classic() +
        ##         coord_equal(ylim=yl, xlim=xl) +
        ##         theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
        ##               strip.background=element_blank(), axis.text.y=element_text(size=10), axis.text.x=element_text(size=10))
        ##     ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_measure',measure,'.zone_A.pdf'), g, width=10, units='in',device=cairo_pdf)
        ##     ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_measure',measure,'.zone_A.png'), g, width=10, units='in',dpi=300)
        ## }
        
        
        ## ## Mosaic plot for Zone/Measure
        ## data.mosaic = data.all %>% ungroup %>%
        ##     mutate(Region=factor(Region, levels=rev(levels(.$Region))))
        ## g = ggplot(data.mosaic, aes(y=Region, x=as.factor(waterYear))) +
        ##     geom_tile(aes(fill=Grade), color='white', size=0.5) +
        ##     scale_y_discrete('',expand=c(0,0)) +
        ##     scale_x_discrete('',expand=c(0,0)) +
        ##     scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
        ##     facet_grid(Label~WaterBody) +
        ##     theme_classic() +
        ##     theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
        ##           strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))
        ## xw=length(unique(data.mosaic$WaterBody))*length(unique(data.mosaic$waterYear))
        ## xh=length(unique(data.mosaic$Region))
        ## ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone.pdf'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',device=cairo_pdf)
        ## ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone.png'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',dpi=300)
        
        ## ## Graphical summary for Zone/Subindicator
        ## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.subindicator.zone.year.*',full.names=TRUE)
        ## data.all <- NULL
        ## for (f in fs) {
        ##     load(f)
        ##     data.all <- rbind(data.all, data.av.subindicator.zone.year)
        ## }
        ## data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        ## data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        ## data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        ## if(!include_enclosedcoastal) data.all =data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
        ## if (stringr:::str_detect(src,'niskin')) data.all$Subindicator = factor(data.all$Subindicator, labels=c('Productivity','Water Clarity','Nutrients'))
        ## if (stringr:::str_detect(src,'flntu')) data.all$Subindicator = factor(data.all$Subindicator, labels=c('Productivity','Water Clarity'))
        ## if (stringr:::str_detect(src,'eReefs')) data.all$Subindicator = factor(data.all$Subindicator, labels=c('Productivity','Water Clarity','Nutrients'))
        ## if (src=='') data.all$Subindicator = factor(data.all$Subindicator, labels=c('Productivity','Water Clarity'))
        ## g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
        ##     annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
        ## annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
        ## annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
        ## geom_line(aes(group=Subindicator)) + geom_point(aes(fill=Subindicator), shape=21, color='black', size=3) +
        ##     scale_x_continuous('Water year') +
        ##     scale_y_continuous('Index')
        ## if (stringr:::str_detect(src,'niskin')) g=g+scale_fill_manual('Index\n',breaks=c('Productivity','Water Clarity','Nutrients'), values=c('green','blue','red'))
        ## if (stringr:::str_detect(src,'flntu')) g=g+scale_fill_manual('Index\n',breaks=c('Chlorophyll','Water Clarity'), values=c('green','blue'))
        ## if (stringr:::str_detect(src,'eReefs')) g=g+scale_fill_manual('Index\n',breaks=c('Productivity','Water Clarity','Nutrients'), values=c('green','blue','red'))
        ## if (src=='') g=g+scale_fill_manual('Index\n',breaks=c('Chlorophyll','Water Clarity'), values=c('green','blue'))           
        ## g=g+facet_grid(Region~WaterBody, scales='free_y') + theme_bw() + theme(strip.background=element_blank())
        ## ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone.pdf'), g, width=10, height=10, units='in',device=cairo_pdf)
        ## ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone.png'), g, width=10, height=10, units='in',dpi=300)
        
        ## ## Flat map (Zone/Subindicator)==========================
        ## data.map = data.all %>% full_join(Polys.df)
        ## yl=range(data.map$lat)
        ## xl=range(data.map$long)
        ## for (subindicator in unique(data.all$Subindicator)) {
        ##     g=ggplot(data.map %>% filter(Subindicator==subindicator), aes(y=lat, x=long)) +
        ##         geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black',size=0.1) +
        ##         geom_polygon(aes(fill=Grade,group=group), color='black',size=0.1) +
        ##         geom_polygon(data=Polys.df, aes(group=group), fill=NA, color='black', size=0.1) +
        ##         facet_wrap(~waterYear) +
        ##         scale_fill_manual('Grade\n', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) + 
        ##     theme_classic() +
        ##         coord_equal(ylim=yl, xlim=xl) +
        ##         theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
        ##               strip.background=element_blank(), axis.text.y=element_text(size=10), axis.text.x=element_text(size=10))
        ##     ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator',subindicator,'.zone_A.pdf'), g, width=10, units='in',device=cairo_pdf)
        ##     ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator',subindicator,'.zone_A.png'), g, width=10, units='in',dpi=300)
        ## }
        
        ## ## Mosaic plot for Zone/Subindicator
        ## data.mosaic = data.all %>% ungroup %>%
        ##     mutate(Region=factor(Region, levels=rev(levels(.$Region))))
        ## g = ggplot(data.mosaic, aes(y=Region, x=as.factor(waterYear))) +
        ##     geom_tile(aes(fill=Grade), color='white', size=0.5) +
        ##     scale_y_discrete('',expand=c(0,0)) +
        ##     scale_x_discrete('',expand=c(0,0)) +
        ##     scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
        ##     facet_grid(Subindicator~WaterBody) +
        ##     theme_classic() +
        ##     theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
        ##           strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1))
        ## xw=length(unique(data.mosaic$WaterBody))*length(unique(data.mosaic$waterYear))
        ## xh=length(unique(data.mosaic$Region))
        ## ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone.pdf'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',device=cairo_pdf)
        ## ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone.png'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',dpi=300)
        
        
        ## ## Graphical summary for Zone/Indicator Pathway A
        ## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.indicator.zone.year.A.*',full.names=TRUE)
        ##                                 #fs.str = unique(gsub('.*\\_{2}.*\\_{2}(.*)\\_{3}(.*).RData','\\1___\\2',fs))
        ## data.all <- NULL
        ## for (f in fs) {
        ##     load(f)
        ##     data.all <- rbind(data.all, data.av.indicator.zone.year.A)
        ## }
        ## data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        ## data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        ## data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        ## if(!include_enclosedcoastal) data.all =data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
        ## g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
        ##     annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
        ## annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
        ## annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
        ## geom_line() + geom_point(aes(fill=Grade), shape=21, color='black', size=3) +
        ##     scale_y_continuous('Water Quality Index') +
        ##     scale_x_continuous('Water Year') +
        ##     scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'),limits=LETTERS[1:5]) +
        ## facet_grid(Region~WaterBody, scales='free_y') + theme_bw()+ theme(strip.background=element_blank(),legend.key=element_blank(),
        ##                                                                   strip.text=element_text(size=12)) +
        ##     guides(fill=guide_legend(override.aes=list(color='black', size=5)))
        ## ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone_A.pdf'), g, width=10, height=10, units='in',device=cairo_pdf)
        ## ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone_A.png'), g, width=10, height=10, units='in',dpi=300)
        
        ## ## Flat map (Zone/Indicator)
        ## data.map = data.all %>% full_join(Polys.df) %>% filter(!is.na(waterYear))
        ## yl=range(data.map$lat)
        ## xl=range(data.map$long)
        ## g=ggplot(data.map, aes(y=lat, x=long)) +
        ##     geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black',size=0.1) +
        ##     geom_polygon(aes(fill=Grade,group=group), color='black',size=0.1) +
        ##     geom_polygon(data=Polys.df, aes(group=group), fill=NA, color='black', size=0.1) +
        ##     facet_wrap(~waterYear) +
        ##     scale_fill_manual('Grade\n', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) + 
        ## theme_classic() +
        ##     coord_equal(ylim=yl, xlim=xl) +
        ##     theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
        ##           strip.background=element_blank(), axis.text.y=element_text(size=10), axis.text.x=element_text(size=10))
        ## ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone_A.pdf'), g, width=10, units='in',device=cairo_pdf)
        ## ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone_A.png'), g, width=10, units='in',dpi=300)
        
        ## ## Mosaic plot for Zone/Indicator
        ## data.mosaic = data.all %>% ungroup %>%
        ##     mutate(Region=factor(Region, levels=rev(levels(.$Region))))
        ## g = ggplot(data.mosaic, aes(y=Region, x=as.factor(waterYear))) +
        ##     geom_tile(aes(fill=Grade), color='white', size=0.5) +
        ##     scale_y_discrete('',expand=c(0,0)) +
        ##     scale_x_discrete('',expand=c(0,0)) +
        ##     scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
        ##     facet_grid(Indicator~WaterBody) +
        ##     theme_classic() +
        ##     theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
        ##           strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
        ##           strip.text=element_text(size=12))
        ## xw=length(unique(data.mosaic$WaterBody))*length(unique(data.mosaic$waterYear))
        ## xh=length(unique(data.mosaic$Region))
        ## ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone.pdf'), g, width=(xw*1)+1, height=(xh*1)+1, units='in')
        ## ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone.png'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',dpi=300)
        
        ## ## Graphical summary for Pathway B
        ## fs = list.files(path=paste0('eReefs/data/aggregated/',index,'/',temporal),pattern='data.av.indicator.zone.year.B.*',full.names=TRUE)
        ## data.all <- NULL
        ## for (f in fs) {
        ##     load(f)
        ##     data.all <- rbind(data.all, data.av.indicator.zone.year.B)
        ## }
        ## data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
        ## data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
        ## data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
        ## g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
        ##     annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
        ## annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
        ## annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
        ## annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
        ## geom_line() + geom_point(aes(fill=Grade), shape=21, color='black', size=3) +
        ##     scale_y_continuous('Water Quality Index') +
        ##     scale_fill_manual('Grade',breaks=LETTERS[1:5], values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24')) +
        ## facet_grid(Region~WaterBody, scales='free_y') + theme_bw()
        ## ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone_B.pdf'), g, width=10, height=12, units='in')
        
        ## ## Maps
    }    
    
}



## index='fsMAMP'
## temporal='Annual'
## src=''
## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
## data.all <- NULL
## for (f in fs) {
##     load(f)
##     data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='Satellite'))
## }
## src='eReefs/'
## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
## for (f in fs) {
##     load(f)
##     data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='eReefs'))
## }
## src='eReefs926/'
## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
## for (f in fs) {
##     load(f)
##     data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='eReefs926'))
## }
## src='niskin/'
## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
## for (f in fs) {
##     load(f)
##     data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='AIMS insitu'))
## }
## src='flntu/'
## fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
## for (f in fs) {
##     load(f)
##     data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='AIMS flntu'))
## }        

## load('data/data.idx.av.sub1.RData')
## d=data.idx.av.sub1 %>% filter(Index=='fsMAMP') %>% select(Indicator,Subindicator,Measure,Region,WaterBody,waterYear,Mean)
## data.all = rbind(data.all,
##                  data.frame(d, Source='AIMS insitu'))

## data.all = data.all %>% mutate(Grade=MMP_generateGrades(Mean))
## data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
## data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
## if(!include_enclosedcoastal) data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')
## measure='chl'
## g=ggplot(data.all %>% filter(Measure==measure), aes(y=Mean, x=waterYear)) +
##     annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') + 
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
##   annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
##   annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
##   geom_line(aes(group=Source)) +
##     geom_point(aes(fill=Source), shape=21, color='black', size=3) +
##     scale_x_continuous('Water year') +
##     scale_y_continuous('Index') +
##     scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu'), labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu'),values=c('green','red','blue','orange')) +
##     facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
##       theme(strip.background=element_blank())+
##             ggtitle(paste0('Measure=',measure,', Index=',index,', Temporal=', temporal))
## ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone.pdf'), g, width=10, height=10, units='in')
## ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone.png'), g, width=10, height=10, units='in',dpi=300)

## measure='nap'
## g=ggplot(data.all %>% filter(Measure==measure), aes(y=Mean, x=waterYear)) +
##     annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') + 
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
##   annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
##   annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
##   geom_line(aes(group=Source)) +
##     geom_point(aes(fill=Source), shape=21, color='black', size=3) +
##     scale_x_continuous('Water year') +
##       scale_y_continuous('Water Quality Index') +
##       scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu'), labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu'),values=c('green','red','blue','orange'), limits=c('Remote sensing','eReefs','eReefs926','AIMS insitu')) +
##     facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
##     theme(strip.background=element_blank())+
##       ggtitle(paste0('Measure=',measure,', Index=',index,', Temporal=', temporal))
## ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone.pdf'), g, width=10, height=10, units='in')
## ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone.png'), g, width=10, height=10, units='in',dpi=300)

## measure='sd'
## g=ggplot(data.all %>% filter(Measure==measure), aes(y=Mean, x=waterYear)) +
##     annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') + 
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
##   annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
##   annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
##   geom_line(aes(group=Source)) +
##     geom_point(aes(fill=Source), shape=21, color='black', size=3) +
##     scale_x_continuous('Water year') +
##       scale_y_continuous('Water Quality Index') +
##       scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu'), labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu'),values=c('green','red','blue','orange'), limits=c('Remote sensing','eReefs','eReefs926','AIMS insitu')) +
##     facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
##     theme(strip.background=element_blank())+
##       ggtitle(paste0('Measure=',measure,', Index=',index,', Temporal=', temporal))
## ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone.pdf'), g, width=10, height=10, units='in')
## ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone.png'), g, width=10, height=10, units='in',dpi=300)


## measure='NOx'
## g=ggplot(data.all %>% filter(Measure==measure), aes(y=Mean, x=waterYear)) +
##     annotate(geom='blank',y=0, x=min(data.all$waterYear))+ 
##     annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') + 
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
##   annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
##   annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
##   annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
##   geom_line(aes(group=Source)) +
##     geom_point(aes(fill=Source), shape=21, color='black', size=3) +
##     scale_x_continuous('Water year') +
##       scale_y_continuous('Water Quality Index') +
##       scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu'), labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu'),values=c('green','red','blue','orange'), limits=c('Remote sensing','eReefs','eReefs926','AIMS insitu')) +
##     facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
##     theme(strip.background=element_blank())+
##       ggtitle(paste0('Measure=',measure,', Index=',index,', Temporal=', temporal))
## ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone.pdf'), g, width=10, height=10, units='in')
## ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone.png'), g, width=10, height=10, units='in',dpi=300)

unlink('status.txt')
cat('Finished aggregate without uncertainty', file='status.txt')
