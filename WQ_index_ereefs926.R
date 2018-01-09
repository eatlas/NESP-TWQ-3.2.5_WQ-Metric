library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
require(foreach)
require(doParallel)


source('WQ_functions.R')


registerDoParallel(cores=24)


fs = list.files(path='eReefs926/data/processed',pattern='data_.*.RData',full.names=TRUE)
foreach(z=fs) %dopar% {
    load(z)
    print(z)
    meas=unique(data$Measure)
    region=unique(data$Region)
    waterbody=unique(data$WaterBody)
    WQ_tryCatch(
    {
        data.idx = data %>% mutate(Binary=WQ_WQI_binary(.),
                                   MAMP=WQ_WQI_mamp(.,capped=FALSE,scaled=FALSE),
                                   fMAMP=WQ_WQI_mamp(.,capped=TRUE, scaled=FALSE),
                                   fsMAMP=WQ_WQI_mamp(.),
                                   fsMAMP4=WQ_WQI_mamp(.,fold=4))
        save(data.idx,file=paste0('eReefs926/data/indexed/data.idx_',meas, '__',region,'___',waterbody,'.RData'))

        ## I will also save all individual waterYear subsets.  This is a logical way to partition up the huge data
        wy=unique(data.idx$waterYear)
        for (y in wy) {
            data.idx.yr = data.idx %>% dplyr:::filter(waterYear==y)
            save(data.idx.yr, file=paste0('eReefs926/data/indexed/data.idx.',y,'_',meas, '__',region,'___',waterbody,'.RData'))
        }
        
        data.idx = data %>% mutate(GL=GL.seasonal) %>% mutate(Binary=WQ_WQI_binary(.),
                                                              MAMP=WQ_WQI_mamp(.,capped=FALSE,scaled=FALSE),
                                                              fMAMP=WQ_WQI_mamp(.,capped=TRUE, scaled=FALSE),
                                                              fsMAMP=WQ_WQI_mamp(.),
                                                              fsMAMP4=WQ_WQI_mamp(.,fold=4))
        save(data.idx,file=paste0('eReefs926/data/indexed/data.idx.seasonal_',meas, '__',region,'___',waterbody,'.RData'))
        
        wy=unique(data.idx$waterYear)
        for (y in wy) {
            data.idx.yr = data.idx %>% dplyr:::filter(waterYear==y)
            save(data.idx.yr, file=paste0('eReefs926/data/indexed/data.idx.seasonal.',y,'_',meas, '__',region,'___',waterbody,'.RData'))
        }

        ## Exceedance and duration(only relevant at year resolution)
        ## Frequency duration is measured as the maximum number of consecutive exceedences
        ## on the available data (expressed as a fraction of available data)
        data.idx = data %>% mutate(Binary=WQ_WQI_binary(.)) %>%
            filter(!is.na(Binary)) %>%
            group_by(Longitude,Latitude,waterYear,Measure,Region,WaterBody,Zone,Source) %>%
            summarize(Exceed=WQ_exceed(Binary),Max_Duration=WQ_duration(Binary))               
        save(data.idx,file=paste0('eReefs926/data/indexed/data.idx.exceed_',meas, '__',region,'___',waterbody,'.RData'))
        
        data.idx = data %>% mutate(GL=GL.seasonal) %>% mutate(Binary=WQ_WQI_binary(.)) %>%
            filter(!is.na(Binary)) %>%
            group_by(Longitude,Latitude,waterYear,Measure,Region,WaterBody,Zone,Source) %>%
            summarize(Exceed=WQ_exceed(Binary),Max_Duration=WQ_duration(Binary))
        ## data.idx = data %>% mutate(GL=GL.seasonal) %>% mutate(Binary=as.numeric(WQ_WQI_binary(.)==0)) %>%
        ##     group_by(Longitude,Latitude,waterYear,Measure,Region,WaterBody,Zone,Source) %>%
        ##     summarize(Exceed=1-(sum(Binary,na.rm=TRUE)/n()))
        save(data.idx,file=paste0('eReefs926/data/indexed/data.idx.exceed.seasonal_',meas, '__',region,'___',waterbody,'.RData'))

    },paste0('indexeReefs926.log'), msg=paste0('Generate index on raw data (Measure=',meas,', Region=',region,', Water Body=',waterbody,')'), return=TRUE)


    rm(data.idx,data)
    gc()

}
