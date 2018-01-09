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

## Now put together according to Measure, Zone
## We will also add:
## - wateryear
## - Season
## - measure data
## - spatial data
registerDoParallel(cores=24)
measures = read.table('parameters/measures.txt', strip.white=TRUE, sep=';', header=TRUE)
wq.guidelines = read.csv('parameters/wq.guidelines.csv', strip.white=TRUE) %>%
    dplyr:::select(-Justification.Source) %>% dplyr:::rename(GL=Annual.guideline) %>% dplyr:::filter(!is.na(GL))

flist <- list.files(path = "eReefs926/data/raw", pattern = "^ereefs.*")
#flist
meas = unique(gsub('ereefs_([a-zA-Z0-9]{2,4}).*','\\1',flist))
zones = unique(gsub('ereefs_[a-zA-Z0-9]{2,4}.*___(.*).RData','\\1',flist))
foreach(m=meas) %do% {
    foreach(z=zones) %dopar% {
        fs.str = grep(paste0('ereefs_',m,'.*___',z,'.RData'),flist)
        data = NULL
        for (i in fs.str) {
            print(i)
            load(file=paste0('eReefs926/data/raw/',flist[i]))
            data=rbind(data,ereefs)
        }
        spatial=read.csv('parameters/spatial.csv', strip.white=TRUE) %>%
            dplyr:::filter(GBRMPA_Zone==z) %>% dplyr:::select(-GBRMPA_Zone)
        region=unique(spatial$Region)
        waterbody=unique(spatial$WaterBody)
        zone=unique(spatial$Zone)
        data=data %>% mutate(Date=tm,waterYear=WQ_waterYear(Date), Season=WQ_season(Date),
                                           Region=region,WaterBody=waterbody,Zone=zone) %>% select(-tm) %>%
            left_join(measures %>% mutate(Measure = as.character(Measure))) %>%
            left_join(wq.guidelines %>% mutate(Measure = as.character(Measure))) %>%
            mutate(GL.seasonal=ifelse(Season=='Dry',Dry,Wet))
        save(data, file=paste0('eReefs926/data/processed/data_',m,'__',region,'___',waterbody,'.RData'))
    }
    
}




