source('WQ_functions.R')

require(sqldf)
require(dplyr)
require(raster)
require(ncdf4)
require(tidyr)
require(lubridate)
require(foreach)
require(doParallel)

unlink('log/processNiskin.log', recursive=TRUE)

WQ_tryCatch(
{
    fs = list.files(path='data/GIS', pattern='spatial.*', full.names = TRUE)
    for (i in fs) load(file=i)
}, paste0('processNiskin.log'), msg='Load shapefiles', return=TRUE)


niskin <- read.csv('data/niskin/niskin.aims.csv', strip.white=TRUE)
niskin= niskin %>%
    mutate(Measure=case_when(as.character(niskin$Measure)=='tss' ~ 'nap',
                             as.character(niskin$Measure)=='chl' ~ 'chl',
                             as.character(niskin$Measure)=='secchi' ~ 'sd',
                             as.character(niskin$Measure)=='NOx' ~ 'NOx'))
niskin.av = niskin %>% 
    group_by(reef.alias) %>%
    summarize_at(vars(LATITUDE,LONGITUDE), funs(mean(.,na.rm=TRUE)))



##flntu data
load(file='data/flntu.all.RData')
flntu = flntu.all %>% select(reef.alias,Date,LATITUDE,LONGITUDE,CHL_QA_AVG,NTU_QA_AVG) %>%
    rename(chl=CHL_QA_AVG, nap=NTU_QA_AVG) %>%
    gather(key=Measure,value=Value, chl,nap) %>%
    mutate(Source='FLNTU')

#########################################################################
## Need to apply the zone information to the niskin data Ideally, this ##
## would be done with one of the rgeos functions.  However, rgeos is   ##
## unavailable.  Therefore, as an alternative we will use              ##
## sp::point.in.poly.  This is less efficient, but for discrete data   ##
## should be ok.                                                       ##
#########################################################################
niskin.full = NULL
flntu.full = NULL
coordinates(niskin) = ~LONGITUDE+LATITUDE
coordinates(flntu) = ~ LONGITUDE+LATITUDE
proj4string(niskin) =CRS('+proj=longlat +ellps=GRS80 +no_defs')
proj4string(flntu) =CRS('+proj=longlat +ellps=GRS80 +no_defs')
fs = list.files(path='data/GIS', pattern='spatial.*', full.names = TRUE)
for (f in fs) {
    waterbody=case_when(stringr:::str_detect(f, 'Enc') ~ 'Enclosed Coastal',
                        stringr:::str_detect(f, 'Open') ~ 'Open Coastal',
                        stringr:::str_detect(f, 'Midshelf') ~ 'Midshelf',
                        stringr:::str_detect(f, 'Offshore') ~ 'Offshore')
    region=case_when(stringr:::str_detect(f, 'Cape') ~ 'Cape York',
                     stringr:::str_detect(f, 'Terrain') ~ 'Wet Tropics',
                     stringr:::str_detect(f, 'Burdekin') ~ 'Dry Tropics',
                     stringr:::str_detect(f, 'Mackay') ~ 'Mackay Whitsunday',
                     stringr:::str_detect(f, 'Fitzroy') ~ 'Fitzroy',
                     stringr:::str_detect(f, 'Burnett') ~ 'Burnett Mary')
    print(f)
    shp=load(file=f)
    eval(parse(text=paste0('shp=',shp)))
    pts = sp::over(niskin,shp)
    pts = if_else(is.na(pts),FALSE,TRUE)
    if (any(pts)) niskin.full=rbind(niskin.full, cbind(as.data.frame(niskin[pts,]), Region=region, waterBody=waterbody))

    pts.flntu = sp::over(flntu,shp)
    pts.flntu = if_else(is.na(pts.flntu),FALSE,TRUE)
    if (any(pts.flntu)) flntu.full=rbind(flntu.full, cbind(as.data.frame(flntu[pts.flntu,]), Region=region, waterBody=waterbody))
}
save(niskin.full, file='data/comparisons/niskin.full.RData')
load(file='data/comparisons/niskin.full.RData')
save(flntu.full, file='data/comparisons/flntu.full.RData')
load(file='data/comparisons/flntu.full.RData')

niskin.full = niskin.full %>% mutate(Date=as.Date(Date), waterYear=WQ_waterYear(Date))
flntu.full = flntu.full %>% mutate(Date=as.Date(Date), waterYear=WQ_waterYear(Date))


## now we are going to produce specific versions that are consistent with processed Satellite and eReefs data
## to facilitate using the same plotting routines for EDA.

measures = read.table('parameters/measures.txt', strip.white=TRUE, sep=';', header=TRUE)
spatial=read.csv('parameters/spatial.csv', strip.white=TRUE)

wq.guidelines = read.csv('parameters/wq.guidelines.csv', strip.white=TRUE) %>%
    dplyr:::select(-Justification.Source) %>% dplyr:::rename(GL=Annual.guideline) %>% dplyr:::filter(!is.na(GL)) %>%
    left_join(
        niskin.full %>% select(reef.alias,Region,waterBody) %>% distinct %>% rename(WaterBody = waterBody) %>%
        left_join(spatial) %>% select(-GBRMPA_Zone)
    ) %>%
    mutate(Region=factor(Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary')),
           WaterBody=factor(WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore')))



##  Niskin
fs=expand.grid(Measure=unique(niskin.full$Measure), Region=unique(niskin.full$Region), WaterBody=unique(niskin.full$waterBody))
fs.str = paste0('data_',fs$Measure,'__',fs$Region,'___',fs$WaterBody)

for (z in 1:nrow(fs)) {
    data = niskin.full %>% filter(Measure==fs$Measure[z], Region==fs$Region[z], waterBody==fs$WaterBody[z])
    data = data %>% dplyr:::select(Longitude=LONGITUDE,Latitude=LATITUDE,Date,Value,waterYear,Season,Measure,Region,WaterBody=waterBody,Source) %>%
        mutate(Zone=paste0(gsub(' ','_',WaterBody),'_',Region)) %>%
        left_join(measures %>% dplyr:::select(-Source)) %>% 
        left_join(wq.guidelines)
    save(data, file=paste0('niskin/data/processed/',fs.str[z],'.RData'))
}


##  FLNTU
fs=expand.grid(Measure=unique(flntu.full$Measure), Region=unique(flntu.full$Region), WaterBody=unique(flntu.full$waterBody))
fs.str = paste0('data_',fs$Measure,'__',fs$Region,'___',fs$WaterBody)

for (z in 1:nrow(fs)) {
    data = flntu.full %>% filter(Measure==fs$Measure[z], Region==fs$Region[z], waterBody==fs$WaterBody[z]) %>%
        mutate(Season=WQ_season(Date))
    data = data %>% dplyr:::select(Longitude=LONGITUDE,Latitude=LATITUDE,Date,Value,waterYear,Season,Measure,Region,WaterBody=waterBody,Source) %>%
        mutate(Zone=paste0(gsub(' ','_',WaterBody),'_',Region)) %>%
        left_join(measures %>% dplyr:::select(-Source)) %>% 
        left_join(wq.guidelines)
    save(data, file=paste0('flntu/data/processed/',fs.str[z],'.RData'))
}



