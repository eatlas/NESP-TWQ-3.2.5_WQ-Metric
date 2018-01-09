library(ggplot2)
library(dplyr)
library(sp)
library(tidyr)
library(broom)
require(doParallel)
library(gridExtra)
library(xtable)

source('WQ_functions.R')


## not using the following yet
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


##We could start by plotting each of the sources from within 5km of MMP Niskin locations
registerDoParallel(cores=20)
comp.df=NULL
comp.coords = NULL
radius=5
lag=Inf
for (m in unique(niskin.full$Measure)) {
    foreach(r=unique(niskin.full$Region)) %dopar% {
#    for (r in unique(niskin.full$Region)) {
        for (w in unique(niskin.full$waterBody)) {
            print(paste('Measure=',m,', Region=',r,', WaterBody=',w))
            nis = niskin.full %>% filter(Measure==m, Region==r, waterBody==w)
            nis.1.coords = nis %>%
                dplyr:::group_by(reef.alias) %>%
                summarize(Latitude=mean(LATITUDE,na.rm=TRUE),Longitude=mean(LONGITUDE,na.rm=TRUE), Date=min(as.Date(Date),na.rm=TRUE),Value=mean(Value,na.rm=TRUE))
            if (nrow(nis.1.coords)==0) next
            ## Start with the Satellite data
            if (length(list.files(path='data/indexed', pattern=paste0('data.idx_',m,'__',r,'___',w,'.RData')))>0) {
                load(paste0('data/indexed/data.idx_',m,'__',r,'___',w,'.RData'))            
                data=data.idx %>% dplyr:::select(-Binary,-MAMP,-fMAMP,-fsMAMP,-fsMAMP4)
                ###Find nearest point in Satellite data
                b2=nearest.space.time(data, nis.1.coords,radius=radius,lag=lag)
            }   
            ## Now the eReefs reanalysis data
            load(paste0('eReefs/data/indexed/data.idx_',m,'__',r,'___',w,'.RData'))
            data=data.idx %>% dplyr:::select(-Binary,-MAMP,-fMAMP,-fsMAMP,-fsMAMP4)
            b3=nearest.space.time(data, nis.1.coords,radius=radius,lag=lag)
            ## Finally, the eReefs control data
            load(paste0('eReefs926/data/indexed/data.idx_',m,'__',r,'___',w,'.RData'))
            data=data.idx %>% dplyr:::select(-Binary,-MAMP,-fMAMP,-fsMAMP,-fsMAMP4)
            b4=nearest.space.time(data, nis.1.coords,radius=radius,lag=lag)
            if (length(list.files(path='data/indexed', pattern=paste0('data.idx_',m,'__',r,'___',w,'.RData')))>0) {
                comp.df = rbind(data.frame(Source='Satellite',b2),data.frame(Source='eReefs',b3),data.frame(Source='eReefs926',b4))
            } else {
                comp.df = bind_rows(data.frame(Source='Satellite'),data.frame(Source='eReefs',b3),data.frame(Source='eReefs926',b4))
                }
            comp.df=comp.df %>% group_by(Source,Measure,Date,reef.alias) %>%
                summarize(Value=mean(Value,na.rm=TRUE), GL=mean(GL,na.rm=TRUE))
            save(comp.df, file=paste0('data/comparisons/comp.full.df_',m,'__',r,'____',w,'.RData'))
        }
    }
}

fs = list.files(path='data/comparisons', pattern='comp.full.df\\_[a-zA-Z]{2,3}.*', full.names = TRUE)
fs
comp.full.df = NULL
for (f in fs) {
    load(f)
    comp.full.df = rbind(comp.full.df,comp.df)
}
save(comp.full.df, file=paste0('data/comparisons/comp.full.df.RData'))
load(file=paste0('data/comparisons/comp.full.df.RData'))


niskin.full = niskin.full %>% mutate(Date=as.Date(Date), waterYear=WQ_waterYear(Date))
flntu.full = flntu.full %>% mutate(Date=as.Date(Date), waterYear=WQ_waterYear(Date))

forLabels = c(unique(comp.full.df$Source), unique(as.character(niskin.full$Source)), unique(as.character(flntu.full$Source)))
registerDoParallel(cores=6)

library(gtable)
library(grid)
gtable_select <- function (x, ...) 
{
    matches <- c(...)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    x
}
gtable_stack <- function(g1, g2){
    g1$grobs <- c(g1$grobs, g2$grobs)
    g1$layout <- transform(g1$layout, z= z-max(z), name="g2")
    g1$layout <- rbind(g1$layout, g2$layout)
    g1
}

measures = read.table('parameters/measures.txt', strip.white=TRUE, sep=';', header=TRUE)

foreach(m = unique(niskin.full$Measure)) %dopar% {
    print(m)
#for (m in unique(niskin.full$Measure)) {
    for (yscale in c('natural','log')) {
        print(yscale)
        unitsLabel = (measures %>% filter(Measure==as.character(m)))$UnitsLabel
        comp.full.df.f = comp.full.df %>% filter(Measure==m) %>% #filter(Source %in% c('Niskin','FLNTU','eReefs')) %>%
            left_join(niskin.full %>% select(Latitude=LATITUDE,reef.alias,Region,WaterBody=waterBody) %>% distinct()) %>%
            arrange(desc(Latitude)) %>%
            mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias)))
        border.color=comp.full.df.f %>% ungroup %>% dplyr:::select(WaterBody,reef.alias) %>% distinct()
        border.color$border.color = ifelse(border.color$WaterBody=='Enclosed Coastal', '#ff000050',
                                    ifelse(border.color$WaterBody=='Open Coastal','#00ff0050','#0000ff50'))
        border.color$Date=NULL
        dummy = ggplot(border.color, aes(y=NULL, x=NULL)) +
            facet_wrap(~reef.alias, ncol=5) +
            geom_rect(aes(xmin=0, xmax=1, ymin=0,ymax=1, fill=border.color)) +
            scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
            scale_fill_identity()+
            theme_minimal()  + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())      
        gg=ggplot(comp.full.df.f, aes(y=Value,x=Date)) +
 #           geom_blank(aes(x=as.numeric(format(Date,'%Y')))) +
            geom_hline(aes(yintercept=GL), linetype='dashed') +
            geom_line(aes(color=Source)) +
            geom_line(data=flntu.full %>% filter(Measure==as.character(m)), aes(y=Value, x=as.Date(Date),color=Source)) +
            geom_point(data=niskin.full %>% filter(Measure==m), aes(y=Value, x=as.Date(Date), color=Source)) +
            geom_line(data=niskin.full %>% filter(Measure==m), aes(y=Value, x=as.Date(Date),color=Source)) +
            facet_wrap(~reef.alias, ncol=5) +
                                        #facet_grid(Region~WaterBody,space='free') +
            scale_x_date('') +
            scale_color_brewer('Source',type='qual',palette='Set1', limits=forLabels) + #[c(2,4,5)]) +
            #annotate(geom='rect', ymin=-Inf,ymax=Inf,xmin=-Inf,xmax=Inf, fill=NA,color=border.color$border.color) +
            ggtitle(paste0('Measure=',unitsLabel,', Spatial Radius=',radius,'km')) +
            theme_bw() +
     theme(legend.position=c(1,0), legend.justification=c(1,0),strip.background=element_blank(),
           legend.key=element_blank())
        if (yscale=='natural')  {
            gg=gg + scale_y_continuous('Observations') + ylim(0,mean(comp.full.df.f$GL)*10)
        } else {
            gg= gg+ scale_y_log10('Observations')
        }
        
        
        g1 <- ggplotGrob(gg)
        g2 <- ggplotGrob(dummy)
       
        panels <- grepl(pattern="panel", g2$layout$name)
        strips <- grepl(pattern="strip_t", g2$layout$name)
        g2$layout$t[panels] <- g2$layout$t[panels] - 1
        g2$layout$b[panels] <- g2$layout$b[panels] - 1

        new_strips <- gtable_select(g2, panels | strips)
        new_plot = gtable_stack(g1,new_strips)
        ggsave(new_plot, file=paste0('Report/Figures/',m,'_eReefs_vs_Satellite_vs_Niskin_.Radius_',radius,'_',yscale,'.pdf'), width=18, height=12, device=cairo_pdf)
        ggsave(new_plot, file=paste0('Report/Figures/',m,'_eReefs_vs_Satellite_vs_Niskin_.Radius_',radius,'_',yscale,'.png'), width=18, height=12,dpi=300)
        print(m)
    }
}


## Calculated the indicies on the 5km Satellite and eReefs
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

comp.full.idx = comp.full.df %>% ungroup %>% select(-GL) %>%
    rbind(niskin.full %>% select(Source, Measure,Date,reef.alias,Value)) %>%
    rbind(flntu.full %>% select(Source, Measure,Date,reef.alias,Value)) %>%
    left_join(wq.guidelines) %>%
    mutate(Binary=WQ_WQI_binary(.),
           MAMP=WQ_WQI_mamp(.,capped=FALSE,scaled=FALSE),
           fMAMP=WQ_WQI_mamp(.,capped=TRUE, scaled=FALSE),
           fsMAMP=WQ_WQI_mamp(.),
           fsMAMP4=WQ_WQI_mamp(.,fold=4)) %>% rename(Raw=Value) %>%
    gather(key=Index, value=Value, Binary:fsMAMP4)
## Region/Waterbody basis
comp.full.idx.av = comp.full.idx %>% mutate(waterYear = WQ_waterYear(Date)) %>%
    group_by(Source,Region,WaterBody,Measure,waterYear,Index) %>%
    summarize(Mean=mean(Value,na.rm=TRUE))
comp.full.idx.av = comp.full.idx.av %>% ungroup %>% droplevels %>%  filter(!is.na(Measure)) %>% mutate(WaterBody=factor(WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf')))

## Mark the first and last years of Satellite, eReefs and eReefs926 as an end
comp.full.idx.av = comp.full.idx.av %>% group_by(Source) %>%
    mutate(Complete = ifelse(Source %in% c('Satellite','eReefs','eReefs926') & (waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
    ungroup


#integer_breaks = function(x) seq(floor(min(x)), ceiling(max(x)))
#integer_breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))
#integer_breaks = function(x) pretty(x)

for (measure in unique(comp.full.idx.av$Measure)) {
    unitLabel = (measures %>% filter(Measure==measure))$Label
    index='fsMAMP'
    gg=ggplot(comp.full.idx.av %>% filter(Measure==measure,Index==index, Complete=='Full'), aes(y=Mean, x=waterYear)) +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
        annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
        annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
        geom_line(aes(group=Source)) +
        geom_point(aes(fill=Source), shape=21, color='black', size=3) +
        scale_x_continuous('Water year',breaks=pretty) +
        scale_y_continuous('Water Quality Index') +
        scale_fill_brewer('Source\n',type='qual',palette='Set1', limits=forLabels) +
                                        #scale_fill_manual('Source',breaks=c('chl','nap','sd'), values=c('green','red','blue')) +
        facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
        theme(strip.background=element_blank(), text=element_text(size=15),legend.key=element_blank()) +
        ggtitle(paste0('Measure=',unitLabel,', Index=',index)) +
        guides(fill=guide_legend(override.aes=list(size=5)))
    ggsave(file=paste0('data/comparisons/comp.idx.',measure,'_',index,'.pdf'),gg,width=15, height=7.5, units='in', device=cairo_pdf)
    ggsave(file=paste0('data/comparisons/comp.idx.',measure,'_',index,'.png'),gg,width=15, height=7.5, units='in',dpi=300)
}

## Now try with monthly aggregation
## Region/Waterbody basis
comp.full.idx.av = comp.full.idx %>% mutate(waterYear = WQ_waterYear(Date), month=format(Date,'%m')) %>%
    group_by(Source,Region,WaterBody,Measure,waterYear,month,Index) %>%
    summarize(Mean=mean(Value,na.rm=TRUE))
comp.full.idx.av = comp.full.idx.av %>% ungroup %>% droplevels %>%  filter(!is.na(Measure)) %>% mutate(WaterBody=factor(WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf')))

#comp.full.idx.av = comp.full.idx.av %>% group_by(Source) %>%
#    mutate(Complete = ifelse(Source %in% c('Satellite','eReefs','eReefs926') & (waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
#    ungroup

for (measure in unique(comp.full.idx.av$Measure)) {
    unitLabel = (measures %>% filter(Measure==measure))$Label
    index='fsMAMP'
    gg=ggplot(comp.full.idx.av %>% filter(Measure==measure,Index==index),
                  aes(y=Mean, x=as.Date(paste0(waterYear,'-',month,'-15')))) +
           geom_line(aes(color=Source)) + #+ geom_point(aes(fill=Source), shape=21, color='black', size=3) +
           scale_x_date('Water year') +
           scale_y_continuous('Water Quality Index') +
                                        #scale_fill_manual('Source',breaks=c('chl','nap','sd'), values=c('green','red','blue')) +
        facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
        scale_color_brewer('Source\n',type='qual',palette='Set1', limits=forLabels) +
        theme(strip.background=element_blank(), text=element_text(size=15),legend.key=element_blank()) +
               ggtitle(paste0('Measure=',unitLabel,', Index=',index))
    ggsave(file=paste0('data/comparisons/comp.idx.month.',measure,'_',index,'.pdf'),gg,width=15, height=7.5, units='in',device=cairo_pdf)
    ggsave(file=paste0('data/comparisons/comp.idx.month.',measure,'_',index,'.png'),gg,width=15, height=7.5, units='in',dpi=300)
}


## Reef basis
comp.full.idx.av = comp.full.idx %>% mutate(waterYear = WQ_waterYear(Date)) %>%
    group_by(Source,reef.alias,Measure,waterYear,Index) %>%
    summarize(Mean=mean(Value,na.rm=TRUE)) %>%
    left_join(niskin.full %>% select(LATITUDE,reef.alias) %>% group_by(reef.alias) %>% summarize(Latitude=mean(LATITUDE,na.rm=TRUE)) %>% ungroup) %>%
    ungroup %>% arrange(desc(Latitude)) %>% mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias))) %>% filter(!is.na(reef.alias))

for (measure in unique(comp.full.idx.av$Measure)) {
    unitLabel = (measures %>% filter(Measure==measure))$Label
    index='fsMAMP'
    border.color = niskin.full %>% select(Latitude=LATITUDE,reef.alias,Region,WaterBody=waterBody) %>% distinct() %>%
        arrange(desc(Latitude)) %>%
        mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias)))
    border.color=border.color %>% ungroup %>% dplyr:::select(WaterBody,reef.alias) %>% distinct()
    border.color$border.color = ifelse(border.color$WaterBody=='Enclosed Coastal', '#ff000050',
                                ifelse(border.color$WaterBody=='Open Coastal','#00ff0050','#0000ff50'))
    border.color$Date=NULL
    dummy = ggplot(border.color, aes(y=NULL, x=NULL)) +
            facet_wrap(~reef.alias) +
            geom_rect(aes(xmin=0, xmax=1, ymin=0,ymax=1, fill=border.color)) +
            scale_fill_identity()+
            theme_minimal() + theme(text=element_text(size=15))        
    gg=ggplot(comp.full.idx.av %>% filter(Measure==measure,Index==index), aes(y=Mean, x=waterYear)) +
           annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.8,ymax=1, fill='#00734D10') +
           annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.6,ymax=0.8, fill='#B0D23510') +
           annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.4,ymax=0.6, fill='#F0C91810') +
           annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0.2,ymax=0.4, fill='#F4772110') +
           annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=0,ymax=0.2, fill='#ED1C2410') +
           annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
           annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
           geom_line(aes(group=Source)) + geom_point(aes(fill=Source), shape=21, color='black', size=3) +
           scale_x_continuous('Water year') +
           scale_y_continuous('Water Quality Index') +
                                        #scale_fill_manual('Source',breaks=c('chl','nap','sd'), values=c('green','red','blue')) +
               facet_wrap(~reef.alias, scales='free') + theme_bw() +
               theme(strip.background=element_blank(), text=element_text(size=15),legend.key=element_blank(),
                     legend.position=c(1,0), legend.justification=c(1,0)) +
                      scale_fill_brewer('Source\n',type='qual',palette='Set1', limits=forLabels) +
               ggtitle(paste0('Measure=',unitLabel,', Index=',index)) +
               guides(fill=guide_legend(override.aes=list(size=5)))
    g1 <- ggplotGrob(gg)
    g2 <- ggplotGrob(dummy)
    
    panels <- grepl(pattern="panel", g2$layout$name)
    strips <- grepl(pattern="strip_t", g2$layout$name)
    g2$layout$t[panels] <- g2$layout$t[panels] - 1
    g2$layout$b[panels] <- g2$layout$b[panels] - 1
    
    new_strips <- gtable_select(g2, panels | strips)
    new_plot = gtable_stack(g1,new_strips)
    ggsave(file=paste0('data/comparisons/comp.idx.reef.',measure,'_',index,'.pdf'),new_plot,width=20, height=20, units='in')
    ggsave(file=paste0('data/comparisons/comp.idx.reef.',measure,'_',index,'.png'),new_plot,width=20, height=20, units='in',dpi=300)
}

## Now try with monthly aggregation
## Region/Waterbody basis
comp.full.idx.av = comp.full.idx %>% mutate(waterYear = WQ_waterYear(Date), month=format(Date,'%m')) %>%
    group_by(Source,reef.alias,Measure,waterYear,month,Index) %>%
    summarize(Mean=mean(Value,na.rm=TRUE)) %>%
    left_join(niskin.full %>% select(LATITUDE,reef.alias) %>% group_by(reef.alias) %>% summarize(Latitude=mean(LATITUDE,na.rm=TRUE)) %>% ungroup) %>%
    ungroup %>% arrange(desc(Latitude)) %>% mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias))) %>% filter(!is.na(reef.alias))

for (measure in unique(comp.full.idx.av$Measure)) {
    index='fsMAMP'
    unitLabel = (measures %>% filter(Measure==measure))$Label
    border.color = niskin.full %>% select(Latitude=LATITUDE,reef.alias,Region,WaterBody=waterBody) %>% distinct() %>%
        arrange(desc(Latitude)) %>%
        mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias)))
    border.color=border.color %>% ungroup %>% dplyr:::select(WaterBody,reef.alias) %>% distinct()
    border.color$border.color = ifelse(border.color$WaterBody=='Enclosed Coastal', '#ff000050',
                                ifelse(border.color$WaterBody=='Open Coastal','#00ff0050','#0000ff50'))
    border.color$Date=NULL
    dummy = ggplot(border.color, aes(y=NULL, x=NULL)) +
        facet_wrap(~reef.alias) +
        geom_rect(aes(xmin=0, xmax=1, ymin=0,ymax=1, fill=border.color)) +
        scale_fill_identity()+
        theme_minimal() + theme(text=element_text(size=15))        
    gg=ggplot(comp.full.idx.av %>% filter(Measure==measure,Index==index),
              aes(y=Mean, x=as.Date(paste0(waterYear,'-',month,'-15')))) +
        geom_line(aes(color=Source)) + #+ geom_point(aes(fill=Source), shape=21, color='black', size=3) +
        scale_x_date('Water year') +
        scale_y_continuous('Water Quality Index') +
                                        #scale_fill_manual('Source',breaks=c('chl','nap','sd'), values=c('green','red','blue')) +
        facet_wrap(~reef.alias, scales='free') + theme_bw()+
        theme(strip.background=element_blank(), text=element_text(size=15),legend.key=element_blank(),
                     legend.position=c(1,0), legend.justification=c(1,0)) +
        scale_color_brewer('Source',type='qual',palette='Set1', limits=forLabels) +
        ggtitle(paste0('Measure=',unitLabel,', Index=',index)) +
        guides(color=guide_legend(override.aes=list(size=3)))
    g1 <- ggplotGrob(gg)
    g2 <- ggplotGrob(dummy)
    
    panels <- grepl(pattern="panel", g2$layout$name)
    strips <- grepl(pattern="strip_t", g2$layout$name)
    g2$layout$t[panels] <- g2$layout$t[panels] - 1
    g2$layout$b[panels] <- g2$layout$b[panels] - 1
    
    new_strips <- gtable_select(g2, panels | strips)
    new_plot = gtable_stack(g1,new_strips)
    
    ggsave(file=paste0('data/comparisons/comp.idx.reef.month.',measure,'_',index,'.pdf'),new_plot, width=20, height=10, units='in', device=cairo_pdf)
    ggsave(file=paste0('data/comparisons/comp.idx.reef.month.',measure,'_',index,'.png'),new_plot, width=20, height=10, units='in',dpi=300)
}


if (1==2) {
## For each region/waterbody and measure, find the nearest (in space)
## location to compare annual satellite and discrete samples.
comp.df=NULL
comp.coords = NULL
registerDoParallel(cores=6)
if (1==2) {
for (m in unique(niskin.full$Measure)) {
    foreach(r=unique(niskin.full$Region)) %dopar% {
    #for (r in unique(niskin.full$Region)) {
        for (w in unique(niskin.full$waterBody)) {
            nis = niskin.full %>% filter(Measure==m, Region==r, waterBody==w)
            nis.coords = nis %>%
                dplyr:::select(reef.alias, Longitude=LONGITUDE, Latitude=LATITUDE) %>%
                group_by(reef.alias) %>% summarize(Latitude=mean(Latitude,na.rm=TRUE), Longitude=mean(Longitude,na.rm=TRUE))
            if (nrow(nis)==0) next
            load(paste0('data/indexed/data.idx_',m,'__',r,'___',w,'.RData'))
            print(m)
            print(r)
            print(w)
            for (radius in c(1:3)) {
                #Find nearest point in Satellite data
                a=data.idx %>% distinct(Latitude,Longitude) %>% mutate(Date=as.Date('2000-01-01')) %>%
                    getWaypoints(nis.coords, radius)
                comp.coords=rbind(comp.coords,
                                  a %>% dplyr:::select(Latitude,Longitude,reef.alias) %>%
                                  left_join(nis.coords %>%
                                            dplyr:::select(Ref.Latitude=Latitude, Ref.Longitude=Longitude, reef.alias) %>%
                                            group_by(reef.alias) %>% summarize(Ref.Latitude=mean(Ref.Latitude), Ref.Longitude=mean(Ref.Longitude))) %>%
                                  data.frame(Measure=m,Region=r,waterBody=w,Radius=radius)
                                  )
                ## Apply these locations to the full satellite
                ## temporal series
                ## Calculate the annual mean
                b = data.idx %>%
                    right_join(a %>% dplyr:::select(Latitude,Longitude,reef.alias)) %>%
                    group_by(Latitude,Longitude,reef.alias,waterYear) %>% summarize(Sat=mean(Value, na.rm=TRUE)) %>%
                    ungroup %>% group_by(reef.alias,waterYear) %>% summarize(Sat=mean(Sat,na.rm=TRUE))
                ## Calculate the annual niskin mean
                b1=nis %>% rename(Niskin=Value) %>%
                    group_by(reef.alias,waterYear) %>% summarize(Niskin=mean(Niskin, na.rm=TRUE))
                ## Join the satellite and niskin annual means together
                b2 = full_join(b,b1)
                ## Bind into running data set
                comp.df=rbind(comp.df,b2 %>% gather(Source, Value, Sat, Niskin) %>% data.frame(Measure=m,Region=r,waterBody=w,Radius=radius))
            }
        }
    }
}


save(comp.coords, file='data/comparisons/comp.coords.RData')
save(comp.df, file='data/comparisons/comp.df.RData')

}

}



## Now we need to find nearest in space and time for each of the Satellite and eReefs data...
registerDoParallel(cores=10)
comp.df=NULL
comp.coords = NULL
radius=10
lag=10
for (m in unique(niskin.full$Measure)) {
    foreach(r=unique(niskin.full$Region)) %dopar% {
#    for (r in unique(niskin.full$Region)) {
        for (w in unique(niskin.full$waterBody)) {
            nis = niskin.full %>% filter(Measure==m, Region==r, waterBody==w)
            nis.1.coords = nis %>%
                dplyr:::select(reef.alias, Longitude=LONGITUDE, Latitude=LATITUDE,Date=Date, Value) %>%
                mutate(Date=as.Date(Date))
            if (nrow(nis)==0) next
            ## Start with the Satellite data
            load(paste0('data/indexed/data.idx_',m,'__',r,'___',w,'.RData'))            
            data=data.idx %>% dplyr:::select(-Binary,-MAMP,-fMAMP,-fsMAMP,-fsMAMP4)
            ###Find nearest point in Satellite data
            b2=nearest.space.time(data, nis.1.coords,radius=radius,lag=lag)

            load(paste0('eReefs/data/indexed/data.idx_',m,'__',r,'___',w,'.RData'))
            data=data.idx %>% dplyr:::select(-Binary,-MAMP,-fMAMP,-fsMAMP,-fsMAMP4)
            b3=nearest.space.time(data, nis.1.coords,radius=radius,lag=lag)
            comp.df = rbind(data.frame(Source='Satellite',b2),data.frame(Source='eReefs',b3))
            save(comp.df, file=paste0('data/comparisons/comp.df_',m,'__',r,'____',w,'.RData'))
        }
    }
}



fs = list.files(path='data/comparisons', pattern='comp.df\\_[a-z]{2,3}.*', full.names = TRUE)
fs
comp.all.df = NULL
for (f in fs) {
    load(f)
    comp.all.df = rbind(comp.all.df,comp.df)
}
save(comp.all.df, file=paste0('data/comparisons/comp.all.df.RData'))
load(file=paste0('data/comparisons/comp.all.df.RData'))


## Now some plots illustrating the spatial location of Satellite and eReefs observations relative to the Niskin samples
## THIS ONLY REQUIRES THE CHL data
ggplotColors = function(n=6,h=c(0,360)+15) {
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h=(seq(h[1],h[2], length=n)),c=100,l=65)
    }
load('data/GIS/layers.RData')
load('data/GIS/Polys.RData')
qld.df=tidy(qld)
reefs.df=tidy(reefs)
Polys.df=tidy(Polys)
gg = list()
comp.all.df = comp.all.df %>% arrange(desc(Latitude),Longitude)
for (s in unique(comp.all.df$Source)) {
    pb <- txtProgressBar(max=length(unique(comp.all.df$reef.alias)), style=3)
    for (i in 1:length(unique(comp.all.df$reef.alias))) {
                                        #   gdata = comp.df %>% filter(reef.alias==unique(comp.df$reef.alias)[i],Radius %in% c(0,1,2,3), Lag==0,Measure=='chl') %>%
                                        #       mutate(Radius = factor(Radius,levels=c(3,2,1,0))) %>% arrange(Radius) %>% droplevels
        gdata = comp.all.df %>% filter(Source==s,reef.alias==unique(comp.all.df$reef.alias)[i],cdistSpace <radius,cdistTime<2,Measure=='chl', !is.na(Value)) %>% droplevels %>%
            mutate(cdistSpace=factor(cdistSpace,levels=rev(sort(unique(cdistSpace))))) %>% arrange(cdistSpace) %>% droplevels
        if (nrow(gdata)<1) next
        xl=range(c(gdata$Longitude,gdata$Niskin.Longitude))
        yl=range(c(gdata$Latitude,gdata$Niskin.Latitude))
        (d = d_longlat(mean(gdata$Niskin.Longitude),mean(gdata$Niskin.Latitude), d=radius, angle=c(0,90,180,270)))
        (xl=range(d[,2]) + c(-0.01,0.01))
        (yl=range(d[,1]) + c(-0.01,0.01))
        
        print(i)
        gc=ggplotColors(4)
        cols = c('1'=gc[1],'2'=gc[2],'3'=gc[3],'4'=gc[4],'Niskin'='black','Zone'='bisque3')
        br = c('<1km','<2km','<3km','<4km','AIMS MMP site','Region/Water body boundary')
        border.color = ifelse(unique(gdata$WaterBody)=='Enclosed Coastal', 'red', ifelse(unique(gdata$WaterBody)=='Open Coastal','green','blue'))
        gg[[i]] = gdata %>%
            ggplot(aes(y=Latitude,x=Longitude)) +
            geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group,color='Zone'), fill=NA) +
            geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black') +
            geom_polygon(data=reefs.df, aes(y=lat, x=long, group=group), fill='lightblue', color='blue') +
                                        #geom_tile(aes(fill=cdistSpace),alpha=0.1) +
            geom_point(aes(color=factor(cdistSpace)),show.legend=TRUE) +
                                        #       geom_text(aes(label=factor(cdistSpace)),show.legend=FALSE) +
            geom_point(aes(y=Niskin.Latitude,x=Niskin.Longitude,color='Niskin')) +
            geom_segment(aes(yend=Niskin.Latitude,xend=Niskin.Longitude,color=factor(cdistSpace)), alpha=0.1, show.legend=TRUE) +
            geom_point(aes(y=Niskin.Latitude,x=Niskin.Longitude), color='black')  +
            ggtitle(unique(comp.all.df$reef.alias)[i]) +
            scale_color_manual('', values=cols,labels=br) +
            coord_equal(ylim=yl, xlim=xl) + theme_classic() +
            theme(panel.border=element_rect(fill=NA,color=border.color, size=1),legend.position="none",
                  panel.margin=unit(c(0,0,0,0), 'pt'), plot.margin=unit(c(0,0,0,0), 'pt'),
                  axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank())
        setTxtProgressBar(pb,i)
#        ggsave(gg[[i]], file='Report/Figures/junk.pdf', width=10,height=10)
    }
    close(pb)
#    g=gg[[i]] + geom_segment(aes(yend=Niskin.Latitude,xend=Niskin.Longitude,color=factor(cdistSpace)), alpha=0.1, show.legend=TRUE)
     g=gg[[i]] +theme(legend.position=c(0.5,0.5)) #+ guides(color='legend')
    #gg[[i+1]] = gg[[i]]
    get_legend = function(g) {
        tmp=ggplot_gtable(ggplot_build(g))
        leg = which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
        legend = tmp$grobs[[leg]]
        return(legend)
    }
    gg[[i+1]]=get_legend(g)
    ggsave(do.call(grid.arrange, gg),
           file=paste0('Report/Figures/',s,'_vs_Niskin_locations_',radius,'km.pdf'), width=10, height=12)
}


## Now how about some time series for each reef - start with

## Start with non-overlapping spatial and temporal distances.  That is, a spatial distance of 10
## represents all observations within 9.5-10.5km
### some preliminaries for the long tables
## RMSE - root mean square error.
##      - standard deviation of prediction error
##      - smaller = better
##      - influenced by outliers
## MAE  - mean absolute error
##      - more robust
##      - smaller = better
## MAPE - mean percentage absolute error
##      - mean of (absolute error expressed as percentage of Niskin values)
##      - smaller = better
## NSE  - Nash-Sutcliffe efficiency between simulated/modelled and observed (Niskin)
##      - relative magnitude of noise (resid) compared to variance in observed (signal)
##      - measure of how well slope of 1 approximates relationship between simulated and observed
##      - NSE=1 (perfect match)
##      - NSE=0 (simulations only as good as the mean of obs at predicting obs)
##      - -Inf < NSE < 0 (mean of obs better at predicting obs that simulations)
## E    - Coefficient of Efficiency (Legates and McCabe 1999)
##      - same as NSE, except that j=1 intead of 2
##      - when j=1, more robust to outliers
##      - E=1 (perfect match)
##      - E=0 (simulations are only as good as the mean of obs at predicting obs)
##      - E=0 (simulations only as good as the mean of obs at predicting obs)      
## d    - Willmott Index of Aggrement
##      - standardized measure of the degree of prediction error
##      - d=1 (perfect match)
##      - d=0 (absolutely no aggrement)
##      - overly sensitive to outliers
addtorow = list()
addtorow$pos = list()
addtorow$pos[1] = c(0)
addtorow$command = c(paste0("\\toprule \n",
                            "\\endfirsthead \n",
                            "\\multicolumn{5}{l}{\\textit{Continued from previous page ...}}\\\\ \n",
                            "\\midrule \n",
                            "\\endhead \n",
                           "\\bottomrule \n",
                           "\\multicolumn{5}{l}{\\textit{Continued on next page ...}} \n",
                           "\\endfoot \n",
                           "\\bottomrule \n",
                           "\\endlastfoot \n"))

library(nlme)
comp.all.sum=comp.all.df %>% group_by(Measure,Source,cdistSpace,cdistTime,reef.alias, waterYear,Niskin.Date) %>%
    summarize(Value=mean(Value,na.rm=TRUE), Niskin=mean(Niskin, na.rm=TRUE)) %>%
    group_by(Measure,Source,cdistSpace,cdistTime) %>%
    do({
        x=.
        x= x %>% ungroup %>% filter(!is.na(Value),!is.na(Niskin),!is.infinite(Value),!is.infinite(Niskin))
        x %>% select(Measure,Source,cdistSpace,cdistTime) %>% distinct %>% print
        s = nlme:::summary.lme(l<-nlme:::lme(fixed=Value~Niskin, random=~1|reef.alias, data=x,na.action=na.omit))
        s=t(s$tTable[2,])
        r2=R2(l)
        xx = x %>% select(Value,Niskin)
        r=rmse(xx)
        m=mae(xx)
        p=mpe(xx)
        #x %>% select(Measure,Source,cdistSpace,cdistTime,Value,Niskin) %>% rename(S=Value,O=Niskin) %>% print
        nse=NSE(xx)
        e=E(xx)
        d = Willmott_d(xx)
        data.frame(nObs=nrow(x),RMSE=r[[1]],RMSE.p=r[[2]],MAE=m[[1]],MAE.p=m[[2]],
                   MAPE=p[[1]],MAPE.p=p[[2]],NSE=nse[[1]],NSE.p=nse[[2]],
                   E=e[[1]],E.p=e[[2]],d=d[[1]],d.p=d[[2]],
                   s,residual.RMSE=rmse_1(nlme:::residuals.lme(l)),residual.MAE=mae_1(nlme:::residuals.lme(l)),R2.marginal=r2[1],R2.conditional=r2[4])
    }) %>% ungroup
comp.all.sum %>% arrange(Measure,Source,cdistSpace,cdistTime) %>% as.data.frame %>%
    xtable(label='tab:comp.all.sum',caption='AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) for spatial/temporal lags.  Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.sum.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='tiny',tabular.environment='longtable',booktabs=TRUE,add.to.row=addtorow,hline.after=c(-1))
#Top 5 versions
comp.all.sum %>% group_by(Measure,Source) %>% arrange(MAPE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.mpe.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAPE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.sum_mpe.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
## comp.all.sum %>% group_by(Measure,Source) %>% arrange(MAPE) %>% slice(1:5) %>% as.data.frame %>%
##     xtable(label='tab:comp.all.mpe.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAPE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
##     print(file='Report/Tables/comp.all.sum_mpe.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='tiny',tabular.environment='longtable',booktabs=TRUE,add.to.row=addtorow,hline.after=c(-1,10,20))
comp.all.sum %>% group_by(Measure,Source) %>% arrange(RMSE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.rmse.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on RMSE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.sum_rmse.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.sum %>% group_by(Measure,Source) %>% arrange(MAE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.mae.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.sum_mae.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
                                        #comp.all.sum %>% group_by(Measure,Source) %>% arrange(R2.marginal) %>% slice(1:3) %>% as.data.frame
comp.all.sum %>% group_by(Measure,Source) %>% arrange(desc(NSE)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.nse.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.sum_nse.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.sum %>% group_by(Measure,Source) %>% arrange(desc(E)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.e.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.sum_e.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.sum %>% group_by(Measure,Source) %>% arrange(desc(d)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.d.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.sum_d.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))


c1=comp.all.df %>%
    arrange(desc(Niskin.Latitude), Niskin.Longitude) %>%
    mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias))) %>%
    group_by(Measure,Source,Region,WaterBody,cdistSpace,cdistTime,reef.alias,waterYear,Niskin.Date) %>% summarize_at(vars(Value,Niskin,GL),funs(median(.,na.rm=TRUE))) %>% ungroup

## Now scatterplots
### Discrete radii and lags
library(gtable)
library(grid)
gtable_select <- function (x, ...) 
{
    matches <- c(...)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    x
}
gtable_stack <- function(g1, g2){
    g1$grobs <- c(g1$grobs, g2$grobs)
    g1$layout <- transform(g1$layout, z= z-max(z), name="g2")
    g1$layout <- rbind(g1$layout, g2$layout)
    g1
}

gg=list()
i=0
pb <- txtProgressBar(max=length(unique(c1$cdistSpace))*length(unique(c1$cdistTime)), style=3)
for (m in unique(c1$Measure)) {
    for (r in unique(c1$cdistSpace)) {
        for (l in unique(c1$cdistTime)) {
            i = i+1
            c1.dat = c1 %>% filter(cdistSpace==r, cdistTime==l, Measure==m)
            if (nrow(c1.dat)<1) {
                i=i-1
                next
            }
            border.color=c1.dat %>% ungroup %>% dplyr:::select(WaterBody,reef.alias) %>% distinct()
            border.color$border.color = ifelse(border.color$WaterBody=='Enclosed Coastal', '#ff000050',
                                        ifelse(border.color$WaterBody=='Open Coastal','#00ff0050','#0000ff50'))
            border.color$Date=NULL
            dummy = ggplot(border.color, aes(y=NULL, x=NULL)) +
                facet_wrap(~reef.alias, ncol=5) +
                geom_rect(aes(xmin=0, xmax=1, ymin=0,ymax=1, fill=border.color)) +
                scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
                scale_fill_identity()+
                theme_minimal() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())   
            gg[[i]] = c1.dat %>%
                ggplot(aes(y=Value,x=Niskin)) +
                geom_hline(aes(yintercept=GL), linetype='dashed', alpha=0.5) +
                geom_vline(aes(xintercept=GL),linetype='dashed', alpha=0.5) +
                geom_point(aes(color=Source), alpha=0.3, size=1) +
                facet_wrap(~reef.alias,ncol=5) +
                scale_x_log10('Niskin observations (over time)') + scale_y_log10(paste0('Grid observations (over time)')) +
                geom_smooth(method='lm', aes(color=Source),se=FALSE) +
                ggtitle(paste0('Measure=',m,', Spatial Radius=',r,' (km), Temporal Lag=',l,' (days)')) +
                theme_bw() + #+ coord_cartesian(xlim=c(0.1,10), ylim=c(0.1,10))
                theme(legend.position=c(1,0), legend.justification=c(1,0),strip.background=element_blank(),legend.key=element_blank())
            g1=ggplotGrob(gg[[i]])
            g2=ggplotGrob(dummy)
            panels <- grepl(pattern="panel", g2$layout$name)
            strips <- grepl(pattern="strip_t", g2$layout$name)
            g2$layout$t[panels] <- g2$layout$t[panels] - 1
            g2$layout$b[panels] <- g2$layout$b[panels] - 1
            
            new_strips <- gtable_select(g2, panels | strips)
            new_plot = gtable_stack(g1,new_strips)
            ggsave(new_plot, file=paste0('Report/Figures/',m,'_grid_vs_Niskin_scatter.Radius_',r,'_Lag_',l,'.pdf'), width=10, height=10)
            ggsave(new_plot, file=paste0('Report/Figures/',m,'_grid_vs_Niskin_scatter.Radius_',r,'_Lag_',l,'.png'), width=10, height=10, units='in', dpi=300)
            setTxtProgressBar(pb,i)
        }
    } 
    close(pb)
}


## Now do the time series associated with the descrete distance/times
#Time series for observations within a specific space/time distance band from the discrete samples.
i=0
c2=c1 %>% ungroup %>% spread(key=Source,value=Value) %>% gather(key=Source,value=Value, Niskin,Satellite,eReefs)
#c2=c1 %>% ungroup %>% gather(key=Source,value=Value, Value,Niskin)
pb <- txtProgressBar(max=length(unique(c2$cdistSpace))*length(unique(c2$cdistTime))*length(unique(c2$Measure)), style=3)
for (r in unique(c2$cdistSpace)) {
    for (l in unique(c2$cdistTime)) {
        for (m in unique(c2$Measure)) {
            i = i+1
            border.color=c2 %>% ungroup %>% dplyr:::select(WaterBody,reef.alias) %>% distinct()
            border.color$border.color = ifelse(border.color$WaterBody=='Enclosed Coastal', '#ff000050',
                                        ifelse(border.color$WaterBody=='Open Coastal','#00ff0050','#0000ff50'))
            border.color$Date=NULL
            dummy = ggplot(border.color, aes(y=NULL, x=NULL)) +
                facet_wrap(~reef.alias, ncol=5) +
                geom_rect(aes(xmin=0, xmax=1, ymin=0,ymax=1, fill=border.color)) +
                scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
                scale_fill_identity()+
                theme_minimal() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())  
            gg[[i]] = c2 %>% filter(cdistSpace==r, cdistTime==l, Measure==m) %>%
                ggplot(aes(y=Value,x=Niskin.Date)) +
                geom_hline(aes(yintercept=GL), linetype='dashed') +
                geom_point(aes(color=Source)) +
                geom_line(aes(color=Source)) +
                facet_wrap(~reef.alias, ncol=5) +
                scale_color_manual(values=c('red','blue','green'),labels=c('eReefs','Niskin','Satellite')) +
                scale_y_log10('Observations') + scale_x_date('') +
                ggtitle(paste0('Measure=',m,', Spatial Radius=',r,' (km), Temporal Lag=',l,' (days)')) +
                theme_bw() +
                theme(legend.position=c(1,0), legend.justification=c(1,0), strip.background=element_blank(),legend.key=element_blank()) 
            g1=ggplotGrob(gg[[i]])
            g2=ggplotGrob(dummy)
            panels <- grepl(pattern="panel", g2$layout$name)
            strips <- grepl(pattern="strip_t", g2$layout$name)
            g2$layout$t[panels] <- g2$layout$t[panels] - 1
            g2$layout$b[panels] <- g2$layout$b[panels] - 1
            
            new_strips <- gtable_select(g2, panels | strips)
            new_plot = gtable_stack(g1,new_strips)           
            ggsave(new_plot, file=paste0('Report/Figures/',m,'_grid_vs_Niskin_time.Radius_',r,'_Lag_',l,'.pdf'), width=20, height=10)
            ggsave(new_plot, file=paste0('Report/Figures/',m,'_grid_vs_Niskin_time.Radius_',r,'_Lag_',l,'.png'), width=20, height=10, units='in', dpi=300)
            setTxtProgressBar(pb,i)
        }
    }
}
close(pb)




##We should consider doing this separately within each Region/Waterbody as the relationships might vary

comp.all.sum=comp.all.df %>% group_by(Measure,Source,Region,WaterBody,cdistSpace,cdistTime,reef.alias, waterYear,Niskin.Date) %>%
    summarize(Value=mean(Value,na.rm=TRUE), Niskin=mean(Niskin, na.rm=TRUE)) %>%
    group_by(Measure,Source,Region,WaterBody,cdistSpace,cdistTime) %>%
    do({
        x=.
        x= x %>% filter(!is.na(Value),!is.na(Niskin),!is.infinite(Value),!is.infinite(Niskin))
        x %>% select(Measure,Source,Region,WaterBody,cdistSpace,cdistTime) %>% distinct %>% print
        if (nrow(x)>2) {
            tag = 0
            tryCatch(l<-nlme:::lme(fixed=Value~Niskin, random=~1|reef.alias, data=x,na.action=na.omit),
                     error=function(err) tag<<-1)
            if (tag==0) {
                s = nlme:::summary.lme(l)
                s=t(s$tTable[2,])
                r2=R2(l)
                residual.RMSE=rmse_1(nlme:::residuals.lme(l))
                residual.MAE=mae_1(nlme:::residuals.lme(l))
            } else {
                s=NA
                r2=rep(NA,4)
                residual.RMSE = NA
                residual.MAE = NA
            }
        } else {
            s=NA
            r2=rep(NA,4)
            residual.RMSE = NA
            residual.MAE = NA
        }
        xx=x %>% select(Value,Niskin)
        r=rmse(xx)
        m=mae(xx)
        p=mpe(xx)
        #x %>% select(Measure,Source,cdistSpace,cdistTime,Value,Niskin) %>% rename(S=Value,O=Niskin) %>% print
        nse=NSE(xx)
        e=E(xx)
        d = Willmott_d(xx)
        data.frame(nObs=nrow(x),RMSE=r[[1]],RMSE.p=r[[2]],MAE=m[[1]],MAE.p=m[[2]],
                   MAPE=p[[1]],MAPE.p=p[[2]],NSE=nse[[1]],NSE.p=nse[[2]],
                   E=e[[1]],E.p=e[[2]],d=d[[1]],d.p=d[[2]],
                   s,residual.RMSE=residual.RMSE,residual.MAE=residual.MAE,R2.marginal=r2[1],R2.conditional=r2[4])
    }) %>% ungroup
comp.all.sum %>% arrange(Measure,Region,WaterBody,cdistSpace,cdistTime,Source) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.sum',caption='AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) for spatial/temporal lags.  Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.sum.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='tiny',tabular.environment='longtable',booktabs=TRUE,add.to.row=addtorow,hline.after=c(-1))
#Top 5 versions
comp.all.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(MAPE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.mpe.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAPE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.sum_mpe.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
## comp.all.sum %>% group_by(Measure,Source) %>% arrange(MAPE) %>% slice(1:5) %>% as.data.frame %>%
##     xtable(label='tab:comp.all.mpe.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAPE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
##     print(file='Report/Tables/comp.all.sum_mpe.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='tiny',tabular.environment='longtable',booktabs=TRUE,add.to.row=addtorow,hline.after=c(-1,10,20))
comp.all.sum %>% group_by(Measure,Source) %>% arrange(RMSE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.rmse.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on RMSE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.sum_rmse.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.sum %>% group_by(Measure,Source) %>% arrange(MAE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.mae.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.sum_mae.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
                                        #comp.all.sum %>% group_by(Measure,Source) %>% arrange(R2.marginal) %>% slice(1:3) %>% as.data.frame
comp.all.sum %>% group_by(Measure,Source) %>% arrange(desc(NSE)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.nse.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.sum_nse.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.sum %>% group_by(Measure,Source) %>% arrange(desc(E)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.e.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.sum_e.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.sum %>% group_by(Measure,Source) %>% arrange(desc(d)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.d.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.sum_d.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))





## now we should lump radius and lag into just less than instances
## It might also be worth generating similar temporal trends for points within a max distance (space and time) from the discrete samples.

i=0
gg=list()
c2=c1 %>% ungroup %>% spread(key=Source,value=Value) %>% gather(key=Source,value=Value, Niskin,Satellite,eReefs)
comp.all.max = NULL
pb <- txtProgressBar(max=length(unique(c2$cdistSpace))*length(unique(c2$cdistTime))*length(unique(c2$Measure)), style=3)
for (r in unique(c2$cdistSpace)) {
    for (l in unique(c2$cdistTime)) {
        for (m in unique(c2$Measure)) {
            i = i+1
            c2.a = c2 %>% filter(cdistSpace<=r, cdistTime<=l, Measure==m) %>% mutate(cdistSpace=r, cdistTime=l) %>%
                group_by(Source,Region,WaterBody,reef.alias,waterYear,Niskin.Date) %>%
                summarize(Value=mean(Value,na.rm=TRUE),GL=mean(GL, na.rm=TRUE))
            border.color=c2.a %>% ungroup %>% dplyr:::select(WaterBody,reef.alias) %>% distinct()
            border.color$border.color = ifelse(border.color$WaterBody=='Enclosed Coastal', '#ff000050',
                                        ifelse(border.color$WaterBody=='Open Coastal','#00ff0050','#0000ff50'))
            border.color$Date=NULL
            dummy = ggplot(border.color, aes(y=NULL, x=NULL)) +
                facet_wrap(~reef.alias, ncol=5) +
                geom_rect(aes(xmin=0, xmax=1, ymin=0,ymax=1, fill=border.color)) +
                scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
                scale_fill_identity()+
                theme_minimal() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
            ## Time series plot-----------------------------------------
            gg[[i]] = c2.a %>% 
                ggplot(aes(y=Value,x=Niskin.Date)) +
                geom_point(aes(color=Source)) +
                geom_line(aes(color=Source)) +
                facet_wrap(~reef.alias,ncol=5) +
                scale_color_manual(values=c('red','blue','green'),labels=c('eReefs','Niskin','Satellite')) +
                scale_y_log10('Observations') + scale_x_date('') +
                ggtitle(paste0('Measure=',m,', Maximum Spatial Radius=',r,' (km), Maximum Temporal Lag=',l,' (days)')) +
                theme_bw() +
                theme(legend.position=c(1,0), legend.justification=c(1,0))
            g1=ggplotGrob(gg[[i]])
            g2=ggplotGrob(dummy)
            panels <- grepl(pattern="panel", g2$layout$name)
            strips <- grepl(pattern="strip_t", g2$layout$name)
            g2$layout$t[panels] <- g2$layout$t[panels] - 1
            g2$layout$b[panels] <- g2$layout$b[panels] - 1
            
            new_strips <- gtable_select(g2, panels | strips)
            new_plot = gtable_stack(g1,new_strips)
            ggsave(new_plot, file=paste0('Report/Figures/',m,'_grid_vs_Niskin_time_max.Radius_',r,'_Lag_',l,'.pdf'), width=20, height=10)
            ggsave(new_plot, file=paste0('Report/Figures/',m,'_grid_vs_Niskin_time_max.Radius_',r,'_Lag_',l,'.png'), width=20, height=10, units='in',dpi=300)
            ## End of time series plot--------------------------------------
            c2.b = c2.a %>% ungroup %>% spread(key=Source, value=Value) %>%
                gather(key=Source, value=Value,-Region,-WaterBody,-reef.alias,-waterYear,-Niskin.Date,-Niskin,-GL) %>%
                mutate(cdistSpace=r, cdistTime=l)
            gg[[i]] = c2.b %>%
                ggplot(aes(y=Value,x=Niskin)) +
                geom_hline(aes(yintercept=GL), linetype='dashed', alpha=0.5) +
                geom_vline(aes(xintercept=GL),linetype='dashed', alpha=0.5) +
                geom_point(aes(color=Source), alpha=0.3, size=1) +
                facet_wrap(~reef.alias,ncol=5) +
                scale_x_log10('Niskin observations (over time)') + scale_y_log10(paste0('Grid observations (over time)')) +
                geom_smooth(method='lm', aes(color=Source),se=FALSE) +
                ggtitle(paste0('Measure=',m,', Spatial Radius=',r,' (km), Temporal Lag=',l,' (days)')) +
                theme_bw() + #+ coord_cartesian(xlim=c(0.1,10), ylim=c(0.1,10))
                theme(legend.position=c(1,0), legend.justification=c(1,0),strip.background=element_blank(),legend.key=element_blank())
            g1=ggplotGrob(gg[[i]])
            g2=ggplotGrob(dummy)
            panels <- grepl(pattern="panel", g2$layout$name)
            strips <- grepl(pattern="strip_t", g2$layout$name)
            g2$layout$t[panels] <- g2$layout$t[panels] - 1
            g2$layout$b[panels] <- g2$layout$b[panels] - 1
            
            new_strips <- gtable_select(g2, panels | strips)
            new_plot = gtable_stack(g1,new_strips)
            ggsave(new_plot, file=paste0('Report/Figures/',m,'_grid_vs_Niskin_scatter.max.Radius_',r,'_Lag_',l,'.pdf'), width=10, height=10)
            ggsave(new_plot, file=paste0('Report/Figures/',m,'_grid_vs_Niskin_scatter.max.Radius_',r,'_Lag_',l,'.png'), width=10, height=10, units='in', dpi=300)
            ## Compile data for stats
            comp.all.max = rbind(comp.all.max,
                                 data.frame(Measure=m,c2.b))
            setTxtProgressBar(pb,i)
        }
    }
}
close(pb)
##up to here
save(comp.all.max, file='comp.all.max.RData')
save(c1, file='c1.RData')

load(file='comp.all.max.RData')
load(file='c1.RData')

comp.all.max.sum=comp.all.max %>% group_by(Measure,Source,Region,WaterBody,cdistSpace,cdistTime,reef.alias, waterYear,Niskin.Date) %>%
    summarize(Value=mean(Value,na.rm=TRUE), Niskin=mean(Niskin, na.rm=TRUE)) %>%
    group_by(Measure,Source,Region,WaterBody,cdistSpace,cdistTime) %>%
    do({
        x=.
        x= x %>% filter(!is.na(Value),!is.na(Niskin),!is.infinite(Value),!is.infinite(Niskin))
        x %>% select(Measure,Source,Region,WaterBody,cdistSpace,cdistTime) %>% distinct %>% print
        if (nrow(x)>2) {
            tag = 0
            tryCatch(l<-nlme:::lme(fixed=Value~Niskin, random=~1|reef.alias, data=x,na.action=na.omit),
                     error=function(err) tag<<-1)
            if (tag==0) {
                s = nlme:::summary.lme(l)
                s=t(s$tTable[2,])
                r2=R2(l)
                residual.RMSE=rmse_1(nlme:::residuals.lme(l))
                residual.MAE=mae_1(nlme:::residuals.lme(l))
                Slope=s[,'Value']
                Std.Error=s[,'Std.Error']
                DF=s[,'DF']
                t=s[,'t-value']
                P=s[,'p-value']
            } else {
                Slope=NA
                Std.Error=NA
                DF=NA
                t=NA
                P=NA
                r2=rep(NA,4)
                residual.RMSE = NA
                residual.MAE = NA
            }
        } else {
            Slope=NA
            Std.Error=NA
            DF=NA
            t=NA
            P=NA
            r2=rep(NA,4)
            residual.RMSE = NA
            residual.MAE = NA
        }
        xx=x %>% select(Value,Niskin)
        r=rmse(xx)
        m=mae(xx)
        p=mpe(xx)
        #x %>% select(Measure,Source,cdistSpace,cdistTime,Value,Niskin) %>% rename(S=Value,O=Niskin) %>% print
        nse=NSE(xx)
        e=E(xx)
        d = Willmott_d(xx)
        data.frame(nObs=nrow(x),RMSE=r[[1]],RMSE.p=r[[2]],MAE=m[[1]],MAE.p=m[[2]],
                   MAPE=p[[1]],MAPE.p=p[[2]],NSE=nse[[1]],NSE.p=nse[[2]],
                   E=e[[1]],E.p=e[[2]],d=d[[1]],d.p=d[[2]],
                   Slope=Slope,Std.Error=Std.Error,DF=DF,t=t,P=P,
                   residual.RMSE=residual.RMSE,residual.MAE=residual.MAE,R2.marginal=r2[1],R2.conditional=r2[4])
    }) %>% ungroup
save(comp.all.max.sum, file='data/comparisons/comp.all.max.sum.RData')
load(file='data/comparisons/comp.all.max.sum.RData')

comp.all.max.sum %>% arrange(Measure,Region,WaterBody,cdistSpace,cdistTime,Source) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.max.sum',caption=c('AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) for spatial/temporal lags.  Dist and Lag represent spatial (km) and temporal (days) lags.','Top five ranked AIMS niskin vs Satellite/eReefs observation association metrics')) %>%
    print(file='Report/Tables/comp.all.zone.max.sum.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='tiny',tabular.environment='longtable',booktabs=TRUE,add.to.row=addtorow,hline.after=c(-1))
comp.all.max.sum %>% arrange(Measure,Region,WaterBody,cdistSpace,cdistTime,Source) %>% as.data.frame %>%
    write.csv(file='Report/Tables/comp.all.zone.max.sum.csv', quote=FALSE, row.names=FALSE)
#Top 5 versions
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(MAPE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.mpe.max.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAPE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.max.sum_mpe.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(MAPE) %>% slice(1:5) %>% as.data.frame %>%
    write.csv(file='Report/Tables/comp.all.zone.max.sum_mpe.csv', quote=FALSE, row.names=FALSE)
## comp.all.sum %>% group_by(Measure,Source) %>% arrange(MAPE) %>% slice(1:5) %>% as.data.frame %>%
##     xtable(label='tab:comp.all.mpe.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAPE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
##     print(file='Report/Tables/comp.all.sum_mpe.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='tiny',tabular.environment='longtable',booktabs=TRUE,add.to.row=addtorow,hline.after=c(-1,10,20))
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(RMSE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.rmse.max.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on RMSE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.max.sum_rmse.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(RMSE) %>% slice(1:5) %>% as.data.frame %>%
    write.csv(file='Report/Tables/comp.all.zone.max.sum_rmse.csv', quote=FALSE, row.names=FALSE)
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(MAE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.mae.max.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.max.sum_mae.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(MAE) %>% slice(1:5) %>% as.data.frame %>%
    write.csv(file='Report/Tables/comp.all.zone.max.sum_mae.csv', quote=FALSE, row.names=FALSE)
                                        #comp.all.sum %>% group_by(Measure,Source) %>% arrange(R2.marginal) %>% slice(1:3) %>% as.data.frame
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(desc(NSE)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.nse.max.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.max.sum_nse.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(desc(NSE)) %>% slice(1:5) %>% as.data.frame %>%
    write.csv(file='Report/Tables/comp.all.zone.max.sum_nse.csv', quote=FALSE, row.names=FALSE)
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(desc(E)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.e.max.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.max.sum_e.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(desc(E)) %>% slice(1:5) %>% as.data.frame %>%
    write.csv(file='Report/Tables/comp.all.zone.max.sum_e.csv', quote=FALSE, row.names=FALSE)
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(desc(d)) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.zone.d.max.sum',caption='Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.zone.max.sum_d.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.max.sum %>% group_by(Measure,Source,Region,WaterBody) %>% arrange(desc(d)) %>% slice(1:5) %>% as.data.frame %>%
    write.csv(file='Report/Tables/comp.all.zone.max.sum_d.csv', quote=FALSE, row.names=FALSE)
## The end..
## The rest is old

library(nlme)
comp.all.sum = NULL
for (r in unique(comp.all.df$cdistSpace)) {
    for (l in unique(comp.all.df$cdistTime)) {
        for (m in unique(comp.all.df$Measure)) {
            for (s in unique(comp.all.df$Source)) {
                x=comp.all.df %>% filter(cdistSpace<=r, cdistTime<=l, Measure==m,Source==s) %>% group_by(reef.alias,Niskin.Date) %>% summarize(Value=mean(Value,na.rm=TRUE),Niskin=mean(Niskin,na.rm=TRUE)) %>% ungroup
                if (nrow(x)<1) next
                x=x %>% filter(!is.na(Value),!is.na(Niskin),!is.infinite(Value))
                ss = nlme:::summary.lme(ll<-nlme:::lme(fixed=Value~Niskin, random=~1|reef.alias, data=x,na.action=na.omit))
                ss=t(ss$tTable[2,])
                r2=R2(ll)
                rr=rmse(x %>% mutate(error=Value-Niskin) %>% select(error))
                mm=mae(x %>% mutate(error=Value-Niskin) %>% select(error))
                p=mpe(x %>% mutate(error=Value-Niskin) %>% select(error,Niskin))
                comp.all.sum = rbind(comp.all.sum,
                                     data.frame(Source=s,Measure=m,Dist=r,Lag=l,RMSE=rr,MAE=mm,MAPE=p,ss,residual.RMSE=rmse(nlme:::residuals.lme(ll)),residual.MAE=mae(nlme:::residuals.lme(ll)),R2.marginal=r2[1],R2.conditional=r2[4]))
            }
        }
    }
}
comp.all.sum.max = comp.all.sum
save(comp.all.sum.max, file='data/comparisons/comp.all.sum.max.RData')
load(file='data/comparisons/comp.all.sum.max.RData')

comp.all.sum.max %>% arrange(Measure,Dist,Lag,Source) %>% as.data.frame %>%
    xtable(label='tab:comp.all.sum.max',caption='AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) for spatial/temporal lags.  Dist and Lag represent spatial (km) and temporal (days) lags.') %>%
    print(file='Report/Tables/comp.all.sum.max.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='tiny',tabular.environment='longtable',booktabs=TRUE,add.to.row=addtorow,hline.after=c(-1))
comp.all.sum.max %>% group_by(Measure,Source) %>% arrange(MAPE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.mpe.sum.max',caption=c('Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAPE. Dist and Lag represent spatial (km) and temporal (days) lags.','Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (ordered by MPE)')) %>%
    print(file='Report/Tables/comp.all.sum_mpe.max.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.sum.max %>% group_by(Measure,Source) %>% arrange(RMSE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.rmse.sum.max',caption=c('Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on RMSE. Dist and Lag represent spatial (km) and temporal (days) lags.','Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (ordered by RMSE)')) %>%
    print(file='Report/Tables/comp.all.sum_rmse.max.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20,30))
comp.all.sum.max %>% group_by(Measure,Source) %>% arrange(MAE) %>% slice(1:5) %>% as.data.frame %>%
    xtable(label='tab:comp.all.mae.sum.max',caption=c('Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (RMSE: root mean square error, MAE: mean absolute error, MAPE: mean percent error, Value: regression slope, residual.RMSE: residual root mean square error, residual.MAE: residual mean absolute error, R2.marginal: $R^2$ marginalized over sites, R2.conditional: $R^2$ conditional on sites) per Measure per source (Satellite, eReefs) for spatial/temporal lags.  Rows ranked and filtered based on MAE. Dist and Lag represent spatial (km) and temporal (days) lags.','Top five ranked AIMS Niskin vs Satellite/eReefs observation association metrics (ordered by MAE)')) %>%
    print(file='Report/Tables/comp.all.sum_mae.max.tex',comment=FALSE,include.rownames=FALSE, caption.placement='top',size='scriptsize',tabular.environment='tabular',booktabs=TRUE,hline.after=c(-1,0,10,20))

## #comp.all.df %>% mutate_(.dots=paste0('cdistSpace<',1:12)) %>% gather(key=Dist,value=Bool, contains('<')) %>% filter(Bool==TRUE) %>% as.data.frame %>% head 
## comp.all.sum.max %>% arrange(Measure,Dist,Lag,Source) %>% as.data.frame %>%
##     xtable %>% print(file='Report/Tables/comp.all.sum.max.tex',comment=FALSE)
## comp.all.sum.max %>% group_by(Measure,Source) %>% arrange(mpe) %>% slice(1:3) %>% as.data.frame %>%
##         xtable %>% print(comment=FALSE)
## comp.all.sum.max %>% group_by(Measure,Source) %>% arrange(mae) %>% slice(1:3) %>% as.data.frame %>%
##     xtable %>% print(comment=FALSE)
## comp.all.sum.max %>% group_by(Measure,Source) %>% arrange(rmse) %>% slice(1:3) %>% as.data.frame %>%
##     xtable %>% print(comment=FALSE)
## comp.all.sum.max %>% group_by(Measure,Source) %>% arrange(R2.marginal) %>% slice(1:3) %>% as.data.frame %>%
##     xtable %>% print(comment=FALSE)




if (1==2) {
##Old version
comp.1.df=NULL
comp.1.coords = NULL
radius=10
lag=10
for (m in unique(niskin.full$Measure)) {
    foreach(r=unique(niskin.full$Region)) %dopar% {
#    for (r in unique(niskin.full$Region)) {
        for (w in unique(niskin.full$waterBody)) {
            nis = niskin.full %>% filter(Measure==m, Region==r, waterBody==w)
            nis.1.coords = nis %>%
                dplyr:::select(reef.alias, Longitude=LONGITUDE, Latitude=LATITUDE,Date=Date, Value) %>%
                mutate(Date=as.Date(Date))
            if (nrow(nis)==0) next
            ## Start with the Satellite data
            load(paste0('data/indexed/data.idx_',m,'__',r,'___',w,'.RData'))            
            data=data.idx %>% dplyr:::select(-Binary,-MAMP,-fMAMP,-fsMAMP,-fsMAMP4)
            ###Find nearest point in Satellite data
            b2=nearest.space.time(data, nis.1.coords,radius=radius,lag=lag)

            #load(paste0('eReefs/data/indexed/data.idx_',m,'__',r,'___',w,'.RData'))
            comp.1.df = b2 #rbind(comp.1.df, b2)
            save(comp.1.df, file=paste0('data/comparisons/comp.1.df_',m,'__',r,'____',w,'.RData'))
        }
    }
}



## What about all within a Radius??



#fs = list.files(path='data/comparisons', pattern='comp.1.df\\_chl.*', full.names = TRUE)
fs = list.files(path='data/comparisons', pattern='comp.1.df\\_[a-z]{2,3}.*', full.names = TRUE)
fs
comp.df = NULL
for (f in fs) {
    load(f)
    comp.df = rbind(comp.df,comp.1.df)
}
save(comp.df, file=paste0('data/comparisons/comp.df.RData'))
load(file=paste0('data/comparisons/comp.df.RData'))

    
c1.sum=comp.df %>% group_by(Measure,cdistSpace,cdistTime,reef.alias, waterYear,Niskin.Date) %>% summarize(Value=mean(Value,na.rm=TRUE), Niskin=mean(Niskin, na.rm=TRUE)) %>%
    group_by(Measure,cdistSpace,cdistTime) %>%
    do({
        x=.
        x= x %>% filter(!is.na(Value),!is.na(Niskin))
        s = nlme:::summary.lme(l<-nlme:::lme(fixed=Value~Niskin, random=~1|reef.alias, data=x,na.action=na.omit))
        s=t(s$tTable[2,])
        r2=R2(l)
        r=rmse(x %>% mutate(error=Value-Niskin) %>% select(error))
        m=mae(x %>% mutate(error=Value-Niskin) %>% select(error))
        p=mpe(x %>% mutate(error=Value-Niskin) %>% select(error,Niskin))
        data.frame(rmse=r,mae=m,mpe=p,s,residual.rmse=rmse(nlme:::residuals.lme(l)),residual.mae=mae(nlme:::residuals.lme(l)),R2.marginal=r2[1],R2.conditional=r2[4])
    }) %>% ungroup
c1.sum %>% as.data.frame
c1.sum %>% group_by(Measure) %>% arrange(mpe) %>% slice(1:3) %>% as.data.frame
c1.sum %>% group_by(Measure) %>% arrange(rmse) %>% slice(1:3) %>% as.data.frame
c1.sum %>% group_by(Measure) %>% arrange(R2.marginal) %>% slice(1:3) %>% as.data.frame


## How about some polots to show the correpondance between Niskin and Satellite at reef.alias sites
## g1=comp.df %>% filter(Radius %in% c(0,1,2,3), Lag==0,Measure=='chl') %>%
##     mutate(Radius = factor(Radius,levels=c(3,2,1,0))) %>% arrange(Radius) %>%
##     ggplot(aes(y=Latitude,x=Longitude)) + geom_point(aes(color=factor(Radius))) +
##     geom_point(aes(y=Niskin.Latitude,x=Niskin.Longitude), color='black') +
##     geom_segment(aes(yend=Niskin.Latitude,xend=Niskin.Longitude,color=factor(Radius)), alpha=0.5) +
##     geom_point(aes(y=Niskin.Latitude,x=Niskin.Longitude), color='black')  +
##     facet_wrap(~reef.alias, scales='free')
## ggsave(g1, file='data/comparisons/junk.pdf', width=20, height=20)

## THIS ONLY REQUIRES THE CHL data
load('data/GIS/layers.RData')
qld.df=tidy(qld)
reefs.df=tidy(reefs)
gg = list()
comp.df = comp.df %>% arrange(desc(Latitude),Longitude)
pb <- txtProgressBar(max=length(unique(comp.df$reef.alias)), style=3)
for (i in 1:length(unique(comp.df$reef.alias))) {
#   gdata = comp.df %>% filter(reef.alias==unique(comp.df$reef.alias)[i],Radius %in% c(0,1,2,3), Lag==0,Measure=='chl') %>%
#       mutate(Radius = factor(Radius,levels=c(3,2,1,0))) %>% arrange(Radius) %>% droplevels
    gdata = comp.df %>% filter(reef.alias==unique(comp.df$reef.alias)[i],cdistSpace <5,cdistTime==0,Measure=='chl') %>%
        mutate(cdistSpace=factor(cdistSpace,levels=4:0)) %>% arrange(cdistSpace) %>% droplevels
   xl=range(gdata$Longitude)
   yl=range(gdata$Latitude)
   gg[[i]] = gdata %>%
       ggplot(aes(y=Latitude,x=Longitude)) +
       geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black') +
       geom_polygon(data=reefs.df, aes(y=lat, x=long, group=group), fill='lightblue', color='blue') +
       #geom_tile(aes(fill=cdistSpace),alpha=0.1) +
       geom_point(aes(color=factor(cdistSpace)),show.legend=FALSE) +
#       geom_text(aes(label=factor(cdistSpace)),show.legend=FALSE) +
       geom_point(aes(y=Niskin.Latitude,x=Niskin.Longitude), color='black') +
       geom_segment(aes(yend=Niskin.Latitude,xend=Niskin.Longitude,color=factor(cdistSpace)), alpha=0.1, show.legend=FALSE) +
           geom_point(aes(y=Niskin.Latitude,x=Niskin.Longitude), color='black')  +
           ggtitle(unique(comp.df$reef.alias)[i]) +
           coord_equal(ylim=yl, xlim=xl) + theme_classic() +
    theme(panel.background=element_rect(fill=NA,color='black'))
    setTxtProgressBar(pb,i)
}
close(pb)
ggsave(do.call(grid.arrange, gg),
       #grid.arrange(gg[[1]],gg[[2]], gg[[3]]),
       file='Report/Figures/Sat_vs_Niskin_locations1.pdf', width=20, height=20)

## Now scatterplots

c1=comp.df %>%
    arrange(desc(Niskin.Latitude), Niskin.Longitude) %>%
    mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias))) %>%
    group_by(Measure,cdistSpace,cdistTime,reef.alias,waterYear,Niskin.Date) %>% summarize_at(vars(Value,Niskin,GL),funs(median(.,na.rm=TRUE))) %>% ungroup
gg=list()
i=0
pb <- txtProgressBar(max=length(unique(c1$cdistSpace))*length(unique(c1$cdistTime)), style=3)
for (r in unique(c1$cdistSpace)) {
    for (l in unique(c1$cdistTime)) {
        i = i+1
        gg[[i]] = c1 %>% filter(cdistSpace==r, cdistTime==l) %>%
            ggplot(aes(y=Value,x=Niskin)) +
            geom_hline(aes(yintercept=GL, color=Measure), linetype='dashed', alpha=0.5) +
            geom_vline(aes(xintercept=GL, color=Measure),linetype='dashed', alpha=0.5) +
            geom_point(aes(color=Measure), alpha=0.3, size=1) +
            facet_wrap(~reef.alias) +
            scale_x_log10('Niskin observations (over time)') + scale_y_log10('Satellite observations (over time)') +
            geom_smooth(method='lm', aes(color=Measure),se=FALSE) +
            ggtitle(paste0('Spatial Radius=',r,' (km), Temporal Lag=',l,' (days)')) +
            theme_bw() + #+ coord_cartesian(xlim=c(0.1,10), ylim=c(0.1,10))
            theme(legend.position=c(1,0), legend.justification=c(1,0))
        ggsave(gg[[i]], file=paste0('Report/Figures/Sat_vs_Niskin_scatter.Radius_',r,'_Lag_',l,'.pdf'), width=10, height=10)
        setTxtProgressBar(pb,i)
    }
}
close(pb)
gg[[1]]

#Time series for observations within a specific space/time distance band from the discrete samples.
i=0
c2=c1 %>% ungroup %>% gather(key=Source,value=Value, Value,Niskin)
pb <- txtProgressBar(max=length(unique(c2$cdistSpace))*length(unique(c2$cdistTime))*length(unique(c2$Measure)), style=3)
for (r in unique(c2$cdistSpace)) {
    for (l in unique(c2$cdistTime)) {
        for (m in unique(c2$Measure)) {
            i = i+1
            gg[[i]] = c2 %>% filter(cdistSpace==r, cdistTime==l, Measure==m) %>%
                ggplot(aes(y=Value,x=Niskin.Date)) +
                geom_hline(aes(yintercept=GL), linetype='dashed') +
                geom_point(aes(color=Source)) +
                geom_line(aes(color=Source)) +
                facet_wrap(~reef.alias) +
                scale_color_manual(values=c('red','blue'),labels=c('Niskin','Satellite')) +
                scale_y_log10('Observations') + scale_x_date('') +
                ggtitle(paste0('Measure=',m,', Spatial Radius=',r,' (km), Temporal Lag=',l,' (days)')) +
                theme_bw() +
                theme(legend.position=c(1,0), legend.justification=c(1,0))
            ggsave(gg[[i]], file=paste0('Report/Figures/Sat_vs_Niskin_time.',m,'.Radius_',r,'_Lag_',l,'.pdf'), width=20, height=10)
            setTxtProgressBar(pb,i)
        }
    }
}
close(pb)

## It might also be worth generating similar temporal trends for points within a max distance (space and time) from the discrete samples.
i=0
c2=c1 %>% ungroup %>% gather(key=Source,value=Value, Value,Niskin)
pb <- txtProgressBar(max=length(unique(c2$cdistSpace))*length(unique(c2$cdistTime))*length(unique(c2$Measure)), style=3)
for (r in unique(c2$cdistSpace)) {
    for (l in unique(c2$cdistTime)) {
        for (m in unique(c2$Measure)) {
            i = i+1
            c2.a = c2 %>% filter(cdistSpace<=r, cdistTime<=l, Measure==m) %>% group_by(Source,reef.alias,Niskin.Date) %>% summarize(Value=mean(Value,na.rm=TRUE))
            gg[[i]] = c2.a %>% 
                ggplot(aes(y=Value,x=Niskin.Date)) +
                geom_point(aes(color=Source)) +
                geom_line(aes(color=Source)) +
                facet_wrap(~reef.alias) +
                scale_color_manual(values=c('red','blue'),labels=c('Niskin','Satellite')) +
                scale_y_log10('Satellite observations') + scale_x_date('') +
                ggtitle(paste0('Measure=',m,', Maximum Spatial Radius=',r,' (km), Maximum Temporal Lag=',l,' (days)')) +
                theme_bw() +
                theme(legend.position=c(1,0), legend.justification=c(1,0))
            ggsave(gg[[i]], file=paste0('Report/Figures/Sat_vs_Niskin_time_max.',m,'.Radius_',r,'_Lag_',l,'.pdf'), width=20, height=10)
            setTxtProgressBar(pb,i)
        }
    }
}
close(pb)


## The associated correspondance stats
c1=comp.df %>% group_by(Measure,cdistSpace,cdistTime,reef.alias, waterYear,Niskin.Date) %>% summarize(Value=mean(Value,na.rm=TRUE), Niskin=mean(Niskin, na.rm=TRUE))
c1.sum = NULL
i=0
pb <- txtProgressBar(max=length(unique(c1$cdistSpace))*length(unique(c1$cdistTime))*length(unique(c1$Measure)), style=3)
for (r in unique(c2$cdistSpace)) {
    for (l in unique(c2$cdistTime)) {
        for (m in unique(c2$Measure)) {
            print(r)
            print(l)
            print(m)
            c2.a = c1 %>% ungroup %>% filter(cdistSpace<=r, cdistTime<=l, Measure==m)
            x= c2.a %>% filter(!is.na(Value),!is.na(Niskin))
            s=tryCatch({nlme:::summary.lme(ll<-nlme:::lme(fixed=Value~Niskin, random=~1|reef.alias, data=x,na.action=na.omit))},error=function(x) NULL)
            if (is.null(s)) {
                s <- t(rep(NA,5))
                colnames(s) <- c('Value','Std.Error',  'DF'   ,'t.value'      ,'p.value')
                residual.rmse=NA
                residual.mae=NA
                R2.marginal=NA
                R2.conditional=NA
            } else {
                s=t(s$tTable[2,])
                r2=R2(ll)
                residual.rmse=rmse(nlme:::residuals.lme(ll))
                residual.mae=mae(nlme:::residuals.lme(ll))
                R2.marginal=r2[1]
                R2.conditional=r2[4]
            }
            
            rr=rmse(x %>% mutate(error=Value-Niskin) %>% select(error))
            mm=mae(x %>% mutate(error=Value-Niskin) %>% select(error))
            p=mpe(x %>% mutate(error=Value-Niskin) %>% select(error,Niskin))
            d=data.frame(rmse=rr,mae=mm,mpe=p,s,residual.rmse=residual.rmse,residual.mae=residual.mae,R2.marginal=R2.marginal,R2.conditional=R2.conditional)
            c1.sum = rbind(c1.sum, data.frame(Measure=m,Radius=r, Lag=l,d))
            i=i+1
            setTxtProgressBar(pb,i)
        }
    }
}
close(pb)

c1.sum %>% group_by(Measure) %>% arrange(mpe) %>% slice(1:3) %>% as.data.frame
c1.sum %>% group_by(Measure) %>% arrange(rmse) %>% slice(1:3) %>% as.data.frame
c1.sum %>% group_by(Measure) %>% arrange(residual.rmse) %>% slice(1:3) %>% as.data.frame
c1.sum %>% group_by(Measure) %>% arrange(residual.mae) %>% slice(1:3) %>% as.data.frame
c1.sum %>% group_by(Measure) %>% arrange(R2.marginal) %>% slice(1:3) %>% as.data.frame

    }

## It might be interesting to explore each of the indicies and then aggregate
## So take comp.df, gather it into sources, apply the indices functions, then aggregate over space/time radius/lag etc...

## Also use a polygon around each reef.alias to extract a subset of the satellite data from which to calculate WQ index etc
