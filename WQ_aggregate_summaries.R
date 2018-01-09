library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
require(foreach)
require(doParallel)
require(broom)
require(sp)
library(dplyr)

source('WQ_functions.R')
getConfigs()


## A toggle to indicate whether to exclude SD as a Measure for any aggregations past Measurement level.
## So by that, it will be included in Measurement level outputs, yet not included in Sub-indicator aggregations.
if (!exists('include_sd')) include_sd = TRUE
if (!exists('include_NOx')) include_NOx=TRUE
if (!exists('include_nap')) include_nap=TRUE
if (!exists('include_enclosedcoastal')) include_enclosedcoastal = FALSE

add.str=''
if (!include_sd) add.str = paste0(add.str, 'noSD')
if (!include_NOx) add.str = paste0(add.str, 'noNOx')
if (!include_nap) add.str = paste0(add.str, 'nonap')
## A toggle to indicate whether to exclude the Enclosed Coastal
ec.str='_with_enclosed_coastal'
if (!include_enclosedcoastal) ec.str='_without_enclosed_coastal'

                                        
SiteMeasureMap=TRUE
SiteSubindicatorMap=TRUE
SiteIndicatorMap=TRUE
ZoneMeasureWorm=TRUE
ZoneMeasureFlatMap=TRUE
ZoneMeasureMosaic=TRUE
ZoneSubindicatorWorm=TRUE
ZoneSubindicatorFlatMap=TRUE
ZoneSubindicatorMosaic=TRUE
ZoneIndicatorWorm=TRUE
ZoneIndicatorFlatMap=TRUE
ZoneIndicatorMosaic=TRUE
WaterBodyMeasureWorm=TRUE
WaterBodyMeasureMosaic=TRUE
WaterBodySubindicatorWorm=TRUE
WaterBodySubindicatorMosaic=TRUE
WaterBodyIndicatorWorm=TRUE
WaterBodyIndicatorMosaic=TRUE
GBRMeasureWorm=TRUE
GBRMeasureMosaic=TRUE
GBRSubindicatorWorm=TRUE
GBRSubindicatorMosaic=TRUE
GBRIndicatorWorm=TRUE
GBRIndicatorMosaic=TRUE

if (!exists('GradeType')) GradeType='Uniform'
gradeMids = WQ_gradeMids(type=GradeType)
gradeBoundaries = WQ_gradeBoundaries(type=GradeType)
    
index_type = rbind(
    expand.grid(pattern='data.idx\\_.*', index=c('Binary','MAMP','fsMAMP','fsMAMP4'), Temporal='Annual'),
    expand.grid(pattern='data.idx.seasonal\\_.*', index=c('Binary','MAMP','fsMAMP','fsMAMP4'), Temporal='Seasonal'),
    expand.grid(pattern='data.idx.exceed\\_.*', index=c('Exceed','Max_Duration'), Temporal='Annual'),
    expand.grid(pattern='data.idx.exceed.seasonal\\_.*', index='Exceed', Temporal='Seasonal')
)

registerDoParallel(cores=10)

unlink('log/aggregate_summaries.log', recursive=TRUE)


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

unlink('log/aggregate_summariesBinary.log', recursive=TRUE)
unlink('log/aggregate_summariesMAMP.log', recursive=TRUE)
unlink('log/aggregate_summariesfsMAMP.log', recursive=TRUE)
unlink('log/aggregate_summariesfsMAMP4.log', recursive=TRUE)
unlink('log/aggregate_summariesExceed.log', recursive=TRUE)
unlink('log/aggregate_summariesMax_Duration.log', recursive=TRUE)


sink('feedback.txt')

measures = read.table('parameters/measures.txt', strip.white=TRUE, sep=';', header=TRUE)
report.card.colors = colorRampPalette(c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'))
temporal='Annual'
foreach(src=c('niskin/','flntu/','','eReefs/','eReefs926/')) %do% {
    print(src)
    foreach(idx=(1:nrow(index_type))[c(1,2,3,4,9,10)]) %do% {
        index=index_type$index[idx]
        print(index)
        logfile=paste0('aggregate_summaries',index,'.log')
        ## Map for Site/Measure
        if (SiteMeasureMap) {
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.site.year.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            yl=bbox(Polys)[2,] #range(data.all$Latitude)
            xl=bbox(Polys)[1,] #range(data.all$Longitude)
            ratio= diff(xl)/diff(yl)
            foreach (measure= unique(data.all$Measure)) %do%  { #c('chl','nap','sd','NOx')) {
                print(paste0('SiteMeasureMap ',measure))
                unitsLabel = measures$Label[measures$Measure==measure]
                WQ_tryCatch(
                {
                    g=ggplot(data.all %>% filter(Measure==measure), aes(y=Latitude,x=Longitude))
                    if (stringr:::str_detect(src,'eReefs')) {
                        sz=7
                        pts=0.1
                        #g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn(paste0(unitsLabel,'\nIndex\n'),colors=rev(report.card.colors(10)), limits=c(0,1)) 
                    } else if (stringr:::str_detect(src,'niskin')) {
                        sz=7
                        pts=2
                    } else if (stringr:::str_detect(src,'flntu')) {
                        sz=7
                        pts=2
                        #g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn(paste0(unitsLabel,'\nIndex\n'),colors=rev(report.card.colors(10)), limits=c(0,1)) 
                    } else {
                        sz=5
                        pts=0.1
                        #g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn(paste0(unitsLabel,'\nIndex\n'),colors=rev(report.card.colors(10)), limits=c(0,1))
                    }
                    g=g+ geom_point(aes(color=Mean), size=pts) +
                        scale_color_gradientn(paste0(unitsLabel,'\nIndex\n'),colors=rev(report.card.colors(6)), values=rev(gradeBoundaries),limits=c(0,1))
                    g= g+geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', size=0.1,color='black') +
                        geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, size=0.1,color='black') +
                        facet_wrap(~waterYear,ncol=4) +
                        theme_classic() +
                        coord_equal(ylim=yl, xlim=xl) +
                        theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
                              strip.background=element_blank(), axis.text.y=element_text(size=sz), axis.text.x=element_text(size=sz),
                              legend.title=element_text(margin=c(1,1),'lines'))
                    nc=length(unique(data.all$waterYear))
                    nx=ifelse(nc<4,nc,4)
                    ny=max(1 + (nc %/% 4))
                    print(getwd())
                    print(measure)
                    print(paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_measure.',measure,'.site_Grade_',GradeType,'.pdf'))
                    ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_measure.',measure,'.site_Grade_',GradeType,'.pdf'), g, width=2*nx, height=(ny*2/ratio) + 0*ratio, units='in', device=cairo_pdf)
                    ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_measure.',measure,'.site_Grade_',GradeType,'.png'), g, width=2*nx, height=(ny*2/ratio) + 0*ratio, units='in',dpi=300)
                    system(paste0('cd Report/Figures/; convert +trim +repage -density 200 "spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_measure.',measure,'.site_Grade_',GradeType,'.pdf" "spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_measure.',measure,'.site_Grade_',GradeType,'_lowres.pdf"'))
                },logfile, msg=paste0('Aggregate summary plot for Measure/Site map (Source=',gsub('/','',src),', Index=',idx,', Measure=',measure,')'), return=TRUE)
            }
        }        
        ## Map for Site/Subindicator
        if (SiteSubindicatorMap) {
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.site.year',add.str,'.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            yl=bbox(Polys)[2,] #range(data.all$Latitude)
            xl=bbox(Polys)[1,] #range(data.all$Longitude)
            foreach (subindicator=unique(data.all$Subindicator)) %dopar% {
                print(paste0('SiteSubindicatorMap ',subindicator))
                WQ_tryCatch(
                {
                    g=ggplot(data.all %>% filter(Subindicator==subindicator), aes(y=Latitude,x=Longitude))
                    if (stringr:::str_detect(src,'eReefs')) {
                        sz=7
                        pts=0.1
                        #g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(10)), limits=c(0,1)) 
                    } else if (stringr:::str_detect(src,'niskin')) {
                        sz=7
                        pts=2
                        #g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(10)), limits=c(0,1))
                    } else if (stringr:::str_detect(src,'flntu')) {
                        sz=7
                        pts=2
                        #g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(10)), limits=c(0,1)) 
                    } else {
                        sz=5
                        pts=0.1
                        #g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(10)), limits=c(0,1))
                    }
                    g=g+ geom_point(aes(color=Mean), size=pts) +scale_color_gradientn(paste0(subindicator,'\nIndex\n'),colors=rev(report.card.colors(6)), values=rev(gradeBoundaries),limits=c(0,1))                    
                    g= g+geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', size=0.1,color='black') +
                        geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, size=0.1,color='black') +
                        facet_wrap(~waterYear) +
                        theme_classic() +
                        coord_equal(ylim=yl, xlim=xl) +
                        theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
                              strip.background=element_blank(), axis.text.y=element_text(size=sz), axis.text.x=element_text(size=sz),
                              legend.title=element_text(margin=c(1,1),'lines'))
                    nc=length(unique(data.all$waterYear))
                    nx=ifelse(nc<4,nc,4)
                    ny=max(1 + (nc %/% 4)) 
                    ## gg=ggplotGrob(g)
                    ## nw=sum(grid:::convertWidth(gg$widths,'in',valueOnly=TRUE)) #sum(sapply(gg$widths,function(x) grid:::convertWidth(x,'in')))
                    ## nh=sum(grid:::convertHeight(gg$heights,'in',valueOnly=TRUE))
                    ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.',subindicator,'.site',add.str,'_Grade_',GradeType,'.pdf'), g, width=2*nx, units='in', device=cairo_pdf)
                    ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.',subindicator,'.site',add.str,'_Grade_',GradeType,'.png'), g, width=2*nx, units='in',dpi=300)
                    system(paste0('cd Report/Figures/; convert -density 200 "spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.',subindicator,'.site',add.str,'_Grade_',GradeType,'.pdf" "spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.',subindicator,'.site',add.str,'_Grade_',GradeType,'_lowres.pdf"'))
                },logfile, msg=paste0('Aggregate summary plot for Subindicator/Site map (Source=',gsub('/','',src),', Index=',idx,', Subindicator=',subindicator,')'), return=TRUE)                    
            }
        }



        ## Map for Site/Indicator
        if (SiteIndicatorMap) {
            print(paste0('SiteIndicatorMap '))
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.site.year',add.str,'.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            yl=range(data.all$Latitude)
            xl=range(data.all$Longitude)
            ratio= diff(xl)/diff(yl)
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Latitude,x=Longitude))
                if (stringr:::str_detect(src,'eReefs')) {
                    sz=7
                    pts=0.1
                    #g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(5)), values=WQ_gradeBoundaries,limits=c(0,1)) 
                } else if (stringr:::str_detect(src,'niskin')) {
                    sz=7
                    pts=2
                    #g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(10)), limits=c(0,1))
                } else if (stringr:::str_detect(src,'flntu')) {
                    sz=7
                    pts=2
                    #g=g+ geom_point(aes(color=Mean), size=2) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(10)), limits=c(0,1)) 
                } else {
                    sz=5
                    pts=0.1
                    #g=g+ geom_point(aes(color=Mean), size=0.1) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(10)), limits=c(0,1))
                }
                g=g+ geom_point(aes(color=Mean), size=pts) +scale_color_gradientn('Water Quality\nIndex\n',colors=rev(report.card.colors(6)), values=rev(gradeBoundaries),limits=c(0,1))                    
                g= g+geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', size=0.1,color='black') +
                    geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, size=0.1,color='black') +
                    facet_wrap(~waterYear) +
                    theme_classic() +
                    coord_equal(ylim=yl, xlim=xl) +
                    theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
                          strip.background=element_blank(), axis.text.y=element_text(size=sz), axis.text.x=element_text(size=sz),
                          legend.title=element_text(margin=c(1,1),'lines'))
                nc=length(unique(data.all$waterYear))
                nx=ifelse(nc<4,nc,4)
                ny=max(1 + (nc %/% 4)) 
                ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.site',add.str,'_A_Grade_',GradeType,'.pdf'), g, width=2*nx, height=(ny*2/ratio) + 0*ratio,units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.site',add.str,'_A_Grade_',GradeType,'.png'), g, width=2*nx, height=(ny*2/ratio) + 0*ratio, units='in',dpi=300)
                system(paste0('cd Report/Figures/; convert +trim +repage -density 200 "spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.site',add.str,'_A_Grade_',GradeType,'.pdf" "spatial_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.site',add.str,'_A_Grade_',GradeType,'_lowres.pdf"'))
            },logfile, msg=paste0('Aggregate summary plot for Indicator/Site map (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                    
        }

        ## Graphical summary for Zone/Measure
        if (ZoneMeasureWorm) {
            print(paste0('ZoneMeasureWorm '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.zone.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
                geom_line(aes(group=Measure)) + geom_point(aes(fill=Measure), shape=21, color='black', size=3) +
                    scale_x_continuous('Water year') +
                    scale_y_continuous('Index')
                if (stringr:::str_detect(src,'eReefs')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd','NOx'), labels=c('Chlorophyll','TSS','Secchi','NOx'), values=c('green','blue','orange','red'))
                } else if (src==''){
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd'), labels=c('Chlorophyll','TSS','Secchi'),values=c('green','blue','orange'))
                } else if (stringr:::str_detect(src,'niskin')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd','NOx'), labels=c('Chlorophyll','TSS','Secchi','NOx'),values=c('green','blue','orange','red'))
                } else if (stringr:::str_detect(src,'flntu')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap'), labels=c('Chlorophyll','TSS'),values=c('green','blue'))
                }
                g=g+facet_grid(Region~WaterBody, scales='free_y') + theme_bw()+
                    theme(strip.background=element_blank(), legend.key=element_blank())
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Measure/Zone worm (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                    
        }

        
        ## Flat map (Zone/Measure)
        if (ZoneMeasureFlatMap) {
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.zone.year',add.str,ec.str,'.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.map = data.all %>% full_join(Polys.df)
            if (GradeType=='MMP') data.map = data.map %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.map = data.map %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.map = data.map %>% mutate(Grade=Grade.Uniform)

            yl=bbox(Polys)[2,] #range(data.map$lat)
            xl=bbox(Polys)[1,] #range(data.map$long)
            ratio= diff(xl)/diff(yl)
            if (stringr:::str_detect(src,'eReefs')) sz=7
            if (stringr:::str_detect(src,'niskin')) sz=7
            if (stringr:::str_detect(src,'flntu')) sz=7
            if (src=='') sz=5
            foreach (measure=unique(data.all$Measure)) %dopar% {
                print(paste0('ZoneMeasureFlatMap ',measure))
                WQ_tryCatch(
                {
                    g=ggplot(data.map %>% filter(Measure==measure), aes(y=lat, x=long)) +
                        geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey',size=0.1, color='black') +
                        geom_polygon(aes(fill=Grade,group=group), color='black',size=0.1) +
                        geom_polygon(data=Polys.df, aes(group=group), fill=NA, color='black', size=0.1) +
                        facet_wrap(~waterYear) +
                        scale_fill_manual('Grade\n', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) + 
                    theme_classic() +
                        coord_equal(ylim=yl, xlim=xl) +
                        theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
                              strip.background=element_blank(), axis.text.y=element_text(size=sz), axis.text.x=element_text(size=sz))
                    nc=length(unique(data.map$waterYear))
                    nx=ifelse(nc<4,nc,4)
                    ny=max(1 + (nc %/% 4)) 
                    ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_measure_',measure,'.zone',add.str,ec.str,'_A_Grade_',GradeType,'.pdf'), g, width=2*nx, height=(ny*2/ratio) + 0*ratio, units='in',device=cairo_pdf)
                    ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_measure_',measure,'.zone',add.str,ec.str,'_A_Grade_',GradeType,'.png'), g, width=2*nx, height=(ny*2/ratio) + 0*ratio, units='in',dpi=300)
                    system(paste0('cd Report/Figures/; convert +trim +repage -density 200 "simple_map_',gsub('/','',src),'_',index,'.',temporal,'_measure_',measure,'.zone',add.str,ec.str,'_A_Grade_',GradeType,'.pdf" "simple_map_',gsub('/','',src),'_',index,'.',temporal,'_measure_',measure,'.zone',add.str,ec.str,'_A_Grade_',GradeType,'_lowres.pdf"'))
                },logfile, msg=paste0('Aggregate summary plot for Measure/Zone flat map (Source=',gsub('/','',src),', Index=',idx,', Measure=',measure,')'), return=TRUE)                    
            }
        }
        
        
        ## Mosaic plot for Zone/Measure
        if (ZoneMeasureMosaic) {
            print(paste0('ZoneMeasureMosaic '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.zone.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup %>%
                mutate(Region=factor(Region, levels=rev(levels(.$Region))))
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Region, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0)) 
                if (src=='') {
                    seq_breaks <- function(by) {function(limits) limits[seq(1,length(limits), by=by)]}
                    g = g + scale_x_discrete('',breaks=seq_breaks(by=2),expand=c(0,0))
                    }
                if (src!='') g = g + scale_x_discrete('',expand=c(0,0))
                g=g+scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    facet_grid(Label~WaterBody) +
                    theme_classic()
                if (src=='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
                          strip.text=element_text(size=12))
                }
                if (src!='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=0,hjust=0.5),
                          strip.text=element_text(size=12))
                }
                xw=length(unique(data.mosaic$WaterBody))*length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Region))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)
                ##ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,'.pdf'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',device=cairo_pdf)
                ##ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,'.png'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Measure/Zone mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                                    
        }
        
        ## Graphical summary for Zone/Subindicator
        if (ZoneSubindicatorWorm) {
            print(paste0('ZoneSubindicatorWorm '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.zone.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                geom_line(aes(group=Subindicator)) + geom_point(aes(fill=Subindicator), shape=21, color='black', size=3) +
                    scale_x_continuous('Water year') +
                    scale_y_continuous('Index')
                if (stringr:::str_detect(src,'niskin')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity','Nutrients'), values=c('green','blue','red'))
                if (stringr:::str_detect(src,'flntu')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity'), values=c('green','blue'))
                if (stringr:::str_detect(src,'eReefs')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity','Nutrients'), values=c('green','blue','red'))
                if (src=='') g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity'), values=c('green','blue'))           
                g=g+facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
                    theme(strip.background=element_blank(), legend.key=element_blank(),panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) +
                    guides(fill=guide_legend(override.aes=list(color='black', size=5)))
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                
            },logfile, msg=paste0('Aggregate summary plot for Subindicator/Zone worm (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                                    
        }
        
        ## Flat map (Zone/Subindicator)==========================
        if (ZoneSubindicatorFlatMap) {
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.zone.year',add.str,ec.str,'.RData'))
            if (src %in% c('Satellite','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.map = data.all %>% full_join(Polys.df)
            if (GradeType=='MMP') data.map = data.map %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.map = data.map %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.map = data.map %>% mutate(Grade=Grade.Uniform)
            
            yl=bbox(Polys)[2,]
            xl=bbox(Polys)[1,] #range(data.map$long)
            if (stringr:::str_detect(src,'eReefs')) sz=7
            if (stringr:::str_detect(src,'niskin')) sz=7
            if (stringr:::str_detect(src,'flntu')) sz=7
            if (src=='') sz=5
            foreach (subindicator=unique(data.all$Subindicator)) %dopar% {
                print(paste0('ZoneSubindicatorFlatMap ', subindicator))
                WQ_tryCatch(
                {
                    g=ggplot(data.map %>% filter(Subindicator==subindicator), aes(y=lat, x=long)) +
                        geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black',size=0.1) +
                        geom_polygon(aes(fill=Grade,group=group), color='black',size=0.1) +
                        geom_polygon(data=Polys.df, aes(group=group), fill=NA, color='black', size=0.1) +
                        facet_wrap(~waterYear) +
                        scale_fill_manual('Grade\n', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) + 
                    theme_classic() +
                        coord_equal(ylim=yl, xlim=xl) +
                        theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
                              strip.background=element_blank(), axis.text.y=element_text(size=sz), axis.text.x=element_text(size=sz))
                    nc=length(unique(data.map$waterYear))
                    nx=ifelse(nc<4,nc,4)
                    ny=max(1 + (nc %/% 4)) 
                    ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator_',subindicator,'.zone',add.str,ec.str,'_A_Grade_',GradeType,'.pdf'), g, width=2*nx, units='in',device=cairo_pdf)
                    ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator_',subindicator,'.zone',add.strec.str,,'_A_Grade_',GradeType,'.png'), g, width=2*nx, units='in',dpi=300)
                    system(paste0('cd Report/Figures/; convert -density 200 "simple_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator_',subindicator,'.zone',add.str,ec.str,'_A_Grade_',GradeType,'.pdf" "simple_map_',gsub('/','',src),'_',index,'.',temporal,'_subindicator_',subindicator,'.zone',add.str,ec.str,'_A_Grade_',GradeType,'_lowres.pdf"'))
                },logfile, msg=paste0('Aggregate summary plot for Subindicator/Zone flat map (Source=',gsub('/','',src),', Index=',idx,', Subindicator=',subindicator,')'), return=TRUE)                                        
            }
        }
        
        ## Mosaic plot for Zone/Subindicator
        if (ZoneSubindicatorMosaic) {
            print(paste0('ZoneSubindicatorMosaic '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.zone.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup %>%
                mutate(Region=factor(Region, levels=rev(levels(.$Region))))
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Region, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0)) +
                    scale_x_discrete('',expand=c(0,0)) +
                    scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    facet_grid(Subindicator~WaterBody) +
                    theme_classic() +
                    theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
                          strip.text=element_text(size=12))
                xw=length(unique(data.mosaic$WaterBody))*length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Region))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.zone',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Subindicator/Zone mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                  
        }
        
        
        ## Graphical summary for Zone/Indicator Pathway A
        if (ZoneIndicatorWorm) {
            print(paste0('ZoneIndicatorWorm '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.zone.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            save(data.all, file=paste0('data/Scores/zone_indicator.',index,'_',temporal,'_',gsub('/','',src),'_',add.str,'.RData'))
            write.csv(data.all %>% arrange(Indicator,waterYear,Region,WaterBody) %>% dplyr:::select(Indicator,waterYear,everything()),
                      file=paste0('data/Scores/zone_indicator.',index,'_',temporal,'_',gsub('/','',src),'_',add.str,'.csv'), quote=FALSE, row.names=FALSE)
            if (GradeType=='MMP') data.all = data.all %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.all = data.all %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.all = data.all %>% mutate(Grade=Grade.Uniform)            
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
                geom_line() + geom_point(aes(fill=Grade), shape=21, color='black', size=3) +
                    scale_y_continuous('Water Quality Index') +
                    scale_x_continuous('Water Year') +
                    scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'),limits=LETTERS[1:5]) +
                facet_grid(Region~WaterBody, scales='free_y') + theme_bw()+ theme(strip.background=element_blank(),legend.key=element_blank(),
                                                                                  strip.text=element_text(size=12),
                                                                                  panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank()) +
                    guides(fill=guide_legend(override.aes=list(color='black', size=5)))
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_A_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_A_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                
            },logfile, msg=paste0('Aggregate summary plot for Indicator/Zone mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                  
        }


        ## Flat map (Zone/Indicator)
        if (ZoneIndicatorFlatMap) {
            print(paste0('ZoneIndicatorFlatMap '))
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.zone.year',add.str,ec.str,'.RData'))
            if (src %in% c('Satellite','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.map = data.all %>% full_join(Polys.df) %>% filter(!is.na(waterYear))
            if (GradeType=='MMP') data.map = data.map %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.map = data.map %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.map = data.map %>% mutate(Grade=Grade.Uniform)
            
            yl=bbox(Polys)[2,] #range(data.map$lat)
            xl=bbox(Polys)[1,] #range(data.map$long)
            if (stringr:::str_detect(src,'eReefs')) sz=7
            if (stringr:::str_detect(src,'niskin')) sz=7
            if (stringr:::str_detect(src,'flntu')) sz=7
            if (src=='') sz=5
            WQ_tryCatch(
            {
                g=ggplot(data.map, aes(y=lat, x=long)) +
                    geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black',size=0.1) +
                    geom_polygon(aes(fill=Grade,group=group), color='black',size=0.1) +
                    geom_polygon(data=Polys.df, aes(group=group), fill=NA, color='black', size=0.1) +
                    facet_wrap(~waterYear) +
                    scale_fill_manual('Grade\n', breaks=LETTERS[1:5], labels=LETTERS[1:5],values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'), limits=LETTERS[1:5]) + 
                theme_classic() +
                    coord_equal(ylim=yl, xlim=xl) +
                    theme(panel.border=element_rect(fill=NA,color='black'),panel.background=element_rect(fill=NA,color='black'),axis.title.x=element_blank(), axis.title.y=element_blank(),
                          strip.background=element_blank(), axis.text.y=element_text(size=sz), axis.text.x=element_text(size=sz))
                nc=length(unique(data.map$waterYear))
                nx=ifelse(nc<4,nc,4)
                ny=max(1 + (nc %/% 4)) 
                ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_A_Grade_',GradeType,'.pdf'), g, width=2*nx, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_A_Grade_',GradeType,'.png'), g, width=2*nx, units='in',dpi=300)
                system(paste0('cd Report/Figures/; convert +trim +repage -density 200 "simple_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_A_Grade_',GradeType,'.pdf" "simple_map_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_A_Grade_',GradeType,'_lowres.pdf"'))
            },logfile, msg=paste0('Aggregate summary plot for Indicator/Zone flat map (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                  
        }
        
        ## Mosaic plot for Zone/Indicator
        if (ZoneIndicatorMosaic) {
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.zone.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('Satellite','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup %>%
                mutate(Region=factor(Region, levels=rev(levels(.$Region))))
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Region, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0))
                                if (src=='') {
                    seq_breaks <- function(by) {function(limits) limits[seq(1,length(limits), by=by)]}
                    g = g + scale_x_discrete('',breaks=seq_breaks(by=2),expand=c(0,0))
                    }
                if (src!='') g = g + scale_x_discrete('',expand=c(0,0))
                g= g + scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    facet_grid(Indicator~WaterBody) +
                    theme_classic()
                                if (src=='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                              strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
                              strip.text=element_text(size=12))
                                }
                if (src!='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                              strip.background=element_blank(),axis.text.x=element_text(angle=0,hjust=0.5),
                              strip.text=element_text(size=12))
                }
                xw=length(unique(data.mosaic$WaterBody))*length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Region))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=5, units='in')
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_indicator.zone',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=5, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Indicator/Zone mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                  
        }


        ## Graphical summary for WaterBody/Measure=======================================================
        if (WaterBodyMeasureWorm) {
            print(paste0('WaterBodyMeasureWorm '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.waterbody.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
                geom_line(aes(group=Measure)) + geom_point(aes(fill=Measure), shape=21, color='black', size=3) +
                    scale_x_continuous('Water year') +
                    scale_y_continuous('Index')
                if (stringr:::str_detect(src,'eReefs')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd','NOx'), labels=c('Chlorophyll','TSS','Secchi','NOx'), values=c('green','blue','orange','red'))
                } else if (src==''){
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd'), labels=c('Chlorophyll','TSS','Secchi'),values=c('green','blue','orange'))
                } else if (stringr:::str_detect(src,'niskin')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd','NOx'), labels=c('Chlorophyll','TSS','Secchi','NOx'),values=c('green','blue','orange','red'))
                } else if (stringr:::str_detect(src,'flntu')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap'), labels=c('Chlorophyll','TSS'),values=c('green','blue'))
                }
                g=g+facet_grid(~WaterBody, scales='free_y') + theme_bw()+
                    theme(strip.background=element_blank(), legend.key=element_blank(),
                          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())
                
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_measure.waterbody',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=4, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_measure.waterbody',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=4, units='in',dpi=300)
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_measure.waterbody',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_measure.waterbody',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                
            },logfile, msg=paste0('Aggregate summary plot for Measure/WaterBody worm (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                    
        }

        ## Mosaic plot for WaterBody/Measure
        if (WaterBodyMeasureMosaic) {
            print(paste0('WaterBodyMeasureMosaic '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.waterbody.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup 
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Label, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0)) 
                if (src=='') {
                    seq_breaks <- function(by) {function(limits) limits[seq(1,length(limits), by=by)]}
                    g = g + scale_x_discrete('',breaks=seq_breaks(by=2),expand=c(0,0))
                    }
                if (src!='') g = g + scale_x_discrete('',expand=c(0,0))
                g=g+scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    facet_grid(~WaterBody) +
                    theme_classic()
                if (src=='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
                          strip.text=element_text(size=12))
                }
                if (src!='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=0,hjust=0.5),
                          strip.text=element_text(size=12))
                }
                xw=length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Measure))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.waterbody',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=4*xw*1, height=xh*1, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.waterbody',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=4*xw*1, height=xh*1, units='in',dpi=300)
                ##ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,'.pdf'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',device=cairo_pdf)
                ##ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,'.png'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Measure/WaterBody mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                                    
        }

    ## Graphical summary for WaterBody/Subindicator
    if (WaterBodySubindicatorWorm) {
        print(paste0('WaterBodySubindicatorWorm '))
        ec.str_temp = '_with_enclosed_coastal'
        load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.waterbody.year',add.str,ec.str_temp,'.RData'))
        if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
                geom_line(aes(group=Subindicator)) + geom_point(aes(fill=Subindicator), shape=21, color='black', size=3) +
                    scale_x_continuous('Water year') +
                    scale_y_continuous('Index')
                if (stringr:::str_detect(src,'niskin')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity','Nutrients'), values=c('green','blue','red'))
                if (stringr:::str_detect(src,'flntu')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity'), values=c('green','blue'))
                if (stringr:::str_detect(src,'eReefs')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity','Nutrients'), values=c('green','blue','red'))
                if (src=='') g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity'), values=c('green','blue'))
                g=g+facet_grid(~WaterBody, scales='free_y') + theme_bw()+
                    theme(strip.background=element_blank(), legend.key=element_blank(),
                          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())
                
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=4, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=4, units='in',dpi=300)
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                
            },logfile, msg=paste0('Aggregate summary plot for Subindicator/GBR worm (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                    
        }
    
    ## Mosaic plot for WaterBody/Subindicator
        if (WaterBodySubindicatorMosaic) {
            print(paste0('WaterBodySubindicatorMosaic '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.waterbody.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup 
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Subindicator, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0)) +
                    scale_x_discrete('',expand=c(0,0)) +
                    scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    facet_grid(~WaterBody) +
                    theme_classic() +
                    theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=0,hjust=0.5),
                          strip.text=element_text(size=12))
                xw=length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Subindicator))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g,  width=4*xw*1, height=xh*1, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.png'), g,  width=4*xw*1, height=xh*1, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Subindicator/WaterBody mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                  
        }

        ## Graphical summary for WaterBody/Indicator
        if (WaterBodyIndicatorWorm) {
            print(paste0('WaterBodyIndicatorWorm '))
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.waterbody.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.all = data.all %>% mutate(Grade=WQ_generateGrades(Mean, type=GradeType))
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
                geom_line(aes(group=Indicator)) + geom_point(aes(fill=Grade), shape=21, color='black', size=3) +
                    scale_x_continuous('Water year') +
                    scale_y_continuous('Index') +
                    scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'),limits=LETTERS[1:5]) 
                g=g+facet_grid(~WaterBody, scales='free_y') + theme_bw()+
                    theme(strip.background=element_blank(), legend.key=element_blank(),
                          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())
                
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=4, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=4, units='in',dpi=300)
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_indicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_indicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                
            },logfile, msg=paste0('Aggregate summary plot for Indicator/GBR worm (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                    
        }

        
        ## Mosaic plot for WaterBody/Indicator
        if (WaterBodyIndicatorMosaic) {
            ec.str_temp = '_with_enclosed_coastal'
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.waterbody.year',add.str,ec.str_temp,'.RData'))
            if (ec.str=='_without_enclosed_coastal') data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')            
            if (src %in% c('Satellite','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Indicator, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0))
                                if (src=='') {
                    seq_breaks <- function(by) {function(limits) limits[seq(1,length(limits), by=by)]}
                    g = g + scale_x_discrete('',breaks=seq_breaks(by=2),expand=c(0,0))
                    }
                if (src!='') g = g + scale_x_discrete('',expand=c(0,0))
                g= g + scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    #facet_grid(Indicator~WaterBody) +
                    theme_classic()
                                if (src=='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                              strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
                              strip.text=element_text(size=12))
                                }
                if (src!='') {
                    g=g+facet_grid(~WaterBody, scales='free_y') + theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                              strip.background=element_blank(),axis.text.x=element_text(angle=0,hjust=0.5),
                              strip.text=element_text(size=12))
                }
                xw=length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Indicator))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_indicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=2, units='in')
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_indicator.waterbody',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=2, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Indicator/GBR mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                  
        }


        
        ## Graphical summary for GBR/Measure===============================================
        if (GBRMeasureWorm) {
            print(paste0('GBRMeasureWorm '))
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.gbr.year',add.str,ec.str,'.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
                geom_line(aes(group=Measure)) + geom_point(aes(fill=Measure), shape=21, color='black', size=3) +
                    scale_x_continuous('Water year') +
                    scale_y_continuous('Index')
                if (stringr:::str_detect(src,'eReefs')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd','NOx'), labels=c('Chlorophyll','TSS','Secchi','NOx'), values=c('green','blue','orange','red'))
                } else if (src==''){
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd'), labels=c('Chlorophyll','TSS','Secchi'),values=c('green','blue','orange'))
                } else if (stringr:::str_detect(src,'niskin')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap','sd','NOx'), labels=c('Chlorophyll','TSS','Secchi','NOx'),values=c('green','blue','orange','red'))
                } else if (stringr:::str_detect(src,'flntu')) {
                    g=g+scale_fill_manual('Measure\n',breaks=c('chl','nap'), labels=c('Chlorophyll','TSS'),values=c('green','blue'))
                }
                g=g+theme_bw()+
                    theme(strip.background=element_blank(), legend.key=element_blank(),
                          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())
                
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_measure.gbr',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=5, height=5, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_measure.gbr',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=5, height=5, units='in',dpi=300)
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_measure.gbr',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_measure.gbr',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                
            },logfile, msg=paste0('Aggregate summary plot for Measure/GBR worm (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                    
        }


        ## Mosaic plot for GBR/Measure
        if (GBRMeasureMosaic) {
            print(paste0('GBRMeasureMosaic '))
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.measure.gbr.year',add.str,ec.str,'.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup 
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Label, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0)) 
                if (src=='') {
                    seq_breaks <- function(by) {function(limits) limits[seq(1,length(limits), by=by)]}
                    g = g + scale_x_discrete('',breaks=seq_breaks(by=2),expand=c(0,0))
                    }
                if (src!='') g = g + scale_x_discrete('',expand=c(0,0))
                g=g+scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    #facet_grid(Label~WaterBody) +
                    theme_classic()
                if (src=='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
                          strip.text=element_text(size=12))
                }
                if (src!='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=0,hjust=0.5),
                          strip.text=element_text(size=12))
                }
                xw=length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Measure))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.gbr',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=5, height=3, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.gbr',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=5, height=3, units='in',dpi=300)
                ##ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,'.pdf'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',device=cairo_pdf)
                ##ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_measure.zone',add.str,'.png'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Measure/Zone mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                                    
        }

        
        ## Graphical summary for GBR/Subindicator
        if (GBRSubindicatorWorm) {
            print(paste0('GBRSubindicatorWorm '))
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.gbr.year',add.str,ec.str,'.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
                geom_line(aes(group=Subindicator)) + geom_point(aes(fill=Subindicator), shape=21, color='black', size=3) +
                    scale_x_continuous('Water year') +
                    scale_y_continuous('Index')
                if (stringr:::str_detect(src,'niskin')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity','Nutrients'), values=c('green','blue','red'))
                if (stringr:::str_detect(src,'flntu')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity'), values=c('green','blue'))
                if (stringr:::str_detect(src,'eReefs')) g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity','Nutrients'), values=c('green','blue','red'))
                if (src=='') g=g+scale_fill_manual('Subindicator\n',breaks=c('Productivity','Water Clarity'), values=c('green','blue'))
                g=g+theme_bw()+
                    theme(strip.background=element_blank(), legend.key=element_blank(),
                          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())
                
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=5, height=5, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=5, height=5, units='in',dpi=300)
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                
            },logfile, msg=paste0('Aggregate summary plot for Subindicator/GBR worm (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                    
        }

        ## Mosaic plot for GBR/Subindicator
        if (GBRSubindicatorMosaic) {
            print(paste0('GBRSubindicatorMosaic '))
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.subindicator.gbr.year',add.str,ec.str,'.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>% 
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup 
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Subindicator, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0)) +
                    scale_x_discrete('',expand=c(0,0)) +
                    scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    #facet_grid(Subindicator~WaterBody) +
                    theme_classic() +
                    theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                          strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
                          strip.text=element_text(size=12))
                xw=length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Subindicator))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_subindicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=(xw*1)+1, height=(xh*1)+1, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Subindicator/Zone mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                  
        }

        
        ## Graphical summary for GBR/Indicator
        if (GBRIndicatorWorm) {
            print(paste0('GBRIndicatorWorm '))
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.gbr.year',add.str,ec.str,'.RData'))
            if (src %in% c('','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.all = data.all %>% mutate(Grade=WQ_generateGrades(Mean, type=GradeType))
            WQ_tryCatch(
            {
                g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
                    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
                annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
                geom_line(aes(group=Indicator)) + geom_point(aes(fill=Grade), shape=21, color='black', size=3) +
                    scale_x_continuous('Water year') +
                    scale_y_continuous('Index') +
                    scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24'),limits=LETTERS[1:5]) 
                g=g+theme_bw()+
                    theme(strip.background=element_blank(), legend.key=element_blank(),
                          panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank())
                
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=5, height=5, units='in',device=cairo_pdf)
                ggsave(file=paste0('Report/Figures/simple_',gsub('/','',src),'_',index,'.',temporal,'_indicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=5, height=5, units='in',dpi=300)
                write.csv(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_indicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.csv'), row.names=FALSE,quote=FALSE)
                save(data.all, file=paste0('Report/Tables/Grades_',gsub('/','',src),'_',index,'.',temporal,'_indicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.RData'))
                
            },logfile, msg=paste0('Aggregate summary plot for Indicator/GBR worm (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                    
        }

        
        ## Mosaic plot for GBR/Indicator
        if (GBRIndicatorMosaic) {
            load(file=paste0(src,'data/aggregated/',index,'/',temporal,'/data.all.indicator.gbr.year',add.str,ec.str,'.RData'))
            if (src %in% c('Satellite','eReefs/','eReefs926/')) {
                data.all = data.all  %>% ungroup %>%
                    mutate(Complete = ifelse((waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
                    filter(Complete=='Full')
            }
            data.mosaic = data.all %>% ungroup
            if (GradeType=='MMP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.mmp)
            if (GradeType=='GHHP') data.mosaic = data.mosaic %>% mutate(Grade=Grade.ghhp)
            if (GradeType=='Uniform') data.mosaic = data.mosaic %>% mutate(Grade=Grade.Uniform)
            WQ_tryCatch(
            {
                g = ggplot(data.mosaic, aes(y=Indicator, x=as.factor(waterYear))) +
                    geom_tile(aes(fill=Grade), color='white', size=0.5) +
                    scale_y_discrete('',expand=c(0,0))
                                if (src=='') {
                    seq_breaks <- function(by) {function(limits) limits[seq(1,length(limits), by=by)]}
                    g = g + scale_x_discrete('',breaks=seq_breaks(by=2),expand=c(0,0))
                    }
                if (src!='') g = g + scale_x_discrete('',expand=c(0,0))
                g= g + scale_fill_manual('Grade\n',breaks=LETTERS[1:5], values=report.card.colors(5), limits=LETTERS[1:5]) +
                    #facet_grid(Indicator~WaterBody) +
                    theme_classic()
                                if (src=='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                              strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),
                              strip.text=element_text(size=12))
                                }
                if (src!='') {
                    g=g+theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
                              strip.background=element_blank(),axis.text.x=element_text(angle=0,hjust=0.5),
                              strip.text=element_text(size=12))
                }
                xw=length(unique(data.mosaic$waterYear))
                xh=length(unique(data.mosaic$Indicator))
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_indicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=2, units='in')
                ggsave(file=paste0('Report/Figures/mosaic_',gsub('/','',src),'_',index,'.',temporal,'_indicator.gbr',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=2, units='in',dpi=300)
            },logfile, msg=paste0('Aggregate summary plot for Indicator/GBR mosaic (Source=',gsub('/','',src),', Index=',idx,')'), return=TRUE)                  
        }


        
        ## Graphical summary for Pathway B
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


        
    } # index types
    print('End')
}

sink()


#Compare sources
foreach(index = c('Binary','fsMAMP','fsMAMP4')) %dopar% {
    temporal='Annual'
    src=''
    fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
    data.all <- NULL
    for (f in fs) {
        load(f)
        data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='Satellite'))
    }
    src='eReefs/'
    fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
    for (f in fs) {
        load(f)
        data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='eReefs'))
    }
    src='eReefs926/'
    fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
    for (f in fs) {
        load(f)
        data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='eReefs926'))
    }
    src='niskin/'
    fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
    for (f in fs) {
        load(f)
        data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='AIMS insitu'))
    }
    src='flntu/'
    fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern='data.av.measure.zone.year.*',full.names=TRUE)
    for (f in fs) {
        load(f)
        data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Source='AIMS flntu'))
    }        
    
                                        #load('data/data.idx.av.sub1.RData')
                                        #d=data.idx.av.sub1 %>% filter(Index=='fsMAMP') %>% select(Indicator,Subindicator,Measure,Region,WaterBody,waterYear,Mean)
                                        #data.all = rbind(data.all,
                                        #                 data.frame(d, Source='AIMS insitu'))
    
    data.all = data.all %>% mutate(Grade.MMP=WQ_generateGrades(Mean),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'),Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'))
    if (GradeType=='MMP') data.all = data.all %>% mutate(Grade=Grade.MMP)
    if (GradeType=='GHHP') data.all = data.all %>% mutate(Grade=Grade.GHHP)
    if (GradeType=='Uniform') data.all = data.all %>% mutate(Grade=Grade.Uniform)
    
    data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
    data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
    if(!include_enclosedcoastal) data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')
    ## Mark the first and last years of Satellite, eReefs and eReefs926 as an end
    data.all = data.all %>% group_by(Source) %>% ungroup %>%
        mutate(Complete = ifelse(Source %in% c('Satellite','eReefs','eReefs926') & (waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
        ungroup
    
    measure='chl'
    unitLabel='Chlorophyll-a'
    g=ggplot(data.all %>% filter(Measure==measure,Complete=='Full'), aes(y=Mean, x=waterYear)) +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
    annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
    annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
    annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
    geom_line(aes(group=Source)) +
        geom_point(aes(fill=Source), shape=21, color='black', size=3) +
        scale_x_continuous('Water year') +
        scale_y_continuous('Index') +
        scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), limits=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu','AIMS flntu'),values=c('green','red','blue','orange','purple')) +
        facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
        theme(strip.background=element_blank(), legend.key=element_blank(),panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())+
        ggtitle(paste0('Measure=',unitLabel,', Index=',index,', Temporal=', temporal)) +
        guides(fill=guide_legend(override.aes=list(color='black', size=5)))
    ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone',add.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in')
    ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone',add.str,'_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)

    if (include_nap) {
        measure='nap'
        unitLabel='TSS'
        g=ggplot(data.all %>% filter(Measure==measure, Complete=='Full'), aes(y=Mean, x=waterYear)) +
            annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
        annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
        geom_line(aes(group=Source)) +
            geom_point(aes(fill=Source), shape=21, color='black', size=3) +
            scale_x_continuous('Water year') +
            scale_y_continuous('Water Quality Index') +
            scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), limits=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu','AIMS flntu'),values=c('green','red','blue','orange','purple')) +
            facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
            theme(strip.background=element_blank(), legend.key=element_blank(),panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())+
            ggtitle(paste0('Measure=',unitLabel,', Index=',index,', Temporal=', temporal)) +
            guides(fill=guide_legend(override.aes=list(color='black', size=5)))
        ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone',add.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in')
        ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone',add.str,'_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)
    }
    if (include_sd) {
        measure='sd'
        unitLabel='Secchi Depth'
        g=ggplot(data.all %>% filter(Measure==measure, Complete=='Full'), aes(y=Mean, x=waterYear)) +
            annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
        annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
        geom_line(aes(group=Source)) +
            geom_point(aes(fill=Source), shape=21, color='black', size=3) +
            scale_x_continuous('Water year') +
            scale_y_continuous('Water Quality Index') +
            scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), limits=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu','AIMS flntu'),values=c('green','red','blue','orange','purple')) +
            facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
            theme(strip.background=element_blank(), legend.key=element_blank(),panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())+
            ggtitle(paste0('Measure=',unitLabel,', Index=',index,', Temporal=', temporal)) +
            guides(fill=guide_legend(override.aes=list(color='black', size=5)))
        ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone',add.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in')
        ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone',add.str,'_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)
    }
    if(include_NOx) {
        measure='NOx'
        unitLabel='NOx'
        g=ggplot(data.all %>% filter(Measure==measure, Complete=='Full'), aes(y=Mean, x=waterYear)) +
             annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
        annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
        annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
        geom_line(aes(group=Source)) +
            geom_point(aes(fill=Source), shape=21, color='black', size=3) +
            scale_x_continuous('Water year') +
            scale_y_continuous('Water Quality Index') +
            scale_fill_manual('Source',breaks=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'), limits=c('Satellite','eReefs','eReefs926','AIMS insitu','AIMS flntu'),labels=c('Remote sensing','eReefs','eReefs926','AIMS insitu','AIMS flntu'),values=c('green','red','blue','orange','purple')) +
            facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
            theme(strip.background=element_blank(), legend.key=element_blank(),panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())+
            ggtitle(paste0('Measure=',unitLabel,', Index=',index,', Temporal=', temporal)) +
            guides(fill=guide_legend(override.aes=list(color='black', size=5)))
        ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone',add.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in')
        ggsave(file=paste0('Report/Figures/All_indicies_',index,'.',temporal,'_measure.',measure,'_zone',add.str,'_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)
    }
}





#Compare indices
## temporal='Annual'
## GradeType='MMP'
## include_sd = TRUE
## include_NOx = TRUE
## include_nap = TRUE

data.all <- NULL
for (src in c('niskin/','flntu/','','eReefs/','eReefs926/')) {
    for (index in c('Binary','MAMP','fsMAMP','fsMAMP4','Exceed','Max_Duration')) {
        fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.measure.zone.year.*',add.str,'.RData'),full.names=TRUE)
        if (add.str=='') {# exclude the ones with explicit excludes
            fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
        }
        if (add.str=='noNOx') {
            fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
        }
        for (f in fs) {
            load(f)
            data.all <- rbind(data.all, data.frame(data.av.measure.zone.year,Index=index,
                                                   Source=ifelse(src=='niskin/', 'AIMS insitu',
                                                          ifelse(src=='flntu/', 'AIMS FLNTU',
                                                          ifelse(src=='', 'Satellite',
                                                          ifelse(src=='eReefs/', 'eReefs','eReefs926'))))))
        }
    }
}
data.all = data.all %>% mutate(Grade.MMP=WQ_generateGrades(Mean),Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'),Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'))
if (GradeType=='MMP') data.all = data.all %>% mutate(Grade=Grade.MMP)
if (GradeType=='GHHP') data.all = data.all %>% mutate(Grade=Grade.GHHP)
if (GradeType=='Uniform') data.all = data.all %>% mutate(Grade=Grade.Uniform)
gradeMids = WQ_gradeMids(type=GradeType)
gradeBoundaries = WQ_gradeBoundaries(type=GradeType)
add.str=''
if (!include_sd) add.str = paste0(add.str, 'noSD')
if (!include_NOx) add.str = paste0(add.str, 'noNOx')
if (!include_nap) add.str = paste0(add.str, 'nonap')

data.all$WaterBody= factor(data.all$WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'))
data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
if(!include_enclosedcoastal) data.all = data.all %>% filter(WaterBody!='Enclosed Coastal')
## Mark the first and last years of Satellite, eReefs and eReefs926 as an end
data.all = data.all %>% group_by(Source) %>% ungroup %>% 
    mutate(Complete = ifelse(Source %in% c('Satellite','eReefs','eReefs926') & (waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
    ungroup
agg.all = data.all

for (src in c('AIMS insitu', 'AIMS FLNTU', 'Satellite', 'eReefs', 'eReefs926')) {
    print(src)
    ms=c('chl','nap','sd','NOx')
    if (!include_sd) ms = ms[ms!='sd']
    if (!include_nap) ms = ms[ms!='nap']
    if (!include_NOx) ms = ms[ms!='NOx']
    
    ## if (include_sd & include_NOx) ms = c('chl', 'nap', 'sd', 'NOx')
    ## if (include_sd & !include_NOx) ms = c('chl', 'nap', 'sd')
    ## if (!include_sd & include_NOx) ms = c('chl', 'nap', 'NOx')
    ## if (!include_sd & !include_NOx) ms = c('chl', 'nap')
    for (measure in ms) {
        print(measure)
        unitLabel = (measures %>% filter(Measure==measure))$Label
        dat = agg.all %>% filter(Measure==measure,Source==src,Complete=='Full')
        if (nrow(dat)>0) {
            g=ggplot(dat, aes(y=Mean, x=waterYear)) +
                annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[2],ymax=gradeBoundaries[1], fill='#00734D10', size=0.2, color='grey90') +
            annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[3],ymax=gradeBoundaries[2], fill='#B0D23510', size=0.2, color='grey90') +
            annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[4],ymax=gradeBoundaries[3], fill='#F0C91810', size=0.2, color='grey90') +
            annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[5],ymax=gradeBoundaries[4], fill='#F4772110', size=0.2, color='grey90') +
            annotate(geom='rect', xmin=-Inf,xmax=Inf,ymin=gradeBoundaries[6],ymax=gradeBoundaries[5], fill='#ED1C2410', size=0.2, color='grey90') +
            annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
            annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
            geom_line(aes(group=Index)) +
                geom_point(aes(fill=Index), shape=21, color='black', size=3) +
                scale_x_continuous('Water year') +
                scale_y_continuous('Index') +
                scale_fill_manual('Index',breaks=c('Binary','fsMAMP','fsMAMP4'), limits=c('Binary','fsMAMP','fsMAMP4'), labels=c('Binary','fsMAMP','fsMAMP4'),values=c('green','red','blue')) +
                facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
                theme(strip.background=element_blank(), legend.key=element_blank(),panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())+
                ggtitle(paste0('Measure=',unitLabel,', Source=',src,', Temporal=', temporal)) +
                guides(fill=guide_legend(override.aes=list(color='black', size=5)))
            ggsave(file=paste0('Report/Figures/All_sources_',src,'.',temporal,'_measure.',measure,'_zone',add.str,ec.str,'_Grade_',GradeType,'.pdf'), g, width=10, height=10, units='in')
            ggsave(file=paste0('Report/Figures/All_sources_',src,'.',temporal,'_measure.',measure,'_zone',add.str,ec.str,'_Grade_',GradeType,'.png'), g, width=10, height=10, units='in',dpi=300)
        }
    }
}



## Now compare Water Quality Index based on different combinations of Chl, TSS, SD and NOx
data.all = NULL
index='fsMAMP'
temporal='Annual'
src='eReefs/'
for (add.str in c('noNOxnonap', 'noSDnoNOx','noNOx','')) {
    fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.indicator.zone.year.A.*',add.str,'.RData'),full.names=TRUE)
    if (add.str=='') {# exclude the ones with explicit excludes
        fs=fs[-1*grep('.*(noSD|nonap|noNOx).*',fs,perl=TRUE)]
    }
    if (add.str=='noNOx') {
        fs=fs[-1*grep('.*(noSDnoNOx).*',fs,perl=TRUE)]
    }
    for (f in fs) {
        load(f)
        data.all <- rbind(data.all, data.frame(data.av.indicator.zone.year.A,Index=index,
                                               Excludes = add.str,
                                               Source=ifelse(src=='niskin/', 'AIMS insitu',
                                                      ifelse(src=='flntu/', 'AIMS FLNTU',
                                                      ifelse(src=='', 'Satellite',
                                                      ifelse(src=='eReefs/', 'eReefs','eReefs926'))))))
    }
}
data.all = data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
data.all$WaterBody= factor(data.all$WaterBody, levels=c('Open Coastal','Midshelf','Offshore'))
data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))

data.all = data.all %>% group_by(Source) %>% ungroup %>% 
    mutate(Complete = ifelse(Source %in% c('Satellite','eReefs','eReefs926') & (waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
    ungroup

data.all = data.all %>%
    mutate(Includes=ifelse(Excludes=='','Chl,(TSS,SD),NOx', ifelse(Excludes=='noNOxnonap', 'Chl,SD', ifelse(Excludes=='noSDnoNOx', 'Chl,TSS','Chl,(TSS,SD)')))) %>%
    filter(Complete=='Full')

g=ggplot(data.all, aes(y=Mean, x=waterYear)) +
    annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') +
annotate(geom='rect', xmin=2009.5,xmax=2011.5,ymin=0,ymax=1, fill='#0000ff30') + 
geom_line(aes(group=Includes)) +
                geom_point(aes(fill=Includes), shape=21, color='black', size=3) +
    scale_x_continuous('Water year') +
    scale_y_continuous('Index') +
#    scale_fill_manual('Index',breaks=c('Binary','fsMAMP','fsMAMP4'), limits=c('Binary','fsMAMP','fsMAMP4'), labels=c('Binary','fsMAMP','fsMAMP4'),values=c('green','red','blue')) +
    facet_grid(Region~WaterBody, scales='free_y') + theme_bw() +
    theme(strip.background=element_blank(), legend.key=element_blank(),panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())+
    ggtitle(paste0('Index=',index,', Source=',gsub('/','',src),', Temporal=', temporal)) +
    guides(fill=guide_legend(override.aes=list(color='black', size=5)))
ggsave(file=paste0('Report/Figures/All_excludes_index_',index,'_',gsub('/','',src),'.',temporal,'_indicator_zone_.pdf'), g, width=10, height=10, units='in')
ggsave(file=paste0('Report/Figures/All_excludes_index_',index,'_',gsub('/','',src),'.',temporal,'_indicator_zone_.png'), g, width=10, height=10, units='in',dpi=300)






## Now compare different GradeTypes
data.all = NULL
index='fsMAMP'
temporal='Annual'
src='eReefs/'
#add.str = 'noNOxnonap'

fs = list.files(path=paste0(src,'data/aggregated/',index,'/',temporal),pattern=paste0('data.av.indicator.zone.year.A.*',add.str,'.RData'),full.names=TRUE)
for (f in fs) {
    load(f)
    data.all <- rbind(data.all, data.frame(data.av.indicator.zone.year.A,Index=index,
                                           Source=ifelse(src=='niskin/', 'AIMS insitu',
                                                  ifelse(src=='flntu/', 'AIMS FLNTU',
                                                  ifelse(src=='', 'Satellite',
                                                  ifelse(src=='eReefs/', 'eReefs','eReefs926'))))))
}
data.all = data.all %>% mutate(
                            Mean=round(Mean,3),
                            Grade.MMP=WQ_generateGrades(Mean,type='MMP'),
                            Grade.Uniform=WQ_generateGrades(Mean,type='Uniform'),
                            Grade.GHHP=WQ_generateGrades(Mean,type='GHHP'))
data.all = data.all %>% filter(WaterBody!='Enclosed Coastal') %>% droplevels
data.all$WaterBody= factor(data.all$WaterBody, levels=c('Open Coastal','Midshelf','Offshore'))
data.all$Region=factor(data.all$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))

data.all = data.all %>% group_by(Source) %>% ungroup %>% 
    mutate(Complete = ifelse(Source %in% c('Satellite','eReefs','eReefs926') & (waterYear==max(waterYear) | waterYear==min(waterYear)), 'Partial','Full')) %>%
    ungroup

data.all = data.all %>%
    filter(Complete=='Full')
#data.gather=data.all %>%
#    gather(key=GradeType, value=Grade, Grade.MMP, Grade.Uniform,Grade.GHHP)

dat = data.all %>% ungroup %>% 
    dplyr:::select(Region,WaterBody,waterYear,Score=Mean,Grade.MMP,Grade.Uniform,Grade.GHHP) %>%
    mutate(Grade.MMP=TableGradeColors(Grade.MMP),
           Grade.Uniform=TableGradeColors(Grade.Uniform),
           Grade.GHHP=TableGradeColors(Grade.GHHP),
           waterYear = as.integer(waterYear)) %>%
    as.data.frame
dat$WaterBody= factor(dat$WaterBody, levels=c('Open Coastal','Midshelf','Offshore'))
dat$Region=factor(dat$Region, levels=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'))
dat = dat %>% arrange(Region,WaterBody,waterYear)
library(xtable)
ATR = list(pos=list(0,nrow(dat)),
           command=c('
%\\sf
\\LTcapwidth=\\linewidth
\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}
\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}
\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}
#\\begin{table}[htbp]
{
\\small
\\begin{longtable}{llccccc}
\\caption{Score and associated Grades based on three different grade control charts (Uniform, MMP and GHHP) for eReefs data indexed via fsMAMP and aggregated to Zone/Indicator level.}\\label{tab:GradeTypeComparisons}\\\\ 
\\toprule
Region&Water Body & Water Year & Score & Grade (MMP) & Grade (Uniform) & Grade (GHHP)\\\\
\\midrule
\\endfirsthead

\\multicolumn{7}{l}{..continued from previous page}\\\\
\\toprule
Region&Water Body & Water Year & Score & Grade (MMP) & Grade (Uniform) & Grade (GHHP)\\\\
\\midrule \n
\\endhead \n
\\bottomrule\n
\\endfoot\n',
'\\hrule
\\end{longtable}
#\\end{table}
}'))
p = print(xtable(dat, caption='',digits=c(1,1,1,1,3,1,1,1)), include.rownames=FALSE, include.colnames=FALSE,
          only.contents=TRUE,add.to.row=ATR,hline.after=-1,
          booktabs=TRUE, comment=FALSE,sanitize.text.function=function(x) x)
writeLines(p, con='Report/Tables/GradeTypeComparison.tex')



