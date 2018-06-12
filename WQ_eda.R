library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
require(foreach)
require(doParallel)
require(broom)
require(sp)
require(viridis)

arrange_within_wateryear = function(dt) {
    yr = format(dt, '%Y')
    a = as.numeric(format(dt,'%j')) - as.numeric(format(as.Date(paste0(yr,'-10-01')),'%j'))
    scales:::rescale((ifelse(a<0,365+a,a))/365, from=c(0,1), to =c(-0.4,0.4))
    }

plottingpars = rbind(
    data.frame(Region='Burnett Mary', WaterBody = 'Enclosed Coastal', Src='', pch=1.5),
    data.frame(Region='Burnett Mary', WaterBody = 'Enclosed Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Burnett Mary', WaterBody = 'Enclosed Coastal', Src='eReefs926/', pch=1),
    data.frame(Region='Burnett Mary', WaterBody = 'Midshelf', Src='', pch=0.5),
    data.frame(Region='Burnett Mary', WaterBody = 'Midshelf', Src='eReefs/', pch=1),
    data.frame(Region='Burnett Mary', WaterBody = 'Midshelf', Src='eReefs926/', pch=1),
    data.frame(Region='Burnett Mary', WaterBody = 'Offshore', Src='', pch=0.1),
    data.frame(Region='Burnett Mary', WaterBody = 'Offshore', Src='eReefs/', pch=1),
    data.frame(Region='Burnett Mary', WaterBody = 'Offshore', Src='eReefs926/', pch=1),
    data.frame(Region='Burnett Mary', WaterBody = 'Open Coastal', Src='', pch=1),
    data.frame(Region='Burnett Mary', WaterBody = 'Open Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Burnett Mary', WaterBody = 'Open Coastal', Src='eReefs926/', pch=1),

    data.frame(Region='Cape York', WaterBody = 'Enclosed Coastal', Src='', pch=0.1),
    data.frame(Region='Cape York', WaterBody = 'Enclosed Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Cape York', WaterBody = 'Enclosed Coastal', Src='eReefs926/', pch=1),
    data.frame(Region='Cape York', WaterBody = 'Midshelf', Src='', pch=0.1),
    data.frame(Region='Cape York', WaterBody = 'Midshelf', Src='eReefs/', pch=1),
    data.frame(Region='Cape York', WaterBody = 'Midshelf', Src='eReefs926/', pch=1),
    data.frame(Region='Cape York', WaterBody = 'Offshore', Src='', pch=0.1),
    data.frame(Region='Cape York', WaterBody = 'Offshore', Src='eReefs/', pch=1),
    data.frame(Region='Cape York', WaterBody = 'Offshore', Src='eReefs926/', pch=1),
    data.frame(Region='Cape York', WaterBody = 'Open Coastal', Src='', pch=0.1),
    data.frame(Region='Cape York', WaterBody = 'Open Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Cape York', WaterBody = 'Open Coastal', Src='eReefs926/', pch=1),

    data.frame(Region='Dry Tropics', WaterBody = 'Enclosed Coastal', Src='', pch=0.1),
    data.frame(Region='Dry Tropics', WaterBody = 'Enclosed Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Dry Tropics', WaterBody = 'Enclosed Coastal', Src='eReefs926/', pch=1),
    data.frame(Region='Dry Tropics', WaterBody = 'Midshelf', Src='niskin/', pch=1),
    data.frame(Region='Dry Tropics', WaterBody = 'Midshelf', Src='', pch=0.1),
    data.frame(Region='Dry Tropics', WaterBody = 'Midshelf', Src='eReefs/', pch=0.5),
    data.frame(Region='Dry Tropics', WaterBody = 'Midshelf', Src='eReefs926/', pch=0.5),
    data.frame(Region='Dry Tropics', WaterBody = 'Offshore', Src='', pch=0.1),
    data.frame(Region='Dry Tropics', WaterBody = 'Offshore', Src='eReefs/', pch=1),
    data.frame(Region='Dry Tropics', WaterBody = 'Offshore', Src='eReefs926/', pch=1),
    data.frame(Region='Dry Tropics', WaterBody = 'Open Coastal', Src='', pch=0.1),
    data.frame(Region='Dry Tropics', WaterBody = 'Open Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Dry Tropics', WaterBody = 'Open Coastal', Src='eReefs926/', pch=1),

    data.frame(Region='Fitzroy', WaterBody = 'Enclosed Coastal', Src='', pch=1),
    data.frame(Region='Fitzroy', WaterBody = 'Enclosed Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Fitzroy', WaterBody = 'Enclosed Coastal', Src='eReefs926/', pch=1),
    data.frame(Region='Fitzroy', WaterBody = 'Midshelf', Src='', pch=0.1),
    data.frame(Region='Fitzroy', WaterBody = 'Midshelf', Src='eReefs/', pch=1),
    data.frame(Region='Fitzroy', WaterBody = 'Midshelf', Src='eReefs926/', pch=1),
    data.frame(Region='Fitzroy', WaterBody = 'Offshore', Src='', pch=0.1),
    data.frame(Region='Fitzroy', WaterBody = 'Offshore', Src='eReefs/', pch=1),
    data.frame(Region='Fitzroy', WaterBody = 'Offshore', Src='eReefs926/', pch=1),
    data.frame(Region='Fitzroy', WaterBody = 'Open Coastal', Src='', pch=0.1),
    data.frame(Region='Fitzroy', WaterBody = 'Open Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Fitzroy', WaterBody = 'Open Coastal', Src='eReefs926/', pch=1),

    data.frame(Region='Mackay Whitsunday', WaterBody = 'Enclosed Coastal', Src='', pch=1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Enclosed Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Enclosed Coastal', Src='eReefs926/', pch=1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Midshelf', Src='', pch=0.1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Midshelf', Src='eReefs/', pch=1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Midshelf', Src='eReefs926/', pch=1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Offshore', Src='', pch=0.1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Offshore', Src='eReefs/', pch=1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Offshore', Src='eReefs926/', pch=1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Open Coastal', Src='', pch=0.1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Open Coastal', Src='eReefs/', pch=1),
    data.frame(Region='Mackay Whitsunday', WaterBody = 'Open Coastal', Src='eReefs926/', pch=1),

    data.frame(Region='Wet Tropics', WaterBody = 'Enclosed Coastal', Src='', pch=0.1),
    data.frame(Region='Wet Tropics', WaterBody = 'Enclosed Coastal', Src='eReefs/', pch=2),
    data.frame(Region='Wet Tropics', WaterBody = 'Enclosed Coastal', Src='eReefs926/', pch=2),
    data.frame(Region='Wet Tropics', WaterBody = 'Midshelf', Src='', pch=0.1),
    data.frame(Region='Wet Tropics', WaterBody = 'Midshelf', Src='eReefs/', pch=2),
    data.frame(Region='Wet Tropics', WaterBody = 'Midshelf', Src='eReefs926/', pch=2),
    data.frame(Region='Wet Tropics', WaterBody = 'Offshore', Src='', pch=0.1),
    data.frame(Region='Wet Tropics', WaterBody = 'Offshore', Src='eReefs/', pch=2),
    data.frame(Region='Wet Tropics', WaterBody = 'Offshore', Src='eReefs926/', pch=2),
    data.frame(Region='Wet Tropics', WaterBody = 'Open Coastal', Src='niskin/', pch=2),
    data.frame(Region='Wet Tropics', WaterBody = 'Open Coastal', Src='', pch=0.1),
    data.frame(Region='Wet Tropics', WaterBody = 'Open Coastal', Src='eReefs/', pch=2),
    data.frame(Region='Wet Tropics', WaterBody = 'Open Coastal', Src='eReefs926/', pch=2)
)

source('WQ_functions.R')

edaYearMeasureViolin=TRUE
edaMonthMeasureViolin=TRUE
edaSpatialYear=TRUE
edaSpatialYearlast=TRUE
edaSpatialYearlastB=TRUE

edaidxYearMeasureViolin=TRUE
edaidxMonthMeasureViolin=TRUE
edaidxSpatialYear=TRUE
edaidxSpatialYearlast=TRUE


registerDoParallel(cores=10)

#yscale='natural'
yscale='log'


load('data/GIS/layers.RData')
load('data/GIS/Polys.RData')
qld.df=tidy(qld)
reefs.df=tidy(reefs)
Polys.df=tidy(Polys)


spatial = read.csv('parameters/spatial.csv', strip.white=TRUE)

unlink('log/eda.log')

## We are going to setup a metadata record file that has:
## Filename, Title, Description, Attribution, Licencing, Location, Latitude, Longitude, Photo Gallery
meta.file = data.frame()
unlink('meta.file_eda.csv')
meta.file = 
    data.frame(Filename=NA,
               Title=NA,
               Description=NA,
               Attribution=NA,
               Licencing=NA,
               Location=NA,
               Latitude=NA,
               Longitude=NA,
               PhotoGallery=NA)
write.table(meta.file[0,], file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=TRUE)


## Start with a summary figure aggregated across all zones
                                        #This takes a long time to run...
src='eReefs/'
fs = list.files(path=paste0(src,'data/processed'),pattern='data_.*.RData',full.names=TRUE)
dat = NULL
load_and_resample = function(x) {
    print(x)
    load(x)
    if (nrow(data)>100000)  data= data%>% sample_n(10000)
    data
}
dat=lapply(fs,load_and_resample)
dat=do.call(bind_rows,dat)
for (meas in c('chl','nap','sd','NOx')) {
    dat1 = dat %>% filter(Measure==meas,Value>0) %>% mutate(year = waterYear + arrange_within_wateryear(Date)) %>%
        arrange(-Latitude) %>% mutate(Region=factor(Region, levels=unique(Region), labels=gsub(' ','\n',unique(Region))), WaterBody=factor(WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore')))
    GLs = dat1 %>% group_by(Region,WaterBody) %>% summarize(GL=unique(GL),Region.1=as.numeric(unique(Region))-0.5, Region.2=as.numeric(unique(Region))+0.5)
    unitsLabel=unique(dat1$UnitsLabel)
    g=ggplot(dat1, aes(y=Value, x=factor(Region))) +
        geom_blank()+
        geom_rect(data=GLs, aes(xmin=Region.1, y=GL,xmax=Region.2, ymin=GL, ymax=GL*2), fill=WQ_reportCardColors[5], alpha=0.2) +
        geom_rect(data=GLs, aes(xmin=Region.1, y=GL,xmax=Region.2, ymin=GL, ymax=GL*4), fill=WQ_reportCardColors[5], alpha=0.1) +
        geom_rect(data=GLs, aes(xmin=Region.1, y=GL,xmax=Region.2, ymin=GL, ymax=GL*0.5), fill=WQ_reportCardColors[1], alpha=0.2) +
        geom_rect(data=GLs, aes(xmin=Region.1, y=GL,xmax=Region.2, ymin=GL, ymax=GL*0.25), fill=WQ_reportCardColors[1], alpha=0.1) +
        facet_grid(~WaterBody)
    if (src == 'niskin/') {g=g+geom_point(aes(x=as.numeric(Region) + arrange_within_wateryear(Date), color=Season),size=3, alpha=1,show.legend=TRUE)
    }else g=g + geom_point(aes(x=as.numeric(Region) + arrange_within_wateryear(Date), color=Season),size=0.01, alpha=1,show.legend=TRUE) 
    g=g+geom_violin(aes(x=Region), alpha=0.5) +
        geom_segment(data=GLs, aes(x=Region.1, y=GL,xend=Region.2, yend=GL), linetype='dashed') +
        geom_segment(data=GLs, aes(x=Region.1, y=GL*2,xend=Region.2, yend=GL*2), linetype='dashed',color=WQ_reportCardColors[5]) +
        geom_segment(data=GLs, aes(x=Region.1, y=GL*0.5,xend=Region.2, yend=GL*0.5), linetype='dashed',color=WQ_reportCardColors[1]) +
        scale_x_discrete('') +
        ggtitle(paste0('Measure=',unitsLabel)) +
        theme_classic() +
        theme(axis.line.x=element_line(), axis.line.y=element_line(), plot.title=element_text(hjust=0),
              legend.position=c(1,1.25),legend.justification=c(1,1),legend.title=element_blank(), legend.direction='horizontal',
              strip.background=element_blank()) +
        guides(color=guide_legend(override.aes=list(size=3)))
    if (yscale=='log') g = g + scale_y_log10(breaks=scales:::log_breaks(10,base=2), labels=function(x) sprintf('%2.2f',x))
    ggsave(filename=paste0('data/eda/eda.year.',meas,'_',gsub('/','',src),'_',yscale,'.pdf'),g,width=17, height=4, device=cairo_pdf)
    ggsave(filename=paste0('data/eda/eda.year.',meas,'_',gsub('/','',src),'_',yscale,'.png'),g,width=17, height=4,dpi=300)
    ggsave(filename=paste0('data/eda/eda.year.',meas,'_',gsub('/','',src),'_',yscale,'.jpg'),g,width=17, height=4,dpi=300)
    meta.file =
        data.frame(Filename=paste0('data/eda/eda.year.',meas,'_',gsub('/','',src),'_',yscale,'.jpg'),
                   Title=paste0('Exploratory data analysis violin plots for ',src,' ', meas,' conditional on Region and Water body over the ',paste(range(dat1$waterYear), collapse='-'),' water years.'),
                   Description=paste0('Observed (logarithmic axis with violin plot overlay) ',src,' ',meas,' data for each Region and Water body over the ',paste(range(dat1$waterYear), collapse='-'),' water years. Observations are ordered over time and colored conditional on season as Wet (blue symbols) and Dry (red symbols). Blue smoother represents Generalized Additive Mixed Model within a water year and purple line represents average within the water year. Horizontal red, black and green dashed lines denote the twice threshold, threshold and half threshold values respectively. Red and green background shading indicates the range (10% shade: x4,/4; 30% shade: x2,/2) above and below threshold respectively'),
                   Attribution='Murray Logan (AIMS)',
                   Licencing='CC-BY',
                   Location='Great Barrier Reef',
                   Latitude=NA,
                   Longitude=NA,
                   PhotoGallery='EDA.observed')
    write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
    system(paste0('cd data/eda/; convert -density 200 "eda.year.',meas,'_',gsub('/','',src),'_',yscale,'.pdf" "eda.year.',meas,'_',gsub('/','',src),'_',yscale,'_lowres.pdf"'))
}

    
foreach(src=c('niskin/','flntu/','','eReefs/','eReefs926/')) %do% {
    ## Explore some real data
    ## Explore the variability in the actual data
    fs = list.files(path=paste0(src,'data/processed'),pattern='data_.*.RData',full.names=TRUE)
    pb <- txtProgressBar(max=length(fs), style=3)
    ii=0
    #fs = fs[grep('(Dry Tropics__Midshelf)|(Wet Tropics__Open Coastal)',fs)]
    #fs = fs[grep('(Dry Tropics___Midshelf)|(Wet Tropics___Open Coastal)',fs)]
                                        #fs
    id = c(10,24,34,48,58,72,82,96)
    if (src=='niskin/') id=c(2,6,9,11,15,18,20,24,27,29,33,36)
    if (src=='flntu/') id = c(2,9,11,18)
    if (src=='') id = c(10,24,34,48,58,72)
    foreach(z=fs[id]) %dopar% {
        load(z)
        meas=unique(data$Measure)
        region=unique(data$Region)
        waterbody=unique(data$WaterBody)
        unitsLabel=unique(data$UnitsLabel)
        if (length(meas)!=0) { 
            print(meas)
            print(region)
            print(waterbody)
            data = data %>% mutate(Season=WQ_season(Date))
            d1=data %>% filter(!is.na(Value)) %>% mutate(Fact=Value/GL) %>%
                mutate(Month=format(Date, '%b')) %>%
                group_by(waterYear,Month) %>%
                summarize(N=n(),mu=mean(Value, na.rm=TRUE), sigma2=var(Value, na.rm=TRUE),Fact=mean(Fact,na.rm=TRUE), GL=mean(GL)) %>%
                mutate(Sigma2=sigma2/(Fact*GL),CV=sigma2/mu,A=sigma2/GL) 
                                        #d1
            
            GL = unique(data$GL)
            ## Temporal distribution of observed data conditioned on water year and season (color)
            dat = data %>% filter(!is.na(Value))
            if (nrow(dat)>100000)  dat= dat %>% sample_n(100000)
            if (edaYearMeasureViolin) {  ## Violin plots
                WQ_tryCatch(
                { 
                    g= dat %>% filter(Value>0) %>% mutate(year = waterYear + arrange_within_wateryear(Date)) %>%
                                        #g=dat %>% arrange(Date) %>% filter(Value>0) %>%
                                        #    mutate(year=waterYear + scales:::rescale(as.numeric(format(Date,'%j'))/365, from=c(0,1), to=c(-0.4,0.4))) %>%
                                        #        group_by(waterYear) %>% mutate(year = waterYear + seq(-0.4,0.4,length=length(waterYear))) %>% ungroup %>% 
                        ggplot(aes(y=Value, x=factor(waterYear))) +
                        geom_blank()+
                        annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=GL,ymax=GL*2, fill=WQ_reportCardColors[5], alpha=0.2) +
                        annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=GL,ymax=GL*4, fill=WQ_reportCardColors[5], alpha=0.1) +
                        annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=GL,ymax=GL*0.5, fill=WQ_reportCardColors[1], alpha=0.2) +
                        annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=GL,ymax=GL*0.25, fill=WQ_reportCardColors[1], alpha=0.1)
                    if (src == 'niskin/') {g=g+geom_point(aes(x=year-min(waterYear)+1, color=Season),size=3, alpha=1,show.legend=TRUE)
                    }else g=g + geom_point(aes(x=year-min(waterYear)+1, color=Season),size=0.01, alpha=1,show.legend=TRUE) 
                    #g=g+geom_point(aes(x=year-min(waterYear)+1, color=Season),size=0.01, alpha=1,show.legend=TRUE) +
                                        #            geom_smooth(aes(x=year-min(waterYear)+1, group=factor(waterYear)), method='gam', formula=y ~ s(x), method.args=list(family='Gamma(link="log")')) +
                    g=g+geom_smooth(aes(x=year-min(waterYear)+1, group=factor(waterYear)), method='gam', formula=y ~ s(x)) +
                        geom_smooth(aes(x=year-min(waterYear)+1, group=factor(waterYear)), method='lm', formula=y ~ 1,se=FALSE, color='purple') +
                        geom_violin(aes(x=factor(waterYear)), alpha=0.5) +
                        geom_hline(yintercept=GL, linetype='dashed', color='black') +
                        geom_hline(yintercept=GL*2, linetype='dashed', color=WQ_reportCardColors[5]) +
                        geom_hline(yintercept=GL*0.5, linetype='dashed', color=WQ_reportCardColors[1]) +
#                        annotate(geom='hline',yintercept=GL*2, y=GL,linetype='dashed', color=WQ_reportCardColors[5]) +
                        scale_x_discrete('Water Year') +
                                        #scale_y_log10(breaks=scales:::log_breaks(10,base=2)) +
                        ##coord_trans(y=scales:::log2_trans())+
                        ggtitle(paste0('Measure=',unitsLabel,', Region=',region,', Waterbody=',waterbody)) +
                                        #labs(title=paste0('Measure=',meas,', Region=',region,', Waterbody=',waterbody)) + 
                        theme_classic() + theme(axis.line.x=element_line(), axis.line.y=element_line(), plot.title=element_text(hjust=0), legend.position=c(1,1.25),legend.justification=c(1,1),legend.title=element_blank(), legend.direction='horizontal') +
                        guides(color=guide_legend(override.aes=list(size=3)))
                    if (yscale=='log') g = g + scale_y_log10(breaks=scales:::log_breaks(10,base=2), labels=function(x) sprintf('%2.2f',x))
                    
                    ggsave(filename=paste0('data/eda/eda.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf'),g,width=10, height=2.5, device=cairo_pdf)
                    ggsave(filename=paste0('data/eda/eda.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.png'),g,width=10, height=2.5,dpi=300)
                    ggsave(filename=paste0('data/eda/eda.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),g,width=10, height=2.5,dpi=300)
                    meta.file =
                        data.frame(Filename=paste0('data/eda/eda.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),
                                   Title=paste0('Exploratory data analysis violin plots for ',gsub('/','',src),' ', meas,' data from the ', waterbody,' of the ',region,' region conditional on water year.'),
                                   Description=paste0('Observed (logarithmic axis with violin plot overlay) ',src,' ',meas,' data from the ', waterbody,' of the ',region,' region conditional on water year. Observations are ordered over time and colored conditional on season as Wet (blue symbols) and Dry (red symbols). Blue smoother represents Generalized Additive Mixed Model within a water year and purple line represents average within the water year. Horizontal red, black and green dashed lines denote the twice threshold, threshold and half threshold values respectively. Red and green background shading indicates the range (10% shade: x4,/4; 30% shade: x2,/2) above and below threshold respectively'),
                                   Attribution='Murray Logan (AIMS)',
                                   Licencing='CC-BY',
                                   Location='Great Barrier Reef',
                                   Latitude=NA,
                                   Longitude=NA,
                                   PhotoGallery='EDA.observed')
                    write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                    system(paste0('cd data/eda/; convert -density 200 "eda.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf" "eda.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowres.pdf"'))
                },paste0('eda.log'), msg=paste0('EDA year plot for  Measure/Site level (Source=',gsub('/','',src),' Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)
            }
                
            ## Temporal distribution of observed data conditioned on month, wateryear and season (color)
            d2 <- with(d1, expand.grid(waterYear=unique(waterYear), Month=factor(unique(Month), levels=month.abb[c(10:12,1:9)])))
            dat = data %>% filter(!is.na(Value))
            if (nrow(dat)>100000)  dat= dat %>% sample_n(100000)
            if (edaMonthMeasureViolin) {
                WQ_tryCatch(
                { 
                    g= dat %>% mutate(Month=factor(format(Date,'%b'), levels=month.abb)) %>%
                        ggplot(aes(y=Value, x=factor(Month))) +
                        geom_blank(data=d2, aes(y=Inf)) +
                        geom_blank(aes(y=GL*4)) +
                        annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=GL,ymax=GL*2, fill=WQ_reportCardColors[5], alpha=0.2) +
                        annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=GL,ymax=GL*4, fill=WQ_reportCardColors[5], alpha=0.1) +
                        annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=GL,ymax=GL*0.5, fill=WQ_reportCardColors[1], alpha=0.2) +
                        annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=GL,ymax=GL*0.25, fill=WQ_reportCardColors[1], alpha=0.1) +
                        geom_hline(yintercept=GL, linetype='solid') 
                                        #annotate(geom='hline',yintercept=GL*2, y=GL,linetype='dashed') +
                                        #annotate(geom='hline',yintercept=GL*0.5, y=GL,linetype='dashed') +
                                        #annotate(geom='hline',yintercept=GL*4, y=GL,linetype='dotted') +
                                        #annotate(geom='hline',yintercept=GL*0.25, y=GL,linetype='dotted') +
                    if (src == 'niskin/') {g=g+geom_point(aes(color=Season),position=position_jitter(width=1), size=2)
                    }else g=g + geom_point(aes(color=Season),position=position_jitter(width=1), size=0.01) 
                    g=g+geom_violin(aes(x=factor(Month)), alpha=0.5) +
                        ##scale_y_continuous() +
                                        #scale_y_log10(breaks=scales:::log_breaks(10,base=2)) +
                        ##coord_trans(y=scales:::log2_trans())+
                        facet_wrap(~waterYear, scales='free_x',switch='x')  +
                                        #facet_wrap(~waterYear, switch='x')  +
                        theme_classic() +
                        ggtitle(paste0('Measure=',unitsLabel,', Region=',region,', Waterbody=',waterbody)) + 
                        theme(axis.line.x=element_line(), axis.line.y=element_line(), axis.title.x=element_blank(), strip.background=element_blank(), panel.border=element_blank()) +
                        geom_text(data=d1, aes(y=Inf, label=N), vjust=1.1, size=3) +
                        guides(color=guide_legend(override.aes=list(size=3)))
                    if (yscale=='log') g = g + scale_y_log10(breaks=scales:::log_breaks(10,base=2), labels=function(x) sprintf('%2.2f',x))
                    
                    ggsave(filename=paste0('data/eda/eda.year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf'),g,width=20, height=10, device=cairo_pdf)
                    ggsave(filename=paste0('data/eda/eda.year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.png'),g,width=20, height=10,dpi=300)
                    ggsave(filename=paste0('data/eda/eda.year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),g,width=20, height=10,dpi=300)
                    meta.file =
                        data.frame(Filename=paste0('data/eda/eda.year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),
                                   Title=paste0('Exploratory data analysis violin plots for ',gsub('/','',src),' ', meas,' data from the ', waterbody,' of the ',region,' region conditional on water year and month.'),
                                   Description=paste0('Observed (logarithmic axis with violin plot overlay) ',gsub('/','',src),' ',meas,' data from the ', waterbody,' of the ',region,' region conditional on water year and month. Observations are ordered over time and colored conditional on season as Wet (blue symbols) and Dry (red symbols). Blue smoother represents Generalized Additive Mixed Model within a water year and purple line represents average within the water year. Horizontal red, black and green dashed lines denote the twice threshold, threshold and half threshold values respectively. Red and green background shading indicates the range (10% shade: x4,/4; 30% shade: x2,/2) above and below threshold respectively'),
                                   Attribution='Murray Logan (AIMS)',
                                   Licencing='CC-BY',
                                   Location='Great Barrier Reef',
                                   Latitude=NA,
                                   Longitude=NA,
                                   PhotoGallery='EDA.observed')
                    write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                    system(paste0('cd data/eda/; convert -density 200 "eda.year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf" "eda.year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowres.pdf"'))
                },paste0('eda.log'), msg=paste0('EDA month plot for  Measure/Site level (Source=',gsub('/','',src),' Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)
            }
            
            ## Spatial, aggregating over wateryear for each zone
            pch=(plottingpars %>% filter(Region==as.character(region),WaterBody==as.character(waterbody),Src==as.character(src)))$pch
            if (length(pch)==0) pch=2
            dat1 = data %>% filter(!is.na(Value)) %>%
                group_by(waterYear,Region,WaterBody,Latitude,Longitude) %>%
                summarize(Value=median(Value,na.rm=TRUE))
            GBRMPA_Zone=(spatial %>% filter(Region==unique(dat1$Region), WaterBody==unique(dat1$WaterBody)))$GBRMPA_Zone
            wch.zone = which(names(Polys)==GBRMPA_Zone)
            if (src %in% c('niskin/', 'flntu/')) {
                yl=bbox(Polys[wch.zone])[2,]
                xl=bbox(Polys[wch.zone])[1,]
            } else {
                yl=range(dat1$Latitude)
                xl=range(dat1$Longitude)
            }
            xd=diff(xl)
            #pch=pch*xd
            lim=c(GL*0.5,GL*2)
            breaks=scales:::log_breaks(n=5,base=2)(lim)
            lab=vector('expression', length(breaks))
            lab[[1]] = bquote(.<=.(breaks[1]))
            for (i in 2:(length(breaks)-1)) lab[[i]] = bquote(.(breaks[i]))
            lab[[length(breaks)]] = bquote(.>=.(breaks[[length(breaks)]]))
            if(edaSpatialYear) {
                WQ_tryCatch(
                { 
                    g = ggplot(dat1 %>% mutate(Value=if_else(Value>GL*2, GL*2, if_else(Value<GL*0.5, GL*0.5,Value))), aes(y=Latitude,x=Longitude)) +
                        geom_point(aes(color=Value), size=pch) +
                        geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, color='black', size=0.1) +
                        geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black', size=0.1)
                    if (yscale=='log')  {
                        g=g +scale_color_gradientn('Value', colors=WQ_reportCardColorPalette(10), limits=lim, trans=scales:::log2_trans(), breaks=breaks, labels=lab)
                    } else g=g +scale_color_gradientn('Value', colors=WQ_reportCardColorPalette(10), limits=lim, breaks=breaks, labels=lab)
                                        #}   
                    g=g+coord_equal(xlim=xl,ylim=yl) +
                        facet_wrap(~waterYear, ncol=5) +
                        ggtitle(paste0('Measure=',unitsLabel,',Region=',region,', Waterbody=',waterbody, ', Threshold=',GL))+
                        theme_classic() + 
                        theme(panel.background=element_rect(fill=NA,color='black'), strip.background=element_blank(), panel.border=element_rect(color='black', fill=NA), plot.margin=unit(c(0,0,0,0), 'lines'))
                    nx = length(unique(dat1$waterYear))
                    nc = ifelse(nx<5,nx,5)
                    nr = (nx %/% 6)+1
                    ratio=diff(xl)/diff(yl)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf'),g,width=(nc*2), height=(nr*2/ratio) + 0.4*ratio, device=cairo_pdf)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.png'),g,width=(nc*2), height=(nr*2/ratio) + 0.4*ratio,dpi=300)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),g,width=(nc*2), height=(nr*2/ratio) + 0.4*ratio,dpi=300)
                    meta.file =
                        data.frame(Filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),
                                   Title=paste0('Spatial distribution of observed ',gsub('/','',src),' ', meas,' region conditional on water year.'),
                                   Description=paste0('Spatial distribution of observed ',gsub('/','',src),' ',meas,' data conditional on water year. Observations are colored according to observed value.'),
                                   Attribution='Murray Logan (AIMS)',
                                   Licencing='CC-BY',
                                   Location='Great Barrier Reef',
                                   Latitude=NA,
                                   Longitude=NA,
                                   PhotoGallery='EDA.observed')
                    write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                    #system(paste0('cd data/eda/; convert -density 200 "eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf" "eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowres.pdf"'))
                },paste0('eda.log'), msg=paste0('EDA spatial plot for  Measure/Site level (Source=',gsub('/','',src),' Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)                    
            }
            ## And now just the years 2010 - 2016
            dat2 = dat1 %>% filter(between(waterYear,2010,2016)) %>% mutate(Value=if_else(Value>GL*2, GL*2, if_else(Value<GL*0.5, GL*0.5,Value))) %>% full_join(data.frame(waterYear=2010:2016))
            if (edaSpatialYearlast) {
                WQ_tryCatch(
                {
                    nx = length(unique(dat2$waterYear))
                    nc = ifelse(nx<7,nx,7)
                    nr = 1
                    ratio=diff(xl)/diff(yl)
                    g = ggplot(dat2, aes(y=Latitude,x=Longitude)) +
                        geom_point(aes(color=Value), size=pch)+
                        geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, color='black',size=0.1) +
                        geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black', size=0.1)
                    if (yscale=='log')  {
                        g=g +scale_color_gradientn('Value', colors=WQ_reportCardColorPalette(10), limits=lim, trans=scales:::log2_trans(), breaks=breaks, labels=lab)
                    } else g=g +scale_color_gradientn('Value', colors=WQ_reportCardColorPalette(10), limits=lim, breaks=breaks, labels=lab)
                                        #}   
                    g=g+coord_equal(xlim=xl,ylim=yl) +
                        facet_wrap(~waterYear, ncol=7) +
                        ggtitle(paste0('Measure=',unitsLabel,',Region=',region,', Waterbody=',waterbody, ', Threshold=',GL))+
                        theme_classic() + 
                        theme(panel.background=element_rect(fill=NA,color='black'), strip.background=element_blank(), panel.border=element_rect(color='black', fill=NA), plot.margin=unit(c(0,0,0,0), 'lines'),
                              axis.text.x=element_text(size=rel(0.75)),axis.text.y=element_text(size=rel(0.75)),
                              axis.title.x=element_blank(),axis.title.y=element_blank(),
                              plot.background=element_rect(fill=NA))

                    #ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.pdf'),g,width=(nc*2), height=(nr*2/ratio), device=cairo_pdf)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.pdf'),g,width=(nc*2), height=(nr*2/ratio)+0.4*ratio, device=cairo_pdf)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.png'),g,width=(nc*2)+0.4*ratio, dpi=300)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.jpg'),g,width=(nc*2)+0.4*ratio, dpi=300)
                    meta.file =
                        data.frame(Filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.jpg'),
                                   Title=paste0('Spatial distribution of observed ',gsub('/','',src),' ', meas,' region conditional on water year (2010-2016).'),
                                   Description=paste0('Spatial distribution of observed ',gsub('/','',src),' ',meas,' data conditional on water year (2010-2016). Observations are colored according to observed value.'),
                                   Attribution='Murray Logan (AIMS)',
                                   Licencing='CC-BY',
                                   Location='Great Barrier Reef',
                                   Latitude=NA,
                                   Longitude=NA,
                                   PhotoGallery='EDA.observed')
                    write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                    #ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.png'),g,width=(nc*3), height=(nr*3/ratio) + 0.8,dpi=300)
                    #system(paste0('cd data/eda/; convert -density 80 "eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.pdf" "eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowresA.pdf"'))
                },paste0('eda.log'), msg=paste0('EDA spatial plot (last years) for  Measure/Site level (Source=',gsub('/','',src),' Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)
            }
            if (edaSpatialYearlastB) {  # This is the one we should be using
                WQ_tryCatch(
                {
                    nx = length(unique(dat2$waterYear))
                    nc = ifelse(nx<7,nx,7)
                    nr = 1
                    ratio=diff(xl)/diff(yl)
                    g = ggplot(dat2, aes(y=Latitude,x=Longitude)) +
                        geom_point(aes(color=Value), size=pch)+
                        geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, color='black',size=0.1) +
                        geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black', size=0.1)
                    if (yscale=='log')  {
                        #g=g +scale_color_continuous('Value',trans=scales:::log_trans(), breaks= pretty(dat2$Value))
                        g=g +scale_color_viridis('Value',trans=scales:::log_trans(), breaks= pretty(dat2$Value))
                    } else {
                        #g=g +scale_color_continous('Value')
                        g=g +scale_color_viridis('Value')
                    }
                    g=g+coord_equal(xlim=xl,ylim=yl) +
                        facet_wrap(~waterYear, ncol=7) +
                        ggtitle(paste0('Measure=',unitsLabel,',Region=',region,', Waterbody=',waterbody, ', Threshold=',GL))+
                        theme_classic() + 
                        theme(panel.background=element_rect(fill=NA,color='black'), strip.background=element_blank(), panel.border=element_rect(color='black', fill=NA), plot.margin=unit(c(0,0,0,0), 'lines'),
                              axis.text.x=element_text(size=rel(0.75)),axis.text.y=element_text(size=rel(0.75)),
                              axis.title.x=element_blank(),axis.title.y=element_blank(),
                              strip.text=element_text(size=rel(1.5)), plot.title=element_text(size=rel(1.5)),
                              legend.key.height = unit(1,'lines'), legend.text=element_text(size=rel(1)),
                                        #legend.title=element_text(size=rel(1.5),vjust=-0.2),
                              legend.title=element_blank(),
                              plot.background=element_rect(fill=NA)) #+
                        #guides(color=guide_colorbar(title.vjust=2))

                                        #ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.pdf'),g,width=(nc*2), height=(nr*2/ratio), device=cairo_pdf)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'B.pdf'),g,width=(nc*2), height=(nr*2/ratio)+0.4*ratio, device=cairo_pdf)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'B.png'),g,width=(nc*2)+0.4*ratio, dpi=300)
                    ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'B.jpg'),g,width=(nc*2)+0.4*ratio, dpi=300)
                                        #ggsave(filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.png'),g,width=(nc*3), height=(nr*3/ratio) + 0.8,dpi=300)
                                        #system(paste0('cd data/eda/; convert -density 80 "eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.pdf" "eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowresA.pdf"'))
                    meta.file =
                        data.frame(Filename=paste0('data/eda/eda.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'B.jpg'),
                                   Title=paste0('Spatial distribution of observed ',gsub('/','',src),' ', meas,' region conditional on water year (2010-2016).'),
                                   Description=paste0('Spatial distribution of observed ',gsub('/','',src),' ',meas,' data conditional on water year (2010-2016). Observations are colored according to observed value.'),
                                   Attribution='Murray Logan (AIMS)',
                                   Licencing='CC-BY',
                                   Location='Great Barrier Reef',
                                   Latitude=NA,
                                   Longitude=NA,
                                   PhotoGallery='EDA.observed')
                    write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                },paste0('eda.log'), msg=paste0('EDA spatial plot (last years) for  Measure/Site level (Source=',gsub('/','',src),' Region=',region,', water body=',waterbody,', Index=fsMAMP)'), return=TRUE)
                
            }
        }
        ii=ii+1
        setTxtProgressBar(pb,i)
    } ##zone/measure
    close(pb)
} #src





## Now for the Indexed data
for (index in c('Binary','fsMAMP','fsMAMP4')) {
    print(index)
                                        #index='Binary'
    yscale='natural'
    #foreach(src=c('niskin/','flntu/','','eReefs/','eReefs926/')) %do% {
    for (src in c('niskin/','flntu/','','eReefs/','eReefs926/')) {
        ## Explore some real data
        ## Explore the variability in the actual data
        fs = list.files(path=paste0(src,'data/indexed'),pattern='data.idx_.*.RData',full.names=TRUE)
        #pb <- txtProgressBar(max=length(fs), style=3)
        ii=0
                                        #fs = fs[grep('(Dry Tropics___Midshelf)|(Wet Tropics___Open Coastal)',fs)]
        id = c(10,24,34,48,58,72,82,96)
        if (src=='niskin/') id=c(2,8,10,16,18,24,27,33)
        if (src=='flntu/') id = c(2,7,9,14)
        if (src=='') id = c(10,24,34,48,58,72)
        foreach(z=fs[id]) %dopar% {
            print(z)
            load(z)
            data=data.idx
            meas=unique(data$Measure)
            region=unique(data$Region)
            waterbody=unique(data$WaterBody)
            unitsLabel=unique(data$Label)
            if (length(meas)!=0) {
                print(index)
                print(meas)
                print(region)
                print(waterbody)
                data = data %>% mutate(Season=WQ_season(Date),V=Value) %>%
                    mutate_(.dots=setNames(index,'Value'))
                d1=data %>% filter(!is.na(Value)) %>% mutate(Fact=Value/GL) %>%
                    mutate(Month=format(Date, '%b')) %>%
                    group_by(waterYear,Month) %>%
                    summarize(N=n(),mu=mean(Value, na.rm=TRUE), sigma2=var(Value, na.rm=TRUE),Fact=mean(Fact,na.rm=TRUE), GL=mean(GL)) %>%
                    mutate(Sigma2=sigma2/(Fact*GL),CV=sigma2/mu,A=sigma2/GL) 
                                        #d1
                
                GL = unique(data$GL)
                ## Temporal distribution of observed data conditioned on water year and season (color)
                dat = data %>% filter(!is.na(Value))
                if (nrow(dat)>100000)  dat= dat %>% sample_n(100000)
                if (edaidxYearMeasureViolin) {
                    WQ_tryCatch(
                    { 
                        g=dat %>% filter(Value>=0) %>% mutate(year = waterYear + arrange_within_wateryear(Date)) %>%
                                        #arrange(Date) %>% filter(Value>0) %>%
                                        #mutate(year=waterYear + scales:::rescale(as.numeric(format(Date,'%j'))/365, from=c(0,1), to=c(-0.4,0.4))) %>%
                                        #        group_by(waterYear) %>% mutate(year = waterYear + seq(-0.4,0.4,length=length(waterYear))) %>% ungroup %>% 
                            ggplot(aes(y=Value, x=factor(waterYear))) +
                            geom_blank()+
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=0,ymax=WQ_gradeBoundaries(type='Uniform')[4], fill=WQ_reportCardColors[5], alpha=0.2) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=WQ_gradeBoundaries(type='Uniform')[4],ymax=WQ_gradeBoundaries(type='Uniform')[3], fill=WQ_reportCardColors[4], alpha=0.2) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=WQ_gradeBoundaries(type='Uniform')[3],ymax=WQ_gradeBoundaries(type='Uniform')[2], fill=WQ_reportCardColors[3], alpha=0.2) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=WQ_gradeBoundaries(type='Uniform')[2],ymax=WQ_gradeBoundaries(type='Uniform')[1], fill=WQ_reportCardColors[2], alpha=0.2) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=WQ_gradeBoundaries(type='Uniform')[1],ymax=1, fill=WQ_reportCardColors[1], alpha=0.2)
                        if (src == 'niskin/') {g=g+geom_point(aes(x=year-min(waterYear)+1, color=Season),size=2, alpha=1,show.legend=TRUE)
                        }else g=g + geom_point(aes(x=year-min(waterYear)+1, color=Season),size=0.01, alpha=1,show.legend=TRUE) 
                        g=g+geom_point(aes(x=year-min(waterYear)+1, color=Season),size=0.01, alpha=1,show.legend=TRUE) +
                                        #geom_smooth(aes(x=year-min(waterYear)+1, group=factor(waterYear)), method='gam', formula=y ~ s(x), method.args=list(family='Gamma(link="log")')) +
                            geom_smooth(aes(x=year-min(waterYear)+1, group=factor(waterYear)), method='gam', formula=y ~ s(x)) +
                            geom_smooth(aes(x=year-min(waterYear)+1, group=factor(waterYear)), method='lm', formula=y ~ 1,se=FALSE, color='purple') +
                            geom_violin(aes(x=factor(waterYear)), alpha=0.5) +
                                        #geom_hline(yintercept=GL, linetype='solid', color=WQ_reportCardColors[3]) +
                            annotate(geom='hline',yintercept=GL*2, y=GL,linetype='dashed', color=WQ_reportCardColors[5]) +
                            scale_x_discrete('Water Year') +
                            ylim(c(0,1)) +
                                        #scale_y_log10(breaks=scales:::log_breaks(10,base=2)) +
                            ##coord_trans(y=scales:::log2_trans())+
                            ggtitle(paste0('Measure=',unitsLabel,', Region=',region,', Waterbody=',waterbody,', Index=',index)) +
                            theme_classic() + theme(axis.line.x=element_line(), axis.line.y=element_line(), plot.title=element_text(hjust=0), legend.position=c(1,1.25),legend.justification=c(1,1),legend.title=element_blank(), legend.direction='horizontal') +
                            guides(color=guide_legend(override.aes=list(size=3)))
                        
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf'),g,width=10, height=2.5, device=cairo_pdf)
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.png'),g,width=10, height=2.5,dpi=300)
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),g,width=10, height=2.5,dpi=300)
                        meta.file =
                            data.frame(Filename=paste0('data/eda/eda.idx_',index,'_year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),
                                       Title=paste0('Exploratory data analysis violin plots for ',index,' indexed ',gsub('/','',src),' ', meas,' data from the ', waterbody,' of the ',region,' region conditional on water year.'),
                                       Description=paste0('Temporal distribution of ',index,' indexed ',src,' ',meas,' data from the ', waterbody,' of the ',region,' region conditional on water year. Index scores are ordered over time and colored conditional on season as Wet (blue symbols) and Dry (red symbols). Blue smoother represents Generalized Additive Mixed Model within a water year and purple line represents average within the water year. Horizontal red, black and green dashed lines denote the twice threshold, threshold and half threshold values respectively. Red and green background shading indicates the range (10% shade: x4,/4; 30% shade: x2,/2) above and below threshold respectively'),
                                       Attribution='Murray Logan (AIMS)',
                                       Licencing='CC-BY',
                                       Location='Great Barrier Reef',
                                       Latitude=NA,
                                       Longitude=NA,
                                       PhotoGallery='EDA.indices')
                        write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                        system(paste0('cd data/eda/; convert -density 200 "eda.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf" "eda.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowres.pdf"'))      
                                        #system(paste0('cd data/eda/; convert -density 200 "eda.idx.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf" "eda.idx.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowres.pdf"'))
                    },paste0('eda.log'), msg=paste0('EDA idx year plot for  Measure/Site level (Source=',gsub('/','',src),' Region=',region,', water body=',waterbody,', Index=',index,')'), return=TRUE)
                }

                
                ## Temporal distribution of observed data conditioned on month, wateryear and season (color)
                d2 <- with(d1, expand.grid(waterYear=unique(waterYear), Month=factor(unique(Month), levels=month.abb[c(10:12,1:9)])))
                
                dat = data %>% filter(!is.na(Value))
                if (nrow(dat)>100000)  dat= dat %>% sample_n(100000)
                if (edaidxMonthMeasureViolin) {
                    WQ_tryCatch(
                    { 
                        g= dat %>% mutate(Month=factor(format(Date,'%b'), levels=month.abb)) %>%
                            ggplot(aes(y=Value, x=factor(Month))) +
                            geom_blank(data=d2, aes(y=Inf)) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=0,ymax=WQ_gradeBoundaries(type='Uniform')[4], fill=WQ_reportCardColors[5], alpha=0.2) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=WQ_gradeBoundaries(type='Uniform')[4],ymax=WQ_gradeBoundaries(type='Uniform')[3], fill=WQ_reportCardColors[4], alpha=0.2) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=WQ_gradeBoundaries(type='Uniform')[3],ymax=WQ_gradeBoundaries(type='Uniform')[2], fill=WQ_reportCardColors[3], alpha=0.2) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=WQ_gradeBoundaries(type='Uniform')[2],ymax=WQ_gradeBoundaries(type='Uniform')[1], fill=WQ_reportCardColors[2], alpha=0.2) +
                            annotate(geom='rect',xmin=-Inf,xmax=Inf, ymin=WQ_gradeBoundaries(type='Uniform')[1],ymax=1, fill=WQ_reportCardColors[1], alpha=0.2)
                        if (src == 'niskin/') {g=g+geom_point(aes(color=Season),position=position_jitter(width=1), size=2)
                        }else g=g + geom_point(aes(color=Season),position=position_jitter(width=1), size=0.01) 
                        g=g+geom_violin(aes(x=factor(Month)), alpha=0.5) +
                                        #geom_point(aes(color=Season),position=position_jitter(width=1), size=0.01, show.legend=FALSE) +
                                        #geom_violin(aes(x=factor(Month)), alpha=0.5) +
                            facet_wrap(~waterYear, scales='free_x',switch='x')  +
                            ylim(c(0,1)) +
                            theme_classic() +
                            ggtitle(paste0('Measure=',unitsLabel,', Region=',region,', Waterbody=',waterbody,', Index=',index)) + 
                            theme(axis.line.x=element_line(), axis.line.y=element_line(), axis.title.x=element_blank(), strip.background=element_blank(), panel.border=element_blank()) +
                            geom_text(data=d1, aes(y=Inf, label=N), vjust=1.1, size=3) +
                            guides(color=guide_legend(override.aes=list(size=3)))
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf'),g,width=20, height=10, device=cairo_pdf)
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.png'),g,width=20, height=10,dpi=300)
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),g,width=20, height=10,dpi=300)
                        meta.file =
                            data.frame(Filename=paste0('data/eda/eda.idx_',index,'_year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),
                                       Title=paste0('Exploratory data analysis violin plots for ',index,' indexed ',gsub('/','',src),' ', meas,' data from the ', waterbody,' of the ',region,' region conditional on water year and month.'),
                                       Description=paste0('Temporal distribution of ',index,' indexed ',src,' ',meas,' data from the ', waterbody,' of the ',region,' region conditional on water year and month. Index scores are ordered over time and colored conditional on season as Wet (blue symbols) and Dry (red symbols). Blue smoother represents Generalized Additive Mixed Model within a water year and purple line represents average within the water year. Horizontal red, black and green dashed lines denote the twice threshold, threshold and half threshold values respectively. Red and green background shading indicates the range (10% shade: x4,/4; 30% shade: x2,/2) above and below threshold respectively'),
                                       Attribution='Murray Logan (AIMS)',
                                       Licencing='CC-BY',
                                       Location='Great Barrier Reef',
                                       Latitude=NA,
                                       Longitude=NA,
                                       PhotoGallery='EDA.indices')
                        write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                                        #system(paste0('cd data/eda/; convert -density 200 "eda.idx.year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf" "eda.idx.year.month.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowres.pdf"'))
                    },paste0('eda.log'), msg=paste0('EDA idx month plot for  Measure/Site level (Source=',gsub('/','',src),' Region=',region,', water body=',waterbody,', Index=',index,')'), return=TRUE)
                }



                ## Spatial, aggregating over wateryear for each zone
                pch=(plottingpars %>% filter(Region==as.character(region),WaterBody==as.character(waterbody),Src==as.character(src)))$pch
                if (length(pch)==0) pch=2
                dat1 = data %>% filter(!is.na(Value)) %>%
                    group_by(waterYear,Region,WaterBody,Latitude,Longitude) %>%
                    summarize(Value=median(Value,na.rm=TRUE))
                GBRMPA_Zone=(spatial %>% filter(Region==unique(dat1$Region), WaterBody==unique(dat1$WaterBody)))$GBRMPA_Zone
                wch.zone = which(names(Polys)==GBRMPA_Zone)
                if (src %in% c('niskin/', 'flntu/')) {
                    yl=bbox(Polys[wch.zone])[2,]
                    xl=bbox(Polys[wch.zone])[1,]
                } else {
                    yl=range(dat1$Latitude)
                    xl=range(dat1$Longitude)
                }
                xd=diff(xl)
                pch=pch*xd
                lim=c(0,1)
                if(edaidxSpatialYear) {
                    WQ_tryCatch(
                    {
                        g = ggplot(dat1, aes(y=Latitude,x=Longitude)) +
                            geom_point(aes(color=Value),size=pch) +
                            geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, color='black',size=0.1) +
                            geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black', size=0.1)
                        g=g +scale_color_gradientn('Value', colors=rev(WQ_reportCardColorPalette(10)), limits=lim)
                                        #}   
                        g=g+coord_equal(xlim=xl,ylim=yl) +
                            facet_wrap(~waterYear) +
                            ggtitle(paste0('Measure=',unitsLabel,',Region=',region,', Waterbody=',waterbody, ', GL=',GL,', Index=',index))+
                            theme_classic() + 
                            theme(panel.background=element_rect(fill=NA,color='black'), strip.background=element_blank(), panel.border=element_rect(color='black', fill=NA), plot.margin=unit(c(0,0,0,0), 'lines'))
                        nx = length(unique(dat1$waterYear))
                        nc = ifelse(nx<5,nx,5)
                        nr = (nx %/% 6)+1
                        ratio=diff(xl)/diff(yl)
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf'),g,width=(nc*2), height=(nr*2/ratio) +0.4*ratio, device=cairo_pdf)
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.png'),g,width=(nc*2), height=(nr*2/ratio) + 0.4*ratio,dpi=300)
                        ggsave(filename=paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),g,width=(nc*2), height=(nr*2/ratio) + 0.4*ratio,dpi=300)
                        meta.file =
                            data.frame(Filename=paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),
                                       Title=paste0('Spatial distribution of ',index,' indexed ',gsub('/','',src),' ', meas,' region conditional on water year.'),
                                       Description=paste0('Spatial distribution of ',index,' indexed ',gsub('/','',src),' ',meas,' data conditional on water year. Observations are colored according to observed value.'),
                                       Attribution='Murray Logan (AIMS)',
                                       Licencing='CC-BY',
                                       Location='Great Barrier Reef',
                                       Latitude=NA,
                                       Longitude=NA,
                                       PhotoGallery='EDA.indices')
                        write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                                        #system(paste0('cd data/eda/; convert -density 200 "eda.idx.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.pdf" "eda.idx.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowres.pdf"'))
                    },paste0('eda.log'), msg=paste0('EDA idx spatial plot for  Measure/Site level (Source=',gsub('/','',src),' Region=',region,', water body=',waterbody,', Index=',index,')'), return=TRUE)                    
                }
                
                ## And now just the years 2010 - 2016
                if (edaidxSpatialYearlast) {                    
                    dat2 = dat1 %>% filter(between(waterYear,2010,2016)) %>%
                                        #mutate(Value=if_else(Value>GL*2, GL*2, if_else(Value<GL*0.5, GL*0.5,Value))) %>%
                        full_join(data.frame(waterYear=2010:2016))
                    g = ggplot(dat2, aes(y=Latitude,x=Longitude)) +
                        geom_point(aes(color=Value), size=pch)+
                        geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, color='black',size=0.1) +
                        geom_polygon(data=qld.df, aes(y=lat, x=long, group=group), fill='grey', color='black', size=0.1)
                    g=g +scale_color_gradientn('Value', colors=rev(WQ_reportCardColorPalette(10)), limits=lim)
                                        #}   
                    g=g+coord_equal(xlim=xl,ylim=yl) +
                        facet_wrap(~waterYear, ncol=7) +
                        ggtitle(paste0('Measure=',unitsLabel,',Region=',region,', Waterbody=',waterbody, ', Threshold=',GL,', Index=',index))+
                        theme_classic() + 
                        theme(panel.background=element_rect(fill=NA,color='black'), strip.background=element_blank(),
                              panel.border=element_rect(color='black', fill=NA), plot.margin=unit(c(0,0,0,0), 'lines'),
                              axis.text.x=element_text(size=rel(0.75)),axis.text.y=element_text(size=rel(0.75)),
                              axis.title.x=element_blank(),axis.title.y=element_blank(),
                              strip.text=element_text(size=rel(1.5)), plot.title=element_text(size=rel(1.5)),
                              legend.key.height = unit(1,'lines'), legend.text=element_text(size=rel(1)),
                                        #legend.title=element_text(size=rel(1.5),vjust=-0.2),
                              legend.title=element_blank(),
                              plot.background=element_rect(fill=NA))
                    nx = length(unique(dat2$waterYear))
                    nc = ifelse(nx<7,nx,7)
                    nr = 1
                    ratio=diff(xl)/diff(yl)
                    print(paste(z,'save'))
                    print(paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.pdf'))
                    ggsave(filename=paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.pdf'),g,width=(nc*2), height=(nr*2/ratio)+0.4*ratio, device=cairo_pdf)
                    ggsave(filename=paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.png'),g,width=(nc*3), height=(nr*3/ratio)+0.4*ratio,dpi=300)
                    ggsave(filename=paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.jpg'),g,width=(nc*3), height=(nr*3/ratio)+0.4*ratio,dpi=300)
                    meta.file =
                        data.frame(Filename=paste0('data/eda/eda.idx_',index,'_spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'.jpg'),
                                   Title=paste0('Spatial distribution of ',index,' indexed ',gsub('/','',src),' ', meas,' region conditional on water year (2010-2016).'),
                                   Description=paste0('Spatial distribution of ',index,' indexed ',gsub('/','',src),' ',meas,' data conditional on water year (2010-2016). Observations are colored according to observed value.'),
                                   Attribution='Murray Logan (AIMS)',
                                   Licencing='CC-BY',
                                   Location='Great Barrier Reef',
                                   Latitude=NA,
                                   Longitude=NA,
                                   PhotoGallery='EDA.indices')
                    write.table(meta.file, file='meta.file_eda.csv', sep=',',row.names=FALSE, col.names=FALSE,append=TRUE)
                                        #system(paste0('cd data/eda/; convert -density 200 "eda.idx.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'A.pdf" "eda.idx.spatial.year.',meas,'_',region,'__',waterbody,'_',gsub('/','',src),'_',yscale,'_lowresA.pdf"'))
                    print(paste(z,'done'))
                }
            }

            ii=ii+1
            #setTxtProgressBar(pb,ii)
        } ##zone/measure
        #close(pb)
    } #src


} #index
