library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
require(foreach)
require(doParallel)

source('WQ_functions.R')




## Generate a series of data that has specific characteristics relative to a guideline value

########################################################################
## Set 1. this set comprises 28 groups of data (6 means by 4          ##
##        variances) Means and variances are expressed as a factor of ##
##        the guideline value Data a drawn from gamma distributions   ##
##        whose shape parameters are derived from the respective mean ##
##        and variance multipliers                                    ##
## Set 2. this set comprises 12 groups                                ##
##        Items of this set vary in their pattern of oscillation      ##
##        around the GL.  Specifically, how many consecutive          ##
##        exceedences (Duration) they have                            ##
########################################################################

groups <- function(GL=10,R=10, set=1) {
    if (set==1) {
        group=rbind(
            expand.grid(Group=1, Fact=0.2, Sigma2=0.1, Rep=1:R,Description='Very small values, very small variance')
           ,expand.grid(Group=2, Fact=0.2, Sigma2=0.2, Rep=1:R, Description='Very small values, small variance')
           ,expand.grid(Group=3, Fact=0.2, Sigma2=0.3, Rep=1:R, Description='Very small values, small variance')
           ,expand.grid(Group=4, Fact=0.2, Sigma2=0.5, Rep=1:R, Description='Very small values, small variance')
           ,expand.grid(Group=5, Fact=0.5, Sigma2=0.1, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=6, Fact=0.5, Sigma2=0.2, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=7, Fact=0.5, Sigma2=0.3, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=8, Fact=0.5, Sigma2=0.5, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=9, Fact=0.75, Sigma2=0.1, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=10, Fact=0.75, Sigma2=0.2, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=11, Fact=0.75, Sigma2=0.3, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=12, Fact=0.75, Sigma2=0.5, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=13, Fact=1, Sigma2=0.1, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=14, Fact=1, Sigma2=0.2, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=15, Fact=1, Sigma2=0.3, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=16, Fact=1, Sigma2=0.5, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=17, Fact=1.5, Sigma2=0.1, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=18, Fact=1.5, Sigma2=0.2, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=19, Fact=1.5, Sigma2=0.3, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=20, Fact=1.5, Sigma2=0.5, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=21, Fact=2, Sigma2=0.1, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=22, Fact=2, Sigma2=0.2, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=23, Fact=2, Sigma2=0.3, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=24, Fact=2, Sigma2=0.5, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=25, Fact=4, Sigma2=0.1, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=26, Fact=4, Sigma2=0.2, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=27, Fact=4, Sigma2=0.3, Rep=1:R, Description='Small values, small variance')
           ,expand.grid(Group=28, Fact=4, Sigma2=0.5, Rep=1:R, Description='Small values, small variance')
        )
        dat = cbind(group, GL=GL, DirectionOfFailure='H')
        dat=dat %>% mutate(Sigma2=GL*Fact*Sigma2,Value=rgamma(n(),(GL*Fact)*(GL*Fact)/Sigma2,1/(Sigma2/(GL*Fact))))
    } else if (set==2) {
        group=rbind(
            data.frame(Group=1, Fact = 1, Sigma2=0.1, Duration=0.1, Rep=1:R)
           ,data.frame(Group=2, Fact = 1, Sigma2=0.1, Duration=0.25, Rep=1:R)
           ,data.frame(Group=3, Fact = 1, Sigma2=0.1, Duration=0.5, Rep=1:R)
           ,data.frame(Group=4, Fact = 1, Sigma2=0.1, Duration=0.75, Rep=1:R)
           ,data.frame(Group=5, Fact = 1, Sigma2=0.1, Duration=0.90, Rep=1:R)
           ,data.frame(Group=6, Fact = 1, Sigma2=0.2, Duration=0.1, Rep=1:R)
           ,data.frame(Group=7, Fact = 1, Sigma2=0.2, Duration=0.25, Rep=1:R)
           ,data.frame(Group=8, Fact = 1, Sigma2=0.2, Duration=0.5, Rep=1:R)
           ,data.frame(Group=9, Fact = 1, Sigma2=0.2, Duration=0.75, Rep=1:R)
           ,data.frame(Group=10, Fact = 1, Sigma2=0.2, Duration=0.9, Rep=1:R)
           ,data.frame(Group=11, Fact = 1, Sigma2=0.3, Duration=0.1, Rep=1:R)
           ,data.frame(Group=12, Fact = 1, Sigma2=0.3, Duration=0.25, Rep=1:R)
           ,data.frame(Group=13, Fact = 1, Sigma2=0.3, Duration=0.5, Rep=1:R)
           ,data.frame(Group=14, Fact = 1, Sigma2=0.3, Duration=0.75, Rep=1:R)
           ,data.frame(Group=15, Fact = 1, Sigma2=0.3, Duration=0.9, Rep=1:R)
           ,data.frame(Group=16, Fact = 1, Sigma2=0.5, Duration=0.1, Rep=1:R)
           ,data.frame(Group=17, Fact = 1, Sigma2=0.5, Duration=0.25, Rep=1:R)
           ,data.frame(Group=18, Fact = 1, Sigma2=0.5, Duration=0.5, Rep=1:R)
           ,data.frame(Group=19, Fact = 1, Sigma2=0.5, Duration=0.75, Rep=1:R)
           ,data.frame(Group=20, Fact = 1, Sigma2=0.5, Duration=0.9, Rep=1:R)
        )
        dat = cbind(group, GL=GL, DirectionOfFailure='H')
        ## use a sine wave with gamma noise
        dat = dat %>% group_by(Group) %>% mutate(Sigma2=GL*Fact*Sigma2,
                                                 Value=(Fact*GL)+((Sigma2)*sin(pi*1/Duration[1]*seq(0,1,len=n()))),
                                                 #Value=Value*rnorm(n(),0,GL*Sigma2),#rgamma(n(),(GL*Fact)*(GL*Fact)/Sigma2,1/(Sigma2/(GL*Fact))),
                                                 Ex=(Value>GL)*1) %>% ungroup
        ## dat=dat %>% mutate(Sigma2=GL*Fact*Sigma2,Value=rgamma(n(),(GL*Fact)*(GL*Fact)/Sigma2,1/(Sigma2/(GL*Fact)))) %>%
        ##     arrange(Group,desc((Value>GL)*1)) %>% group_by(Group) %>% mutate(Ex=(Value>GL)*1,Max=round(n()*Duration,0)) %>% ungroup
        ## dat=dat %>% group_by(Group) %>%
        ##     do({
        ##         x=.
        ##         x1=x %>% slice(1:unique(Max)) %>% sample_n(size=unique(.$Max), replace=FALSE)
        ##         x2=x %>% slice((unique(Max)+1):n()) %>% sample_n(size=nrow(x)-unique(.$Max),replace=FALSE)
        ##         rbind(x1,x2)
        ##     })
    }
    dat
}



GL=1
dat=NULL
for (Mean in c(0.05,0.1,0.15,0.2,0.5,1,2,4)) {
    Var=Mean^2
    dat=dat %>% bind_rows(data.frame(Value=rgamma(1000,Mean*Mean/Var,1/(Var/Mean)), Mean=factor(Mean), GL=GL,DirectionOfFailure='H'))
}
g1=ggplot(dat, aes(y=Value, x=Mean)) +
    geom_violin() +
    scale_y_log10(breaks=scales:::log_breaks(10,base=2), labels=function(x) sprintf('%2.2f',x)) +
    geom_hline(yintercept=mean(dat$GL), linetype='solid') +
    geom_hline(yintercept=mean(dat$GL*2), linetype='dashed') +
    geom_hline(yintercept=mean(dat$GL*0.5), linetype='dashed') +
    geom_hline(yintercept=mean(dat$GL*4), linetype='dotted') +
    geom_hline(yintercept=mean(dat$GL*0.25), linetype='dotted')+
    geom_hline(yintercept=mean(dat$GL*16), color='red',linetype='dotted') +
    geom_hline(yintercept=mean(dat$GL*(1/16)), color='red',linetype='dotted')
dat %>% group_by(GL,Mean) %>% mutate(Mu=mean(Value),l=log(Mu/Value, 2),ll=Mu/Value) %>%
    summarize(mu=mean(Value),quantile(ll,p=0.1),quantile(ll,p=0.9))
dat1 = dat %>% ungroup %>%
                mutate(Binary=WQ_WQI_binary(.),
                       MAMP=WQ_WQI_mamp(.,capped=FALSE,scaled=FALSE),
                       fMAMP=WQ_WQI_mamp(.,capped=TRUE, scaled=FALSE),
                       fsMAMP=WQ_WQI_mamp(.),
                       fsMAMP4=WQ_WQI_mamp(.,fold=4),
                       fsMAMP16=WQ_WQI_mamp(.,fold=16)
                       )
dat.sum = dat1 %>%
    group_by(GL,Mean) %>%
    summarize_at(vars(Binary,MAMP,fMAMP,fsMAMP,fsMAMP4,fsMAMP16), funs(mean))
dat.sum

dat.sum.exceed = dat1 %>% filter(!is.na(Binary)) %>% group_by(GL,Mean) %>%
    summarize(Exceed=WQ_exceed(Binary),Max_Duration=WQ_duration(Binary))
dat.sum = dat.sum %>% full_join(dat.sum.exceed)

dat.sum.melt = dat.sum %>% gather(key='Index', value='Value',-GL,-Mean) %>%
    mutate(Index=factor(Index, levels=c('Binary','Exceed','Max_Duration','MAMP','fMAMP','fsMAMP','fsMAMP4','fsMAMP16')))

g2=ggplot(dat.sum.melt, aes(y=Value, x=as.numeric(Mean))) +
                geom_blank() +
                annotate(geom='blank',y=c(0,1), x=c(1,1))+
                geom_point(aes(x=as.numeric(Mean))) + facet_wrap(~Index, scales='free', nrow=2) +
                #geom_line(aes(x=as.numeric(Group),color=factor((Sigma2/Fact/GL)))) +
                #scale_color_discrete(expression(sigma^2)) +
                #scale_x_continuous('Group', breaks=pretty_breaks()) + theme_bw() +
                theme(axis.line.x=element_line(), axis.line.y=element_line(), strip.background=element_blank(), panel.border=element_blank(),
                      legend.position=c(1,0), legend.justification=c(1,0),legend.key=element_blank(),
                      text=element_text(size=15)) +
    scale_x_log10(breaks=scales:::log_breaks(10,base=2), labels=function(x) sprintf('%2.2f',x))
grid.arrange(g1,g2, heights=c(1,2))
ggsave(filename=paste0('data/sensitivity/A_comp.GL_',GL,'_',yscale,'.pdf'),grid.arrange(g1,g2, heights=c(1,2)), width=15, height=10)

## dat = groups(GL=0.5,R=1000,set=2)
## dat %>% head
## dim(dat)
## d = dat %>% filter(Group==4)
## head(d)
## rle(d$Ex)
## mean(d$Value)
## d$Value
## m=max(rle(dat$Ex)$length)
## which(rle(dat$Ex)$length==m)
yscale='log'
#yscale=''
for (grp in 1:2) {
    for (R in c(10,100,1000)) {    
        for (GL in c(0.1,0.5,1,2,5,10,50,100)) {
            set.seed(123)
            print(paste0('grp=',grp,', R=',R,', GL=',GL))
            #dat = cbind(groups[[grp]], GL=GL, DirectionOfFailure='H')
            #dat = dat %>%
                                        #mutate(Sigma2=GL*Fact*Sigma2,Value=rgamma(n(),(GL*Fact)*(GL*Fact)/Sigma2,1/(Sigma2/(GL*Fact)))) %>%
            dat = groups(GL=GL,R=R, set=grp) %>% ungroup %>%
                mutate(Binary=WQ_WQI_binary(.),
                       MAMP=WQ_WQI_mamp(.,capped=FALSE,scaled=FALSE),
                       fMAMP=WQ_WQI_mamp(.,capped=TRUE, scaled=FALSE),
                       fsMAMP=WQ_WQI_mamp(.),
                       fsMAMP4=WQ_WQI_mamp(.,fold=4)
                       )
            d1=dat %>% filter(!is.na(Value)) %>% mutate(Fact=Value/GL) %>% group_by(Group) %>%
                summarize(mu=mean(Value, na.rm=TRUE), sigma2=var(Value, na.rm=TRUE),Fact=mean(Fact,na.rm=TRUE)) %>%
                mutate(Sigma2=sigma2/(Fact*GL),CV=sigma2/mu,A=sigma2/GL)
            d1
            
            ## g1=dat  %>% ggplot(aes(y=Value, x=factor(Group))) + geom_hline(yintercept=GL, linetype='solid') +
            ##     geom_hline(yintercept=GL*c(2,0.5), linetype='dashed') +
            ##     geom_hline(yintercept=GL*c(4,0.25), linetype='dotted') +
            ##     geom_point(position=position_jitter(width=1), size=1/nrow(dat)) + geom_violin(aes(x=factor(Group)), alpha=0.5) +
            ##    scale_y_continuous()

            g1=dat %>% group_by(Group) %>%
                #mutate(G=sort(jitter(Group))) %>%
                mutate(G=Group + seq(-0.45,0.45, len=n())) %>%
                ggplot(aes(y=Value, x=factor(Group))) +
                geom_blank()+
                geom_hline(yintercept=GL, linetype='solid') +
                annotate(geom='hline',yintercept=GL*2, y=GL,linetype='dashed') +
                annotate(geom='hline',yintercept=GL*0.5, y=GL,linetype='dashed') +
                annotate(geom='hline',yintercept=GL*4, y=GL,linetype='dotted') +
                annotate(geom='hline',yintercept=GL*0.25, y=GL,linetype='dotted') +
#                geom_hline(yintercept=GL*c(2,0.5), linetype='dashed') +
#                geom_hline(yintercept=GL*c(4,0.25), linetype='dotted') +
    geom_point(aes(x=G,color=factor(Sigma2/Fact/GL)),size=1/nrow(dat), show.legend=FALSE) +
    geom_violin(aes(x=factor(Group),color=factor(Sigma2/Fact/GL)), alpha=0.5,show.legend=FALSE) +
                scale_x_discrete('Group') + scale_y_continuous() +
                ggtitle(paste0('Threshold=',GL,', Number of samples=',R)) +
                theme_bw() +
    theme(axis.line.x=element_line(), axis.line.y=element_line(), strip.background=element_blank(), panel.border=element_blank(),
          text=element_text(size=15))
    if (yscale=='log') g1 = g1 + scale_y_log10(breaks=scales:::log_breaks(10,base=2), labels=function(x) sprintf('%2.2f',x))           

            
            dat.sum = dat %>%
                group_by(Group,GL,Fact,Sigma2) %>%
                summarize_at(vars(Binary,MAMP,fMAMP,fsMAMP,fsMAMP4), funs(mean))
            dat.sum
            
            dat.sum.exceed = dat %>% filter(!is.na(Binary)) %>% group_by(Group,GL,Fact,Sigma2) %>%
                summarize(Exceed=WQ_exceed(Binary),Max_Duration=WQ_duration(Binary))
            dat.sum = dat.sum %>% full_join(dat.sum.exceed)
            
            dat.sum.melt = dat.sum %>% gather(key='Index', value='Value',-Group,-GL,-Fact,-Sigma2) %>%
                mutate(Index=factor(Index, levels=c('Binary','Exceed','Max_Duration','MAMP','fMAMP','fsMAMP','fsMAMP4')))
            
            g2=ggplot(dat.sum.melt, aes(y=Value, x=Group)) +
                geom_blank() +
                annotate(geom='blank',y=c(0,1), x=c(1,1))+
                geom_point(aes(x=as.numeric(Group))) + facet_wrap(~Index, scales='free', nrow=2) +
                geom_line(aes(x=as.numeric(Group),color=factor((Sigma2/Fact/GL)))) +
                scale_color_discrete(expression(sigma^2)) +
                scale_x_continuous('Group', breaks=pretty_breaks()) + theme_bw() +
                theme(axis.line.x=element_line(), axis.line.y=element_line(), strip.background=element_blank(), panel.border=element_blank(),
                      legend.position=c(1,0), legend.justification=c(1,0),legend.key=element_blank(),
                      text=element_text(size=15))
            
            ggsave(filename=paste0('data/sensitivity/comp.Group_',grp,'.GL_',GL,'.R_',R,'_',yscale,'.pdf'),grid.arrange(g1,g2, heights=c(1,2)), width=15, height=10)
            ggsave(filename=paste0('Report/Figures/sensitivity.Group_',grp,'.GL_',GL,'.R_',R,'_',yscale,'.pdf'),grid.arrange(g1,g2, heights=c(1,2)), width=15, height=10)
        }
    }    
}
