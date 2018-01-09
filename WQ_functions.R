##+++++++++++++++Functions for all scripts++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###################################################################################################
## The following function uses the key=value pairs in config/config.config to set global configs ##
## input:                                                                                        ##
##      NULL                                                                                     ##
## output:                                                                                       ##
##      NULL                                                                                     ##
## sideeffects:                                                                                  ##
##      generates global variables                                                               ##
###################################################################################################
getConfigs <- function() {
    en<-parent.frame()
    if (file.exists('config/config.config')) {
        config = readLines('config/config.config')
        config = gsub('(.*Date)=(.*)','\\1=as.Date(\'\\2\')',config)
        eval(parse(text=config),en)
    }
}


setConfigs <- function(conf.c=c(sensitivity.tests=TRUE)) {
    if (file.exists('config/config.config')) {
        config = readLines('config/config.config')
        for (i in seq_along(conf.c)) cc=gsub(paste0(names(conf.c)[i],'=.*'), paste0(names(conf.c)[i],'=',conf.c[i]), config)
        writeLines(text=cc, con='config/config.config')
    } else {
        config = c(sensitivity.tests=TRUE,
                   get.BOM=TRUE,
                   process.BOM=TRUE,
                   index.BOM=TRUE,
                   get.eReefs=TRUE,
                   process.eReefs=TRUE,               
                   index.eReefs=TRUE,
                   get.eReefs926=TRUE,                
                   process.eReefs926=TRUE,
                   index.eReefs926=TRUE,              
                   process.Niskin=TRUE,
                   index.Niskin=TRUE,                 
                   index.FLNTU=TRUE,
                   eda=TRUE,                          
                   compare.sources=TRUE,
                   aggregate.without.uncertainty=TRUE,
                   aggregate.summaries=TRUE,
                   include_enclosedcoastal=TRUE,      
                   include_sd=TRUE,
                   include_NOx=TRUE,                 
                   include_nap=TRUE,
                   GradeType='Uniform')
        for (i in seq_along(conf.c)) cc=gsub(paste0(names(conf.c)[i],'=.*'), paste0(names(conf.c)[i],'=',conf.c[i]), config)
        writeLines(text=cc, con='config/config.config')
    }
}
#################################################################
## The following function evaluates an expression and based on ##
## whether it succeeds of fails it will print the status to a  ##
## logfile.                                                    ##
#################################################################
WQ_tryCatch <- function(expr, logFile, expectedClass=NULL, msg=NULL, return=NULL) {
    ## Check if the log file exists, and if it does not, create it
    files <- list.files('log')
    if(!any(grepl(paste0('^',logFile,'$'),files))) system(paste0('touch log/',logFile))
    now <- Sys.time()
    options(digits.secs=2)              ## switch to subsecond display
    #msg <- paste0(now, '| ', msg)
    W <- NULL
	w.handler <- function(w){ # warning handler
		W <<- w
		invokeRestart("muffleWarning")
	}
	ret <- list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), 
					warning = w.handler),warning = W)
    if (exists('ret$value$message')) {
        ## An error occurred
        class(ret) <- "try-error"
        msg <- paste0(now, '|','FAILED: ',msg)
        if( !is.null(msg)){ write(msg,file=paste0('log/',logFile),append=TRUE)}
        if(!is.null(return)) {
            FALSE
        }#else return()
    } else { #no error
        msg <- paste0(now, '|','SUCCESS: ',msg)
        write(msg,file=paste0('log/',logFile),append=TRUE)
        if(!is.null(return)) {
            TRUE
        }#else return()

    }
}
##------------------------------------------------------------------------------------------------------------



##+++++++++++++++Functions for WQ_processBOM.R++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##########################################################
## The following function clips a raster to a polygon   ##
## input:                                               ##
##    raster:     a raster object                       ##
##    shape:      a Spatialpolygons object              ##
## output:                                              ##
##    data.frame: a data.frame containing x,y,layers... ##
##########################################################
clip<-function(raster,shape) {
    a1_crop<-crop(raster,shape)
    step1<-rasterize(shape,a1_crop)
    a1_crop*step1}

##################################################################
## The following function generates a water year vector         ##
## The water year is defined as 1st Oct through to 30 September ##
## input:                                                       ##
##    Dt: a Date vector                                         ##
## output:                                                      ##
##    waterYear: a integer representing the water year          ##
##################################################################
WQ_waterYear <- function(Dt) {
    as.numeric(as.character(format(Dt+(as.Date("1970-12-31")-as.Date("1970-10-01")+1), format="%Y")))
}

############################################################################
## The following function generates a season vector                       ##
## The Wet Season is defined as Nov-Apr                                   ##
## input:                                                                 ##
##    Dt: a Date vector                                                   ##
## output:                                                                ##
##    Season: a character vector representing the Season ("Wet" or "Dry") ##
############################################################################
WQ_season <- function(Dt) {
    factor(ifelse(lubridate:::month(Dt,label=TRUE) %in% c("Nov","Dec","Jan","Feb","Mar","Apr"), "Wet","Dry"))
    }


##########################################################################
## The following function is a wrapper used to process the BOM          ##
## satellite data.  Specifically it:                                    ##
## 1. converts the raster into a data.frame                             ##
## 2. renames the layers to a near-date label                           ##
## 3. melts the data into x,y,Date,Value                                ##
## 4. converts Date into an actual Date vector                          ##
## 5. adds a waterYear vector                                           ##
##                                                                      ##
## Input:                                                               ##
##    dat:   a data.frame                                               ##
##    dates: a character vector of DYYYYMMDD                            ##
## Output:                                                              ##
##    data.frame: a data.frame with Date, waterYear and Season appended ##
##########################################################################
WQ_process_BOM <- function(dat, dates) {
    as.data.frame(rasterToPoints(dat)) %>%
        setNames(nm=c("x","y",dates)) %>%
        gather(key=Date, value=Value,-x,-y) %>%
        mutate(Date=as.Date(gsub('D([0-9]{4,})([0-9][0-9])([0-9][0-9])','\\1-\\2-\\3',Date)),
               waterYear=WQ_waterYear(Date),
               Season=WQ_season(Date))         
}

##------------------------------------------------------------------------------------------------------------

##+++++++++++++++Functions for WQ_indexBOM.R++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##################################################################################
## The following function calculates a binary compliance index.                 ##
## If the value exceeds the guidelines, it gets a value of zero otherwise it    ##
## gets a value of one.                                                         ##
## parameters:                                                                  ##
##    value:       a character vector of measurements (of a single measurement) ##
##    guideline:   a data frame row for the associated guideline value          ##
##    benchmark:   a single character string indicating which field of          ##
##                  the guidelines data frame is to be considered the           ##
##                  guideline value.                                            ##
## returns:                                                                     ##
##    vector:      a numeric [0,1] vector of binary indexes                     ##
##  -------------------------------------------------------------------------   ##
## Change log: Commented out. Not being used from 2015 report card onwards      ##
## Update by: Sandra Johnson, May 2015                                  SJ 2015 ##
##################################################################################
WQ_WQI_binary<- function(dat) {
    return(ifelse((dat$DirectionOfFailure=='H' & !dat$Value>dat$GL | dat$DirectionOfFailure=='L' & !dat$Value<dat$GL), 1,
                              ifelse((dat$DirectionOfFailure=='H' & !dat$Value<dat$GL | dat$DirectionOfFailure=='L' & !dat$Value>dat$GL),0,NA)
                              ))
}

##################################################################################
## The following function calculates the scaled modified amplitude index.       ##
## The index is equal to log2 of the ratio of the value to the guideline value. ##
## The index can also be optionally scaled to a range defined by a              ##
## certain fold (e.g. 2 fold) difference from guidelines.                       ##
## parameters:                                                                  ##
##    wq_dt:       data table of monitoring data, guidelines & rules            ##
##    fold:        optional folding factor (default is 2).                      ##
##                 indexes are rescaled to a maximum of log2(2)                 ##
##                  and a minimum of -log2(2)                                   ##
## returns:                                                                     ##
##    vector:      a numeric [-1,1] vector of indexes                           ##
##################################################################################
WQ_WQI_mamp <- function(dat, fold=2, capped=TRUE,scaled=TRUE) {
    require(scales)
    e <- NULL
    E <- NULL
    midpoint <- NULL

    foldU<-foldL<-fold
    
    e <- ifelse(dat$DirectionOfFailure=='H' & !is.na(dat$GL), log((dat$Value/dat$GL)^-1,2),
                ifelse(dat$DirectionOfFailure=='L' & !is.na(dat$GL),log((dat$Value/dat$GL)^1,2),NA
                       ))
    E=e
    if (capped) E=ifelse(is.na(dat$Value) | (is.na(dat$GL) & is.na(dat$DirectionOfFailure)), NA,
                    ifelse(e>log(foldU,2), log(foldU,2),ifelse(e< -log(foldL,2), -log(foldL,2),e)))
    if(scaled) {
        return(scales:::rescale(E,from=c(-log(foldL,2), log(foldU,2)), to=c(0,1)) )
    } else return(E)
}


## The following function needs to be tested!!
WQ_WQI_CCME <- function(dat) {
        data.F1 = dat %>% mutate(Binary=WQ_WQI_binary(.)) %>%
            group_by(Longitude,Latitude,waterYear,Measure,Region,WaterBody,Zone,Source) %>%
            summarize(Exceed=ifelse(sum(Binary,na.rm=TRUE)==0,1,0)) %>%
            group_by(Longitude,Latitude,waterYear,Region,WaterBody,Zone,Source) %>%
            summarize(F1=sum(Exceed,na.rm=TRUE)/n())
        data.F2 = dat %>% mutate(Binary=WQ_WQI_binary(.)) %>%
            group_by(Longitude,Latitude,waterYear,Region,WaterBody,Zone,Source) %>%
            summarize(F2=ifelse(sum(Binary,na.rm=TRUE)==0,1,0))
        data.F3 =dat %>% group_by(Longitude,Latitude,waterYear,Region,WaterBody,Zone,Source) %>%
        do({
            dat.1=.
            lambda = ifelse(dat.1$DirectionOfFailure=='H', -1,1)
            z = 1-WQ_WQI_binary(dat.1)
            e = z * (((dat.1$Value/dat.1$GL)^lambda)-1)
            E = sum(e, na.rm=TRUE)/length(e)
            dat.1$F3=(100*E)/(1+E)
            dat.1
        })
        data.F1 %>% full_join(data.F2) %>% full_join(data.F3) %>%
            mutate(Index=100-(sqrt(F1^2 + F2^2 + F3^2)/sqrt(3)))
}



## The following function calculates the maximum run of exceedences
## as a measure of exceedence duration.
WQ_exceed <- function(x) {
    mean(x)
}
WQ_duration <- function(x) {
    if (length(x)==1) {
        m=x
        n=1
    } else {
        r = rle(x)
        if (!any(r$values==0)) m=1
        else {
            m = max(r$lengths[r$values==0])
            n=length(x)
            m=1-(m/n)
        }
    }
    m
}


##------------------------------------------------------------------------------------------------------------

## The following functions all related to Bootstrapping

######################################################################################
## The following function  implements the Bags of little bootstrap algorithm        ##
## to calculate means and confidence intervals.                                     ##
##                                                                                  ##
## Input: Data set to calculate point estimates and CI, gamma and r paramters       ##
##                                                                                  ##
##  Output: Computed means and corresponding CI                                     ##
######################################################################################


Baglb <- function (data, gamma, r) {
    #print(dim(data))
    n = nrow(data)
    b = ceiling(n*gamma)
    s = n %/% b
    Xbar = Xbar1 <- c()
    CI <- matrix(NA, nrow = s, ncol =2)
    for (j in 1:s) {
        #print(paste('j=',j))
        I = sample(1:n, b)
        resam = rmultinom(r, n, rep(1/b, b))
        d=as.matrix(data[I,'Boot'])
        Xbar = apply(resam, 2, function(x) {t(d)%*%x /n})
        #for (k in 1:r) {
        #    print(paste('k=',k))
        #    d=as.matrix(data[I,'Boot'])
        #    #print(head(data[I,'Boot']))
        #    #print(class(data[I,'Boot']))
        #    Xbar[k] = (d%*%resam[,k]) / n  # weighted mean
        #                                # I think it is possible to use the mean of the  multinomial distribution p_i*n
        #                                # Xmean[k] = (resam[,k]/max(resam[,k]))%*%n
        #}
        Xbar1[j] <- mean(Xbar, na.rm=TRUE)
        CI[j,] = quantile(Xbar, probs = c(0.05, 0.95), na.rm=TRUE)
    }
    CIf =apply(CI, 2, mean, na.rm=TRUE)
    m = mean(Xbar1, na.rm=TRUE)
    return(data.frame(Mean=m, lower=CIf[1], upper=CIf[2]))
}






MMP_bootStats <- function(dat, AggOver=NULL, Uncertainty.within=FALSE) {
    grps.all=grps=dat %>% groups %>% as.character
    if (!is.null(AggOver)) grps=grps[-which(grps == AggOver)]
    a=dat %>% group_by_(.dots=grps) %>% summarize(Mean=round(mean(Boot,na.rm=TRUE),decimal.places), SD=round(sd(Boot,na.rm=TRUE),decimal.places)) %>%
        mutate(#Confidence=scales:::rescale(sqrt(Var), from=c(0,sqrt(var(c(rep(0,5000),rep(1,5000))))), to=c(1,0)),
               #Signal = ifelse(Confidence>=0.8,5,ifelse(Confidence>=0.6,4,ifelse(Confidence>=0.4,3,ifelse(Confidence>=0.2,2,1)))),
               Grade=MMP_generateGrades(Mean))
    b=dat %>% 
        sample_ns(seed=Seed,size=Size, replace = TRUE) %>% mutate(Rep=1:n()) %>%
            group_by_(.dots=c(grps,'Rep'))
    if (Uncertainty.within) {
        b=b%>%mutate(n=n(),P=rbinom(n,1,0.5)) %>%
            summarize(Value=weighted.mean(Boot,Weight*P,na.rm=TRUE))
    } else {
        b=b%>%summarize(Value=weighted.mean(Boot,Weight,na.rm=TRUE))
    }
    b=b%>%ungroup() %>% group_by_(.dots=grps) %>%
        summarize(mean=round(mean(Value,na.rm=TRUE),decimal.places),
                  lower=round(quantile(Value,na.rm=TRUE,p=0.025),decimal.places),
                  upper=round(quantile(Value,na.rm=TRUE,p=0.975),decimal.places))
    a %>% full_join(b,by=grps)
}

WQ_bootStats <- function(dat, AggOver=NULL) {
    grps.all=grps=dat %>% groups %>% as.character
    #print(grps)
    if (!is.null(AggOver)) grps=grps[-which(grps == AggOver)]
    #print(grps) 
    a=dat %>% dplyr:::group_by_(.dots=grps) %>% dplyr:::summarize(Mean=round(mean(Boot,na.rm=TRUE),decimal.places), Var=round(var(Boot,na.rm=TRUE),decimal.places),
                                                                  Median=round(median(Boot,na.rm=TRUE)),Signal=boot_signal(var(Boot,na.rm=TRUE))) %>%
        dplyr:::mutate(Confidence=scales:::rescale(sqrt(Var), from=c(0,sqrt(var(c(rep(0,5000),rep(1,5000))))), to=c(1,0)),
               Signal = ifelse(Confidence>=0.8,5,ifelse(Confidence>=0.6,4,ifelse(Confidence>=0.4,3,ifelse(Confidence>=0.2,2,1)))),
               Grade=MMP_generateGrades(Mean))
    b=dat %>% 
        sample_ns(seed=Seed,size=Size, replace = TRUE) %>% dplyr:::mutate(Rep=1:n()) %>%
            dplyr:::group_by_(.dots=c(grps,'Rep')) %>%#group_by(Rep, add=TRUE) %>%
                dplyr:::summarize(Value=weighted.mean(Boot,Weight,na.rm=TRUE)) %>%
                ungroup() %>%
                dplyr:::group_by_(.dots=grps) %>%
                dplyr:::summarize(mean=round(mean(Value,na.rm=TRUE),decimal.places),
                                  median=round(median(Value,na.rm=TRUE),decimal.places),
                                  lower=round(quantile(Value,na.rm=TRUE,p=0.1),decimal.places),
                                  upper=round(quantile(Value,na.rm=TRUE,p=0.9),decimal.places))

    a %>% dplyr:::full_join(b,by=grps)
}


estBeta <- function(x, weights) {
    n <- length(x)
    if (missing(weights)) {
        weights <- rep(1, n)
    }
    else {
        weights <- n * weights/sum(weights)
    }
    sample.mean <- mean(weights * x)
    sample.var <- (mean(weights * x^2) - sample.mean^2) * n/(n - 
                                                       1)
    v <- sample.mean * (1 - sample.mean)
    if (sample.var < v) {
        shape1 <- sample.mean * (v/sample.var - 1)
        shape2 <- (1 - sample.mean) * (v/sample.var - 1)
    }
    else {
        shape2 <- sample.mean * (v/sample.var - 1)
        shape1 <- (1 - sample.mean) * (v/sample.var - 1)
    }
    est.par <- list(shape1 = shape1, shape2 = shape2)
    est.par.se <- rep(NA, length(est.par))
    return(est.par)
}

estBinom <- function(x) {
    fnobj <- function(par, fix.arg, obs, ddistnam) {
        -sum(do.call(ddistnam, c(list(obs), as.list(par), as.list(fix.arg), 
                                 log = TRUE)))
    }
    optim(par = list(prob=0.5), fn = fnobj, fix.arg = list(size=1), 
                 obs = x, gr = NULL, ddistnam = 'dbinom', hessian = TRUE, 
            method = 'BFGS', lower = -Inf, upper = Inf)$par[[1]]
}

WQ_aggStats <- function(dat) {
   dat %>% do({
        x=.
        if(length(x$Boot)==1) {
            if (x$Boot %in% c(0,1)) e=list(dist='binom',param1=NaN,param2=NaN)
            else e=list(dist='beta',param1=NaN, param2=NaN)      
        }else {
            if (paste(names(table(x$Boot)),collapse='')=='01') e=list(dist='binom', param1=estBinom(x), param2=NaN)
            else {
                e=estBeta(x$Boot[!is.na(x$Boot)])
                e=list(dist='beta',param1=e[[1]],param2=e[[2]])
            }
        }
        data.frame(Mean=mean(x$Boot, na.rm=TRUE), Median=median(x$Boot, na.rm=TRUE), n=length(x$Boot),dist=e[[1]],param1=e[[2]], param2=e[[3]])
        })
}


WQ_reportCardColors = c('#00734D','#B0D235','#F0C918','#F47721','#ED1C24')
WQ_reportCardColorPalette = colorRampPalette(WQ_reportCardColors)

WQ_gradeBoundaries = c(0.5 + (2/3*0.5), 0.5+1/3*0.5, 0.5, 0.25,0)
WQ_gradeBoundaries.mmp = c(0.5 + (2/3*0.5), 0.5+1/3*0.5, 0.5, 0.25,0)
WQ_gradeBoundaries.ghhp = c(0.85, 0.65, 0.5, 0.25,0)
WQ_gradeBoundaries.uniform = c(0.8, 0.6, 0.4, 0.2,0)

WQ_gradeMids = function(type='MMP') {
    gradeBoundaries=NA
    if (type=='MMP') gradeBoundaries=zoo:::rollmean(c(1,WQ_gradeBoundaries.mmp),2)
    if (type=='GHHP') gradeBoundaries=zoo:::rollmean(c(1,WQ_gradeBoundaries.ghhp),2)
    if (type=='Uniform') gradeBoundaries=zoo:::rollmean(c(1,WQ_gradeBoundaries.uniform),2)
    return(gradeBoundaries)
}
WQ_gradeBoundaries = function(type='MMP') {
    gradeBoundaries=NA
    if (type=='MMP') gradeBoundaries=c(1,WQ_gradeBoundaries.mmp)
    if (type=='GHHP') gradeBoundaries=c(1,WQ_gradeBoundaries.ghhp)
    if (type=='Uniform') gradeBoundaries=c(1,WQ_gradeBoundaries.uniform)
    return(gradeBoundaries)
}


MMP_generateGrades <- function(x) {
    ifelse(is.na(x),'NA',ifelse(x>=0.5 + (2/3*0.5), 'A', ifelse(x>=0.5+1/3*0.5, 'B', ifelse(x>=0.5, 'C',  ifelse(x>=0.25, 'D', 'E')))))
}

WQ_generateGrades <- function(x,type='MMP') {
    if (!type %in% c('Uniform','MMP','GHHP')) warning('type must be either MMP, GHHP or Uniform')
    if (type=='MMP')
        g=ifelse(is.na(x),'NA',ifelse(x>=0.5 + (2/3*0.5), 'A', ifelse(x>=0.5+1/3*0.5, 'B', ifelse(x>=0.5, 'C',  ifelse(x>=0.25, 'D', 'E')))))
    if (type=='GHHP')
        g=ifelse(is.na(x),'NA',ifelse(x>=0.85, 'A', ifelse(x>=0.65, 'B', ifelse(x>=0.5, 'C',  ifelse(x>=0.25, 'D', 'E')))))
    if (type=='Uniform')
        g=ifelse(is.na(x),'NA',ifelse(x>=0.8, 'A', ifelse(x>=0.6, 'B', ifelse(x>=0.4, 'C',  ifelse(x>=0.2, 'D', 'E')))))
    return(g)
}



sample_ns <- function(df, seed=seed, size=Size, replace=TRUE){
    #print(size)
    set.seed(seed)
    sample_n(df,size=size, replace=replace)
}

WQ_weights <- function(data, weights,lev=NULL) {
    #print(data)
    wt=1            # }
    o <- NULL       # } set some initial values
    Value=1         # }
    data.cols = colnames(data %>% dplyr:::select(-Boot))
    if (nrow(weights)>0) {
        for (i in 1:nrow(weights)) {
            wts = weights[i,!is.na(weights[i,])]
            weights.cols = colnames(wts %>% dplyr:::select(-Weight))
            if (all((lev %in% weights.cols) & (weights.cols %in% data.cols))) { # comment 1
                #d1 = data %>% select_(.dots=weights.cols)
                ##print(d1)
                #print(wts)
                #d1 = suppressWarnings(left_join(d1,wts, by=weights.cols))
                                        #d2 = d2 %>% mutate(Weight=ifelse(is.na(Weight),1,Weight))
                o = rbind(o, wts) #c(o,ifelse(is.na(d1$Weight[1]),1,d1$Weight[1]))
                #print(o)
            }
        }
    }
    if (!is.null(o)) {
        data = suppressWarnings(left_join(data,o, by=colnames(o %>% dplyr:::select(-Weight))))
        data = data %>% mutate(Weight=ifelse(is.na(Weight),1,Weight))
    } else {
        data = data %>% mutate(Weight=1)
    }
    
    data %>% mutate_(P=paste0('length(unique(',lev,'))')) %>%
        group_by_(.dots=lev, add=TRUE) %>%
        do({sample_ns(.,seed=Seed, size=ceiling((Size/.$P[1])*.$Weight[1]), replace=TRUE)}) %>%
        dplyr:::select(-P)
                                        #%>% sample_ns(seed=Seed, size=Size, Weight=Weight, replace=TRUE)
}
WQ_Overwrites<- function(data, overwrite, recursive=FALSE) {
    if (nrow(overwrite)>0) {
        overwrite = overwrite %>% dplyr:::rename(Grade1=Grade)
        measures <- c('Component','Indicator','Subindicator','Measure')
        cols.data <- colnames(data)[colnames(data) %in% measures]
        if (recursive==FALSE) {
            other.cols <-measures[is.na(match(measures,cols.data))]
                                        #print(other.cols)
            ovwts = overwrite %>% dplyr:::filter_(.dots=paste0('!is.na(',cols.data,')'))
            data = data %>% left_join(ovwts, by=cols.data)
                                        # if (recursive
            data = data %>% dplyr:::mutate(
                Match=
                    ifelse(!is.na(ZONE.y) & as.character(ZONE.x) == as.character(ZONE.y),TRUE,
                           ifelse(!is.na(Site.y) & as.character(Site.x) == as.character(Site.y),TRUE,
                                  ifelse(is.na(ZONE.y) & is.na(Site.y) & !is.na(Grade1), TRUE,
                                         ifelse(as.character(ZONE.x) != as.character(ZONE.y) | as.character(Site.x) != as.character(Site.y), FALSE, 
                                                ifelse(!is.na(Grade1) & !is.na(Score),TRUE,FALSE)
                                                )
                                         )
                                  )
                           ),
                Match=ifelse(is.na(Match),FALSE,Match),
                Boot=ifelse(!Match,Boot,
                    ifelse(Grade1=='-',NA,Score))
                
            )
            data= data %>% dplyr:::select(-ZONE.y,-Site.y,-Grade1,-Score) %>% dplyr:::rename(ZONE=ZONE.x, Site=Site.x, EO=Match)
        } else {
            measures <- c('Component','Indicator','Subindicator','Measure','ZONE','Site')
            data$Match=FALSE
            replaces <-NULL
            for (i in 1:nrow(overwrite)) {
                ovwts=overwrite[i,] %>% "["(colSums(!is.na(.)) > 0) #remove columns with missing data from ovwts
                cols.data = cols.ovwts=colnames(ovwts)[colnames(ovwts) %in% measures]
                cols.data = colnames(data)[colnames(data) %in% cols.data]
                if(length(cols.data)==length(cols.ovwts) && cols.data %in% cols.ovwts) {
                    data1 = data %>% left_join(ovwts, by=cols.data)
                    data1 = data1 %>% dplyr:::mutate(
                        Match=
                            ifelse(exists(x='Zone',where=.) & !is.na(Grade1),TRUE,FALSE),
                        Match=ifelse(is.na(Match),FALSE,Match),
                        Boot=ifelse(!Match,Boot,
                            ifelse(Grade1=='-',NA,Score))
                        
                    )
                    mtchs <- data1$Match==TRUE
                    if (any(mtchs)) {
                        data[mtchs,'Boot'] <- data1[mtchs,'Boot']
                        data$Match[mtchs] <- data1$Match[mtchs]
                    } 
                }
            }
            data= data %>% dplyr:::rename(EO=Match)
        }
    } else {
        data = data %>% mutate(EO=FALSE)
    }
    data
}
boot_signal <-function(Var) {
    Confidence=scales:::rescale(sqrt(Var), from=c(0,sqrt(var(c(rep(0,5000),rep(1,5000))))), to=c(1,0))
Signal = ifelse(Confidence>=0.8,5,ifelse(Confidence>=0.6,4,ifelse(Confidence>=0.4,3,ifelse(Confidence>=0.2,2,1))))
Signal
}





estDist <- function(vM) {
    if (paste(names(table(vM)), collapse='')=='01') {
        vM=vM[!is.na(vM)]
        e=list(dist='binom', param1=estBinom(vM), param2=NaN)
    } else {
        e=estBeta(vM[!is.na(vM)])
        if (any(is.nan(unlist(e)))) { #NaN estimates when all values either 0 or 1
        } else if(any(unlist(e)<=0)) {  #Negative beta parameters not valid, so resample to generate larger data set and try again
            vM=vM[!is.na(vM)]
            vM=sample(vM,1000,replace=TRUE)
            e=estBeta(vM)
            }
        e=list(dist='beta',param1=e[[1]],param2=e[[2]])
    }
    e
}




## R: number of randomizations
## P: number of 'groups' - input distributions
## M: an RxP matrix of random draws from input distributions 
##param1,param2: are the parameters of the distribution of means
## If estDist for a beta returns zero or negative parameters - this indicates
## that the data do not match a beta...
WQ_AS <- function(dat, aggOver=NULL,R=100) {
    dat %>% do({
        x=.
#        print(x)
        ## Count the number of groups
        #eval(parse(text=paste0('P=length(unique(x$',aggOver,'))')))
        if (exists('param1',x)) {
            mBin = x %>% filter(Dist=='binom')
                                        #eval(parse(text=paste0('P=length(unique(mBin$',aggOver,'))')))
            P = nrow(mBin)
            if (nrow(mBin)>0) {
                MBin = matrix(rbinom(R*P, prob=mBin$param1, size=1), byrow=TRUE,ncol=P)
                wch=which(is.na(mBin$param1))
                MBin[,wch] <- matrix(rep(mBin$Mean[wch],each=nrow(MBin)),nrow=nrow(MBin))
            } else MBin=NULL
            mBeta = x %>% filter(Dist=='beta')
            eval(parse(text=paste0('P=length(unique(mBeta$',aggOver,'))')))
            P = nrow(mBeta)
            if (nrow(mBeta)>0) {
                MBeta = matrix(rbeta(R*P, mBeta$param1, mBeta$param2), byrow=TRUE,ncol=P)
                wch=which(is.na(mBeta$param1))
                MBeta[,wch] <- matrix(rep(mBeta$Mean[wch],each=nrow(MBeta)),nrow=nrow(MBeta))
            } else MBin=NULL
            M=cbind(MBin, MBeta)
            vM=as.vector(M)
            ci=quantile(vM,p=c(0.025,0.975),na.rm=TRUE)
            e=estDist(vM)
        } else { # That is, when only raw data available
            P = length(x$Boot)
            M = cbind(replicate(P,sample(x$Boot, size=R,replace=TRUE)))
            ci=quantile(as.vector(M),p=c(0.025,0.975),na.rm=TRUE)
            if(length(x$Boot)==1) {
                if (x$Boot %in% c(0,1)) e=list(dist='binom',param1=NaN,param2=NaN)
                else e=list(dist='beta',param1=NaN, param2=NaN)
            }else {
                e=estDist(x$Boot)
            }
            fvM=as.vector(M)
            fe=estDist(fvM)
            fci=quantile(fvM,p=c(0.025,0.975), na.rm=TRUE)
        
        }
        m=rowMeans(M,na.rm=TRUE)
        M = M[!is.na(M)]
        if (length(unique(M))==1) m.ci=c(NA,NA)
        else m.ci=t.test(M)$conf.int
        stats=c(mean(m,na.rm=TRUE),median(m,na.rm=TRUE),quantile(m,p=0.025, na.rm=TRUE),quantile(m,p=0.975,na.rm=TRUE))
        
        if (exists('Param1', x)) {
            fmBin = x %>% filter(Dist=='binom')
                                        #eval(parse(text=paste0('P=length(unique(fmBin$',aggOver,'))')))
            P=nrow(fmBin)
            if (nrow(fmBin)>0) {
                fMBin = matrix(rbinom(R*P, prob=fmBin$Param1, size=1), byrow=TRUE,ncol=P)
                wch=which(is.na(fmBin$Param1))
                fMBin[,wch] <- matrix(rep(fmBin$Mean[wch],each=nrow(fMBin)),nrow=nrow(fMBin))
            } else fMBin=NULL
            fmBeta = x %>% filter(Dist=='beta')
            ##eval(parse(text=paste0('P=length(unique(fmBeta$',aggOver,'))')))
            P=nrow(fmBeta)
            if (nrow(fmBeta)>0) {
                fMBeta = matrix(rbeta(R*P, fmBeta$Param1, fmBeta$Param2), byrow=TRUE,ncol=P)
                wch=which(is.na(fmBeta$Param1))
                fMBeta[,wch] <- matrix(rep(fmBeta$Mean[wch],each=nrow(fMBeta)),nrow=nrow(fMBeta))
            } else fmBeta = NULL
            fM=cbind(fMBin, fMBeta)
            fvM=as.vector(fM)
            fci=quantile(fvM,p=c(0.025,0.975),na.rm=TRUE)
            fe=estDist(fvM)
#            fvM = fvM[!is.na(fvM)]
#            fcit=ifelse(length(unique(fvM))==1,c(NA,NA),t.test(fvM)$conf.int)
        } else {
#            fe = e
#            fci=ci
        }
        

        fvM = fvM[!is.na(fvM)]
        if (length(unique(fvM))==1) fcit=c(NA,NA)
        else fcit=t.test(fvM)$conf.int
        
        ## asdf
        ## if (exists('param1', x)) {
        ##     M=cbind(replicate(P, generateValues(x,R=100)))
        ##     print(M)
        ##     asdf
        ##     vM=as.vector(M)
        ##     ci=quantile(vM,p=c(0.025,0.975),na.rm=TRUE)
        ##     e=estDist(vM)
        ## } else {
        ##     M = cbind(replicate(P,sample(x$Boot, size=R,replace=TRUE)))
        ##     ci=quantile(as.vector(M),p=c(0.025,0.975),na.rm=TRUE)
        ##     if(length(x$Boot)==1) {
        ##         if (x$Boot %in% c(0,1)) e=list(dist='binom',param1=NaN,param2=NaN)
        ##         else e=list(dist='beta',param1=NaN, param2=NaN)
        ##     }else {
        ##         e=estDist(x$Boot)
        ##         ## if (paste(names(table(x$Boot)),collapse='')=='01') e=list(dist='binom', param1=estBinom(x$Boot), param2=NaN)
        ##         ## else {
        ##         ##     e=estBeta(x$Boot[!is.na(x$Boot)])
        ##         ##     e=list(dist='beta',param1=e[[1]],param2=e[[2]])
        ##         ## }
        ##     }
        ## }
        ## if (exists('Param1', x)) {
        ##     fM=cbind(replicate(P, GenerateValues(x,R=100)))
        ##     fvM=as.vector(fM)
        ##     fci=quantile(fvM,p=c(0.025,0.975),na.rm=TRUE)
        ##     fe=estDist(fvM)
        ## } else {
        ##     fe = e
        ##     }
        
        ## m=rowMeans(M)
        ##ee=estDist(m)
#        print(m)
                                        #       print(stats)
        data.frame(Mean=mean(x$Boot, na.rm=TRUE), Median=median(x$Boot, na.rm=TRUE),
                   cv=var(x$Boot,na.rm=TRUE)/mean(x$Boot,na.rm=TRUE),
                   CV=var(m,na.rm=TRUE)/mean(m,na.rm=TRUE),n=length(x$Boot),
                   lower=stats[3], upper=stats[4], dist=e[[1]], param1=e[[2]], param2=e[[3]],Lower=fci[1],Upper=fci[2],
                   Dist=fe[[1]],Param1=fe[[2]], Param2=fe[[3]],
                   mCI.lower=m.ci[1],mCI.upper=m.ci[2],fCI.lower=fcit[1],fCL.upper=fcit[2])
        
        })
}


geodist <- function(points, ref) {
    ## Spherical Law of Cosines (http://www.movable-type.co.uk/scripts/latlong.html)
    D <- (60*1.1515*acos (sin(pi*ref$y/180) * sin(pi*points$y/180) +
                          cos(pi*ref$y/180) * cos(pi*points$y/180) * cos((ref$x-points$x) *pi / 180) 
                          )  * 180 / pi)
    D
}

getFocalPoints <- function(data,ref) {
  cat("\nGetting nearest focal points to the reference points\n")
  pb <- txtProgressBar(max=nrow(ref), style=3)
  j <- NULL
  #get the first date (day)
  fd <- data$Date[1]
  for (i in 1:nrow(ref)) { #for each of the reference points
                                        #find the most similar location to the reference points
      b<-with(subset(data, Date==fd), geodist(points=list(x=Longitude,y=Latitude),ref=list(x=ref$Longitude[i],y=ref$Latitude[i])))
      if (all(is.na(b))) {
      }else {
          bb<-data[which(b==min(b,na.rm=TRUE))[1],]
          j <- rbind(j,data.frame(reef.alias=ref$reef.alias[i],filter(data, Longitude == bb$Longitude, Latitude==bb$Latitude)))
      }
      setTxtProgressBar(pb,i)
  }
  close(pb)
  j
}

getSurroundingPoints <- function(data,ref,radius=1) {
  cat("\nGetting points surrounding focal points to the specified radius\n")
  pb <- txtProgressBar(max=nrow(ref), style=3)
  dat <- NULL
  #print(head(data))
  for (i in 1:nrow(ref)) {
      #print(ref)
      dat <- rbind(dat,
                   data.frame(data[data$long.i<=(ref$long.i[i]+radius)
                              & data$long.i>=(ref$long.i[i]-radius)
                              & data$lat.i<=(ref$lat.i[i]+radius)
                              & data$lat.i>=(ref$lat.i[i]-radius),],
                         ref.long.i=ref$long.i[i],
                         ref.lat.i=ref$lat.i[i],
                         ref.long=ref$Longitude[i],
                         ref.lat=ref$Latitude[i],reef.alias=ref$reef.alias[i]))
      setTxtProgressBar(pb,i)
  }
  close(pb)
  dat
}


getRadialPoints <- function(data,coords,radius=1) {
    ##get the actual focal points in the dataset
    fp <- getFocalPoints(data, coords)
    ##get the lat.i and long.i values for the focal points (unique)
    #fp1<-plyr:::ddply(fp,~lat.i+long.i,function(df) {data.frame(Longitude=unique(df$Longitude),Latitude=unique(df$Latitude),reef.alias=unique(df$reef.alias))})
    fp <- fp %>% group_by(lat.i, long.i) %>% summarise(Longitude=unique(Longitude), Latitude=unique(Latitude), reef.alias=unique(reef.alias)[1]) %>% ungroup
    #print(head(fp1))
    #print(head(fp))
    ##get all the points within a x units around the focal point (for all days)
    rp<- getSurroundingPoints(data,subset(fp,!is.na(Longitude)),radius=radius)
    rp
}

## data are the satellite or flood plume data
## ref are the waypoint midpoints
getWaypoints <- function(data, ref, radius=1) {
    if (any(names(data)=='Date')) {
        data.d <- filter(data, Date==min(Date, na.rm=TRUE))
        data.d$long.i <- as.numeric(factor(data.d$Longitude))
        data.d$lat.i <- as.numeric(factor(data.d$Latitude))
        g=getRadialPoints(data.d,ref,radius=radius)
    } else {
        g=getRadialPoints(data,ref,radius=radius)
    }
    g
}



d_longlat <- function(longitude,latitude, d=1,r_earth=6371, angle=0) {
    brng=angle*(pi/180)
    lat1 = pi*latitude/180
    lon1 = pi*longitude/180
    lat2= asin(sin(lat1)*cos(d/r_earth) + cos(lat1)*sin(d/r_earth)*cos(brng))
    lon2=lon1 + atan2(sin(brng)*sin(d/r_earth)*cos(lat1), cos(d/r_earth)-sin(lat1)*sin(lat2))
    
    matrix(c(lat2*180/pi,lon2*180/pi),byrow=FALSE,nrow=4)
}


getDist = function(lat,long,rlat,rlong) {
    (60*1.1515*acos (sin(pi*rlat/180) * sin(pi*lat/180) +
                          cos(pi*rlat/180) * cos(pi*lat/180) * cos((rlong-long) *pi / 180) 
                     )  * 180 / pi)
}

##################################################################################################
## The following function finds all satellite observations within a specific radius (km) and    ##
## time lag of each discrete observation (Niskin data)                                          ##
## Input:                                                                                       ##
##    data:    a data.frame containing Latitude,Longitude,Date and Value for Satellite data     ##
##    ref:     a data.frame containing Latitude,Longitude,Date and reef.alias for discrete data ##
##    radius:  maximum spatial distance from reference observation                              ##
##    lag:     maximum temporal distance from reference observation                             ##
## Returns:                                                                                     ##
##    df:      a data.frame containing:                                                         ##
##             - all fields from data                                                           ##
##             - Radius: maximum radius                                                         ##
##             - Lag: maximum lag                                                               ##
##             - reef.alias                                                                     ##
##             - Niskin.Latitude,Niskin.Longitude: focal location                               ##
##             - Niskin.Date: focal date                                                        ##
##             - Niskin: observed value                                                         ##
##             - distSpace: distance from data to reference point (km)                          ##
##             - distTime: time distance from data to reference point (days)                    ##
##             - cdistSpace: binned version of distSpace                                        ##
##             - cdistTime: binned version of distTime                                          ##
##################################################################################################
nearest.space.time <- function(data, ref, radius=1, lag=1) {
    pb <- txtProgressBar(max=nrow(ref), style=3)
    bb = NULL
    for (i in 1:nrow(ref)) {
        a=data %>% dplyr:::select(x=Longitude, y=Latitude) %>%
            geodist(ref = ref %>% slice(i) %>% dplyr:::select(x=Longitude, y=Latitude))
        ## aa=data[which(a==min(a)),] %>% mutate(Latitude=as.numeric(as.character(Latitude)),
        ##                                   Longitude=as.numeric(as.character(Longitude)),
        ##                                   Date=as.Date(Date))
        aa=data[which(between(a,min(a)-radius,min(a)+radius)),] %>% mutate(Latitude=as.numeric(as.character(Latitude)),
                                          Longitude=as.numeric(as.character(Longitude)),
                                          Date=as.Date(Date))
        ## The nearest location
        a1 = data[which.min(a),]
        d = d_longlat(a1$Longitude,a1$Latitude, d=radius, angle=c(0,90,180,270))
        #d = d_longlat(ref$Longitude[i],ref$Latitude[i], d=radius, angle=c(0,90,180,270))
        d = matrix(c(min(d[,1]),max(d[,1]), min(d[,2]),max(d[,2])),nrow=2)
        b=aa %>% #mutate(Latitude=as.numeric(as.character(Latitude)),
                   #       Longitude=as.numeric(as.character(Longitude))) %>%
            filter(between(Date, ref$Date[i]-lag,ref$Date[i]+lag),
                   between(Latitude, d[1,1],d[1,2]),
                   between(Longitude, d[2,1],d[2,2]))
        b = b %>% #dplyr:::select(-Binary,-MAMP,-fMAMP,-fsMAMP,-fsMAMP4) %>%
            mutate(Radius=radius, Lag=lag, reef.alias=ref$reef.alias[i],
                   Niskin.Latitude=ref$Latitude[i], Niskin.Longitude=ref$Longitude[i],
                   Niskin.Date=ref$Date[i],Niskin=ref$Value[i],
                   distSpace=getDist(Latitude,Longitude,Niskin.Latitude,Niskin.Longitude),
                   distTime=abs(as.numeric(as.Date(Niskin.Date)-as.Date(Date))),
                   cdistSpace=cut(distSpace,breaks=-1:(radius+2),label=FALSE)-1,
                   cdistTime=cut(distTime,breaks=-1:(radius+2),label=FALSE)-1
                   )
        bb=rbind(bb,b)    
        setTxtProgressBar(pb,i)
    }
    close(pb)
    bb
}


rmse_1 = function(error) {error = as.vector(as.data.frame(error)[,1]);sqrt(mean(error^2,na.rm=TRUE));}
rmse = function(dat) {
    S=dat$Value
    O=dat$Niskin
    error = S-O
    r=sqrt(mean(error^2,na.rm=TRUE))
    for (i in 1:1000) {
        s=sample(S,replace=TRUE)
        o=sample(O,replace=TRUE)
        r=c(r,sqrt(mean((s-o)^2,na.rm=TRUE)))
    }
    list(r=r[1], r.p=sum(r<=r[1])/1001,rs=r)
}
mae_1 = function(error) {
    error = as.vector(as.data.frame(error)[,1])
    mean(abs(error),na.rm=TRUE)
    }
mae = function(dat) {
    S=dat$Value
    O=dat$Niskin
    error = S-O
    m=mean(abs(error),na.rm=TRUE)
    for (i in 1:1000) {
        s=sample(S,replace=TRUE)
        o=sample(O,replace=TRUE)
        error=s-o
        m=c(m,mean(abs(error),na.rm=TRUE))
    }
    list(m=m[1], m.p=sum(m<=m[1])/1001,ms=m)
    }

#mpe = function(error) {
#    mean(abs(error$error/error$Niskin),na.rm=TRUE)
#    }
mpe = function(dat) {
    S=dat$Value
    O=dat$Niskin
    error = S-O
    m=mean(abs(error/O),na.rm=TRUE)
    for (i in 1:1000) {
        s=sample(S,replace=TRUE)
        o=sample(O,replace=TRUE)
        error=s-o
        m=c(m,mean(abs(error/o),na.rm=TRUE))
    }
    list(m=m[1], m.p=sum(m<=m[1])/1001,ms=m)
    }

## Nash-Sutcliff efficiency between S (simulated/modelled) and O (observed - Niskin)
## NSE = function(dat) {
##     S=dat$Value
##     O=dat$Niskin
##     denom = sum((O-mean(O, na.rm=TRUE))^2)
##     if (denom!=0) {
##         nse = 1-(sum((O-S)^2)/denom)
##     } else {
##         nse=NA
##     }
##     return(nse)
## }
NSE = function(dat) {
    S=dat$Value
    O=dat$Niskin
    temp = function(S,O) {
        denom = sum((O-mean(O, na.rm=TRUE))^2)
        if (denom!=0) {
            nse = 1-(sum((O-S)^2)/denom)
        } else {
            nse=NA
        }
        return(nse)
    }
    nse=temp(S,O)
    for (i in 1:1000) {
        s=sample(S,replace=TRUE)
        o=sample(O,replace=TRUE)
        nse=c(nse,temp(s,o))
    }
    list(nse=nse[1], nse.p=sum(nse>=nse[1])/1001,nses=nse)
}

E = function(dat, j=1) {
    S=dat$Value
    O=dat$Niskin
    temp=function(S,O) {
        denom = sum((O-mean(O, na.rm=TRUE))^2)
        if (denom!=0) {
            e = 1-(sum(abs(O-S)^j)/denom)
        } else {
            e=NA
        }
        return(e)
    }
    e=temp(S,O)
    for (i in 1:1000) {
        s=sample(S,replace=TRUE)
        o=sample(O,replace=TRUE)
        e=c(e,temp(s,o))
    }
    list(e=e[1], e.p=sum(e>=e[1])/1001,es=e)
}

Willmott_d = function(dat) {
    S=dat$Value
    O=dat$Niskin
    Om = mean(O, na.rm=TRUE)
    temp = function(S,O) {
        denom = sum((abs(S-Om) + abs(O-Om))^2)
        if (denom!=0) {
            d = 1-(sum((O-S)^2)/denom)
        } else {
            d = NA
        }
        return(d)
    }
    d=temp(S,O)
    for (i in 1:1000) {
        s=sample(S,replace=TRUE)
        o=sample(O,replace=TRUE)
        d=c(d,temp(s,o))
    }
    list(d=d[1], d.p=sum(d>=d[1])/1001,ds=d)
}

R2 <- function(mod) {
    library(nlme)
    Xmat <- model.matrix(eval(mod$call$fixed)[-2], mod$data)
    X.var <- var(as.vector(nlme::fixef(mod) %*% t(Xmat)))
    Z.var <- sum(suppressWarnings(as.numeric(nlme::VarCorr(mod)[
                                                       rownames(nlme::VarCorr(mod)) != "Residual",1])), na.rm=T)
    R.var <- as.numeric(nlme::VarCorr(mod)[rownames(nlme::VarCorr(mod))==
                                           "Residual", 1])
    (R2.marginal <- X.var/(X.var+Z.var+R.var))
    ## The proportion of variance due to random effects
    (R2.random <- Z.var/(X.var+Z.var+R.var))
    ## The proportion of variance due to residuals
    (R2.resid <- R.var/(X.var+Z.var+R.var))
    ## The conditional R2 (proportion of variance due to fixed and random effects)
    (R2.conditional <- (X.var+Z.var)/(X.var+Z.var+R.var))
    c(R2.marginal=R2.marginal,R2.random=R2.random,R2.resid=R2.resid,R2.conditional=R2.conditional)
}


TableGradeColors <- function(Grade) {
    ifelse(Grade=='NA','',
    ifelse(Grade=='A', gsub('(.)', '\\\\cellcolor[HTML]{00734D}{\\1}', Grade),
    ifelse(Grade=='B', gsub('(.)','\\\\cellcolor[HTML]{B0D235}{\\1}', Grade),
    ifelse(Grade=='C', gsub('(.)','\\\\cellcolor[HTML]{F0C918}{\\1}', Grade),
    ifelse(Grade=='D', gsub('(.)','\\\\cellcolor[HTML]{F47721}{\\1}', Grade),gsub('(.)','\\\\cellcolor[HTML]{ED1C24}{\\1}', Grade)))))
    )
}
