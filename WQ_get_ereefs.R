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



load('data/GIS/Polys.RData')
load('data/GIS/layers.RData')
plot(Polys)
Polys.df = tidy(Polys)
qld.df = tidy(qld)


## Get the 2014-2017 reanalysis data=====================================================================
flist <- list.files(path = "/export/databases2/ereefs/", pattern = "^.*\\.(nc|NC|Nc|Nc)$")
flist

##Chl: Chl_a_sum, Nap: EFI, SD: 1.7/KD490, NO3
meas.ereef = c('Chl_a_sum','EFI','Kd_490','NO3')
meas = c('chl','nap','sd','NOx')
registerDoParallel(cores=20)
foreach(i=1:length(flist)) %dopar% {
    year = gsub('.*_all_([0-9]{4}).*','\\1',flist[i])
    month = gsub('.*_all_[0-9]{4}-([0-9]{2}).*','\\1',flist[i])

    nc <- nc_open(paste0("/export/databases2/ereefs/", flist[i]))
                                        #print(nc)
    
                                        #attributes(nc)$names
                                        #nc$nvars
    foreach(m=meas) %do% {
        m.e = meas.ereef[which(meas==m)]
        ## get lat and long
        dat = data.frame(Latitude=as.vector(ncvar_get(nc,'y_centre', start=c(1,1),count=c(-1,-1))),
                         Longitude=as.vector(ncvar_get(nc,'x_centre', start=c(1,1),count=c(-1,-1))))
        tm=ncvar_get(nc,'t', start=c(1),count=c(-1))
        tm = as.Date(tm, origin='1990-01-01')
        dat = dat %>% expand(tm,dat)
        dat = dat %>% mutate(Measure=m,Value=as.vector(ncvar_get(nc,c(m.e), start=c(1,1,44,1),count=c(-1,-1,1,-1))))
        if (m=='sd') dat = dat %>% mutate(Value=1.0/Value)  #1.7/Value
        ## if (m=='sd') {
        ##     z_grid=ncvar_get(nc,'z_grid')
        ##     z_thk=diff(z_grid)

        ##     Kd_490=ncvar_get(nc,'Kd_490', start=c(1,1,1,1),count=c(-1,-1,-1,-1))
        ##     PAR=ncvar_get(nc,'PAR', start=c(1,1,1,1),count=c(-1,-1,-1,-1))
        ##     ff = function(p,k,z_thk) {
        ##         if (all(is.na(p)) | sum(p,na.rm=TRUE)==0) return(NA)
        ##         wet=max(which(diff(p)>0))+1
        ##         zz=wet
        ##         if (all(is.infinite(zz))) return(NA)
        ##         while(!is.na(p[zz]) & p[zz]>0.1827*p[wet]) zz=zz-1
        ##         sum(z_thk[(zz-1):wet]*k[(zz-1):wet], na.rm=TRUE)/sum(z_thk[(zz-1):wet],na.rm=TRUE)
        ##     }
        ##     sd = array(0, dim=dim(PAR)[c(1,2,4)])
        ##     pb <- txtProgressBar(max=length(sd), style=3)
        ##     ii = 0
        ##     foreach(y=1:dim(PAR)[1],.combine=c) %do% { #dim(PAR)[1]) %do% {
        ##         foreach(x=1:dim(PAR)[2],.combine=c) %do% { #dim(PAR)[2]) %do% {
        ##             foreach(t=1:dim(PAR)[4], .combine=c) %do% { #dim(PAR)[4]) %do% {
        ##                 a=ff(PAR[y,x,,t],Kd_490[y,x,,t],z_thk)
        ##                 sd[y,x,t] = 1.7/a
        ##                 ii = ii+1
        ##                 setTxtProgressBar(pb,ii)
        ##             }
        ##         }
        ##     }
        ##     Value=as.vector(sd)
        ##     close(pb)            
        ##     ## dat = data.frame(Latitude=as.vector(ncvar_get(nc,'y_centre', start=c(1,1),count=c(-1,-1))),
        ##     ##                  Longitude=as.vector(ncvar_get(nc,'x_centre', start=c(1,1),count=c(-1,-1))))
        ##     ## tm=ncvar_get(nc,'t', start=c(1),count=c(-1))
        ##     ## tm = as.Date(tm, origin='1990-01-01')
        ##     ## z=as.vector(ncvar_get(nc,'z_centre', start=c(1),count=c(-1)))
        ##     ## dat = dat %>% expand(tm,z,dat)
            
        ##     ## z_grid=ncvar_get(nc,'z_grid')
        ##     ## z_thk=diff(z_grid)

        ##     ## Kd_490=ncvar_get(nc,'Kd_490', start=c(1,1,1,1),count=c(-1,-1,-1,-1))
        ##     ## PAR=ncvar_get(nc,'PAR', start=c(1,1,1,1),count=c(-1,-1,-1,-1))
        ##     ## ff = function(p,k,z_thk) {
        ##     ##     if (all(is.na(p)) | sum(p,na.rm=TRUE)==0) return(NA)
        ##     ##     wet=max(which(diff(p)>0))
        ##     ##     zz=wet
        ##     ##     if (all(is.infinite(zz))) return(NA)
        ##     ##     while(!is.na(p[zz]) & p[zz]>0.1827*p[wet]) zz=zz-1
        ##     ##     sum(z_thk[(zz-1):wet]*k[(zz-1):wet], na.rm=TRUE)/sum(z_thk[(zz-1):wet],na.rm=TRUE)
        ##     ## }
        ##     ## sd = array(0, dim=dim(PAR)[c(1,2,4)])
        ##     ## pb <- txtProgressBar(max=length(sd), style=3)
        ##     ## ii = 0
        ##     ## foreach(y=1:dim(PAR)[1],.combine=c) %do% { #dim(PAR)[1]) %do% {
        ##     ##     foreach(x=1:dim(PAR)[2],.combine=c) %do% { #dim(PAR)[2]) %do% {
        ##     ##         foreach(t=1:dim(PAR)[4], .combine=c) %do% { #dim(PAR)[4]) %do% {
        ##     ##             a=ff(PAR[y,x,,t],Kd_490[y,x,,t],z_thk)
        ##     ##             sd[y,x,t] = 1.7/a
        ##     ##             ii = ii+1
        ##     ##             setTxtProgressBar(pb,ii)
        ##     ##         }
        ##     ##     }
        ##     ## }
        ##     ## a=as.vector(sd)
            
        ##     ## a= apply(PAR[98:100,98:100,,],3,'[')
        ##     ## ff = function(p,k,z_thk) {
        ##     ##     if (all(is.na(p))) return(NA)
        ##     ##     wet=max(which(diff(p)>0))
        ##     ##     zz=wet
        ##     ##     while(p[zz]>0.1827*p[wet]) zz=zz-1
        ##     ##     zz
        ##     ## }
        ##     ## a = apply(list(PAR[99:100,99:100,,1:5],Kd_490[99:100,99:100,,1:5]),
        ##     ##           MARGIN=c(1,2,3,4,5),
        ##     ##           function(x) print(x))
        ##     ## a =apply(PAR[99:100,99:100,,1:5],MARGIN=c(1,2,4),FUN=ff,k=Kd_490[99:100,99:100,,],z_thk=z_thk)
        ##     ## Kd_490[99:100,99:100,,1:5]
        ##     ## a = Map(ff,p=PAR[99:100,99:100,,],k=Kd_490[99:100,99:100,,],z_thk=z_thk)
        ##     ## a=apply(PAR[,,,],c(1,2,4),ff,z_thk=z_thk,Kd_490)
        ##     ## kdmean = sum(z_thk[(zz-1):wet]*Kd_490[(zz-1):wet])/sum(z_thk[(zz-1):wet]))
            
        ##     dat = dat %>% mutate(Value=Value)
        ## }
        if (m=='nap') dat = dat %>% mutate(Value=Value*1000)
        dat = dat %>% filter(!is.na(Latitude),!is.na(Longitude))
        coordinates(dat) = ~Longitude+Latitude
        pts=sp:::over(dat,Polys)
        #pts = pts[!is.na(pts)]
        foreach (z=1:length(names(Polys))) %do% {
            pts.1 = if_else(is.na(pts),FALSE,if_else(pts==z,TRUE,FALSE))
            ereefs=as.data.frame(dat[pts.1,])
            save(ereefs,file=paste0('eReefs/data/raw/ereefs_',m,'__',year,'__',month,'___',names(Polys)[[z]],'.RData'))
        }
    }
}

#load(paste0('eReefs/data/raw/ereefs_',sd,'__',2014,'__',month,'___',names(Polys)[[z]],'.RData'))


## ## Get the 2010-2014 926 control run data=====================================================================
## #flist <- list.files(path = "/export/databases2/ereefs/gbr4_bgc_926/", pattern = "^.*\\.(nc|NC|Nc|Nc)$")
## flist <- list.files(path = "data/eReefs926", pattern = "^.*\\.(nc|NC|Nc|Nc)$")
## flist

## ##Chl: Chl_a_sum, Nap: EFI, SD: 1.7/KD490
## meas.ereef = c('Chl_a_sum','EFI','Kd_490','NO3')
## meas = c('chl','nap','sd','NOx')
## registerDoParallel(cores=10)
## foreach(i=1:length(flist)) %dopar% {
##     year = gsub('.*_all_([0-9]{4}).*','\\1',flist[i])
##     month = gsub('.*_all_[0-9]{4}-([0-9]{2}).*','\\1',flist[i])

##     #nc <- nc_open('data/eReefs926/gbr4_bgc_simple_2012-06.nc')
##     #nc <- nc_open(paste0("/export/databases2/ereefs/gbr4_bgc_926/", flist[i]))
##     nc <- nc_open(paste0("data/eReefs926/", flist[i]))
##     foreach(m=meas[-3]) %do% {
##         m.e = meas.ereef[which(meas==m)]
##         ## get lat and long
##         dat = data.frame(Latitude=as.vector(ncvar_get(nc,'y_centre', start=c(1,1),count=c(-1,-1))),
##                          Longitude=as.vector(ncvar_get(nc,'x_centre', start=c(1,1),count=c(-1,-1))))
##         tm=ncvar_get(nc,'t', start=c(1),count=c(-1))
##         tm = as.Date(tm, origin='1990-01-01')
##         dat = dat %>% expand(tm,dat)
##         dat = dat %>% mutate(Measure=m,Value=as.vector(ncvar_get(nc,c(m.e), start=c(1,1,44,1),count=c(-1,-1,1,-1))))
##         if (m=='sd') {
##             z_grid=ncvar_get(nc,'z_grid')
##             z_thk=diff(z_grid)

##             Kd_490=ncvar_get(nc,'Kd_490', start=c(1,1,1,1),count=c(-1,-1,-1,-1))
##             PAR=ncvar_get(nc,'PAR', start=c(1,1,1,1),count=c(-1,-1,-1,-1))
##             ff = function(p,k,z_thk) {
##                 if (all(is.na(p)) | sum(p,na.rm=TRUE)==0) return(NA)
##                 wet=max(which(diff(p)>0))+1
##                 zz=wet
##                 if (all(is.infinite(zz))) return(NA)
##                 while(!is.na(p[zz]) & p[zz]>0.1827*p[wet]) zz=zz-1
##                 sum(z_thk[(zz-1):wet]*k[(zz-1):wet], na.rm=TRUE)/sum(z_thk[(zz-1):wet],na.rm=TRUE)
##             }
##             sd = array(0, dim=dim(PAR)[c(1,2,4)])
##             pb <- txtProgressBar(max=length(sd), style=3)
##             ii = 0
##             foreach(y=1:dim(PAR)[1],.combine=c) %do% { #dim(PAR)[1]) %do% {
##                 foreach(x=1:dim(PAR)[2],.combine=c) %do% { #dim(PAR)[2]) %do% {
##                     foreach(t=1:dim(PAR)[4], .combine=c) %do% { #dim(PAR)[4]) %do% {
##                         a=ff(PAR[y,x,,t],Kd_490[y,x,,t],z_thk)
##                         sd[y,x,t] = 1.7/a
##                         ii = ii+1
##                         setTxtProgressBar(pb,ii)
##                     }
##                 }
##             }
##             Value=as.vector(sd)
##             close(pb)            
            
##             dat = dat %>% mutate(Value=Value)
##         }
##         if (m=='nap') dat = dat %>% mutate(Value=Value*1000)
##         dat = dat %>% filter(!is.na(Latitude),!is.na(Longitude))
##         coordinates(dat) = ~Longitude+Latitude
##         pts=sp:::over(dat,Polys)
##         #pts = pts[!is.na(pts)]
##         foreach (z=1:length(names(Polys))) %do% {
##             pts.1 = if_else(is.na(pts),FALSE,if_else(pts==z,TRUE,FALSE))
##             ereefs=as.data.frame(dat[pts.1,])
##             save(ereefs,file=paste0('eReefs/data/raw/ereefs_',m,'__',year,'__',month,'___',names(Polys)[[z]],'.RData'))
##         }
##     }
## }




## ff = function(x) {
##     wet=max(which(diff(x)>0))+1
##     if (is.infinite(wet)) return(NA)
##     zz=wet
##     while(x[zz]>0.1827*x[wet]) zz = zz-1
##     return(zz)
##     }

## nc <- nc_open(paste0("/export/databases2/ereefs/", flist[i]))
## dat1 = data.frame(Latitude=as.vector(ncvar_get(nc,'y_centre', start=c(50,50),count=c(1,1))),
##                  Longitude=as.vector(ncvar_get(nc,'x_centre', start=c(50,50),count=c(1,1))))
## (Kd_490=as.vector(ncvar_get(nc,'Kd_490', start=c(50,50,1,1),count=c(1,1,-1,1))))
## (PAR=as.vector(ncvar_get(nc,'PAR', start=c(50,50,1,1),count=c(1,1,-1,1))))
## z_grid=ncvar_get(nc,'z_grid')
## z_thk=diff(z_grid)

## (wet=max(which(diff(PAR)>0))+1)
## zz=wet
## while(PAR[zz]>0.1827*PAR[wet]) zz = zz-1
## (kdmean = sum(z_thk[(zz-1):wet]*Kd_490[(zz-1):wet])/sum(z_thk[(zz-1):wet]))


if (1==2) {        
    ## This first bit is a visual of the first day of the month
    lat=as.vector(ncvar_get(nc,'y_centre', start=c(1,1),count=c(-1,-1)))
                                        #lat=as.vector(ncvar_get(nc,'y_grid', start=c(1,1),count=c(-1,-1)))
    lon=as.vector(ncvar_get(nc,'x_centre', start=c(1,1),count=c(-1,-1)))
    tm=ncvar_get(nc,'t', start=c(1),count=c(1))
    tm = as.Date(tm, origin='1990-01-01')
    chl=as.vector(ncvar_get(nc,c('Chl_a_sum'), start=c(1,1,44,1),count=c(-1,-1,1,1)))
    dat = data.frame(Date=tm, Latitude=lat, Longitude=lon, Chl=chl)
                                        #dat %>% filter(Date==Date[1]) %>% 
                                        #ggplot(aes(y=Latitude,x=Longitude)) + geom_tile(aes(fill=Chl)) 
    ggplot(dat, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + scale_color_gradientn(colors=(heat.colors(10))) + coord_equal()
                                        #ggplot(dat, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=0.001) + coord_equal()

    ## Unfortunately, it is an irregular grid..  This means that we cannot simply put it into a raster...
    dat = dat %>% filter(!is.na(Latitude),!is.na(Longitude))
    coordinates(dat) = ~Longitude+Latitude
    pts=sp:::over(dat,Polys)

    gg=list()
    for (i in 1:length(names(Polys))) {
        print(i)
        pts.1 = if_else(is.na(pts),FALSE,if_else(pts==i,TRUE,FALSE))
        a=dat[pts.1,]
        a=as.data.frame(dat[pts.1,]) %>% filter(!is.na(Chl))
        e = with(a, c(min(Longitude), max(Longitude), min(Latitude), max(Latitude)))
        ed1 = e[2]-e[1]
        ed2= e[4]-e[3]
        if (ed2>ed1) {
            e[1:2] = mean(e[1:2]) + c(-1,1)*(e[4]-e[3])/2 
        }else {
            e[3:4] = mean(e[3:4]) + c(-1,1)*(e[2]-e[1])/2 
        }
        gg[[i]]=ggplot(a, aes(y=Latitude,x=Longitude)) +
            geom_polygon(data=Polys.df, aes(y=lat, x=long, group=group), fill=NA, color='grey') +
            geom_polygon(data=qld.df, aes(y=lat, x=long, group=group)) +
            geom_point(aes(color=Chl)) +
                                        #       geom_raster(aes(fill=Chl), interpolate = TRUE) +
                                        #stat_summary_2d(geom='tile', aes(z=Chl), binwidth=0.03)+
            theme_classic() + theme(panel.background = element_rect(color='black',fill='lightblue')) +
            coord_equal(, xlim=e[1:2], ylim=e[3:4]) +
            scale_color_gradientn(colors=heat.colors(10))+
            ggtitle(paste0(names(Polys)[[i]],', ',year,'-',month,'-',01))
        ggsave(gg[[i]], file=paste0('eReefs/data/raw/locations_',year,'__',month,'___',names(Polys)[[i]],'.pdf'))
    }



















    library(akima)
    library(ggplot2) 
    my.df.interp <- with(a %>% filter(!is.na(Latitude),!is.na(Longitude), !is.na(Chl)), interp(x = Longitude, y = Latitude, z = Chl, nx = 30, ny = 30) )
    my.df.interp.xyz <- as.data.frame(interp2xyz(my.df.interp))
    names(my.df.interp.xyz) <- c("Longitude", "Latitude", "Chl")
    ggplot(my.df.interp.xyz, aes(y=Latitude,x=Longitude)) + geom_tile(aes(fill=Chl))

    pts.1 = if_else(is.na(pts),FALSE,if_else(pts==1,TRUE,FALSE))
    a=dat[pts.1,]
    a=as.data.frame(dat[pts.1,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.2 = if_else(is.na(pts),FALSE,if_else(pts==2,TRUE,FALSE))
    a=as.data.frame(dat[pts.2,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.3 = if_else(is.na(pts),FALSE,if_else(pts==3,TRUE,FALSE))
    a=as.data.frame(dat[pts.3,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.4 = if_else(is.na(pts),FALSE,if_else(pts==4,TRUE,FALSE))
    a=as.data.frame(dat[pts.4,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.5 = if_else(is.na(pts),FALSE,if_else(pts==5,TRUE,FALSE))
    a=as.data.frame(dat[pts.5,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.6 = if_else(is.na(pts),FALSE,if_else(pts==6,TRUE,FALSE))
    a=as.data.frame(dat[pts.6,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.7 = if_else(is.na(pts),FALSE,if_else(pts==7,TRUE,FALSE))
    a=as.data.frame(dat[pts.7,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.8 = if_else(is.na(pts),FALSE,if_else(pts==8,TRUE,FALSE))
    a=as.data.frame(dat[pts.8,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.9 = if_else(is.na(pts),FALSE,if_else(pts==9,TRUE,FALSE))
    a=as.data.frame(dat[pts.9,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.10 = if_else(is.na(pts),FALSE,if_else(pts==10,TRUE,FALSE))
    a=as.data.frame(dat[pts.10,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.11 = if_else(is.na(pts),FALSE,if_else(pts==11,TRUE,FALSE))
    a=as.data.frame(dat[pts.11,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.12 = if_else(is.na(pts),FALSE,if_else(pts==12,TRUE,FALSE))
    a=as.data.frame(dat[pts.12,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()

    pts.13 = if_else(is.na(pts),FALSE,if_else(pts==13,TRUE,FALSE))
    a=as.data.frame(dat[pts.13,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=5,shape=16) + coord_equal()

    pts.24 = if_else(is.na(pts),FALSE,if_else(pts==24,TRUE,FALSE))
    a=as.data.frame(dat[pts.24,])
    ggplot(a, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=3) + coord_equal()



    spdf <- with(dat %>% filter(!is.na(Latitude),!is.na(Longitude)), SpatialPointsDataFrame( data.frame( x = Longitude , y = Latitude ) , data = data.frame( z = Chl ) ))
    r <- raster( nrows = 600 , ncols = 180 , ext = extent(spdf) )
    rf <- rasterize( spdf , r , field = "z" , fun = mean )
    plot(rf)
    d = data.frame(rasterToPoints(rf))
    ggplot(d, aes(y=y, x=x)) + geom_tile(aes(fill=layer))

    r = raster(ncols=600,nrows=180)
    d = dat %>% filter(Date==Date[1],!is.na(Latitude),!is.na(Longitude))
    coordinates(d) <- ~Longitude+Latitude
    r = rasterize(d,r, fun='mean')

    r = dat %>% rename(x=Longitude,y=Latitude,z=Chl) %>%
        rasterFromXYZ()

    library(sp)
    spdf <- with(dat %>% filter(!is.na(Latitude),!is.na(Longitude)), SpatialPointsDataFrame( data.frame( x = Longitude , y = Latitude ) , data = data.frame( z = Chl ) ))
    plot(spdf, cex=0.001)
    library(raster)
    e <- extent(spdf)


    b <- brick(paste0("/media/murray/Seagate Expansion Drive/", flist[2]),varname='Chl_a_sum', level=47)
    b = t(b)
    b=flip(b,'x')
    extent(b) <- e
    nlayers(b)
    names(b)
    plot(b)
    plot(b[[1]])

    d = rasterToPoints(b[[1]])
    ggplot(data.frame(d), aes(y=y, x=x)) + geom_tile(aes(fill=X1))



    chl=ncvar_get(nc,c('Chl_a_sum'), start=c(1,1,47,1),count=c(-1,-1,1,1))


    nc <- nc_open(paste0("/media/murray/Seagate Expansion Drive/", flist[2]))
    nc <- nc_open(paste0("/export/databases2/ereefs/", flist[2]))

    print(nc)

    attributes(nc)$names
    nc$nvars

    v1 <- nc$var[['y_grid']]
    lat=ncvar_get(nc,v1, start=c(1,1),count=c(-1,-1))
    lat=as.vector(ncvar_get(nc,'y_centre', start=c(1,1),count=c(-1,-1)))
    lon=as.vector(ncvar_get(nc,'x_centre', start=c(1,1),count=c(-1,-1)))
    tm=ncvar_get(nc,'t', start=c(1),count=c(1))
    tm = as.Date(tm, origin='1990-01-01')

    chl=as.vector(ncvar_get(nc,c('Chl_a_sum'), start=c(1,1,47,1),count=c(-1,-1,1,1)))
    dat = data.frame(Date=tm, Latitude=lat, Longitude=lon, Chl=chl)
    dat %>% filter(Date==Date[1]) %>% 
        ggplot(aes(y=Latitude,x=Longitude)) + geom_tile(aes(fill=Chl)) 
    ggplot(dat, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal()
    ggplot(dat, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=0.001) + coord_equal()

    ggplot(dat, aes(y=Latitude,x=Longitude)) + geom_point(aes(color=Chl), size=1) + coord_equal(ratio=1/cos(mean(dat$Latitude,na.rm=TRUE)*pi/180))


    r<-rasterFromXYZ(data.frame(spdf$x,spdf$y,spdf$z))


    ##=======
    library(sp)
    spdf <- with(dat %>% filter(!is.na(Latitude),!is.na(Longitude)), SpatialPointsDataFrame( data.frame( x = Longitude , y = Latitude ) , data = data.frame( z = Chl ) ))
    plot(spdf, cex=0.001)
    library(raster)
    e <- extent(spdf)
                                        # Determine ratio between x and y dimensions
    ratio <- ( e@xmax - e@xmin ) / ( e@ymax - e@ymin )

                                        # Create template raster to sample to
    r<-rasterFromXYZ(data.frame(spdf$x,spdf$y,spdf$z))

    r <- raster( nrows = 100 , ncols = floor( 100 * ratio ) , ext = extent(spdf) )
    rf <- rasterize( spdf , r , field = "z" , fun = mean )

                                        # Attributes of our new raster (# cells quite close to original data)
    rf

                                        # We can then plot this using `geom_tile()` or `geom_raster()`
    rdf <- data.frame( rasterToPoints( rf ) )    
    ggplot( NULL ) + geom_raster( data = rdf , aes( x , y , fill = layer ) )
    ggplot( NULL ) + geom_tile( data = rdf , aes( x , y , fill = layer ) )

    b <- brick(paste0("/media/murray/Seagate Expansion Drive/", flist[2]),varname='Chl_a_sum', level=47)
    b
    nlayers(b)
    names(b)
    plot(b)

    lat=raster(paste0("/media/murray/Seagate Expansion Drive/", flist[2]),varname='y_centre')
    lon=raster(paste0("/media/murray/Seagate Expansion Drive/", flist[2]),varname='x_centre')
    remap.tbl <- data.frame(coordinates(lon),
                            lon=as.vector(lon),lat=as.vector(lat))
    ggplot(remap.tbl, aes(y=y, x=x)) + geom_point(size=0.0001)
    ggplot(remap.tbl, aes(y=lat, x=lon)) + geom_point(size=0.0001)
    library(akima)

    remap.tbl = remap.tbl %>% filter(!is.na(lat), !is.na(lon))
    lon.pts <- with(remap.tbl, seq(min(lon,na.rm=TRUE), max(lon,na.rm=TRUE), l=1000))
    lat.pts <- with(remap.tbl, seq(min(lat,na.rm=TRUE), max(lat,na.rm=TRUE), l=1000))
    x.pts<-interpp(remap.tbl$lon,remap.tbl$lat,remap.tbl$x,
                   xo=lon.pts,yo=lat.pts)
    y.pts<-interpp(remap.tbl$lon,remap.tbl$lat,remap.tbl$y,
                   xo=lon.pts,yo=lat.pts)
    temp <- extract(b[[1]],data.frame(x.pts$z,y.pts$z),method="bilinear")
    d <- data.frame(lon=x.pts$z, lat=y.pts$z, temp)
    head(d)
    ggplot(d, aes(y=lat,x=lon)) + geom_tile(aes(fill=temp)) + coord_equal()

    x.pts<-interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$x,
                  xo=lon.pts,yo=lat.pts)
    y.pts<-interpp(remap.tbl$lon,remap.tbl$lat,remap.tbl$y,
                   xo=lon.pts,yo=lat.pts)
                                        #pts <- data.frame(lon=lon.pts,lat=lat.pts,x=x.pts$z,y=y.pts$z)
                                        #ggplot(dat, aes(y=lat,x=long)) + geom_tile(aes(fill=Value)) + coord_equal()

    temp <- extract(b[[1]],data.frame(as.vector(x.pts$z),as.vector(y.pts$z)),method="bilinear")
    d <- data.frame(lon=as.vector(x.pts$z), lat=as.vector(y.pts$z), temp)
    head(d)
    ggplot(d, aes(y=lat,x=lon)) + geom_tile(aes(fill=temp)) + coord_equal()



    geo.r <- raster(e)
    res(geo.r) <- c(0.25,0.25)
    r.coords.x <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$x,
                         xo=xFromCol(geo.r),yo=yFromRow(geo.r))
    r.coords.y <- interp(remap.tbl$lon,remap.tbl$lat,remap.tbl$y,
                         xo=xFromCol(geo.r),yo=yFromRow(geo.r))
    r.coords <- expand.grid(lon=xFromCol(geo.r),
                            lat=yFromRow(geo.r))
    r.coords$x <- as.vector(r.coords.x$z)
    r.coords$y <- as.vector(r.coords.y$z)
    r.coords.sp <- r.coords
    coordinates(r.coords.sp) <- ~lat +lon
    r.coords$temp <- extract(b[[1]],r.coords.sp,method="bilinear")

    coordinates(remap.tbl) <- ~x +y
    temp <- extract(b[[1]],data.frame(x.pts,y.pts),method="bilinear")
                                        #temp <- extract(b[[1]],data.frame(r.coords$lon,r.coords$lat),method="simple")
    d <- data.frame(lon=r.coords$lon, lat=r.coords$lat, temp)
    ggplot(d, aes(y=lat,x=lon)) + geom_tile(aes(fill=temp)) + coord_equal()

    geo.r <- setValues(geo.r,r.coords$temp)
    plot(geo.r)
    ggplot(r.coords, aes(y=lat,x=long)) + geom_tile(aes(fill=temp)) + coord_equal()

    ##=========
    chl=ncvar_get(nc,c('Chl_a_sum'), start=c(1,1,47,1),count=c(-1,-1,1,1))
    dat = data.frame(lat=nrow(chl):1, chl) %>% gather(long,Value,-lat) %>% mutate(long=as.numeric(str_sub(long,2)))

    dat$lat = lat
    dat$long=lon
    ggplot(dat, aes(y=lat,x=long)) + geom_tile(aes(fill=Value)) + coord_equal()
    ggplot(dat, aes(y=lat,x=long)) + geom_point(aes(color=Value), size=0.0001) + coord_equal()


    dat = data.frame(Date=tm, Latitude=lat, Longitude=lon, Chl=chl)
    dat %>% filter(Date==Date[1]) %>% 
        ggplot(aes(y=Latitude,x=Longitude)) + geom_tile(aes(fill=Chl)) 

    a=ncvar_get(nc,c('Chl_a_sum'), start=c(1,1,47,1),count=c(-1,-1,1,-1))
    image(a[,,1])
    ncatt_get(nc, attributes(nc$var)$names[1])
    chla_mean <- ncvar_get(nc, attributes(nc$var)$names[1])

    attributes(nc$dim)$names
    nc_lat <- ncvar_get( nc, attributes(nc$dim)$names[1])
    nc_lon <- ncvar_get( nc, attributes(nc$dim)$names[2])

    library(raster)

    b <- brick(paste0("/media/murray/Seagate Expansion Drive/", flist[2]),varname='latitude')
}
