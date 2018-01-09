source('WQ_functions.R')
getConfigs()



for (src in c('niskin/','flntu/','','eReefs/','eReefs926/')) {
    base.dir = paste0(src,'data')
    if (!dir.exists(base.dir)) dir.create(base.dir)
    base.dir = paste0(base.dir,'/aggregated')
    if (!dir.exists(base.dir)) dir.create(base.dir)
    for (index in c('Binary','Exceed','MAMP','fsMAMP','fsMAMP4','Max_Duration')) {
        base.dir.1 = paste0(base.dir,'/',index)
        if (!dir.exists(base.dir.1)) dir.create(base.dir.1)
        for (seasonal in c('Annual','Seasonal')) {
            base.dir.2 = paste0(base.dir.1,'/',seasonal)
            if (!dir.exists(base.dir.2)) dir.create(base.dir.2)
        }
    }
}




