###############################################################
## The following module collates the satellite data from BOM ##
## Into timeseries per measure per zone                      ##
## Input:                                                    ##
##     data/YR/        *.df.RData                            ##
## Output:                                                   ##
##     data/processed  *.RData                               ##
###############################################################

source('WQ_functions.R')

require(dplyr)
require(tidyr)
require(foreach)
require(doParallel)

unlink('log/collateBOM.log', recursive=TRUE)

## Now read in each year/measure/zone
## - concatinate (rbind) across all dates
registerDoParallel(cores=3)
fs = list.files(path=paste0('data/2002'), pattern='*.df.RData',full.names = TRUE)
measure = unique(gsub('data/2002/(.*)\\..*.df.RData','\\1',fs))
zone = unique(gsub('data/2002/.*\\.(.*).df.RData','\\1',fs))

foreach (m=measure) %dopar% {
    for (z in zone) {
        eval(parse(text=paste0(m,'_',z,' = NULL')))
        for (yr in as.character(seq(2002,2016,by=1))) {
            WQ_tryCatch(
            {
                fs = list.files(path=paste0('data/',yr), pattern=paste0(m,'.',z,'.df.RData'),full.names = TRUE)
                load(fs)
                eval(parse(text=paste0(m,"_",z," = rbind(",m,"_",z,", ",m,".",z,".df)")))
                rm(fs)
                gc()
            },paste0('collateBOM.log'), msg=paste0('Add year ',yr,' for ',m,' (Zone=',z,')'), return=TRUE)
        }
        WQ_tryCatch(
        {
            eval(parse(text=paste0('save(',m,'_',z,', file="data/processed/',m,'_',z,'.RData")' )))
            eval(parse(text=paste0('rm(',m,'_',z,')')))
            gc()
        },paste0('collateBOM.log'), msg=paste0('Save collation for ',m,' (Zone=',z,')'), return=TRUE)
    }
}

