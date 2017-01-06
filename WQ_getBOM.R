############################################################
## The following module retrieves satellite data from BOM ##
############################################################
require(sqldf)
require(dplyr)
require(raster)
require(ncdf4)


sink('out.txt')
cat('here')
for (yr in as.character(seq(2002,2016,by=1))) {

    download.file(paste0('http://ereeftds.bom.gov.au/ereefs/tds/catalog/ereef/mwq/P1D/',yr,'/catalog.html'),paste0('data/',yr,'.xml'))
    fs=system(paste0("sed -n 's/.*<tt>\\(A.*\\)<\\/tt>.*/\\1/pg' data/",yr,".xml"), intern=TRUE)

    present.files = list.files(path=paste0('data/',yr), pattern=paste0('A',yr))
    for ( i in fs) {
        if (!i %in% present.files)
            download.file(paste0('http://ereeftds.bom.gov.au/ereefs/tds/fileServer/ereef/mwq/P1D/',yr,'/',i),paste0('data/',yr,'/',i))
    }

}


sink()
