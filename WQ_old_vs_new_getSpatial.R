source('WQ_functions.R')
require(sqldf)
require(dplyr)
require(ggplot2)
require(gridExtra)
require(raster)
require(ncdf4)
require(tidyr)
require(lubridate)
require(foreach)
require(doParallel)

### oldzones
fs = list.files(path='data/GIS/old', pattern='oldspatial.*', full.names = TRUE)
for (i in fs) load(file=i)
oldzones = gsub('data.*/oldspatial\\.(.*).RData','\\1',fs)
load('data/GIS/old/Polys.df_old.RData')
g1=ggplot(Polys.df, aes(y=lat, x=long)) + geom_polygon(aes(group=group),color='black', fill=NA) + theme_bw() + coord_equal() +
    ggtitle('Old Zones')

### newzones
fs = list.files(path='data/GIS', pattern='spatial.*', full.names = TRUE)
for (i in fs) load(file=i)
newzones = gsub('data.*/spatial\\.(.*).RData','\\1',fs)
load('data/GIS/Polys.df.RData')
g2=ggplot(Polys.df, aes(y=lat, x=long)) + geom_polygon(aes(group=group),color='black', fill=NA) + theme_bw() + coord_equal()+
    ggtitle('New Zones')

grid.arrange(g1,g2, nrow=1)
ggsave(filename='Report/figures/Old_vs_new/zones.pdf', grid.arrange(g1,g2, nrow=1), width=10, height=5)
ggsave(filename='Report/figures/Old_vs_new/zones.png', grid.arrange(g1,g2, nrow=1), width=10, height=5, dpi=300)

ggsave(filename='data/Old_vs_new/figures/Compare_zone_map.pdf', grid.arrange(g1,g2, nrow=1), width=10, height=5)
ggsave(filename='data/Old_vs_new/figures/Compare_zone_map.png', grid.arrange(g1,g2, nrow=1), width=10, height=5, dpi=300)
