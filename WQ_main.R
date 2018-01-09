#########################################################################
## The following scripts consistitute the statistical analyses for     ##
## NESP 3.2.5 - Testing and implimentation of the water quality metric ##
## for 2017 and 2018 reef report cards.                                ##
##                                                                     ##
## They have been written by Murray Logan (AIMS) and run under R/3.2.5 ##
## Ensure that the following HPC modules are loaded:                   ##
## module load R/3.2.5                                                 ##
#########################################################################

source('WQ_functions.R')
getConfigs()


source('WQ_pathPreparations.R')


############################################
## Index/aggregation Sensitivity analysis ##
############################################
if (sensititity.tests) source('WQ_sensitivity.R')

##################################################
## Retrieve, process and index the data sources ##
##################################################

##,---------------
##| Satellite data
##`---------------
## if (getSatelliteData) source('WQ_getData.R')
if (get.BOM) source('WQ_getBOM.R')
if (process.BOM) source('WQ_processBOM.R')
if (index.BOM) source('WQ_indexBOM.R')

##,-------
##| eReefs
##`-------
if (get.eReefs) source('WQ_get_ereefs.R')
if (process.eReefs) source('WQ_process_ereefs.R')
if (index.eReefs) source('WQ_index_ereefs.R')

##,----------
##| eReefs926
##`----------
if (get.eReefs926) source('WQ_get_ereefs926.R')
if (process.eReefs926) source('WQ_process_ereefs926.R')
if (index.eReefs926) source('WQ_index_ereefs926.R')

##,-----------------
##| Niskin and FLNTU
##`-----------------
if (process.Niskin) source('WQ_processNiskin.R')
if (index.Niskin) source('WQ_index_niskin.R')
if (index.FLNTU) source('WQ_index_flntu.R')

###############################
## Exploratory data analysis ##
###############################
if (eda) source('WQ_eda.R')

#####################
## Compare sources ##
#####################
if (compare.sources) source('WQ_comp.R')

#######################################################################################################
## Aggregations                                                                                      ##
## There is a need to run large numbers of combinations of settings                                  ##
## here. These include:                                                                              ##
## - which measures to include                                                                       ##
## - which zones to include                                                                          ##
## - loop through each source (niskin, flntu, Satellite, eReefs and eReefs926)                       ##
## - loop through index methods ([Binary,MAMP,fsMAMP,fsMAMP4,Exceed,Max Duration]x[Annual,Seasonal]) ##
## The last two of these are controlled by loops within the scripts                                  ##
## and the first two are controlled outside the script level.                                        ##
## That is the scripts will internally perform aggregations for each                                 ##
## source and index combination.  Combinations of which variables are                                ##
## controlled by running the scripts multiple times.                                                 ##
##                                                                                                   ##
## The external settings (Zones and measures) to include are best set                                ##
## by altering config/config.config file                                                             ##
#######################################################################################################

##include_enclosedcoastal=TRUE;include_sd=TRUE; include_NOx=TRUE; include_nap=TRUE;
setConfigs(c(include_enclosedcoastal=TRUE, include_sd=TRUE, include_NOx=TRUE, include_nap=TRUE,GradeType='Uniform'))
if (aggregate.without.uncertainty) source('WQ_aggregate_without_uncertainty.R')
#GradeType='Uniform' #can be Uniform,MMP,GHHP
if (aggregate.summaries) source('WQ_aggregate_summaries.R')

setConfigs(c(include_enclosedcoastal=FALSE, include_sd=TRUE, include_NOx=TRUE, include_nap=TRUE,GradeType='Uniform'))
if (aggregate.without.uncertainty) source('WQ_aggregate_without_uncertainty.R')
if (aggregate.summaries) source('WQ_aggregate_summaries.R')

setConfigs(c(include_enclosedcoastal=FALSE, include_sd=TRUE, include_NOx=FALSE, include_nap=TRUE,GradeType='Uniform'))
if (aggregate.without.uncertainty) source('WQ_aggregate_without_uncertainty.R')
if (aggregate.summaries) source('WQ_aggregate_summaries.R')

