###-----------------------------------------------------------------------------------------###
### TITLE:       EXTRACT RASTER STATISTICS                                                  ###
### DESCRIPTION: Extracts raster statistics for climate data of:                            ###
###              "Model-based Impact Analysis of Climate Change and Land-use Intensification###
###              on Trophic Networks."                                                      ###                                                                 ###
### PROCEDURE:   Crops climate data input to extent of study regions and extracts summary   ###
###              statistics for variables: HANPP, NPP, TAS, PR -> Base for climate bar plots###
### DATE:        21.09.2022                                                                 ###
###-----------------------------------------------------------------------------------------###    

library(ggplot2)
library(dplyr)
library(psych)
library(raster)

###---------------------------------------------------------------------------###
###                          CREATE SETTINGS / ENVIRONMENT                    ###
###---------------------------------------------------------------------------###
source("settings.R")
source("InsertClimateDataFunction.R") 

myoutpath <- outpath   # outpath in RData will override local outpath settings, see below
myfigpath <- figpath

#load spatial input madingley / filter for year
#sp_inputs = insert_climate_data("ClimateData/historical_2014_0.5degree/")
#sp_inputs = insert_climate_data("ClimateData/SSP126_2100_0.5degree/")
sp_inputs = insert_climate_data("ClimateData/SSP585_2100_0.5degree/")

#manual definition of climate scenario
#scenario <- "Historical"
#scenario <- "SSP1-2.6"
scenario <- "SSP5-8.5"

#create new datapath to store output 
datapath <- paste0(figpath,"data_climate_rasters",sep="/")
#create folder to store data
ifelse(!dir.exists(file.path(datapath)),dir.create(file.path(datapath)),print("This folder already exists"))

#generate yearly climate rasters for each variable 
tas <- sum(sp_inputs$`near-surface_temperature`)/12
pr <- sum(sp_inputs$precipitation)/12
NPP <- sum(sp_inputs$terrestrial_net_primary_productivity)/12
HANPP <- sp_inputs$hanpp

###---------------------------------------------------------------------------###
###                              CREATE STUDY AREAS                           ###
###---------------------------------------------------------------------------###
#create spatial window extent for each region  
spatial_window_eu = c(-12, 25, 35, 60) #  Europe
spatial_window_ind = c(66,91,5,35) #India 
spatial_window_col = c(-72,-69.1,-1.2,2.1) #Vaupés, Colombia
spatial_window_nam = c(18,21,-19,-17) #Kavango, Namibia
spatial_window_fr = c(0,3,46,48) #Central France
spatial_window_fi = c(24,26.8,61.4,63.6) # Central Finland 

#create bounding box for cropping out of spatial_window for each region 
#Europe 
ext_eu <- as(extent(spatial_window_eu), 'SpatialPolygons')
crs(ext_eu) <- "+proj=longlat +datum=WGS84 +no_defs"
#India 
ext_ind <- as(extent(spatial_window_ind), 'SpatialPolygons')
crs(ext_ind) <- "+proj=longlat +datum=WGS84 +no_defs"
#Colombia
ext_col <- as(extent(spatial_window_col), 'SpatialPolygons')
crs(ext_col) <- "+proj=longlat +datum=WGS84 +no_defs"
#Namibia
ext_nam <- as(extent(spatial_window_nam), 'SpatialPolygons')
crs(ext_nam) <- "+proj=longlat +datum=WGS84 +no_defs"
#France
ext_fr <- as(extent(spatial_window_fr), 'SpatialPolygons')
crs(ext_fr) <- "+proj=longlat +datum=WGS84 +no_defs"
#Finland 
ext_fi <- as(extent(spatial_window_fi), 'SpatialPolygons')
crs(ext_fi) <- "+proj=longlat +datum=WGS84 +no_defs"

#Chris: crop rasters to spatial_window // region of concern
#Europe
europe_tas <- crop(tas,ext_eu)
europe_pr <- crop(pr,ext_eu)
europe_NPP <- crop(NPP,ext_eu)
europe_HANPP <- crop(HANPP,ext_eu)
#India
india_tas <- crop(tas,ext_ind)
india_pr <- crop(pr,ext_ind)
india_NPP <- crop(NPP,ext_ind)
india_HANPP <- crop(HANPP,ext_ind)
#Colombia
colombia_tas <- crop(tas,ext_col)
colombia_pr <- crop(pr,ext_col)
colombia_NPP <- crop(NPP,ext_col)
colombia_HANPP <- crop(HANPP,ext_col)
#Namibia
namibia_tas <- crop(tas,ext_nam)
namibia_pr <- crop(pr,ext_nam)
namibia_NPP <- crop(NPP,ext_nam)
namibia_HANPP <- crop(HANPP,ext_nam)
#France
france_tas <- crop(tas,ext_fr)
france_pr <- crop(pr,ext_fr)
france_NPP <- crop(NPP,ext_fr)
france_HANPP <- crop(HANPP,ext_fr)
#Finland
finland_tas <- crop(tas,ext_fi)
finland_pr <- crop(pr,ext_fi)
finland_NPP <- crop(NPP,ext_fi)
finland_HANPP <- crop(HANPP,ext_fi)

###---------------------------------------------------------------------------###
###                        EXTRAXT HISTORICAL CLIMATE DATA                    ###
###---------------------------------------------------------------------------###
###    
#1. HANPP 
#analysis
hanpp_summary <- data.frame("Region" = c("Europe","India","Colombia","Namibia","France","Finland"),
                            "Mean" = c(cellStats(europe_HANPP$hanpp_2005,stat=mean,na.rm=T),cellStats(india_HANPP$hanpp_2005,stat=mean,na.rm=T),cellStats(colombia_HANPP$hanpp_2005,stat=mean,na.rm=T),cellStats(namibia_HANPP$hanpp_2005,stat=mean,na.rm=T),cellStats(france_HANPP$hanpp_2005,stat=mean,na.rm=T),cellStats(finland_HANPP$hanpp_2005,stat=mean,na.rm=T)),
                            "SD" = c(cellStats(europe_HANPP$hanpp_2005,stat=sd,na.rm=T),cellStats(india_HANPP$hanpp_2005,stat=sd,na.rm=T),cellStats(colombia_HANPP$hanpp_2005,stat=sd,na.rm=T),cellStats(namibia_HANPP$hanpp_2005,stat=sd,na.rm=T),cellStats(france_HANPP$hanpp_2005,stat=sd,na.rm=T),cellStats(finland_HANPP$hanpp_2005,stat=sd,na.rm=T)),
                            "Max" =c(cellStats(europe_HANPP$hanpp_2005,stat=max,na.rm=T),cellStats(india_HANPP$hanpp_2005,stat=max,na.rm=T),cellStats(colombia_HANPP$hanpp_2005,stat=max,na.rm=T),cellStats(namibia_HANPP$hanpp_2005,stat=max,na.rm=T),cellStats(france_HANPP$hanpp_2005,stat=max,na.rm=T),cellStats(finland_HANPP$hanpp_2005,max,na.rm=T)),
                            "Min" =c(cellStats(europe_HANPP$hanpp_2005,stat=min,na.rm=T),cellStats(india_HANPP$hanpp_2005,stat=min,na.rm=T),cellStats(colombia_HANPP$hanpp_2005,stat=min,na.rm=T),cellStats(namibia_HANPP$hanpp_2005,stat=min,na.rm=T),cellStats(france_HANPP$hanpp_2005,stat=min,na.rm=T),cellStats(finland_HANPP$hanpp_2005,min,na.rm=T)),
                            "Scenario" = paste0(scenario))
write.csv(hanpp_summary,datapath %+% paste0("HANPP_summary","_",scenario,".csv"))

#2. NPP 
#analysis
npp_summary <- data.frame("Region" = c("Europe","India","Colombia","Namibia","France","Finland"),
                            "Mean" = c(cellStats(europe_NPP$layer,stat=mean,na.rm=T),cellStats(india_NPP$layer,stat=mean,na.rm=T),cellStats(colombia_NPP$layer,stat=mean,na.rm=T),cellStats(namibia_NPP$layer,stat=mean,na.rm=T),cellStats(france_NPP$layer,stat=mean,na.rm=T),cellStats(finland_NPP$layer,stat=mean,na.rm=T)),
                            "SD" = c(cellStats(europe_NPP$layer,stat=sd,na.rm=T),cellStats(india_NPP$layer,stat=sd,na.rm=T),cellStats(colombia_NPP$layer,stat=sd,na.rm=T),cellStats(namibia_NPP$layer,stat=sd,na.rm=T),cellStats(france_NPP$layer,stat=sd,na.rm=T),cellStats(finland_NPP$layer,stat=sd,na.rm=T)),
                            "Max" =c(cellStats(europe_NPP$layer,stat=max,na.rm=T),cellStats(india_NPP$layer,stat=max,na.rm=T),cellStats(colombia_NPP$layer,stat=max,na.rm=T),cellStats(namibia_NPP$layer,stat=max,na.rm=T),cellStats(france_NPP$layer,stat=max,na.rm=T),cellStats(finland_NPP$layer,max,na.rm=T)),
                            "Min" =c(cellStats(europe_NPP$layer,stat=min,na.rm=T),cellStats(india_NPP$layer,stat=min,na.rm=T),cellStats(colombia_NPP$layer,stat=min,na.rm=T),cellStats(namibia_NPP$layer,stat=min,na.rm=T),cellStats(france_NPP$layer,stat=min,na.rm=T),cellStats(finland_NPP$layer,min,na.rm=T)),
                            "Scenario" = paste0(scenario))
write.csv(npp_summary,datapath %+% paste0("NPP_summary","_",scenario,".csv"))

#3. TAS 
#analysis
tas_summary <- data.frame("Region" = c("Europe","India","Colombia","Namibia","France","Finland"),
                            "Mean" = c(cellStats(europe_tas$layer,stat=mean,na.rm=T),cellStats(india_tas$layer,stat=mean,na.rm=T),cellStats(colombia_tas$layer,stat=mean,na.rm=T),cellStats(namibia_tas$layer,stat=mean,na.rm=T),cellStats(france_tas$layer,stat=mean,na.rm=T),cellStats(finland_tas$layer,stat=mean,na.rm=T)),
                            "SD" = c(cellStats(europe_tas$layer,stat=sd,na.rm=T),cellStats(india_tas$layer,stat=sd,na.rm=T),cellStats(colombia_tas$layer,stat=sd,na.rm=T),cellStats(namibia_tas$layer,stat=sd,na.rm=T),cellStats(france_tas$layer,stat=sd,na.rm=T),cellStats(finland_tas$layer,stat=sd,na.rm=T)),
                            "Max" =c(cellStats(europe_tas$layer,stat=max,na.rm=T),cellStats(india_tas$layer,stat=max,na.rm=T),cellStats(colombia_tas$layer,stat=max,na.rm=T),cellStats(namibia_tas$layer,stat=max,na.rm=T),cellStats(france_tas$layer,stat=max,na.rm=T),cellStats(finland_tas$layer,max,na.rm=T)),
                            "Min" =c(cellStats(europe_tas$layer,stat=min,na.rm=T),cellStats(india_tas$layer,stat=min,na.rm=T),cellStats(colombia_tas$layer,stat=min,na.rm=T),cellStats(namibia_tas$layer,stat=min,na.rm=T),cellStats(france_tas$layer,stat=min,na.rm=T),cellStats(finland_tas$layer,min,na.rm=T)),
                            "Scenario" = paste0(scenario))
write.csv(tas_summary,datapath %+% paste0("tas_summary","_",scenario,".csv"))


#4. PR 
#analysis
pr_summary <- data.frame("Region" = c("Europe","India","Colombia","Namibia","France","Finland"),
                            "Mean" = c(cellStats(europe_pr$layer,stat=mean,na.rm=T),cellStats(india_pr$layer,stat=mean,na.rm=T),cellStats(colombia_pr$layer,stat=mean,na.rm=T),cellStats(namibia_pr$layer,stat=mean,na.rm=T),cellStats(france_pr$layer,stat=mean,na.rm=T),cellStats(finland_pr$layer,stat=mean,na.rm=T)),
                            "SD" = c(cellStats(europe_pr$layer,stat=sd,na.rm=T),cellStats(india_pr$layer,stat=sd,na.rm=T),cellStats(colombia_pr$layer,stat=sd,na.rm=T),cellStats(namibia_pr$layer,stat=sd,na.rm=T),cellStats(france_pr$layer,stat=sd,na.rm=T),cellStats(finland_pr$layer,stat=sd,na.rm=T)),
                            "Max" =c(cellStats(europe_pr$layer,stat=max,na.rm=T),cellStats(india_pr$layer,stat=max,na.rm=T),cellStats(colombia_pr$layer,stat=max,na.rm=T),cellStats(namibia_pr$layer,stat=max,na.rm=T),cellStats(france_pr$layer,stat=max,na.rm=T),cellStats(finland_pr$layer,max,na.rm=T)),
                            "Min" =c(cellStats(europe_pr$layer,stat=min,na.rm=T),cellStats(india_pr$layer,stat=min,na.rm=T),cellStats(colombia_pr$layer,stat=min,na.rm=T),cellStats(namibia_pr$layer,stat=min,na.rm=T),cellStats(france_pr$layer,stat=min,na.rm=T),cellStats(finland_pr$layer,min,na.rm=T)),
                            "Scenario" = paste0(scenario))
write.csv(pr_summary,datapath %+% paste0("pr_summary","_",scenario,".csv"))


