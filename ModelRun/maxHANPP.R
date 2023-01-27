###---------------------------------------------------------------------------------------------------------###
### TITLE:                      MADINGLEYR: MAX.HANPP PHASE - MAXIMUM LAND-USE INTENSITY                    ###
### DESCRIPTION: Runs MadingleyR for different regions / climate datasets of:                               ###                                 ###
###              "Model-based Impact Analysis of Climate Change and Land-use Intensification                ###
###              on Trophic Networks."                                                                      ###
### NOTES:       -  Can be applied to different regions (spatial extents are below in the code)             ###                                                              ###
###              - 1x execution creates: vegetation reduction via rising lu intensity + postreduction output### 
###                for historical, SSP2.6,SSP8.5 climate                                                    ###
###              - Input = results of Spinup_HANPP script (applied hanpp layer is used as spin-up / control)###
### DATE:        16.09.2022                                                                                 ###
###---------------------------------------------------------------------------------------------------------###

library("MadingleyR")
source("settings.R")
source("InsertClimateDataFunction.R") #Source: Hoeks, S. (2022)
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/make_spatial_biomass_raster_autotrophs.R") #this loads the autotroph biomass plot function
# NEW! source combined timeline function #Source: Hoeks, S. (2022)
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/plot_combined_timelines7.R")
# NEW! source function to crop spatial rasters using spatial window #Source: Hoeks, S. (2022)
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/crop_spatial_rasters_to_window.r")
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/plot_timelines_2.r")

###----------------------------------------------###
###Create model input for historical climate data###
###----------------------------------------------###

#create spatial window
spatial_window = c(-12, 25, 35, 60) #  Europe
#spatial_window = c(66, 91, 5, 35) #India
#spatial_window = c(-72,-69,-1.2,2.1)#Vaupés, Colombia 
#spatial_window = c(18,20.1,-19.2,-17.4) #Kavango,Namibia
#spatial_window = c(24,26.8,61.4,63.6) #Central Finland, Finland
#spatial_window = c(0.1,3.1,46.3,48.1) #Central France (Indre, Cher, Loir-et-Cher, Indre-et-Loire), France


# Can be used to check if we defined the spatial window correctly
plot_spatialwindow(spatial_window)

###---------------------------------------###
### PREPARATION / FRACTIONAL HANPP RASTER ###
###---------------------------------------###

#first load image from Climate_HANPP (includes model initialization, spin-up + hanpp application in gC/m2/s --> This is used for control & spin-up here)
load(outpath %+% "historical_2014_HANPP_0.5degree.Rdata")

#second calculate the fractional hanpp layer
#calculation of fractional hanpp raster
sp_inputs_histo_2014$hanpp[] = sp_inputs_histo_2014$hanpp[] + abs(min(sp_inputs_histo_2014$hanpp[], na.rm = TRUE)) # This code line adds the minimum value of the HANPP layer (I think in order to eliminate negative HANPP values) -
sp_inputs_histo_2014$hanpp[] = sp_inputs_histo_2014$hanpp[] / max(sp_inputs_histo_2014$hanpp[], na.rm = TRUE)  # This code line divides the HANPP layer by the maximum values of the HANPP layer, which creates a raster with fractional values (If you multiply by 100 you'll get the percentage values of hanpp)
sp_inputs_histo_2014$hanpp[] = 1-sp_inputs_histo_2014$hanpp[]  # The subtraction 1-HANPP leads to swapped values, this means the lower values are the values with the highest HANPP and the higher values are the values with lowest HANPP --> So, we can assume that a reduction in these values is a reduction in biomass, and not a reduction in HANPP.

#remove NA's
sp_inputs_histo_2014$hanpp[is.na(sp_inputs_histo_2014$hanpp[])] = 0.001

#plot to check if computation of fractional raster is done 
plot(sp_inputs_histo_2014$hanpp) # values close to 0 mean intensive reduction in veg mass, e.g. a value of 0.1 is a 0.9 reduction

###---------------###
### RUN THE MODEL ###
###---------------###
###VEGETATION REDUCTION###

#!!! SH, just to check later
hanpp_backup = sp_inputs_histo_2014$hanpp

#!!! SH, While loop, stop when needed (no more values above 0.1 in hanpp raster)
#!!! SH, this is possible because the raster is cropped, otherwise not I think?
while(max(sp_inputs_histo_2014$hanpp[])>0.1) {
  
  #use ifelse to reduce vegetation by 0.1 per 5 years (if its > 0.1), but not below the threshold 0.1, because otherwise to high HANPP intensity
  sp_inputs_histo_2014$hanpp[] = ifelse(sptl_inp$hanpp[] > 0.1,
                                        ifelse(sptl_inp$hanpp[] - 0.1 < 0.1,0.1,sptl_inp$hanpp[] - 0.1),
                                        sptl_inp$hanpp[])
  print(sp_inputs_histo_2014$hanpp)
  plot(sp_inputs_histo_2014$hanpp)
  
  #!!! SH, use length to add data to the end of the list
  historical_2014_list[[length(historical_2014_list)+1]] = madingley_run(out_dir = outpath,
                                                                         years = 10, 
                                                                         madingley_data = historical_2014_list[[length(historical_2014_list)]],
                                                                         spatial_inputs = sp_inputs_histo_2014,
                                                                         silenced = TRUE,
                                                                         max_cohort = 500, # at least 500
                                                                         apply_hanpp = 1) 
}

#!!! SH, compare hanpp
par(mfrow=c(1,2))
plot(hanpp_backup,main="starting hanpp")
plot(sp_inputs_histo_2014$hanpp,main="hanpp reduced to max of 0.1")
par(mfrow=c(1,1))

###POST VEGETATION REDUCTION###
#!!! SH, insert next mdata into list previously created
historical_2014_list[[length(historical_2014_list)+1]]  = madingley_run(out_dir = outpath,
                                                                        madingley_data = historical_2014_list[[length(historical_2014_list)]], #!!! SH, get last element in list
                                                                        years = 200, #at least 200
                                                                        model_parameters = mdl_prms, 
                                                                        spatial_inputs = sp_inputs_histo_2014,
                                                                        cohort_def = chrt_def,
                                                                        max_cohort = 500, #at least 500
                                                                        apply_hanpp = 1)

#plot combined timelines to look if every stage is fully applied 
plot_combined_timelines(historical_2014_list,legend_ypos=7.5)

#save image for further use in next scripts
save.image(outpath %+% "historical_2014_vegreduced_0.5degree.Rdata")

#clear all data frames from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])
#clear all lists from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

###----------------------------------------------###
###  Create model input for SSP126 climate data  ###
###----------------------------------------------###

###---------------------------------------###
### PREPARATION / FRACTIONAL HANPP RASTER ###
###---------------------------------------###

#first load image from Climate_HANPP (includes model initialization, spin-up + hanpp application in gC/m2/s --> This is used for control & spin-up here)
load(outpath %+% "SSP126_2100_HANPP_0.5degree.Rdata")

#second calculate the fractional hanpp layer
#calculation of fractional hanpp raster
sp_inputs_SSP2.6_2100$hanpp[] = sp_inputs_SSP2.6_2100$hanpp[] + abs(min(sp_inputs_SSP2.6_2100$hanpp[], na.rm = TRUE)) # This code line adds the minimum value of the HANPP layer (I think in order to eliminate negative HANPP values) -
sp_inputs_SSP2.6_2100$hanpp[] = sp_inputs_SSP2.6_2100$hanpp[] / max(sp_inputs_SSP2.6_2100$hanpp[], na.rm = TRUE)  # This code line divides the HANPP layer by the maximum values of the HANPP layer, which creates a raster with fractional values (If you multiply by 100 you'll get the percentage values of hanpp)
sp_inputs_SSP2.6_2100$hanpp[] = 1-sp_inputs_SSP2.6_2100$hanpp[]  # The subtraction 1-HANPP leads to swapped values, this means the lower values are the values with the highest HANPP and the higher values are the values with lowest HANPP --> So, we can assume that a reduction in these values is a reduction in biomass, and not a reduction in HANPP.

#remove NA's
sp_inputs_SSP2.6_2100$hanpp[is.na(sp_inputs_SSP2.6_2100$hanpp[])] = 0.001

#plot to check if computation of fractional raster is done 
plot(sp_inputs_SSP2.6_2100$hanpp) # values close to 0 mean intensive reduction in veg mass, e.g. a value of 0.1 is a 0.9 reduction

###---------------###
### RUN THE MODEL ###
###---------------###
###VEGETATION REDUCTION###

#!!! SH, just to check later
hanpp_backup = sp_inputs_SSP2.6_2100$hanpp

#!!! SH, While loop, stop when needed (no more values above 0.1 in hanpp raster)
#!!! SH, this is possible because the raster is cropped, otherwise not I think?
while(max(sp_inputs_SSP2.6_2100$hanpp[])>0.1) {
  
  #use ifelse to reduce vegetation by 0.1 per 5 years (if its > 0.1), but not below the threshold 0.1, because otherwise to high HANPP intensity
  sp_inputs_SSP2.6_2100$hanpp[] = ifelse(sptl_inp$hanpp[] > 0.1,
                                         ifelse(sptl_inp$hanpp[] - 0.1 < 0.1,0.1,sptl_inp$hanpp[] - 0.1),
                                         sptl_inp$hanpp[])
  print(sp_inputs_SSP2.6_2100$hanpp)
  plot(sp_inputs_SSP2.6_2100$hanpp)
  
  #!!! SH, use length to add data to the end of the list
  SSP2.6_2100_list[[length(SSP2.6_2100_list)+1]] = madingley_run(out_dir = outpath,
                                                                 years = 10, 
                                                                 madingley_data = SSP2.6_2100_list[[length(SSP2.6_2100_list)]],
                                                                 spatial_inputs = sp_inputs_SSP2.6_2100,
                                                                 silenced = TRUE,
                                                                 max_cohort = 500, # at least 500
                                                                 apply_hanpp = 1) 
}

#!!! SH, compare hanpp
par(mfrow=c(1,2))
plot(hanpp_backup,main="starting hanpp")
plot(sp_inputs_SSP2.6_2100$hanpp,main="hanpp reduced to max of 0.1")
par(mfrow=c(1,1))

###POST VEGETATION REDUCTION###
#!!! SH, insert next mdata into list previously created
SSP2.6_2100_list[[length(SSP2.6_2100_list)+1]]  = madingley_run(out_dir = outpath,
                                                                madingley_data = SSP2.6_2100_list[[length(SSP2.6_2100_list)]], #!!! SH, get last element in list
                                                                years = 200, #at least 200
                                                                model_parameters = mdl_prms, 
                                                                spatial_inputs = sp_inputs_SSP2.6_2100,
                                                                cohort_def = chrt_def,
                                                                max_cohort = 500, #at least 500
                                                                apply_hanpp = 1)

#plot combined timelines to look if every stage is fully applied 
plot_combined_timelines(SSP2.6_2100_list,legend_ypos=7.5)

#save image for further use in next scripts
save.image(outpath %+% "SSP126_2100_vegreduced_0.5degree.Rdata")

#clear all data frames from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])
#clear all lists from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

###----------------------------------------------###
###  Create model input for SSP585 climate data  ###
###----------------------------------------------###

###---------------------------------------###
### PREPARATION / FRACTIONAL HANPP RASTER ###
###---------------------------------------###

#first load image from Climate_HANPP (includes model initialization, spin-up + hanpp application in gC/m2/s --> This is used for control & spin-up here)
load(outpath %+% "SSP585_2100_HANPP_0.5degree.Rdata")

#second calculate the fractional hanpp layer
#calculation of fractional hanpp raster
sp_inputs_SSP8.5_2100$hanpp[] = sp_inputs_SSP8.5_2100$hanpp[] + abs(min(sp_inputs_SSP8.5_2100$hanpp[], na.rm = TRUE)) # This code line adds the minimum value of the HANPP layer (I think in order to eliminate negative HANPP values) -
sp_inputs_SSP8.5_2100$hanpp[] = sp_inputs_SSP8.5_2100$hanpp[] / max(sp_inputs_SSP8.5_2100$hanpp[], na.rm = TRUE)  # This code line divides the HANPP layer by the maximum values of the HANPP layer, which creates a raster with fractional values (If you multiply by 100 you'll get the percentage values of hanpp)
sp_inputs_SSP8.5_2100$hanpp[] = 1-sp_inputs_SSP8.5_2100$hanpp[]  # The subtraction 1-HANPP leads to swapped values, this means the lower values are the values with the highest HANPP and the higher values are the values with lowest HANPP --> So, we can assume that a reduction in these values is a reduction in biomass, and not a reduction in HANPP.

#remove NA's
sp_inputs_SSP8.5_2100$hanpp[is.na(sp_inputs_SSP8.5_2100$hanpp[])] = 0.001

#plot to check if computation of fractional raster is done 
plot(sp_inputs_SSP8.5_2100$hanpp) # values close to 0 mean intensive reduction in veg mass, e.g. a value of 0.1 is a 0.9 reduction

###---------------###
### RUN THE MODEL ###
###---------------###
###VEGETATION REDUCTION###

#!!! SH, just to check later
hanpp_backup = sp_inputs_SSP8.5_2100$hanpp

#!!! SH, While loop, stop when needed (no more values above 0.1 in hanpp raster)
#!!! SH, this is possible because the raster is cropped, otherwise not I think?
while(max(sp_inputs_SSP8.5_2100$hanpp[])>0.1) {
  
  #use ifelse to reduce vegetation by 0.1 per 5 years (if its > 0.1), but not below the threshold 0.1, because otherwise to high HANPP intensity
  sp_inputs_SSP8.5_2100$hanpp[] =  ifelse(sptl_inp$hanpp[] > 0.1,
                                          ifelse(sptl_inp$hanpp[] - 0.1 < 0.1,0.1,sptl_inp$hanpp[] - 0.1),
                                          sptl_inp$hanpp[])
  print(sp_inputs_SSP8.5_2100$hanpp)
  plot(sp_inputs_SSP8.5_2100$hanpp)
  
  #!!! SH, use length to add data to the end of the list
  SSP8.5_2100_list[[length(SSP8.5_2100_list)+1]] = madingley_run(out_dir = outpath,
                                                                 years = 10, 
                                                                 madingley_data = SSP8.5_2100_list[[length(SSP8.5_2100_list)]],
                                                                 spatial_inputs = sp_inputs_SSP8.5_2100,
                                                                 silenced = TRUE,
                                                                 max_cohort = 500, # at least 500
                                                                 apply_hanpp = 1) 
}

#!!! SH, compare hanpp
par(mfrow=c(1,2))
plot(hanpp_backup,main="starting hanpp")
plot(sp_inputs_SSP8.5_2100$hanpp,main="hanpp reduced to max of 0.1")
par(mfrow=c(1,1))

###POST VEGETATION REDUCTION###
#!!! SH, insert next mdata into list previously created
SSP8.5_2100_list[[length(SSP8.5_2100_list)+1]]  = madingley_run(out_dir = outpath,
                                                                madingley_data = SSP8.5_2100_list[[length(SSP8.5_2100_list)]], #!!! SH, get last element in list
                                                                years = 200, #at least 200
                                                                model_parameters = mdl_prms, 
                                                                spatial_inputs = sp_inputs_SSP8.5_2100,
                                                                cohort_def = chrt_def,
                                                                max_cohort = 500, #at least 500
                                                                apply_hanpp = 1)

#plot combined timelines to look if every stage is fully applied 
plot_combined_timelines(SSP8.5_2100_list,legend_ypos=7.5)

#save image for further use in next scripts
save.image(outpath %+% "SSP585_2100_vegreduced_0.5degree.Rdata")

#clear all data frames from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])
#clear all lists from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])