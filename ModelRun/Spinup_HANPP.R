###-------------------------------------------------------------------------------------------------------###
### TITLE:                      MADINGLEYR: SPIN-UP PHASE + HANPP PHASE                                   ###
### DESCRIPTION: Runs MadingleyR for different regions / climate datasets of:                             ###                                 ###
###              "Model-based Impact Analysis of Climate Change and Land-use Intensification              ###
###              on Trophic Networks."                                                                    ###
### NOTES:- Can be applied to different regions (spatial extents are below in the code)                   ###                                              
###       - 1x execution creates Spin-up (Control) + HANPP (Current LU-Intensity) for histo, SSP2.6,SSP8.5###
###       - Output = Baseline for maxHANPP (Maximum LU-Intensity Phase)                                   ### 
### DATE:        16.09.2022                                                                               ###
###-------------------------------------------------------------------------------------------------------###

library("MadingleyR")
source("settings.R")
source("InsertClimateDataFunction.R") #Source: Hoeks, S. (2022)
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/make_spatial_biomass_raster_autotrophs.R") #this loads the autotroph biomass plot function
# NEW! source combined timeline function #Source: Hoeks, S. (2022)
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/plot_combined_timelines2.R")
# NEW! source function to crop spatial rasters using spatial window #Source: Hoeks, S. (2022)
source("https://raw.githubusercontent.com/SHoeks/RandomMadingleyRFunctions/master/crop_spatial_rasters_to_window.r")

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

#create Madingley model inputs
sp_inputs_histo_2014 = insert_climate_data("ClimateData/historical_2014_0.5degree/")
chrt_def = madingley_inputs("cohort definition")
stck_def = madingley_inputs("stock definition")
mdl_prms = madingley_inputs("model parameters")

#check if hanpp layer is correctly loaded to madingley
plot(sp_inputs_histo_2014$hanpp, main="HANPP in gC/m^2/year")

# crop the raster to spatial window extent
# helps to speed up loading times and makes working with the inputs easier
sp_inputs_histo_2014 = crop_spatial_rasters_to_window(inputs = sp_inputs_histo_2014, spatial_window = spatial_window)

###------------------###
### INITIALIZE MODEL ###
###------------------###

historical_2014 = madingley_init(spatial_window=spatial_window, 
                                 cohort_def=chrt_def,
                                 stock_def=stck_def,
                                 spatial_inputs=sp_inputs_histo_2014, #run model with default climate data = sptl_input  
                                 max_cohort= 500)

###---------------###
### RUN THE MODEL ###
###---------------###
###PHASE 1 - SPIN-UP PHASE###

###create a list file, where all outputs will be stored, makes combined_timeline_plot possible(SH)
historical_2014_list = list()

#CONTROL; run the model with init params for a 200-year period without intervention (model spin-up) --> without applying HANPP (which disabled in the default settings), because we don't want to reduce biomass by hanpp, yet

historical_2014_list[[1]] = madingley_run(out_dir = outpath, #path to output directory
                                          madingley_data = historical_2014,
                                          years = 200, # at least 200 years recommended
                                          cohort_def = chrt_def,
                                          stock_def = stck_def,
                                          spatial_inputs = sp_inputs_histo_2014,
                                          model_parameters = mdl_prms,
                                          max_cohort = 500) # at least 500 is recommended 

###PHASE 2 - APPLY HANPP ###
#run model for addtional 200 years with applying hanpp as input raster with gC/m-2/year
historical_2014_list[[2]] = madingley_run(out_dir = outpath, #path to output directory
                                          madingley_data = historical_2014_list[[1]],
                                          years = 200, # at least 200 years recommended
                                          cohort_def = chrt_def,
                                          stock_def = stck_def,
                                          spatial_inputs = sp_inputs_histo_2014,
                                          model_parameters = mdl_prms,
                                          max_cohort = 500, # at least 500 is recommended 
                                          apply_hanpp = 2)  #apply_hanpp = 2 is to apply hanpp with gC/m-2/year as input

#save image for further use in next scripts
save.image(outpath %+% "historical_2014_HANPP_0.5degree.Rdata")

#clear all data frames from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])
#clear all lists from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

################################################################################
################################################################################

###----------------------------------------------###
###  Create model input for SSP126 climate data  ###
###----------------------------------------------###

#create Madingley model inputs
sp_inputs_SSP2.6_2100 = insert_climate_data("ClimateData/SSP126_2100_0.5degree") 
chrt_def = madingley_inputs("cohort definition")
stck_def = madingley_inputs("stock definition")
mdl_prms = madingley_inputs("model parameters")

#check if hanpp layer is correctly loaded to madingley
plot(sp_inputs_SSP2.6_2100$hanpp, main="HANPP in gC/m^2/year")

# crop the raster to spatial window extent
# helps to speed up loading times and makes working with the inputs easier
sp_inputs_SSP2.6_2100 = crop_spatial_rasters_to_window(inputs = sp_inputs_SSP2.6_2100, spatial_window = spatial_window)

###------------------###
### INITIALIZE MODEL ###
###------------------###

SSP2.6_2100 = madingley_init(spatial_window=spatial_window, 
                             cohort_def=chrt_def,
                             stock_def=stck_def,
                             spatial_inputs=sp_inputs_SSP2.6_2100, #run model with default climate data = sptl_input  
                             max_cohort= 500)

###---------------###
### RUN THE MODEL ###
###---------------###

###PHASE 1 - SPIN-UP PHASE###
###create a list file, where all outputs will be stored, makes combined_timeline_plot possible(SH)
SSP2.6_2100_list = list()

#CONTROL; run the model with init params for a 200-year period without intervention (model spin-up) --> without applying HANPP (which disabled in the default settings), because we don't want to reduce biomass by hanpp, yet
SSP2.6_2100_list[[1]] = madingley_run(out_dir = outpath, #path to output directory
                                      madingley_data = SSP2.6_2100,
                                      years = 200, # at least 200 years recommended
                                      cohort_def = chrt_def,
                                      stock_def = stck_def,
                                      spatial_inputs = sp_inputs_SSP2.6_2100,
                                      model_parameters = mdl_prms,
                                      max_cohort = 500) # at least 500 is recommended 

###PHASE 2 - APPLY HANPP ###
#run model for addtional 200 years with applying hanpp as input raster with gC/m-2/year
SSP2.6_2100_list[[2]] = madingley_run(out_dir = outpath, #path to output directory
                                      madingley_data = SSP2.6_2100_list[[1]],
                                      years = 200, # at least 200 years recommended
                                      cohort_def = chrt_def,
                                      stock_def = stck_def,
                                      spatial_inputs = sp_inputs_SSP2.6_2100,
                                      model_parameters = mdl_prms,
                                      max_cohort = 500, # at least 500 is recommended 
                                      apply_hanpp = 2)  #apply_hanpp = 2 is to apply hanpp with gC/m-2/year as input

#save image for further use in next scripts
save.image(outpath %+% "SSP126_2100_HANPP_0.5degree.Rdata")

#clear all data frames from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])
#clear all lists from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

################################################################################
################################################################################

###----------------------------------------------###
###  Create model input for SSP585 climate data  ###
###----------------------------------------------###

#create Madingley model inputs
sp_inputs_SSP8.5_2100 = insert_climate_data("ClimateData/SSP585_2100_0.5degree") 
chrt_def = madingley_inputs("cohort definition")
stck_def = madingley_inputs("stock definition")
mdl_prms = madingley_inputs("model parameters")

#check if hanpp layer is correctly loaded to madingley
plot(sp_inputs_SSP8.5_2100$hanpp, main="HANPP in gC/m^2/year")

# crop the raster to spatial window extent
# helps to speed up loading times and makes working with the inputs easier
sp_inputs_SSP8.5_2100 = crop_spatial_rasters_to_window(inputs = sp_inputs_SSP8.5_2100, spatial_window = spatial_window)

###------------------###
### INITIALIZE MODEL ###
###------------------###

SSP8.5_2100 = madingley_init(spatial_window=spatial_window, 
                             cohort_def=chrt_def,
                             stock_def=stck_def,
                             spatial_inputs=sp_inputs_SSP8.5_2100, #run model with default climate data = sptl_input  
                             max_cohort= 500)

###---------------###
### RUN THE MODEL ###
###---------------###

###PHASE 1 - SPIN-UP PHASE###
###create a list file, where all outputs will be stored, makes combined_timeline_plot possible(SH)
SSP8.5_2100_list = list()

#CONTROL; run the model with init params for a 200-year period without intervention (model spin-up) --> without applying HANPP (which disabled in the default settings), because we don't want to reduce biomass by hanpp, yet
SSP8.5_2100_list[[1]] = madingley_run(out_dir = outpath, #path to output directory
                                      madingley_data = SSP8.5_2100,
                                      years = 200, # at least 200 years recommended
                                      cohort_def = chrt_def,
                                      stock_def = stck_def,
                                      spatial_inputs = sp_inputs_SSP8.5_2100,
                                      model_parameters = mdl_prms,
                                      max_cohort = 500) # at least 500 is recommended 

###PHASE 2 - APPLY HANPP ###
#run model for addtional 200 years with applying hanpp as input raster with gC/m-2/year
SSP8.5_2100_list[[2]] = madingley_run(out_dir = outpath, #path to output directory
                                      madingley_data = SSP8.5_2100_list[[1]],
                                      years = 200, # at least 200 years recommended
                                      cohort_def = chrt_def,
                                      stock_def = stck_def,
                                      spatial_inputs = sp_inputs_SSP8.5_2100,
                                      model_parameters = mdl_prms,
                                      max_cohort = 500, # at least 500 is recommended 
                                      apply_hanpp = 2)  #apply_hanpp = 2 is to apply hanpp with gC/m-2/year as input

#save image for further use in next scripts
save.image(outpath %+% "SSP585_2100_HANPP_0.5degree.Rdata")

#clear all data frames from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"])
#clear all lists from environment for next scenario
rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])

