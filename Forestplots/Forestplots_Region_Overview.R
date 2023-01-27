###-----------------------------------------------------------------------------------------###
### TITLE:       FORESTPLOTS REGIONS                                                        ###
### DESCRIPTION: Creates region overview forestplots for Climate Phase of:                  ###
###              "Model-based Impact Analysis of Climate Change and Land-use Intensification###
###              on Trophic Networks."                                                      ###                                                                 ###
### PROCEDURE:   Loads previous calculated and plotted forestplots for each scenario        ###
###              (climate,hanpp,maxhanpp) and arranges them for region / scenario overview  ###
###              (overall biomass mean)                                                     ###
### DATE:        16.09.2022                                                                 ###
###-----------------------------------------------------------------------------------------###        
###LOAD LIBRARIES
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(grid)

###---------------------------------------------------------------------------###
###                          CREATE SETTINGS / ENVIRONMENT                    ###
###---------------------------------------------------------------------------###
source("settings.R")
myoutpath <- outpath   # outpath in RData will override local outpath settings, see below
myfigpath <- figpath #same as outpath

#manual definition of region, then all files will be saved in logRR folder with region as subfolder
region <- "Overview"

#create directory for storage of csv files and pngs
ifelse(!dir.exists(file.path(figpath, "data_logRR")), dir.create(file.path(figpath, "data_logRR")), cat("The file already exists"))
#create new datapath
datapath <- paste0(figpath,"data_logRR",sep="/")
ifelse(!dir.exists(file.path(datapath, paste0(region))), dir.create(file.path(datapath, paste0(region))), cat("The file already exists"))
#create new path for region
regionpath <- paste0(datapath,region,sep="/")

#load images from previous scripts (Forestplots_Climate_Sep2022, Forestplots_HANPP_Sep2022, Forestplots_maxHANPP_Sep2022)
load(outpath %+% "Forestplot_Climate_Image.Rdata")
load(outpath %+% "Forestplot_HANPP_Image.Rdata")
load(outpath %+% "Forestplot_maxHANPP_Image.Rdata")
Regions_Climate
Regions_HANPP
Regions_maxHANPP
###-----------------------------------------------------------------###
###         ARRANGE ALL PLOTS TO DINA4 AND SAVE AS PDF              ###
###-----------------------------------------------------------------###
#Makes a big plot of all 9 ggplot objects (FG's) + delete redundant information 
a <- ggarrange(Regions_Climate+rremove("ylab")+rremove("xlab"),Regions_HANPP+rremove("y.text")+rremove("ylab")+rremove("xlab"),Regions_maxHANPP+rremove("y.text")+rremove("ylab")+rremove("xlab"), 
               ncol = 3, nrow = 1,
               widths = c(1,0.7,0.7),
               labels = c("A", "B", "C","D","E","F"),
               legend = "right",
             #  common.legend = T,
               legend.grob = get_legend(Regions_HANPP),
               align = "h")#dev.off()

#add title + x and y axis captions 
annotate_figure(a, top = text_grob("Overall Impact on Regions", 
                                   face = "bold", size = 14),
                bottom = text_grob("Effect Size (% Change)",hjust=0.2,face="bold",size=14),
                left = text_grob("Regions",rot=90,hjust=0.5,face="bold",size=14))

#save pdf file in dina4
ggsave(regionpath %+% "Regions_Scenarios_Overview.svg",width=330,height=120,units="mm")
