###-----------------------------------------------------------------------------------------###
### TITLE:       SCATTERPLOTS: Abundance/Body mass                                          ###
### DESCRIPTION: Creates scatterplots of:                                                   ###
###              "Model-based Impact Analysis of Climate Change and Land-use Intensification###
###              on Trophic Networks."                                                      ###                                                               
### PROCEDURE:   Executes regression analysis in ggplot and plots+exports results as        ###
###              scatterplots. Analysis is done for each feeding guild & FG                 ###
### NOTES:       Region has to be defined in: load(datainput_region), and region            ###
### DATE:        13.10.2022                                                                 ###
###-----------------------------------------------------------------------------------------###        

###LOAD LIBRARIES
library(MadingleyR)
library(ggplot2)
library(dplyr)
library(psych)
library(ARPobservation)
library(RColorBrewer)
library(ggpubr)
library(ggpmisc)
library(MASS)
library(scales)

###---------------------------------------------------------------------------###
###                          CREATE SETTINGS / ENVIRONMENT                    ###
###---------------------------------------------------------------------------###
source("settings.R")
myoutpath <- outpath   # outpath in RData will override local outpath settings, see below
myfigpath <- figpath #same as outpath
#load workspace images from outpath #new: since new results are list files, it's sufficient to load vegreduced backups (list file contains all stages)
#Insert possibilities: Europe, India, Vaupés_Colombia, Kavango_Namibia, Central_France, Central_Finland
load(outpath %+% "Europe_historical_2014_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Europe_SSP126_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Europe_SSP585_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath

#get output directory path
out_dir <- outpath 

#manual definition of region, then all files will be saved in logRR folder with region as subfolder
region <- "Europe"

#create directory for storage of csv files and pngs
ifelse(!dir.exists(file.path(figpath, "data_Scatterplot")), dir.create(file.path(figpath, "data_Scatterplot")), cat("The file already exists"))
#create new datapath
datapath <- paste0(figpath,"data_Scatterplot",sep="/")

ifelse(!dir.exists(file.path(datapath, paste0(region))), dir.create(file.path(datapath, paste0(region))), cat("The file already exists"))
#create new path for region
regionpath <- paste0(datapath,region,sep="/")

###-------------------------------------------------------------------------###
###                                   HANPP (Current LU)                    ###
###-------------------------------------------------------------------------###
###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
#just load data again 
#list[1] = control; list[2] = HANPP; last list entry [individual on region] = After vegreduction
cohorts_histo <- data.frame(historical_2014_list[[2]]$cohorts) 
cohorts_SSP126 <- data.frame(SSP2.6_2100_list[[2]]$cohorts)
cohorts_SSP585 <- data.frame(SSP8.5_2100_list[[2]]$cohorts) 
#generate  control datasets for comparison in plot
cohorts_histo_c <- data.frame(historical_2014_list[[1]]$cohorts) 
cohorts_SSP126_c <- data.frame(SSP2.6_2100_list[[1]]$cohorts)
cohorts_SSP585_c <- data.frame(SSP8.5_2100_list[[1]]$cohorts) 

#subset data 
cohorts_histo <- cohorts_histo[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP126 <- cohorts_SSP126[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP585 <- cohorts_SSP585[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
#for control
cohorts_histo_c <- cohorts_histo_c[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP126_c <- cohorts_SSP126_c[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP585_c <- cohorts_SSP585_c[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]

#aggregate data to FG per gridcell // calculates mean for each gridcell & FG
cohorts_histo <- aggregate(cohorts_histo[3:4],list(cohorts_histo$GridcellIndex,cohorts_histo$FunctionalGroupIndex),mean)
cohorts_SSP126 <- aggregate(cohorts_SSP126[3:4],list(cohorts_SSP126$GridcellIndex,cohorts_SSP126$FunctionalGroupIndex),mean)
cohorts_SSP585 <- aggregate(cohorts_SSP585[3:4],list(cohorts_SSP585$GridcellIndex,cohorts_SSP585$FunctionalGroupIndex),mean)
#for control
cohorts_histo_c <- aggregate(cohorts_histo_c[3:4],list(cohorts_histo_c$GridcellIndex,cohorts_histo_c$FunctionalGroupIndex),mean)
cohorts_SSP126_c <- aggregate(cohorts_SSP126_c[3:4],list(cohorts_SSP126_c$GridcellIndex,cohorts_SSP126_c$FunctionalGroupIndex),mean)
cohorts_SSP585_c <- aggregate(cohorts_SSP585_c[3:4],list(cohorts_SSP585_c$GridcellIndex,cohorts_SSP585_c$FunctionalGroupIndex),mean)

#create new grouping variable for 
cohorts_histo$group = "Historical"
cohorts_SSP126$group = "SSP1-2.6"
cohorts_SSP585$group = "SSP5-8.5"
#control
cohorts_histo_c$group = "Historical"
cohorts_SSP126_c$group = "SSP1-2.6"
cohorts_SSP585_c$group = "SSP5-8.5"

#combine dataframes 
test <- rbind(cohorts_histo,cohorts_SSP126,cohorts_SSP585)
#convert to kg 
test[3] <- test[3]/1000 
#control
test_c <- rbind(cohorts_histo_c,cohorts_SSP126_c,cohorts_SSP585_c)
#convert to kg 
test_c[3] <- test_c[3]/1000

#change naming
names(test) <- c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance","group")
#control
names(test_c) <- c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance","group")

test$FunctionalGroupIndex <- recode(test$FunctionalGroupIndex, '0' = "Endotherm Herbivores (i.)", '1' = "Endotherm Carnivores (i.)", '2' =  "Endotherm Omnivores (i.)",'3'="Ectotherm Herbivores (s.)",'4'="Ectotherm Carnivores (s.)",'5'="Ectotherm Omnivores (s.)",'6'="Ectotherm Herbivores (i.)",'7'="Ectotherm Carnivores (i.)",'8'="Ectotherm Omnivores (i.)" )
test_c$FunctionalGroupIndex <- recode(test_c$FunctionalGroupIndex, '0' = "Endotherm Herbivores (i.)", '1' = "Endotherm Carnivores (i.)", '2' =  "Endotherm Omnivores (i.)",'3'="Ectotherm Herbivores (s.)",'4'="Ectotherm Carnivores (s.)",'5'="Ectotherm Omnivores (s.)",'6'="Ectotherm Herbivores (i.)",'7'="Ectotherm Carnivores (i.)",'8'="Ectotherm Omnivores (i.)" )

###-------------------------------------------------------------------------###
###                             2. SCATTERPLOT                              ###
###                 FEEDING GUILD: ABUNDANCE VS BODYSIZE                    ###
###Note: uses above prepared data sets (test)                               ###
###-------------------------------------------------------------------------###

#create objects for each FEEDING GUILD
herbivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Herbivores (i.)","Ectotherm Herbivores (s.)","Ectotherm Herbivores (i.)"),]
carnivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Carnivores (i.)","Ectotherm Carnivores (s.)","Ectotherm Carnivores (i.)"),]
omnivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Omnivores (i.)","Ectotherm Omnivores (s.)","Ectotherm Omnivores (i.)"),]

#control
herbivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Herbivores (i.)","Ectotherm Herbivores (s.)","Ectotherm Herbivores (i.)"),]
carnivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Carnivores (i.)","Ectotherm Carnivores (s.)","Ectotherm Carnivores (i.)"),]
omnivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Omnivores (i.)","Ectotherm Omnivores (s.)","Ectotherm Omnivores (i.)"),]

#create individual y label size so that we don't have to adjust ylabels (regression equation / R and p-value) when changing dataset
labely <- 0
labelyeq <- 1
labelx <- log10(min(herbivores$IndividualBodyMass))
#plot scatter plot for each feeding guild
Herbivores <- ggplot()+
  geom_point(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance)),color = "black",size=1,stat="identity",alpha=0.5)+
  geom_point(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  geom_smooth(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  stat_cor(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black",label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x = labelx, show.legend=F, size=4)+
  stat_smooth(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  geom_smooth(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_smooth(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_cor(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely, label.x = labelx, show.legend=F, size=4)+
  scale_color_manual(values = c("Historical" = "#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C","black" = "#CC66CC"),labels=c("Historical","SSP1-2.6","SSP5-8.5","Natural Undisturbed Ecosystem State"))+
  scale_fill_brewer(palette="Pastel1")+
  facet_wrap(~group,scales="fixed")+
  theme_classic()+
  labs(title='Herbivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",fill="Functional Group",col="Climate Scenario")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  lims(y=c(0,12.5))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Herbivores

labelx <- log10(min(omnivores$IndividualBodyMass))

#plot scatterplot
Omnivores <- ggplot()+
  geom_point(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance)),color = "black",size=1,stat="identity",alpha=0.5)+
  geom_point(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  geom_smooth(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  stat_cor(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black",label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x = labelx, show.legend=F, size=4)+
  stat_smooth(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  geom_smooth(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_smooth(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_cor(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y=labely, label.x=labelx, show.legend=F, size=4)+
  scale_color_manual(values = c("Historical" = "#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C","black" = "#CC66CC"),labels=c("Historical","SSP1-2.6","SSP5-8.5","Natural Undisturbed Ecosystem State"))+
  scale_fill_brewer(palette = "Pastel2")+
  facet_wrap(~group)+
  theme_classic()+
  labs(title='Omnivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",fill="Functional Group",col="Climate Scenario")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  lims(y=c(0,12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Omnivores

labelx <- log10(min(carnivores$IndividualBodyMass))
#plot scatterplot
Carnivores <- ggplot()+
  geom_point(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance)),color = "black",size=1,stat="identity",alpha=0.5)+
  geom_point(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  geom_smooth(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  stat_cor(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black",label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x = labelx, show.legend=F, size=4)+
  stat_smooth(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  geom_smooth(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_regline_equation(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  stat_smooth(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  scale_color_manual(values = c("Historical" = "#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C","black" = "#CC66CC"),labels=c("Historical","SSP1-2.6","SSP5-8.5","Natural Undisturbed Ecosystem State"))+
  scale_fill_brewer(palette = "Set2")+
  facet_wrap(~group)+
  stat_cor(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y=labely, label.x =labelx, show.legend=F, size=4)+
  theme_classic()+
  labs(title='Carnivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",fill="Functional Group",col="Climate Scenario")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  lims(y=c(0,10))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Carnivores
#arrange all 3 plots side by side with ggarrange
ggarrange(Herbivores,Carnivores,Omnivores, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3,
          legend = "right",
          common.legend=F,
          label.x = 0,
          label.y = 1)#dev.off()

#save file 
ggsave(regionpath %+% region %+% "_" %+% "CurrLU_Abundance_Bodymass.pdf",dpi=600,width=14,height=12)

###-------------------------------------------------------------------------###
###                               max.HANPP (Max. LU)                       ###
###-------------------------------------------------------------------------###
###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
rm(cohorts_histo,cohorts_SSP126,cohorts_SSP585,herbivores,carnivores,omnivores)

#just load data again 
#list[1] = control; list[2] = HANPP; last list entry [individual on region] = After vegreduction
cohorts_histo <- data.frame(historical_2014_list[[13]]$cohorts) 
cohorts_SSP126 <- data.frame(SSP2.6_2100_list[[13]]$cohorts)
cohorts_SSP585 <- data.frame(SSP8.5_2100_list[[13]]$cohorts) 
#generate  control datasets for comparison in plot
cohorts_histo_c <- data.frame(historical_2014_list[[2]]$cohorts) 
cohorts_SSP126_c <- data.frame(SSP2.6_2100_list[[2]]$cohorts)
cohorts_SSP585_c <- data.frame(SSP8.5_2100_list[[2]]$cohorts) 

#subset data 
cohorts_histo <- cohorts_histo[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP126 <- cohorts_SSP126[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP585 <- cohorts_SSP585[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
#for control
cohorts_histo_c <- cohorts_histo_c[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP126_c <- cohorts_SSP126_c[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP585_c <- cohorts_SSP585_c[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]

#aggregate data to FG per gridcell // calculates mean for each gridcell & FG
cohorts_histo <- aggregate(cohorts_histo[3:4],list(cohorts_histo$GridcellIndex,cohorts_histo$FunctionalGroupIndex),mean)
cohorts_SSP126 <- aggregate(cohorts_SSP126[3:4],list(cohorts_SSP126$GridcellIndex,cohorts_SSP126$FunctionalGroupIndex),mean)
cohorts_SSP585 <- aggregate(cohorts_SSP585[3:4],list(cohorts_SSP585$GridcellIndex,cohorts_SSP585$FunctionalGroupIndex),mean)
#for control
cohorts_histo_c <- aggregate(cohorts_histo_c[3:4],list(cohorts_histo_c$GridcellIndex,cohorts_histo_c$FunctionalGroupIndex),mean)
cohorts_SSP126_c <- aggregate(cohorts_SSP126_c[3:4],list(cohorts_SSP126_c$GridcellIndex,cohorts_SSP126_c$FunctionalGroupIndex),mean)
cohorts_SSP585_c <- aggregate(cohorts_SSP585_c[3:4],list(cohorts_SSP585_c$GridcellIndex,cohorts_SSP585_c$FunctionalGroupIndex),mean)

#create new grouping variable for 
cohorts_histo$group = "Historical"
cohorts_SSP126$group = "SSP1-2.6"
cohorts_SSP585$group = "SSP5-8.5"
#control
cohorts_histo_c$group = "Historical"
cohorts_SSP126_c$group = "SSP1-2.6"
cohorts_SSP585_c$group = "SSP5-8.5"

#combine dataframes 
test <- rbind(cohorts_histo,cohorts_SSP126,cohorts_SSP585)
#convert to kg 
test[3] <- test[3]/1000 
#control
test_c <- rbind(cohorts_histo_c,cohorts_SSP126_c,cohorts_SSP585_c)
#convert to kg 
test_c[3] <- test_c[3]/1000 
#change naming
names(test) <- c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance","group")
#control
names(test_c) <- c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance","group")

test$FunctionalGroupIndex <- recode(test$FunctionalGroupIndex, '0' = "Endotherm Herbivores (i.)", '1' = "Endotherm Carnivores (i.)", '2' =  "Endotherm Omnivores (i.)",'3'="Ectotherm Herbivores (s.)",'4'="Ectotherm Carnivores (s.)",'5'="Ectotherm Omnivores (s.)",'6'="Ectotherm Herbivores (i.)",'7'="Ectotherm Carnivores (i.)",'8'="Ectotherm Omnivores (i.)" )
test_c$FunctionalGroupIndex <- recode(test_c$FunctionalGroupIndex, '0' = "Endotherm Herbivores (i.)", '1' = "Endotherm Carnivores (i.)", '2' =  "Endotherm Omnivores (i.)",'3'="Ectotherm Herbivores (s.)",'4'="Ectotherm Carnivores (s.)",'5'="Ectotherm Omnivores (s.)",'6'="Ectotherm Herbivores (i.)",'7'="Ectotherm Carnivores (i.)",'8'="Ectotherm Omnivores (i.)" )

###-------------------------------------------------------------------------###
###                             2. SCATTERPLOT                              ###
###                 FEEDING GUILD: ABUNDANCE VS BODYSIZE                    ###
###Note: uses above prepared data sets (test)                               ###
###-------------------------------------------------------------------------###

#create objects for each FEEDING GUILD
herbivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Herbivores (i.)","Ectotherm Herbivores (s.)","Ectotherm Herbivores (i.)"),]
carnivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Carnivores (i.)","Ectotherm Carnivores (s.)","Ectotherm Carnivores (i.)"),]
omnivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Omnivores (i.)","Ectotherm Omnivores (s.)","Ectotherm Omnivores (i.)"),]

#control
herbivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Herbivores (i.)","Ectotherm Herbivores (s.)","Ectotherm Herbivores (i.)"),]
carnivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Carnivores (i.)","Ectotherm Carnivores (s.)","Ectotherm Carnivores (i.)"),]
omnivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Omnivores (i.)","Ectotherm Omnivores (s.)","Ectotherm Omnivores (i.)"),]

labelx <- log10(min(herbivores$IndividualBodyMass))
#plot scatter plot for each feeding guild
Herbivores <- ggplot()+
  geom_point(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance)),color = "black",size=1,stat="identity",alpha=0.5)+
  geom_point(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  geom_smooth(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  stat_cor(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black",label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x = labelx, show.legend=F, size=4)+
  stat_smooth(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  geom_smooth(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_smooth(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_cor(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely, label.x =labelx, show.legend=F, size=4)+
  scale_color_manual(values = c("Historical" = "#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C","black" = "#CC66CC"),labels=c("Historical","SSP1-2.6","SSP5-8.5","Current Land-Use Intensity"))+
  scale_fill_brewer(palette="Pastel1",labels = c("Endotherm Herbivores (i.)","Ectotherm Herbivores (s.)","Ectotherm Herbivores (i.)"))+
  facet_wrap(~group,scales="fixed")+
  theme_classic()+
  labs(title='Herbivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",col="Climate Scenario",fill="Functional Group")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  lims(y=c(0,12.5))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Herbivores

labelx <- log10(min(omnivores$IndividualBodyMass))
#plot scatterplot
Omnivores <- ggplot()+
  geom_point(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance)),color = "black",size=1,stat="identity",alpha=0.5)+
  geom_point(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  geom_smooth(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  stat_cor(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black",label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x = labelx, show.legend=F, size=4)+
  stat_smooth(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  geom_smooth(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_regline_equation(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  stat_smooth(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  scale_color_manual(values = c("Historical" = "#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C","black" = "#CC66CC"),labels=c("Historical","SSP1-2.6","SSP5-8.5","Current Land-Use Intensity"))+
  scale_fill_brewer(palette = "Pastel2",labels = c("Endotherm Omnivores (i.)","Ectotherm Omnivores (s.)","Ectotherm Omnivores (i.)"))+
  facet_wrap(~group)+
  stat_cor(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y=labely, label.x=labelx, show.legend=F, size=4)+
  theme_classic()+
  labs(title='Omnivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",col="Climate Scenario",fill="Functional Group")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  lims(y=c(0,12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Omnivores

labelx <- log10(min(carnivores$IndividualBodyMass))

#plot scatterplot
Carnivores <- ggplot()+
  geom_point(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance)),color = "black",size=1,stat="identity",alpha=0.5)+
  geom_point(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  geom_smooth(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  stat_regline_equation(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  stat_cor(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black",label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x = labelx, show.legend=F, size=4)+
  stat_smooth(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color="black"),method="lm")+
  geom_smooth(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_regline_equation(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  stat_smooth(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  scale_color_manual(values = c("Historical" = "#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C","black" = "#CC66CC"),labels=c("Historical","SSP1-2.6","SSP5-8.5","Current Land-Use Intensity"))+
  scale_fill_brewer(palette = "Set2",labels = c("Endotherm Carnivores (i.)","Ectotherm Carnivores (s.)","Ectotherm Carnivores (i.)"))+
  facet_wrap(~group)+
  stat_cor(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y=labely, label.x = labelx, show.legend=F, size=4)+
  theme_classic()+
  labs(title='Carnivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",col="Climate Scenario",fill="Functional Group")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  lims(y=c(0,10))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Carnivores
#arrange all 3 plots side by side with ggarrange
ggarrange(Herbivores,Carnivores,Omnivores, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3,
          legend = "right",
          common.legend=F,
          label.x = 0,
          label.y = 1)#dev.off()

#save file 
ggsave(regionpath %+% region %+% "_" %+% "MaxLU_FeedingGuild.pdf",dpi=600,width=14,height=12)

###-------------------------------------------------------------------------###
###                                      CLIMATE                            ###
###-------------------------------------------------------------------------###
###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
rm(cohorts_histo,cohorts_SSP126,cohorts_SSP585,herbivores,carnivores,omnivores)
###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
#just load data again 
#list[1] = control; list[2] = HANPP; last list entry [individual on region] = After vegreduction
cohorts_histo <- data.frame(historical_2014_list[[1]]$cohorts) 
cohorts_SSP126 <- data.frame(SSP2.6_2100_list[[1]]$cohorts)
cohorts_SSP585 <- data.frame(SSP8.5_2100_list[[1]]$cohorts) 

#subset data 
cohorts_histo <- cohorts_histo[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP126 <- cohorts_SSP126[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]
cohorts_SSP585 <- cohorts_SSP585[ ,c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance")]

#aggregate data to FG per gridcell // calculates mean for each gridcell & FG
cohorts_histo <- aggregate(cohorts_histo[3:4],list(cohorts_histo$GridcellIndex,cohorts_histo$FunctionalGroupIndex),mean)
cohorts_SSP126 <- aggregate(cohorts_SSP126[3:4],list(cohorts_SSP126$GridcellIndex,cohorts_SSP126$FunctionalGroupIndex),mean)
cohorts_SSP585 <- aggregate(cohorts_SSP585[3:4],list(cohorts_SSP585$GridcellIndex,cohorts_SSP585$FunctionalGroupIndex),mean)

#create new grouping variable for 
cohorts_histo$histo = "Historical"
cohorts_SSP126$group = "SSP1-2.6"
cohorts_SSP585$group = "SSP5-8.5"

#combine dataframes 
test <- rbind(cohorts_SSP126,cohorts_SSP585)
#convert to kg 
test[3] <- test[3]/1000 
#change naming
names(test) <- c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance","group")

#control
test_c <- rbind(cohorts_histo)
#convert to kg 
test_c[3] <- test_c[3]/1000
#control
names(test_c) <- c("GridcellIndex","FunctionalGroupIndex","IndividualBodyMass","CohortAbundance","histo")


test$FunctionalGroupIndex <- recode(test$FunctionalGroupIndex, '0' = "Endotherm Herbivores (i.)", '1' = "Endotherm Carnivores (i.)", '2' =  "Endotherm Omnivores (i.)",'3'="Ectotherm Herbivores (s.)",'4'="Ectotherm Carnivores (s.)",'5'="Ectotherm Omnivores (s.)",'6'="Ectotherm Herbivores (i.)",'7'="Ectotherm Carnivores (i.)",'8'="Ectotherm Omnivores (i.)" )
test_c$FunctionalGroupIndex <- recode(test_c$FunctionalGroupIndex, '0' = "Endotherm Herbivores (i.)", '1' = "Endotherm Carnivores (i.)", '2' =  "Endotherm Omnivores (i.)",'3'="Ectotherm Herbivores (s.)",'4'="Ectotherm Carnivores (s.)",'5'="Ectotherm Omnivores (s.)",'6'="Ectotherm Herbivores (i.)",'7'="Ectotherm Carnivores (i.)",'8'="Ectotherm Omnivores (i.)" )

#create objects for each FEEDING GUILD
herbivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Herbivores (i.)","Ectotherm Herbivores (s.)","Ectotherm Herbivores (i.)"),]
carnivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Carnivores (i.)","Ectotherm Carnivores (s.)","Ectotherm Carnivores (i.)"),]
omnivores <- test[test$FunctionalGroupIndex%in%c("Endotherm Omnivores (i.)","Ectotherm Omnivores (s.)","Ectotherm Omnivores (i.)"),]

#control
herbivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Herbivores (i.)","Ectotherm Herbivores (s.)","Ectotherm Herbivores (i.)"),]
carnivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Carnivores (i.)","Ectotherm Carnivores (s.)","Ectotherm Carnivores (i.)"),]
omnivores_c <- test_c[test_c$FunctionalGroupIndex%in%c("Endotherm Omnivores (i.)","Ectotherm Omnivores (s.)","Ectotherm Omnivores (i.)"),]

labelx <- log10(min(herbivores$IndividualBodyMass))

###-------------------------------------------------------------------------###
###                             2. SCATTERPLOT                              ###
###                 FEEDING GUILD: ABUNDANCE VS BODYSIZE                    ###
###Note: uses above prepared data sets (test)                               ###
###-------------------------------------------------------------------------###
#plot scatter plot for each feeding guild
Herbivores <- ggplot()+
  geom_point(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),size=1,stat="identity",shape=21)+
  geom_point(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  stat_regline_equation(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  stat_regline_equation(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color= histo),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  geom_smooth(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  geom_smooth(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),method="lm")+
  stat_smooth(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_smooth(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),method="lm")+
  stat_cor(herbivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely, label.x = labelx, show.legend=F, size=4)+
  stat_cor(herbivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x = labelx, show.legend=F, size=4)+
  scale_color_manual(values = c("Historical"= "#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C"))+
  scale_fill_brewer(palette="Pastel1")+
  facet_wrap(~group,scales="fixed")+
  theme_classic()+
  labs(title='Herbivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",col="Climate Scenario",fill="Functional Group")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  lims(y=c(0,12.5))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Herbivores

labelx <- log10(min(omnivores$IndividualBodyMass))

#plot scatterplot
Omnivores <- ggplot()+
  geom_point(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),size=1,stat="identity",shape=21)+
  geom_point(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  geom_smooth(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  geom_smooth(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),method="lm")+
  stat_regline_equation(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  stat_regline_equation(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color= histo),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  stat_smooth(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_smooth(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),method="lm")+
  scale_color_manual(values = c("Historical"="#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C"))+
  scale_fill_brewer(palette = "Pastel2")+
  facet_wrap(~group)+
  stat_cor(omnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y=labely, label.x=labelx, show.legend=F, size=4)+
  stat_cor(omnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x = labelx, show.legend=F, size=4)+
  theme_classic()+
  labs(title='Omnivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",col="Climate Scenario",fill="Functional Group")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  lims(y=c(0,12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Omnivores

labelx <- log10(min(carnivores$IndividualBodyMass))

#plot scatterplot
Carnivores <- ggplot()+
  geom_point(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),size=1,stat="identity",shape=21)+
  geom_point(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),fill=factor(FunctionalGroupIndex)),size=1,stat="identity",shape=21,key_glyph="rect",alpha=0.5)+
  geom_smooth(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  geom_smooth(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),method="lm")+
  stat_regline_equation(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),label.y=labelyeq,label.x=labelx,size=4,show.legend=F)+
  stat_regline_equation(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color= histo),label.y=labelyeq+2,label.x=labelx,size=4,show.legend=F)+
  stat_smooth(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group),method="lm")+
  stat_smooth(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo),method="lm")+
  scale_color_manual(values = c("Historical"="#4DAF4A","SSP1-2.6" = "#377EB8","SSP5-8.5" = "#E41A1C"))+
  scale_fill_brewer(palette = "Set2")+
  facet_wrap(~group)+
  stat_cor(carnivores,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=group,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y=labely, label.x=labelx, show.legend=F, size=4)+
  stat_cor(carnivores_c,mapping=aes(x=IndividualBodyMass,y=log10(CohortAbundance),color=histo,label=paste(..rr.label..,..p.label..,sep = "~`,`~")), method = "pearson",label.y =labely+2, label.x =labelx, show.legend=F, size=4)+
  theme_classic()+
  ylim(0,10)+
  labs(title='Carnivores', x='Log10 Body Mass bin [kg]', y = "Log10 Abundance",col="Climate Scenario",fill="Functional Group")+
  guides(
    col = guide_legend(order = 1),
    fill = guide_legend(order = 2))+
  theme(plot.title = element_text(face = "bold", hjust =0.5))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12)) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12))+
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)(c(1e-4,1e3)),
                labels = trans_format("log10", math_format(10^.x)))
Carnivores

#arrange all 3 plots side by side with ggarrange
ggarrange(Herbivores,Carnivores,Omnivores, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3,
          legend = "right",
          common.legend=F,
          label.x = 0,
          label.y = 1)#dev.off()

#save file
ggsave(regionpath %+% region %+% "_" %+%  "Climate_FeedingGuild.pdf",dpi=600,width=14,height=12)



