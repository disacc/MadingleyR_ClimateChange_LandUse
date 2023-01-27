###-----------------------------------------------------------------------------------------###
### TITLE:       CLIMATE BARPLOTS                                                           ###
### DESCRIPTION: Plots climate barplots of:                                                 ###
###              "Model-based Impact Analysis of Climate Change and Land-use Intensification###
###              on Trophic Networks."                                                      ###                                                                 ###
### PROCEDURE:   Plots climate barplots for each region, climate scenario and variable      ###
###              Input is loaded from Extract_raster_statistics.R output                    ###
### DATE:        21.09.2022                                                                 ###
###-----------------------------------------------------------------------------------------###  

library(dplyr)
library(ggplot2)
library(ggpubr)
library(colorRamps)
library(RStoolbox)
library(data.table)

###---------------------------------------------------------------------------###
###                          CREATE SETTINGS / ENVIRONMENT                    ###
###---------------------------------------------------------------------------###

source("settings.R")

myoutpath <- outpath   # outpath in RData will override local outpath settings, see below
myfigpath <- figpath

#load datapath to read in climate statistics 
datapath <- paste0(figpath,"data_climate_rasters",sep="/")

###---------------------------------------------------------------------------###
###                       PREPARE CLIMATE DATA FOR PLOTTING                   ###
###---------------------------------------------------------------------------###

#read in climate statistics
temp <- list.files(datapath,pattern="*.csv") 
for (i in 1:length(temp)) assign(temp[i], read.csv(datapath%+%temp[i]))

#create combined df for each variable 
hanpp_summary <- rbind(HANPP_summary_Historical.csv) #hanpp only needs 1 scenario, because layer always stays the same in each climate scenario
npp_summary <- rbind(NPP_summary_Historical.csv,`NPP_summary_SSP1-2.6.csv`,`NPP_summary_SSP5-8.5.csv`)
tas_summary <- rbind(tas_summary_Historical.csv,`tas_summary_SSP1-2.6.csv`,`tas_summary_SSP5-8.5.csv`)
pr_summary <- rbind(pr_summary_Historical.csv,`pr_summary_SSP1-2.6.csv`,`pr_summary_SSP5-8.5.csv`)

#remove the old ones 
rm(HANPP_summary_Historical.csv,`HANPP_summary_SSP1-2.6.csv`,`HANPP_summary_SSP5-8.5.csv`,
   NPP_summary_Historical.csv,`NPP_summary_SSP1-2.6.csv`,`NPP_summary_SSP5-8.5.csv`,
   tas_summary_Historical.csv,`tas_summary_SSP1-2.6.csv`,`tas_summary_SSP5-8.5.csv`,
   pr_summary_Historical.csv,`pr_summary_SSP1-2.6.csv`,`pr_summary_SSP5-8.5.csv`)

###---------------------------------------------------------------------------###
###                                     PLOT                                  ###
###---------------------------------------------------------------------------###

#plot & save result mean HANPP & SD 
#create factor lvl for regions
hanpp_summary$Region <- factor(hanpp_summary$Region,levels=c("Finland","France","Namibia","Colombia","India","Europe"))

hanpp_plot <- ggplot()+
  geom_bar(hanpp_summary,mapping=aes(x=Mean,y=Region),,fill="grey",stat="identity",width=0.8)+
#  geom_errorbar(hanpp_summary,mapping=aes(xmax=Mean+SD,xmin=Mean-SD,y=Region),stat="identity",width=0.05,size=0.2)+
 # scale_fill_brewer(palette="Set3")+
  scale_y_discrete(labels=c("Central Finland","Central France","Kavango, Namibia","Vaupés, Colombia","India","Europe"))+
  theme_classic()+
  labs(title='HANPP', x=expression(paste("Mean HANPP [gC/m"^2, "/y]")), y = "Region",fill="Region")+
  theme(axis.text = element_text(size=20))+
  theme(axis.text.y = element_text(hjust=0))+
  theme(plot.title = element_text(face = "bold", hjust =0.5, size = 20))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=20))+
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=20))+
  theme(legend.text = element_text(size=20),legend.title = element_text(size=20,face="bold"))

hanpp_plot

ggsave(figpath %+% "HANPP_Regions.png",dpi=600,width=4,height=2)

#plot & save result mean NPP & SD 
#create factor lvl for climate scenario
npp_summary$Scenario<- factor(npp_summary$Scenario,levels=c("SSP5-8.5","SSP1-2.6","Historical"))
#create factor lvl for regions
npp_summary$Region <- factor(npp_summary$Region,levels=c("Finland","France","Namibia","Colombia","India","Europe"))

#plot
npp_plot <- ggplot()+
  geom_bar(npp_summary,mapping=aes(x=Mean,y=Region,fill=Scenario),position="dodge",stat="identity",width=0.8)+
 # geom_errorbar(npp_summary,mapping=aes(xmax=Mean+SD,xmin=Mean-SD,y=Region),width=0.05,size=0.2,position=position_dodge(0.1))+
  scale_fill_brewer(palette = "Set1")+
  scale_y_discrete(labels=c("Central Finland","Central France","Kavango, Namibia","Vaupés, Colombia","India","Europe"))+
  theme_classic()+
  labs(title='NPP', x=expression(paste("Mean NPP [gC/m"^2, "/d]")), y = "Region",fill="Climate Scenario")+
  theme(axis.text = element_text(size=20))+
  theme(axis.text.y = element_text(hjust=0))+
  theme(plot.title = element_text(face = "bold", hjust =0.5, size = 20))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=20))+
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=20))+
  theme(legend.text = element_text(size=20),legend.title = element_text(size=20,face="bold"))

npp_plot

ggsave(figpath %+% "NPP_Regions.png",dpi=600,width=4,height=2)

#plot & save result mean tas 
#create factor lvl for climate scenario
tas_summary$Scenario<- factor(tas_summary$Scenario,levels=c("SSP5-8.5","SSP1-2.6","Historical"))
#create factor lvl for regions
tas_summary$Region <- factor(tas_summary$Region,levels=c("Finland","France","Namibia","Colombia","India","Europe"))

#plot
tas_plot <- ggplot()+
  geom_bar(tas_summary,mapping=aes(x=Mean,y=Region,fill=Scenario),position="dodge",stat="identity",width=0.8)+
  # geom_errorbar(tas_summary,mapping=aes(xmax=Mean+SD,xmin=Mean-SD,y=Region),width=0.05,size=0.2,position=position_dodge(0.1))+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  scale_y_discrete(labels=c("Central Finland","Central France","Kavango, Namibia","Vaupés, Colombia","India","Europe"))+
  labs(title='Near-Surface Temperature', x="Mean Temperature [°C]", y = "Region",fill="Climate Scenario")+
  theme(axis.text = element_text(size=20))+
  theme(axis.text.y = element_text(hjust=0))+
  theme(plot.title = element_text(face = "bold", hjust =0.5, size = 20))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=20))+
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=20))+
  theme(legend.text = element_text(size=20),legend.title = element_text(size=20,face="bold"))

tas_plot 

ggsave(figpath %+% "tas_Regions.png",dpi=600,width=4,height=2)

#plot & save result mean pr
#create factor lvl for climate scenario
pr_summary$Scenario<- factor(pr_summary$Scenario,levels=c("SSP5-8.5","SSP1-2.6","Historical"))
#create factor lvl for regions
pr_summary$Region <- factor(pr_summary$Region,levels=c("Finland","France","Namibia","Colombia","India","Europe"))

#plot
pr_plot <- ggplot()+
  geom_bar(pr_summary,mapping=aes(x=Mean,y=Region,fill=Scenario),position="dodge",stat="identity",width=0.8)+
  # geom_errorbar(pr_summary,mapping=aes(xmax=Mean+SD,xmin=Mean-SD,y=Region),width=0.05,size=0.2,position=position_dodge(0.1))+
  scale_fill_brewer(palette = "Set1")+
  scale_y_discrete(labels=c("Central Finland","Central France","Kavango, Namibia","Vaupés, Colombia","India","Europe"))+
  theme_classic()+
  labs(title="Precipitation", x="Mean Precipitation [mm/month]", y = "Region",fill="Climate Scenario")+
  theme(axis.text = element_text(size=20))+
  theme(axis.text.y = element_text(hjust=0))+
  theme(plot.title = element_text(face = "bold", hjust =0.5, size = 20))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=20))+
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=20))+
  theme(legend.text = element_text(size=20),legend.title = element_text(size=20,face="bold"))

pr_plot

ggsave(figpath %+% "pr_Regions.png",dpi=600,width=4,height=2)

#plot overview: 
ggarrange(npp_plot,tas_plot+ rremove("y.text") + rremove ("y.title"),pr_plot +rremove("y.text") + rremove ("y.title"),hanpp_plot + rremove("y.text") + rremove ("y.title") +rremove("legend"),
          ncol = 4, nrow = 1,
          common.legend = T,
          widths = c(1.2,0.7,0.7,0.7),
          legend = "bottom",
          align = "h")

ggsave(figpath %+% "Climate_Overview.pdf",width=16.6,height = 4)

