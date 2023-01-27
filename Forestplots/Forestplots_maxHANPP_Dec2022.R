###-----------------------------------------------------------------------------------------###
### TITLE:       FORESTPLOTS maxHANPP SCENARIOS                                             ###
### DESCRIPTION: Creates forestplots for Climate Phase of:                                  ###
###              "Model-based Impact Analysis of Climate Change and Land-use Intensification###
###              on Trophic Networks."                                                      ###                                                                 ###
### PROCEDURE:   Calculates percentage change based on logRR between the distinct scenarios ###
###              for each region and exports results as forestplot images + csv datasets    ###
### DATE:        02.02.2023                                                                 ###
###-----------------------------------------------------------------------------------------### 

###LOAD LIBRARIES
library(MadingleyR)
library(ggplot2)
library(dplyr)
library(ARPobservation)
library(RColorBrewer)
library(ggpubr)
library(grid)
library(boot)#bootstrap CIs

###To Do: Correct Error of extinct FGs
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

###------###
###EUROPE###
###------###
#load workspace images from outpath 
#list file can be read: 
#entry 1 = spinup, entry 2 = apply hanpp, 
#entry 3 - 12 = vegetation reduction stages, 
#last entry = 13 = post reduction run (with 0.1 vegetation left)
load(outpath %+% "Europe_historical_2014_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Europe_SSP126_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Europe_SSP585_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath

###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
#1. Create df for control + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_control <- historical_2014_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_control <- SSP2.6_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_control <- SSP8.5_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#2. Create df for HANPP scenarios + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_HANPP <- historical_2014_list[[13]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_HANPP <- SSP2.6_2100_list[[13]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_HANPP <- SSP8.5_2100_list[[13]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#3. Reorganize data: to create 1 df per climate scenario --> for logRR calculation 
#creates df with control and hanpp application observations (can be distinguished with group variable)
#stick control & treatment dataframe for historical
historical <- rbind(cohorts_historical_control,cohorts_historical_HANPP)
unique(historical$Group)
#calculation of log10 values
historical[3:9] <- log10(historical[3:9]) 

#stick control & treatment dataframe for SSP2.6
SSP2.6 <- rbind(cohorts_SSP2.6_2100_control,cohorts_SSP2.6_2100_HANPP)
unique(SSP2.6$Group)
#calculation of log10 values 
SSP2.6[3:9] <- log10(SSP2.6[3:9])

#stick control & treatment dataframe for SSP8.5
SSP8.5 <- rbind(cohorts_SSP8.5_2100_control,cohorts_SSP8.5_2100_HANPP)
unique(SSP8.5$Group)
#calculation of log10 values 
SSP8.5[3:9] <- log10(SSP8.5[3:9]) 

###-----------------------------------------------------------------###
###  2.       EFFECT SIZE CALCULATION // logRR                      ###
###-----------------------------------------------------------------###

###Set up function which calculates logRR effect sizes & variances between samples for bootstrapping 
logRR <- function(x,y) {
  
  resample <- x[y]
  control <- resample[1:119]
  treatment <- resample[120:238]
  
  mean_c <- mean(control)
  mean_t <- mean(treatment)
  
  var_c <- var(control)
  var_t <- var(treatment)
  
  variance <- as.numeric(c(var_c,var_t))
  mean <- as.numeric(c(mean_c,mean_t))
  n <- as.numeric(c(length(control),length(treatment)))
  
  V_lRR <- sum(variance/(n*mean^2))
  
  V_lRR <- sqrt(V_lRR)
  
  logRR <- log((mean_t))-log((mean_c))
  
  #CI <- logRR + c(-1, 1) * stats::qnorm(1-(1-0.95)/2) * sqrt(V_lRR)
  
  return(c(logRR,V_lRR))
}

###Historical natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_historical_control$Biomass_FG_0)
n2 <- length(cohorts_historical_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(historical$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(historical$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(historical$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(historical$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(historical$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(historical$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(historical$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_historical <- data.frame(c("Europe"),
                                c("End. Herbivores","End. Carnivores","End. Omnivores",
                                  "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                                c(1,2,3,4,5,6,7),
                                c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                                c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                                c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                                c("Historical"),
                                c(sum(historical$Biomass_FG_0),sum(historical$Biomass_FG_1),sum(historical$Biomass_FG_2),sum(historical$Biomass_FG_3),sum(historical$Biomass_FG_4),sum(historical$Biomass_FG_5),sum(historical$BiomassSum)))

names(effect_historical) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_historical[4:6] <- (exp(effect_historical[4:6])-1)*100

###SSP1-2.6 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP2.6_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP2.6_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP2.6$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP2.6$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP2.6$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP2.6$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP2.6$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP2.6$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP2.6$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_SSP2.6 <- data.frame(c("Europe"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP1-2.6"),
                            c(sum(SSP2.6$Biomass_FG_0),sum(SSP2.6$Biomass_FG_1),sum(SSP2.6$Biomass_FG_2),sum(SSP2.6$Biomass_FG_3),sum(SSP2.6$Biomass_FG_4),sum(SSP2.6$Biomass_FG_5),sum(SSP2.6$BiomassSum)))

names(effect_SSP2.6) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP2.6[4:6] <- (exp(effect_SSP2.6[4:6])-1)*100

###SSP5-8.5 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP8.5_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP8.5_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP8.5$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP8.5$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP8.5$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP8.5$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP8.5$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP8.5$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP8.5$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_SSP8.5 <- data.frame(c("Europe"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP5-8.5"),
                            c(sum(SSP8.5$Biomass_FG_0),sum(SSP8.5$Biomass_FG_1),sum(SSP8.5$Biomass_FG_2),sum(SSP8.5$Biomass_FG_3),sum(SSP8.5$Biomass_FG_4),sum(SSP8.5$Biomass_FG_5),sum(SSP8.5$BiomassSum)))

names(effect_SSP8.5) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP8.5[4:6] <- (exp(effect_SSP8.5[4:6])-1)*100 

###-----------------------------------------------------------------###
###  3.                  PLOTTING: FORESTPLOT                       ###
###-----------------------------------------------------------------###

#create factor lvls 
effect_historical$`Functional Group`<- factor(effect_historical$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP2.6$`Functional Group`<- factor(effect_SSP2.6$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP8.5$`Functional Group`<- factor(effect_SSP8.5$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))

#add overall biomass line

effect_historical$GrandMean <- ifelse(grepl("End.",effect_historical$`Functional Group`)|grepl("Ect.",effect_historical$`Functional Group`),FALSE,TRUE)
effect_SSP2.6$GrandMean <- ifelse(grepl("End.",effect_SSP2.6$`Functional Group`)|grepl("Ect.",effect_SSP2.6$`Functional Group`),FALSE,TRUE)
effect_SSP8.5$GrandMean <- ifelse(grepl("End.",effect_SSP8.5$`Functional Group`)|grepl("Ect.",effect_SSP8.5$`Functional Group`),FALSE,TRUE)

#now plot the forest plot 
Europe <- ggplot() +
  geom_point(data=effect_historical,aes(x=Eff_size,y=`Functional Group`,col="Historical",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_historical,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="Historical"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP2.6,aes(x=Eff_size,y=`Functional Group`,col="SSP1-2.6",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP2.6,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP1-2.6"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP8.5,aes(x=Eff_size,y=`Functional Group`,col="SSP5-8.5",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP8.5,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP5-8.5"),width=0.1,size=0.3)+
  scale_color_brewer(palette = "Set1",direction=-1)+
  scale_size_continuous(guide="none")+
  scale_x_continuous(breaks = seq(-100,100,10),limits=c(-55,10))+
  scale_shape_manual(values = c("circle","diamond"),guide="none")+
  labs(title="Europe", x='Effect Size (% Change)', y = "Functional Group (FG)",col="Climate Scenario",trans="reverse")+
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_classic()+
  theme(axis.text.y = element_text(face = c('bold','plain', 'plain', 'plain', 'plain', 'plain', 'plain'),size=12,hjust=0))+
  theme(axis.text.x = element_text(size=12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12,face="bold")) +
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))

Europe

#create df for later csv output:
maxHANPP <- rbind(effect_historical,effect_SSP2.6,effect_SSP8.5)

###------###
### INDIA###
###------###
#load workspace images from outpath 
#list file can be read: 
#entry 1 = spinup, entry 2 = apply hanpp, 
#entry 3 - 12 = vegetation reduction stages, 
#last entry = 13 = post reduction run (with 0.1 vegetation left)
load(outpath %+% "India_historical_2014_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "India_SSP126_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "India_SSP585_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath

###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
#1. Create df for control + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_control <- historical_2014_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_control <- SSP2.6_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_control <- SSP8.5_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#2. Create df for HANPP scenarios + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_HANPP <- historical_2014_list[[13]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_HANPP <- SSP2.6_2100_list[[13]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_HANPP <- SSP8.5_2100_list[[13]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#3. Reorganize data: to create 1 df per climate scenario --> for logRR calculation 
#creates df with control and hanpp application observations (can be distinguished with group variable)
#stick control & treatment dataframe for historical
historical <- rbind(cohorts_historical_control,cohorts_historical_HANPP)
unique(historical$Group)
#calculation of log10 values
historical[3:9] <- log10(historical[3:9]) 

#stick control & treatment dataframe for SSP2.6
SSP2.6 <- rbind(cohorts_SSP2.6_2100_control,cohorts_SSP2.6_2100_HANPP)
unique(SSP2.6$Group)
#calculation of log10 values 
SSP2.6[3:9] <- log10(SSP2.6[3:9])

#stick control & treatment dataframe for SSP8.5
SSP8.5 <- rbind(cohorts_SSP8.5_2100_control,cohorts_SSP8.5_2100_HANPP)
unique(SSP8.5$Group)
#calculation of log10 values 
SSP8.5[3:9] <- log10(SSP8.5[3:9]) 

###-----------------------------------------------------------------###
###  2.       EFFECT SIZE CALCULATION // logRR                      ###
###-----------------------------------------------------------------###

###Historical natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_historical_control$Biomass_FG_0)
n2 <- length(cohorts_historical_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(historical$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(historical$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(historical$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(historical$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(historical$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(historical$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(historical$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_historical <- data.frame(c("India"),
                                c("End. Herbivores","End. Carnivores","End. Omnivores",
                                  "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                                c(1,2,3,4,5,6,7),
                                c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                                c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                                c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                                c("Historical"),
                                c(sum(historical$Biomass_FG_0),sum(historical$Biomass_FG_1),sum(historical$Biomass_FG_2),sum(historical$Biomass_FG_3),sum(historical$Biomass_FG_4),sum(historical$Biomass_FG_5),sum(historical$BiomassSum)))

names(effect_historical) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_historical[4:6] <- (exp(effect_historical[4:6])-1)*100

###SSP1-2.6 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP2.6_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP2.6_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP2.6$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP2.6$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP2.6$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP2.6$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP2.6$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP2.6$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP2.6$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_SSP2.6 <- data.frame(c("India"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP1-2.6"),
                            c(sum(SSP2.6$Biomass_FG_0),sum(SSP2.6$Biomass_FG_1),sum(SSP2.6$Biomass_FG_2),sum(SSP2.6$Biomass_FG_3),sum(SSP2.6$Biomass_FG_4),sum(SSP2.6$Biomass_FG_5),sum(SSP2.6$BiomassSum)))

names(effect_SSP2.6) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP2.6[4:6] <- (exp(effect_SSP2.6[4:6])-1)*100

###SSP5-8.5 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP8.5_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP8.5_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP8.5$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP8.5$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP8.5$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP8.5$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP8.5$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP8.5$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP8.5$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_SSP8.5 <- data.frame(c("India"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP5-8.5"),
                            c(sum(SSP8.5$Biomass_FG_0),sum(SSP8.5$Biomass_FG_1),sum(SSP8.5$Biomass_FG_2),sum(SSP8.5$Biomass_FG_3),sum(SSP8.5$Biomass_FG_4),sum(SSP8.5$Biomass_FG_5),sum(SSP8.5$BiomassSum)))

names(effect_SSP8.5) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP8.5[4:6] <- (exp(effect_SSP8.5[4:6])-1)*100 

###-----------------------------------------------------------------###
###  3.                  PLOTTING: FORESTPLOT                       ###
###-----------------------------------------------------------------###

#create factor lvls 
effect_historical$`Functional Group`<- factor(effect_historical$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP2.6$`Functional Group`<- factor(effect_SSP2.6$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP8.5$`Functional Group`<- factor(effect_SSP8.5$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))

#add overall biomass line

effect_historical$GrandMean <- ifelse(grepl("End.",effect_historical$`Functional Group`)|grepl("Ect.",effect_historical$`Functional Group`),FALSE,TRUE)
effect_SSP2.6$GrandMean <- ifelse(grepl("End.",effect_SSP2.6$`Functional Group`)|grepl("Ect.",effect_SSP2.6$`Functional Group`),FALSE,TRUE)
effect_SSP8.5$GrandMean <- ifelse(grepl("End.",effect_SSP8.5$`Functional Group`)|grepl("Ect.",effect_SSP8.5$`Functional Group`),FALSE,TRUE)

#now plot the forest plot 
India <- ggplot() +
  geom_point(data=effect_historical,aes(x=Eff_size,y=`Functional Group`,col="Historical",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_historical,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="Historical"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP2.6,aes(x=Eff_size,y=`Functional Group`,col="SSP1-2.6",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP2.6,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP1-2.6"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP8.5,aes(x=Eff_size,y=`Functional Group`,col="SSP5-8.5",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP8.5,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP5-8.5"),width=0.1,size=0.3)+
  scale_color_brewer(palette = "Set1",direction=-1)+
  scale_size_continuous(guide="none")+
  scale_x_continuous(breaks = seq(-100,100,10),limits=c(-55,10))+
  scale_shape_manual(values = c("circle","diamond"),guide="none")+
  labs(title="India", x='Effect Size (% Change)', y = "Functional Group (FG)",col="Climate Scenario",trans="reverse")+
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_classic()+
  theme(axis.text.y = element_text(face = c('bold','plain', 'plain', 'plain', 'plain', 'plain', 'plain'),size=12,hjust=0))+
  theme(axis.text.x = element_text(size=12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12,face="bold")) +
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))

India

#create df for later csv output:
maxHANPP <- rbind(maxHANPP,effect_historical,effect_SSP2.6,effect_SSP8.5)

###--------###
###COLOMBIA###
###--------###
#load workspace images from outpath 
#list file can be read: 
#entry 1 = spinup, entry 2 = apply hanpp, 
#entry 3 - 12 = vegetation reduction stages, 
#last entry = 13 = post reduction run (with 0.1 vegetation left)
load(outpath %+% "Vaupés_Colombia_historical_2014_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Vaupés_Colombia_SSP126_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Vaupés_Colombia_SSP585_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath

###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
#1. Create df for control + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_control <- historical_2014_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_control <- SSP2.6_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_control <- SSP8.5_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#2. Create df for HANPP scenarios + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_HANPP <- historical_2014_list[[11]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_HANPP <- SSP2.6_2100_list[[11]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_HANPP <- SSP8.5_2100_list[[11]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#3. Reorganize data: to create 1 df per climate scenario --> for logRR calculation 
#creates df with control and hanpp application observations (can be distinguished with group variable)
#stick control & treatment dataframe for historical
historical <- rbind(cohorts_historical_control,cohorts_historical_HANPP)
unique(historical$Group)
#calculation of log10 values
historical[3:9] <- log10(historical[3:9]) 

#stick control & treatment dataframe for SSP2.6
SSP2.6 <- rbind(cohorts_SSP2.6_2100_control,cohorts_SSP2.6_2100_HANPP)
unique(SSP2.6$Group)
#calculation of log10 values 
SSP2.6[3:9] <- log10(SSP2.6[3:9])

#stick control & treatment dataframe for SSP8.5
SSP8.5 <- rbind(cohorts_SSP8.5_2100_control,cohorts_SSP8.5_2100_HANPP)
unique(SSP8.5$Group)
#calculation of log10 values 
SSP8.5[3:9] <- log10(SSP8.5[3:9]) 

###-----------------------------------------------------------------###
###  2.       EFFECT SIZE CALCULATION // logRR                      ###
###-----------------------------------------------------------------###

###Historical natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_historical_control$Biomass_FG_0)
n2 <- length(cohorts_historical_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(historical$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(historical$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(historical$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(historical$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(historical$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(historical$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(historical$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_historical <- data.frame(c("Vaupés, Colombia"),
                                c("End. Herbivores","End. Carnivores","End. Omnivores",
                                  "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                                c(1,2,3,4,5,6,7),
                                c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                                c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                                c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                                c("Historical"),
                                c(sum(historical$Biomass_FG_0),sum(historical$Biomass_FG_1),sum(historical$Biomass_FG_2),sum(historical$Biomass_FG_3),sum(historical$Biomass_FG_4),sum(historical$Biomass_FG_5),sum(historical$BiomassSum)))

names(effect_historical) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_historical[4:6] <- (exp(effect_historical[4:6])-1)*100

###SSP1-2.6 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP2.6_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP2.6_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP2.6$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP2.6$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP2.6$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP2.6$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP2.6$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP2.6$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP2.6$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_SSP2.6 <- data.frame(c("Vaupés, Colombia"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP1-2.6"),
                            c(sum(SSP2.6$Biomass_FG_0),sum(SSP2.6$Biomass_FG_1),sum(SSP2.6$Biomass_FG_2),sum(SSP2.6$Biomass_FG_3),sum(SSP2.6$Biomass_FG_4),sum(SSP2.6$Biomass_FG_5),sum(SSP2.6$BiomassSum)))

names(effect_SSP2.6) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP2.6[4:6] <- (exp(effect_SSP2.6[4:6])-1)*100

###SSP5-8.5 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP8.5_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP8.5_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP8.5$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP8.5$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP8.5$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP8.5$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP8.5$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP8.5$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP8.5$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
effect_SSP8.5 <- data.frame(c("Vaupés, Colombia"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],FG2$t0[1],FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],FG2_CI$stud[4],FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],FG2_CI$stud[5],FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP5-8.5"),
                            c(sum(SSP8.5$Biomass_FG_0),sum(SSP8.5$Biomass_FG_1),sum(SSP8.5$Biomass_FG_2),sum(SSP8.5$Biomass_FG_3),sum(SSP8.5$Biomass_FG_4),sum(SSP8.5$Biomass_FG_5),sum(SSP8.5$BiomassSum)))

names(effect_SSP8.5) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP8.5[4:6] <- (exp(effect_SSP8.5[4:6])-1)*100 

###-----------------------------------------------------------------###
###  3.                  PLOTTING: FORESTPLOT                       ###
###-----------------------------------------------------------------###

#create factor lvls 
effect_historical$`Functional Group`<- factor(effect_historical$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP2.6$`Functional Group`<- factor(effect_SSP2.6$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP8.5$`Functional Group`<- factor(effect_SSP8.5$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))

#add overall biomass line

effect_historical$GrandMean <- ifelse(grepl("End.",effect_historical$`Functional Group`)|grepl("Ect.",effect_historical$`Functional Group`),FALSE,TRUE)
effect_SSP2.6$GrandMean <- ifelse(grepl("End.",effect_SSP2.6$`Functional Group`)|grepl("Ect.",effect_SSP2.6$`Functional Group`),FALSE,TRUE)
effect_SSP8.5$GrandMean <- ifelse(grepl("End.",effect_SSP8.5$`Functional Group`)|grepl("Ect.",effect_SSP8.5$`Functional Group`),FALSE,TRUE)

#now plot the forest plot 
Colombia <- ggplot() +
  geom_point(data=effect_historical,aes(x=Eff_size,y=`Functional Group`,col="Historical",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_historical,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="Historical"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP2.6,aes(x=Eff_size,y=`Functional Group`,col="SSP1-2.6",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP2.6,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP1-2.6"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP8.5,aes(x=Eff_size,y=`Functional Group`,col="SSP5-8.5",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP8.5,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP5-8.5"),width=0.1,size=0.3)+
  scale_color_brewer(palette = "Set1",direction=-1)+
  scale_size_continuous(guide="none")+
  scale_x_continuous(breaks = seq(-100,100,10),limits=c(-55,10))+
  scale_shape_manual(values = c("circle","diamond"),guide="none")+
  labs(title="Vaupés, Colombia", x='Effect Size (% Change)', y = "Functional Group (FG)",col="Climate Scenario",trans="reverse")+
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_classic()+
  theme(axis.text.y = element_text(face = c('bold','plain', 'plain', 'plain', 'plain', 'plain', 'plain'),size=12,hjust=0))+
  theme(axis.text.x = element_text(size=12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12,face="bold")) +
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))

Colombia

#create df for later csv output:
maxHANPP <- rbind(maxHANPP,effect_historical,effect_SSP2.6,effect_SSP8.5)

###-------###
###NAMIBIA###
###-------###
#load workspace images from outpath 
#list file can be read: 
#entry 1 = spinup, entry 2 = apply hanpp, 
#entry 3 - 12 = vegetation reduction stages, 
#last entry = 13 = post reduction run (with 0.1 vegetation left)
load(outpath %+% "Kavango_Namibia_historical_2014_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Kavango_Namibia_SSP126_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Kavango_Namibia_SSP585_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath

###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
#1. Create df for control + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_control <- historical_2014_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_control <- SSP2.6_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_control <- SSP8.5_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#2. Create df for HANPP scenarios + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_HANPP <- historical_2014_list[[7]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_HANPP <- SSP2.6_2100_list[[7]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_HANPP <- SSP8.5_2100_list[[7]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#3. Reorganize data: to create 1 df per climate scenario --> for logRR calculation 
#creates df with control and hanpp application observations (can be distinguished with group variable)
#stick control & treatment dataframe for historical
historical <- rbind(cohorts_historical_control,cohorts_historical_HANPP)
unique(historical$Group)
#calculation of log10 values
historical[3:9] <- log10(historical[3:9]) 

#stick control & treatment dataframe for SSP2.6
SSP2.6 <- rbind(cohorts_SSP2.6_2100_control,cohorts_SSP2.6_2100_HANPP)
unique(SSP2.6$Group)
#calculation of log10 values 
SSP2.6[3:9] <- log10(SSP2.6[3:9])

#stick control & treatment dataframe for SSP8.5
SSP8.5 <- rbind(cohorts_SSP8.5_2100_control,cohorts_SSP8.5_2100_HANPP)
unique(SSP8.5$Group)
#calculation of log10 values 
SSP8.5[3:9] <- log10(SSP8.5[3:9]) 

###-----------------------------------------------------------------###
###  2.       EFFECT SIZE CALCULATION // logRR                      ###
###-----------------------------------------------------------------###
###Historical natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_historical_control$Biomass_FG_0)
n2 <- length(cohorts_historical_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(historical$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(historical$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(historical$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(historical$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(historical$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(historical$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(historical$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot+
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)
effect_historical <- data.frame(c("Kavango, Namibia"),
                                c("End. Herbivores","End. Carnivores","End. Omnivores",
                                  "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                                c(1,2,3,4,5,6,7),
                                c(FG1$t0[1],NA,FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                                c(FG1_CI$stud[4],NA,FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                                c(FG1_CI$stud[5],NA,FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                                c("Historical"),
                                c(sum(historical$Biomass_FG_0),sum(historical$Biomass_FG_1),sum(historical$Biomass_FG_2),sum(historical$Biomass_FG_3),sum(historical$Biomass_FG_4),sum(historical$Biomass_FG_5),sum(historical$BiomassSum)))

names(effect_historical) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_historical[4:6] <- (exp(effect_historical[4:6])-1)*100

###SSP1-2.6 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP2.6_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP2.6_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP2.6$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP2.6$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP2.6$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP2.6$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP2.6$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP2.6$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP2.6$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)

effect_SSP2.6 <- data.frame(c("Kavango, Namibia"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],NA,FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],NA,FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],NA,FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP1-2.6"),
                            c(sum(SSP2.6$Biomass_FG_0),sum(SSP2.6$Biomass_FG_1),sum(SSP2.6$Biomass_FG_2),sum(SSP2.6$Biomass_FG_3),sum(SSP2.6$Biomass_FG_4),sum(SSP2.6$Biomass_FG_5),sum(SSP2.6$BiomassSum)))

names(effect_SSP2.6) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP2.6[4:6] <- (exp(effect_SSP2.6[4:6])-1)*100

###SSP5-8.5 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP8.5_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP8.5_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP8.5$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP8.5$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP8.5$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP8.5$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP8.5$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP8.5$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP8.5$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)
effect_SSP8.5 <- data.frame(c("Kavango, Namibia"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],NA,NA,FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],NA,NA,FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],NA,NA,FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP5-8.5"),
                            c(sum(SSP8.5$Biomass_FG_0),sum(SSP8.5$Biomass_FG_1),sum(SSP8.5$Biomass_FG_2),sum(SSP8.5$Biomass_FG_3),sum(SSP8.5$Biomass_FG_4),sum(SSP8.5$Biomass_FG_5),sum(SSP8.5$BiomassSum)))

names(effect_SSP8.5) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP8.5[4:6] <- (exp(effect_SSP8.5[4:6])-1)*100 

###-----------------------------------------------------------------###
###  3.                  PLOTTING: FORESTPLOT                       ###
###-----------------------------------------------------------------###

#create factor lvls 
effect_historical$`Functional Group`<- factor(effect_historical$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP2.6$`Functional Group`<- factor(effect_SSP2.6$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP8.5$`Functional Group`<- factor(effect_SSP8.5$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))

#add overall biomass line

effect_historical$GrandMean <- ifelse(grepl("End.",effect_historical$`Functional Group`)|grepl("Ect.",effect_historical$`Functional Group`),FALSE,TRUE)
effect_SSP2.6$GrandMean <- ifelse(grepl("End.",effect_SSP2.6$`Functional Group`)|grepl("Ect.",effect_SSP2.6$`Functional Group`),FALSE,TRUE)
effect_SSP8.5$GrandMean <- ifelse(grepl("End.",effect_SSP8.5$`Functional Group`)|grepl("Ect.",effect_SSP8.5$`Functional Group`),FALSE,TRUE)

#now plot the forest plot 
Namibia <- ggplot() +
  geom_point(data=effect_historical,aes(x=Eff_size,y=`Functional Group`,col="Historical",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_historical,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="Historical"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP2.6,aes(x=Eff_size,y=`Functional Group`,col="SSP1-2.6",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP2.6,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP1-2.6"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP8.5,aes(x=Eff_size,y=`Functional Group`,col="SSP5-8.5",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP8.5,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP5-8.5"),width=0.1,size=0.3)+
  scale_color_brewer(palette = "Set1",direction=-1)+
  scale_size_continuous(guide="none")+
  scale_x_continuous(breaks = seq(-100,100,10),limits=c(-55,10))+
  scale_shape_manual(values = c("circle","diamond"),guide="none")+
  labs(title="Kavango, Namibia", x='Effect Size (% Change)', y = "Functional Group (FG)",col="Climate Scenario",trans="reverse")+
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_classic()+
  theme(axis.text.y = element_text(face = c('bold','plain', 'plain', 'plain', 'plain', 'plain', 'plain'),size=12,hjust=0))+
  theme(axis.text.x = element_text(size=12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12,face="bold")) +
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))

Namibia

#create df for later csv output:
maxHANPP <- rbind(maxHANPP,effect_historical,effect_SSP2.6,effect_SSP8.5)

###------###
###FRANCE###
###------###
#load workspace images from outpath 
#list file can be read: 
#entry 1 = spinup, entry 2 = apply hanpp, 
#entry 3 - 12 = vegetation reduction stages, 
#last entry = 13 = post reduction run (with 0.1 vegetation left)
load(outpath %+% "Central_France_historical_2014_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Central_France_SSP126_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Central_France_SSP585_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath

###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
#1. Create df for control + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_control <- historical_2014_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_control <- SSP2.6_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_control <- SSP8.5_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#2. Create df for HANPP scenarios + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_HANPP <- historical_2014_list[[7]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_HANPP <- SSP2.6_2100_list[[7]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_HANPP <- SSP8.5_2100_list[[7]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#3. Reorganize data: to create 1 df per climate scenario --> for logRR calculation 
#creates df with control and hanpp application observations (can be distinguished with group variable)
#stick control & treatment dataframe for historical
historical <- rbind(cohorts_historical_control,cohorts_historical_HANPP)
unique(historical$Group)
#calculation of log10 values
historical[3:9] <- log10(historical[3:9]) 

#stick control & treatment dataframe for SSP2.6
SSP2.6 <- rbind(cohorts_SSP2.6_2100_control,cohorts_SSP2.6_2100_HANPP)
unique(SSP2.6$Group)
#calculation of log10 values 
SSP2.6[3:9] <- log10(SSP2.6[3:9])

#stick control & treatment dataframe for SSP8.5
SSP8.5 <- rbind(cohorts_SSP8.5_2100_control,cohorts_SSP8.5_2100_HANPP)
unique(SSP8.5$Group)
#calculation of log10 values 
SSP8.5[3:9] <- log10(SSP8.5[3:9]) 

###-----------------------------------------------------------------###
###  2.       EFFECT SIZE CALCULATION // logRR                      ###
###-----------------------------------------------------------------###

###Historical natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_historical_control$Biomass_FG_0)
n2 <- length(cohorts_historical_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(historical$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(historical$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(historical$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(historical$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(historical$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(historical$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(historical$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)
effect_historical <- data.frame(c("Central France"),
                                c("End. Herbivores","End. Carnivores","End. Omnivores",
                                  "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                                c(1,2,3,4,5,6,7),
                                c(FG1$t0[1],NA,FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                                c(FG1_CI$stud[4],NA,FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                                c(FG1_CI$stud[5],NA,FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                                c("Historical"),
                                c(sum(historical$Biomass_FG_0),sum(historical$Biomass_FG_1),sum(historical$Biomass_FG_2),sum(historical$Biomass_FG_3),sum(historical$Biomass_FG_4),sum(historical$Biomass_FG_5),sum(historical$BiomassSum)))

names(effect_historical) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_historical[4:6] <- (exp(effect_historical[4:6])-1)*100

###SSP1-2.6 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP2.6_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP2.6_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP2.6$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP2.6$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP2.6$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP2.6$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP2.6$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP2.6$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP2.6$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)
effect_SSP2.6 <- data.frame(c("Central France"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],NA,FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],NA,FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],NA,FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP1-2.6"),
                            c(sum(SSP2.6$Biomass_FG_0),sum(SSP2.6$Biomass_FG_1),sum(SSP2.6$Biomass_FG_2),sum(SSP2.6$Biomass_FG_3),sum(SSP2.6$Biomass_FG_4),sum(SSP2.6$Biomass_FG_5),sum(SSP2.6$BiomassSum)))

names(effect_SSP2.6) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP2.6[4:6] <- (exp(effect_SSP2.6[4:6])-1)*100

###SSP5-8.5 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP8.5_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP8.5_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP8.5$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP8.5$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP8.5$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP8.5$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP8.5$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP8.5$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP8.5$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)
effect_SSP8.5 <- data.frame(c("Central France"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],NA,FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],NA,FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],NA,FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP5-8.5"),
                            c(sum(SSP8.5$Biomass_FG_0),sum(SSP8.5$Biomass_FG_1),sum(SSP8.5$Biomass_FG_2),sum(SSP8.5$Biomass_FG_3),sum(SSP8.5$Biomass_FG_4),sum(SSP8.5$Biomass_FG_5),sum(SSP8.5$BiomassSum)))

names(effect_SSP8.5) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP8.5[4:6] <- (exp(effect_SSP8.5[4:6])-1)*100 

###-----------------------------------------------------------------###
###  3.                  PLOTTING: FORESTPLOT                       ###
###-----------------------------------------------------------------###

#create factor lvls 
effect_historical$`Functional Group`<- factor(effect_historical$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP2.6$`Functional Group`<- factor(effect_SSP2.6$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP8.5$`Functional Group`<- factor(effect_SSP8.5$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))

#add overall biomass line

effect_historical$GrandMean <- ifelse(grepl("End.",effect_historical$`Functional Group`)|grepl("Ect.",effect_historical$`Functional Group`),FALSE,TRUE)
effect_SSP2.6$GrandMean <- ifelse(grepl("End.",effect_SSP2.6$`Functional Group`)|grepl("Ect.",effect_SSP2.6$`Functional Group`),FALSE,TRUE)
effect_SSP8.5$GrandMean <- ifelse(grepl("End.",effect_SSP8.5$`Functional Group`)|grepl("Ect.",effect_SSP8.5$`Functional Group`),FALSE,TRUE)

#now plot the forest plot 
France <- ggplot() +
  geom_point(data=effect_historical,aes(x=Eff_size,y=`Functional Group`,col="Historical",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_historical,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="Historical"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP2.6,aes(x=Eff_size,y=`Functional Group`,col="SSP1-2.6",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP2.6,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP1-2.6"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP8.5,aes(x=Eff_size,y=`Functional Group`,col="SSP5-8.5",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP8.5,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP5-8.5"),width=0.1,size=0.3)+
  scale_color_brewer(palette = "Set1",direction=-1)+
  scale_size_continuous(guide="none")+
  scale_x_continuous(breaks = seq(-100,100,10),limits=c(-55,10))+
  scale_shape_manual(values = c("circle","diamond"),guide="none")+
  labs(title="Central France", x='Effect Size (% Change)', y = "Functional Group (FG)",col="Climate Scenario",trans="reverse")+
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_classic()+
  theme(axis.text.y = element_text(face = c('bold','plain', 'plain', 'plain', 'plain', 'plain', 'plain'),size=12,hjust=0))+
  theme(axis.text.x = element_text(size=12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12,face="bold")) +
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))

France

#create df for later csv output:
maxHANPP <- rbind(maxHANPP,effect_historical,effect_SSP2.6,effect_SSP8.5)

###-------###
###FINLAND###
###-------###
#load workspace images from outpath 
#list file can be read: 
#entry 1 = spinup, entry 2 = apply hanpp, 
#entry 3 - 12 = vegetation reduction stages, 
#last entry = 13 = post reduction run (with 0.1 vegetation left)
load(outpath %+% "Central_Finland_historical_2014_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Central_Finland_SSP126_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath
load(outpath %+% "Central_Finland_SSP585_2100_vegreduced_0.5degree.Rdata")
outpath <- myoutpath
figpath <- myfigpath

###-----------------------------------------------------------------###
###  1.                      DATA PREPARATION                       ###
###-----------------------------------------------------------------###
#1. Create df for control + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_control <- historical_2014_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_control <- SSP2.6_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_control <- SSP8.5_2100_list[[2]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "control",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#2. Create df for HANPP scenarios + sum up monthly biomass of each scenario (overall effect), filter = > Month 1080, last 10 years
#historical
cohorts_historical_HANPP <- historical_2014_list[[4]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="Historical",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()  

#SSP2.6_2100
cohorts_SSP2.6_2100_HANPP <- SSP2.6_2100_list[[4]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP1-2.6",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()
#SSP8.5_2100
cohorts_SSP8.5_2100_HANPP <- SSP8.5_2100_list[[4]]$time_line_cohorts %>% filter(Month > 2280) %>% rowwise() %>% 
  dplyr::mutate(BiomassSum = sum(c(Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,Biomass_FG_6,Biomass_FG_7,Biomass_FG_8)),
                Scenario="SSP5-8.5",Group = "HANPP",
                Biomass_FG_3 = sum(c(Biomass_FG_3,Biomass_FG_6)),Biomass_FG_4 = sum(c(Biomass_FG_4,Biomass_FG_7)),Biomass_FG_5 = sum(c(Biomass_FG_5, Biomass_FG_8))) %>%
  summarize(Month,Year,Biomass_FG_0,Biomass_FG_1,Biomass_FG_2,Biomass_FG_3,Biomass_FG_4,Biomass_FG_5,BiomassSum,Scenario,Group) %>%
  ungroup() %>%   as.data.frame()

#3. Reorganize data: to create 1 df per climate scenario --> for logRR calculation 
#creates df with control and hanpp application observations (can be distinguished with group variable)
#stick control & treatment dataframe for historical
historical <- rbind(cohorts_historical_control,cohorts_historical_HANPP)
unique(historical$Group)
#calculation of log10 values
historical[3:9] <- log10(historical[3:9]) 

#stick control & treatment dataframe for SSP2.6
SSP2.6 <- rbind(cohorts_SSP2.6_2100_control,cohorts_SSP2.6_2100_HANPP)
unique(SSP2.6$Group)
#calculation of log10 values 
SSP2.6[3:9] <- log10(SSP2.6[3:9])

#stick control & treatment dataframe for SSP8.5
SSP8.5 <- rbind(cohorts_SSP8.5_2100_control,cohorts_SSP8.5_2100_HANPP)
unique(SSP8.5$Group)
#calculation of log10 values 
SSP8.5[3:9] <- log10(SSP8.5[3:9]) 

###-----------------------------------------------------------------###
###  2.       EFFECT SIZE CALCULATION // logRR                      ###
###-----------------------------------------------------------------###

###Historical natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_historical_control$Biomass_FG_0)
n2 <- length(cohorts_historical_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(historical$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(historical$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(historical$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(historical$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(historical$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(historical$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(historical$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)
effect_historical <- data.frame(c("Central Finland"),
                                c("End. Herbivores","End. Carnivores","End. Omnivores",
                                  "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                                c(1,2,3,4,5,6,7),
                                c(FG1$t0[1],NA,FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                                c(FG1_CI$stud[4],NA,FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                                c(FG1_CI$stud[5],NA,FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                                c("Historical"),
                                c(sum(historical$Biomass_FG_0),sum(historical$Biomass_FG_1),sum(historical$Biomass_FG_2),sum(historical$Biomass_FG_3),sum(historical$Biomass_FG_4),sum(historical$Biomass_FG_5),sum(historical$BiomassSum)))

names(effect_historical) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_historical[4:6] <- (exp(effect_historical[4:6])-1)*100

###SSP1-2.6 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP2.6_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP2.6_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP2.6$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP2.6$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP2.6$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP2.6$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP2.6$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP2.6$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP2.6$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)
effect_SSP2.6 <- data.frame(c("Central Finland"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],NA,FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],NA,FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],NA,FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP1-2.6"),
                            c(sum(SSP2.6$Biomass_FG_0),sum(SSP2.6$Biomass_FG_1),sum(SSP2.6$Biomass_FG_2),sum(SSP2.6$Biomass_FG_3),sum(SSP2.6$Biomass_FG_4),sum(SSP2.6$Biomass_FG_5),sum(SSP2.6$BiomassSum)))

names(effect_SSP2.6) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP2.6[4:6] <- (exp(effect_SSP2.6[4:6])-1)*100

###SSP5-8.5 natural ecosystem state vs. HANPP application###
#define length for bootstrapping samples (used for strata)
n1 <- length(cohorts_SSP8.5_2100_control$Biomass_FG_0)
n2 <- length(cohorts_SSP8.5_2100_HANPP$Biomass_FG_0)

#calculate ordinary bootstrap replicates with replacement because: 
FG1 <- boot(SSP8.5$Biomass_FG_0,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG2 <- boot(SSP8.5$Biomass_FG_1,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG3 <- boot(SSP8.5$Biomass_FG_2,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG4 <- boot(SSP8.5$Biomass_FG_3,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG5 <- boot(SSP8.5$Biomass_FG_4,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
FG6 <- boot(SSP8.5$Biomass_FG_5,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")
Overall <- boot(SSP8.5$BiomassSum,logRR, R=10000, strata= rep(1:2,c(n1,n2)),sim="ordinary")

#calculate stud bootstrap cis (bias corrected, accelerated confidence interval), because: 
FG1_CI <- boot.ci(FG1,conf=0.95,type = "stud")
FG2_CI <- boot.ci(FG2,conf=0.95,type = "stud")
FG3_CI <- boot.ci(FG3,conf=0.95,type = "stud")
FG4_CI <- boot.ci(FG4,conf=0.95,type = "stud")
FG5_CI <- boot.ci(FG5,conf=0.95,type = "stud")
FG6_CI <- boot.ci(FG6,conf=0.95,type = "stud")
Overall_CI <- boot.ci(Overall,conf=0.95,type = "stud")

#create a effect size data frame = base for forest plot
#inserted NAs, were FGs got instinct (because if not, values from previous region stored in environment will be used)
effect_SSP8.5 <- data.frame(c("Central Finland"),
                            c("End. Herbivores","End. Carnivores","End. Omnivores",
                              "Ect. Herbivores","Ect. Carnivores","Ect. Omnivores","Overall Biomass"),
                            c(1,2,3,4,5,6,7),
                            c(FG1$t0[1],NA,FG3$t0[1],FG4$t0[1],FG5$t0[1],FG6$t0[1],Overall$t0[1]),
                            c(FG1_CI$stud[4],NA,FG3_CI$stud[4],FG4_CI$stud[4],FG5_CI$stud[4],FG6_CI$stud[4],Overall_CI$stud[4]),
                            c(FG1_CI$stud[5],NA,FG3_CI$stud[5],FG4_CI$stud[5],FG5_CI$stud[5],FG6_CI$stud[5],Overall_CI$stud[5]),
                            c("SSP5-8.5"),
                            c(sum(SSP8.5$Biomass_FG_0),sum(SSP8.5$Biomass_FG_1),sum(SSP8.5$Biomass_FG_2),sum(SSP8.5$Biomass_FG_3),sum(SSP8.5$Biomass_FG_4),sum(SSP8.5$Biomass_FG_5),sum(SSP8.5$BiomassSum)))

names(effect_SSP8.5) = c("Region","Functional Group","Index","Eff_size","lower","upper","Scenario","Biomass")

#exponentiate / convert to percentage change
effect_SSP8.5[4:6] <- (exp(effect_SSP8.5[4:6])-1)*100 

###-----------------------------------------------------------------###
###  3.                  PLOTTING: FORESTPLOT                       ###
###-----------------------------------------------------------------###

#create factor lvls 
effect_historical$`Functional Group`<- factor(effect_historical$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP2.6$`Functional Group`<- factor(effect_SSP2.6$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))
effect_SSP8.5$`Functional Group`<- factor(effect_SSP8.5$`Functional Group`,levels=c("Overall Biomass","End. Herbivores","End. Carnivores","End. Omnivores","Ect. Herbivores","Ect. Carnivores","Ect. Omnivores"))

#add overall biomass line

effect_historical$GrandMean <- ifelse(grepl("End.",effect_historical$`Functional Group`)|grepl("Ect.",effect_historical$`Functional Group`),FALSE,TRUE)
effect_SSP2.6$GrandMean <- ifelse(grepl("End.",effect_SSP2.6$`Functional Group`)|grepl("Ect.",effect_SSP2.6$`Functional Group`),FALSE,TRUE)
effect_SSP8.5$GrandMean <- ifelse(grepl("End.",effect_SSP8.5$`Functional Group`)|grepl("Ect.",effect_SSP8.5$`Functional Group`),FALSE,TRUE)

#now plot the forest plot 
Finland <- ggplot() +
  geom_point(data=effect_historical,aes(x=Eff_size,y=`Functional Group`,col="Historical",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_historical,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="Historical"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP2.6,aes(x=Eff_size,y=`Functional Group`,col="SSP1-2.6",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP2.6,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP1-2.6"),width=0.1,size=0.3)+
  geom_point(data=effect_SSP8.5,aes(x=Eff_size,y=`Functional Group`,col="SSP5-8.5",size=ifelse(grepl("res",`Functional Group`),Biomass,2600),shape=GrandMean))+
  geom_errorbar(data=effect_SSP8.5,aes(xmin=lower,xmax=upper,y=`Functional Group`,col="SSP5-8.5"),width=0.1,size=0.3)+
  scale_color_brewer(palette = "Set1",direction=-1)+
  scale_size_continuous(guide="none")+
  scale_x_continuous(breaks = seq(-100,100,10),limits=c(-55,10))+
  scale_shape_manual(values = c("circle","diamond"),guide="none")+
  labs(title="Central Finland", x='Effect Size (% Change)', y = "Functional Group (FG)",col="Climate Scenario",trans="reverse")+
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_classic()+
  theme(axis.text.y = element_text(face = c('bold','plain', 'plain', 'plain', 'plain', 'plain', 'plain'),size=12,hjust=0))+
  theme(axis.text.x = element_text(size=12))+
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12,face="bold")) +
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))

Finland

#create df for later csv output:
maxHANPP <- rbind(maxHANPP,effect_historical,effect_SSP2.6,effect_SSP8.5)

#subset df to relevant information for csv output:
maxHANPP <- maxHANPP %>% 
  summarize(Region, Scenario,`Functional Group`,Eff_size,lower,upper)
#new column names
names(maxHANPP)=c("Region","Scenario","Functional Group","Effect Size","Lower CI","Upper CI")
#export df as csv 
write.csv(maxHANPP,regionpath %+% "maxHANPP_effect.csv")

###-----------------------------------------------------------------###
###         ARRANGE ALL PLOTS TO DINA4 AND SAVE AS PDF              ###
###-----------------------------------------------------------------###
#Makes a big plot of all 9 ggplot objects (FG's) + delete redundant information 
a <- ggarrange(Europe+rremove("ylab")+rremove("xlab"),India+rremove("y.text")+rremove("ylab")+rremove("xlab"),Colombia+rremove("y.text")+rremove("xlab")+rremove("ylab"),Namibia+rremove("ylab")+rremove("xlab"),France+rremove("y.text")+rremove("ylab")+rremove("xlab"),Finland+rremove("y.text")+rremove("ylab")+rremove("xlab"), 
               ncol = 3, nrow = 2,
               widths = c(1,0.7,0.7),
               labels = c("A", "B", "C","D","E","F"),
               legend = "top",
               common.legend = T,
               align = "h")#dev.off()

#add title + x and y axis captions 
annotate_figure(a, top = text_grob("Effect of Maximum Land-Use Intensity", 
                                   face = "bold", size = 14),
                bottom = text_grob("Effect Size (% Change)",hjust=0.2,face="bold",size=14),
                left = text_grob("Functional Group (FG)",rot=90,hjust=0.5,face="bold",size=14))

#save pdf file in dina4
ggsave(regionpath %+% "change_maxHANPP.pdf",width=297,height=210,units="mm")

###-----------------------------------------------------------------###
###      CREATE OVERALL BIOMASS FORESTPLOT OVERVIEW FOR REGIONS     ###
###-----------------------------------------------------------------###
#Historical
regions_hist <- maxHANPP %>%
  subset(`Functional Group`=="Overall Biomass"& Scenario == "Historical") 

names(regions_hist) = c("Region","Scenario","FG", "Eff_size","lower","upper")

#SSP1-2.6
regions_SSP126 <- maxHANPP %>%
  subset(`Functional Group`=="Overall Biomass"& Scenario == "SSP1-2.6") 

names(regions_SSP126) = c("Region","Scenario","FG", "Eff_size","lower","upper")

#SSP5-8.5
regions_SSP585 <- maxHANPP %>%
  subset(`Functional Group`=="Overall Biomass"& Scenario == "SSP5-8.5") 

names(regions_SSP585) = c("Region","Scenario","FG", "Eff_size","lower","upper")

#now plot the forest plot 
Regions_maxHANPP <- ggplot() +
  geom_point(data=regions_hist,aes(x=Eff_size,y=Region,col="Historical"),size=3)+
  geom_errorbar(data=regions_hist,aes(xmin=lower,xmax=upper,y=Region,col="Historical"),width=0.1,size=0.3)+
  geom_point(data=regions_SSP126,aes(x=Eff_size,y=Region,col="SSP1-2.6"),size=3)+
  geom_errorbar(data=regions_SSP126,aes(xmin=lower,xmax=upper,y=Region,col="SSP1-2.6"),width=0.1,size=0.3)+
  geom_point(data=regions_SSP585,aes(x=Eff_size,y=Region,col="SSP5-8.5"),size=3)+
  geom_errorbar(data=regions_SSP585,aes(xmin=lower,xmax=upper,y=Region,col="SSP5-8.5"),width=0.1,size=0.3)+
  scale_color_brewer(palette = "Set1",direction=-1)+
  scale_size_continuous(guide="none")+
  scale_x_continuous(breaks = seq(-100,100,2),limits=c(-8,2))+
  scale_shape_manual(values = c("circle","diamond"),guide="none")+
  labs(title="Maximum Land−Use Intensity", x='Effect Size (% Change)', y = "Regions",col="Climate Scenario",trans="reverse")+
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  theme_classic()+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12,hjust=0))+
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=12,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=12,face="bold")) +
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12,face="bold"))

Regions_maxHANPP

#save image to get regions forestplot for later comparison with other two scenarios
save.image(outpath %+% "Forestplot_maxHANPP_Image.Rdata")
