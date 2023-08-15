##load the necessary libraries

library(readxl)
library(lme4)
library(reshape2)
library(ggplot2)
library(lmerTest)
library(tidyverse)
library(emmeans)
library(gtools)
library(cowplot)
library(readr)
library(dplyr)
library(GLMMadaptive)
library(effects)
library(car)
library(kableExtra)
library("agricolae")
library("ggsignif")
library("RColorBrewer")
library(ggpubr)
library (broom.mixed)




## Read in individual files to one big dataset starting with sub 2 (excluding subs 1, 5, 10, 13, 16, 25, 33, 40, 54 and 57)
a <- 2

Data <- read_excel(paste("C:/Users/idaho/Desktop/Master Thesis/Brent Data/PrcsdData_S", toString(a), ".xlsx", sep = ""), 
                   col_types = c("text", "numeric", "text", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "numeric", "numeric", "text", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "text", "text", "numeric", 
                                 "numeric", "numeric", "text", "text"))

## Add this subjects tonic and phasis correlation
AllCorrelations <- c(cor(Data$Tonic_E, Data$Phasic_E, method = 'pearson'))

# Calculate n_observations per condition
HS <- nrow(Data[Data$Patch_Type == 'Small' & Data$Env_Type == 'High',])
HM <- nrow(Data[Data$Patch_Type == 'Medium' & Data$Env_Type == 'High',])
HL <- nrow(Data[Data$Patch_Type == 'Large' & Data$Env_Type == 'High',])

LS <- nrow(Data[Data$Patch_Type == 'Small' & Data$Env_Type == 'Low',])
LM <- nrow(Data[Data$Patch_Type == 'Medium' & Data$Env_Type == 'Low',])
LL <- nrow(Data[Data$Patch_Type == 'Large' & Data$Env_Type == 'Low',])

ObservationCounts = c(LS, LM, LL, HS, HM, HL)

# Raise alarm is one count is below 15
if (any(ObservationCounts < 15)){
  print(paste('### Subject ', toString(a), ' has alarming observation counts! ###', sep = ""))
}

# Do this for the remaining subjects
for (a in c(3,4,6,5,7,8,9,11,12,14,15,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,58)){
  ThisFile <- read_excel(paste("C:/Users/idaho/Desktop/Master Thesis/Brent Data/PrcsdData_S", toString(a), ".xlsx", sep = ""), 
                         col_types = c("text", "numeric", "text", 
                                       "text", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "text", "text", 
                                       "numeric", "numeric", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "text", "text", "numeric", 
                                       "numeric", "numeric", "text", "text"))
  # Delete rows that need to be excluded
  #ThisFile <- ThisFile[ThisFile$Exclude != 1,]
  
  ThisFile$Tonic_E <- as.numeric(ThisFile$Tonic_E)
  ThisFile$Phasic_E <- as.numeric(ThisFile$Phasic_E)
  
  AllCorrelations <- rbind(AllCorrelations, cor(ThisFile$Tonic_E, ThisFile$Phasic_E))
  
  # Calculate n_observations per condition
  HS <- nrow(ThisFile[ThisFile$Patch_Type == 'Small' & ThisFile$Env_Type == 'High',])
  HM <- nrow(ThisFile[ThisFile$Patch_Type == 'Medium' & ThisFile$Env_Type == 'High',])
  HL <- nrow(ThisFile[ThisFile$Patch_Type == 'Large' & ThisFile$Env_Type == 'High',])
  
  LS <- nrow(ThisFile[ThisFile$Patch_Type == 'Small' & ThisFile$Env_Type == 'Low',])
  LM <- nrow(ThisFile[ThisFile$Patch_Type == 'Medium' & ThisFile$Env_Type == 'Low',])
  LL <- nrow(ThisFile[ThisFile$Patch_Type == 'Large' & ThisFile$Env_Type == 'Low',])
  
  ObservationCounts = c(LS, LM, LL, HS, HM, HL)
  
  if (any(ObservationCounts < 15))
    print(paste('### Subject ', toString(a), ' has alarming observation counts! ###', sep = ""))
  
  Data <- rbind(Data, ThisFile)
  
setwd("C:/Users/idaho/Desktop/Master Thesis")
#Read Optimum files and add to behavioral files

OptimumData <- data.frame()

for (a in c(2,3,4,6,5,7,8,9,11,12,14,15,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,58)){
  OptimumFile <- read_delim(paste("Optimum/Optimum_S",toString(a),".csv", sep=""),
                            delim = ",", col_types = cols(.default = "n"),
                            col_names =c("Low_Small","Low_Medium","Low_Large",
                                         "High_Small","High_Medium","High_Large","Berries"))
  OptimumFile$Participant <-a
  OptimumData <- rbind(OptimumData, OptimumFile)
  ThisData <- Data[Data$Participant == a, ]
  ThisData$Optimum <- ifelse(ThisData$Env_Type == "Low" & ThisData$Patch_Type == "Small", OptimumFile$Low_Small,
                             ifelse(ThisData$Env_Type == "Low" & ThisData$Patch_Type == "Medium", OptimumFile$Low_Medium,
                                    ifelse(ThisData$Env_Type == "Low" & ThisData$Patch_Type == "Large", OptimumFile$Low_Large,
                                           ifelse(ThisData$Env_Type == "High" & ThisData$Patch_Type == "Small", OptimumFile$High_Small,
                                                  ifelse(ThisData$Env_Type == "High" & ThisData$Patch_Type == "Medium", OptimumFile$High_Medium,
                                                         OptimumFile$High_Large
                                                  )))))
  Data[Data$Participant == a, "Optimum"] <- ThisData$Optimum
}


#Read pupl preprocessed file 

dir_path <- setwd("C:/Users/idaho/Desktop/Master Thesis/Pupil Data/Preprocessed_all")
csv_files <- list.files(dir_path, pattern = ".csv", full.names = TRUE)
csv_files_sorted <- mixedsort(csv_files)
csv_list <- lapply(csv_files_sorted, read.csv)
combined_df <- do.call(rbind, csv_list)


#Select the column to average from -0.5 to 0 to extract the tonic pupil size
start_col <- 9  
end_col <- 509  


# Calculate the average of the selected columns
df_avg <- combined_df[, start_col:end_col] %>% rowMeans(na.rm = TRUE)

combined_df$avg<- combined_df[,start_col:end_col] %>% rowMeans(na.rm=T)

Data$avg<- combined_df[start_col:end_col] %>% rowMeans(na.rm=T)
#Save Data file as a csv.
write.csv(Data, file = "Data_all.csv", row.names = TRUE)


#Delete the last trials from Blocks 3 & 4 and create a loop for our needed variables: averaged tonic pupil across the patches and tonic pupil of the first trial per patch 

OverharvestData<- data.frame()
for (a in c(2,3,4,6,5,7,8,9,11,12,14,15,17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,58)){
  ThisData <- Data[Data$Participant==a,]
  MaxPatch3 <- max(ThisData$G_Patch[ThisData$Block == 3])
  MaxPatch4 <- max(ThisData$G_Patch[ThisData$Block == 4])
  ThisData <- filter(ThisData, G_Patch != MaxPatch3)
  ThisData <- filter(ThisData, G_Patch != MaxPatch4)
  ThisDataOverharvest <-ThisData[ThisData$Leave==1,]
  ThisDataFirstTrial <-ThisData[ThisData$Residence==1,]
  ThisDataLeave <-ThisData[ThisData$Leave==1,]
  ThisDataLeave$First_Trial_Avg<-ThisDataFirstTrial$avg
  beg <- ThisDataFirstTrial$G_Trial
  end<-ThisDataOverharvest$G_Trial
  
  #ThisData$Patch_Avg<-(ThisData[ThisData$G_trial])
  Patch_Avgs<-c()
  for (a in c(1:length(beg))){
    mean_index = c(which(ThisData$G_Trial==beg[a]): which(ThisData$G_Trial==end[a]))
    Patch_Avg = mean(ThisData[mean_index,]$avg,na.rm=T)
    Patch_Avgs = c(Patch_Avgs, Patch_Avg)
  }
  ThisDataOverharvest$Patch_Avg <-Patch_Avgs
  ThisDataOverharvest$First_Trial_Avg <- ThisDataFirstTrial$avg
  OverharvestData<-rbind(OverharvestData,ThisDataOverharvest)
}

# Create an overharvesting variable
OverharvestData$Overharvest <- OverharvestData$Residence - OverharvestData$Optimum


#Set the variables as factors
OverharvestData$Patch_Type <- as.factor(OverharvestData$Patch_Type)
OverharvestData$Env_Type <- as.factor(OverharvestData$Env_Type)
OverharvestData$Lab <- as.factor(OverharvestData$Lab)
OverharvestData$Residence <- as.numeric(OverharvestData$Residence)

#z-scoring data
Data$tonic_pupl <-(Data$avg- mean(Data$avg,na.rm=T))/sd(Data$avg,na.rm=T) 
OverharvestData$tonic_pupil_avg <- (OverharvestData$avg - mean(OverharvestData$avg, na.rm = T)) / OverharvestData$avg
OverharvestData$tonic_patch_avg <- (OverharvestData$Patch_Avg - mean(OverharvestData$Patch_Avg,na.rm=T))/sd(OverharvestData$Patch_Avg,na.rm=T) 
OverharvestData$tonic_firsttrial_avg <- (OverharvestData$First_Trial_Avg - mean(OverharvestData$First_Trial_Avg,na.rm=T))/sd(OverharvestData$First_Trial_Avg,na.rm=T)

#Behavioral Data Analysis (Residence Time & Overharvestign Calculation)

model_residence <- lmer(Residence ~ Env_Type + Patch_Type + Lab + Env_Type:Patch_Type + (1|Participant), OverharvestData)
model1_overharvest <- lmer(Overharvest ~ Patch_Type*Env_Type + (1|Lab/Participant), OverharvestData)
model_env_variability <- lmer(Patch_Avg ~ Env_Type + (1|Lab/Participant), OverharvestData)
model2_env_variability <- lmer(First_Trial_Avg ~ Env_Type + (1|Lab/Participant), OverharvestData)

#Contrast analysis

m.emm1 <- emmeans(model_residence, pairwise ~ Patch_Type:Env_Type,lmer.df = "satterthwaite", lmerTest.limit = 3023)
m.emm2 <- emmeans(model1_overharvest, pairwise ~ Patch_Type:Env_Type,lmer.df = "satterthwaite", lmerTest.limit = 3023)
m.emm3 <- emmeans(model_env_variability,pairwise ~ Env_Type, lmer.df = "satterthwaite", lmerTest.limit = 3023)
m.emm4 <- emmeans(model2_env_variability,pairwise ~ Env_Type, lmer.df = "satterthwaite", lmerTest.limit = 3023)

###Data Analysis with the z-scored data (Pupil Data)

#Models including averaged tonic pupil across the patches
model1_tonic_pupil <- lmer(Overharvest ~ tonic_patch_avg + (1|Lab/Participant), OverharvestData)
model2_tonic_fullmodel <-  lmer(Overharvest ~ tonic_patch_avg*Patch_Type*Env_Type + (1|Participant), OverharvestData)

#Models including tonic pupil calcualted as average of the first trials per patch 
model2.1_tonic_pupil <- lmer(Overharvest ~ tonic_firsttrial_avg + (1|Participant), OverharvestData)
model3.1_tonic_fullmodel <-  lmer(Overharvest ~ tonic_firsttrial_avg *Patch_Type*Env_Type + (1|Lab/Participant), OverharvestData)

#Contrast analysis

m.emm5 <- emmeans(model1_tonic_pupil, pairwise ~ tonic_patch_avg:Env_Type,lmer.df = "satterthwaite", lmerTest.limit = 3023)
m.emm6 <- emmeans(model2_tonic_fullmodel, pairwise ~ tonic_patch_avg:Patch_Type,lmer.df = "satterthwaite", lmerTest.limit = 3023)
m.emm7 <- emmeans(model2.1_tonic_pupil, pairwise ~ tonic_patch_avg:Env_Type,lmer.df = "satterthwaite", lmerTest.limit = 3023)
m.emm8 <- emmeans(model3.1_tonic_fullmodel, pairwise ~ tonic_patch_avg:Patch_Type,lmer.df = "satterthwaite", lmerTest.limit = 3023)


# Calculate the means and standard deviations for each combination of Env_Type and Patch_Type for residence time, averaged over all participants

means_data <- aggPatchEnv %>%
  group_by(Env_Type, Patch_Type) %>%
  summarise(mean_Residence = mean(Residence, na.rm = TRUE))
sds_data <- aggPatchEnv %>%
  group_by(Env_Type, Patch_Type) %>%
  summarise(sd_Residence = sd(Residence, na.rm = TRUE))
means_data
sds_data

means_data <- aggPatchEnv %>%
  group_by(Patch_Type) %>%
  summarise(mean_Residence = mean(Residence, na.rm = TRUE))
sds_data <- aggPatchEnv %>%
  group_by(Patch_Type) %>%
  summarise(sd_Residence = sd(Residence, na.rm = TRUE))
means_data
sds_data

means_data <- aggPatchEnv %>%
  group_by(Env_Type) %>%
  summarise(mean_Residence = mean(Residence, na.rm = TRUE))
sds_data <- aggPatchEnv %>%
  group_by(Env_Type) %>%
  summarise(sd_Residence = sd(Residence, na.rm = TRUE))
means_data
sds_data


# Mean and SD for Overharvesting per Patch and Environment

means_data <- aggPatchEnv %>%
  group_by(Env_Type, Patch_Type) %>%
  summarise(mean_Overharvest = mean(Overharvest, na.rm = TRUE))

sds_data <- aggPatchEnv %>%
  group_by(Env_Type, Patch_Type) %>%
  summarise(sd_Overharvest = sd(Overharvest, na.rm = TRUE))

print(means_data)
print(sds_data)

means_data <- aggPatchEnv %>%
  group_by(Patch_Type) %>%
  summarise(mean_Overharvest = mean(Overharvest, na.rm = TRUE))

sds_data <- aggPatchEnv %>%
  group_by(Patch_Type) %>%
  summarise(sd_Overharvest = sd(Overharvest, na.rm = TRUE))

print(means_data)
print(sds_data)


#Mean and SD for Tonic Pupil in Each Environment

mean_sd_env <- OverharvestData %>%
  group_by(Env_Type) %>%
  summarize(mean_tonic_patch_avg = mean(tonic_patch_avg, na.rm = TRUE),
            sd_tonic_patch_avg = sd(tonic_patch_avg, na.rm = TRUE))

mean_sd_patch <- OverharvestData %>%
  group_by(Patch_Type) %>%
  summarize(mean_tonic_patch_avg = mean(tonic_patch_avg, na.rm = TRUE),
            sd_tonic_patch_avg = sd(tonic_patch_avg, na.rm = TRUE))

#Violin plots

aggRes <- aggregate(OverharvestData,
                    by = list(OverharvestData$Participant, OverharvestData$Patch_Type, OverharvestData$Env_Type),
                    FUN = mean, na.rm=TRUE)

aggRes$Participant <- aggRes$Group.1
aggRes$Patch_Type <- aggRes$Group.2
aggRes$Env_Type <- aggRes$Group.3

aggEnv <- aggregate(OverharvestData,
                    by = list(OverharvestData$Participant, OverharvestData$Env_Type),
                    FUN = mean, na.rm=TRUE)

aggEnv$Participant <- aggEnv$Group.1
aggEnv$Environment <- aggEnv$Group.2

aggPatch <- aggregate(OverharvestData,
                      by = list(OverharvestData$Participant, OverharvestData$Patch_Type),
                      FUN = mean, na.rm=TRUE)

aggPatch$Participant <- aggPatch$Group.1
aggPatch$Patch <- aggPatch$Group.2

#Patch plot

yColors <- c("#E41A1C", "#377EB8", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")

PatchPlot <- ggplot(aggPatch, aes(x = Patch, y = Residence, fill = Patch, color = Patch)) +
  geom_flat_violin(aes(fill = Patch, alpha = 0.3), position = position_nudge(x = .1), adjust = 1.5, trim = FALSE, colour = NA) +  # Remove the black border and set alpha
  geom_point(position = position_jitter(0), size = 2, alpha = 0.3) +
  geom_boxplot(width = 0.2, lwd = 0.6, alpha = 0.3, fatten = NULL, outlier.shape = NA) +  # Adjust the width and alpha
  scale_fill_manual(values = myColors) +  # Set the fill colors
  scale_colour_manual(values = myColors) +  # Set the border colors
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "none") +
  ggtitle("A) Main Effect of Patch\n") +
  xlab("\nPatch") +
  ylab("Residence (trials)\n") +
  stat_summary(fun = "mean", color = "white") +
  theme(axis.text = element_text(size = 14)) +
  ylim(2, 22)
PatchPlot

#Plot Environment Type & Residence Times

EnvironmentPlot <- ggplot(aggEnv, aes(x = Environment, y = Residence, fill = Environment, color = Environment)) +
  geom_flat_violin(aes(alpha = 0.3), position = position_nudge(x = 0.1), adjust = 1.5, trim = FALSE, colour = NA) +
  geom_boxplot(width = 0.2, lwd = 0.6, alpha = 0.3, fatten = NULL, outlier.shape = NA, position = position_nudge(x = 0.1)) +
  geom_point(position = position_jitter(0), size = 2, alpha = 0.3) +
  scale_fill_manual(values = myColors) +
  scale_colour_manual(values = myColors) +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  theme(legend.position = "none") +
  ggtitle("B) Main Effect of Environment\n") +
  ylab("Residence (Trials)\n") +
  xlab("\nEnvironment") +
  stat_summary(fun = "mean", color = "white", position = position_nudge(x = 0.1)) +  # Adjust position for mean dots inside the boxplot
  theme(axis.text = element_text(size = 14)) +
  ylim(2, 19)
EnvironmentPlot

#Plot Residence per Patch and Environment

EnvironmentPlot <- ggplot(aggEnv, aes(x = Environment, y = Residence, fill = Environment, color = Environment)) +
  geom_flat_violin(aes(alpha = 0.3), position = position_nudge(x = 0.1), adjust = 1.5, trim = FALSE, colour = NA) +
  geom_boxplot(width = 0.2, lwd = 0.6, alpha = 0.3, fatten = NULL, outlier.shape = NA, position = position_nudge(x = 0.1)) +
  geom_point(position = position_jitter(0), size = 2, alpha = 0.3) +
  scale_fill_manual(values = myColors) +
  scale_colour_manual(values = myColors) +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  theme(legend.position = "none") +
  ggtitle("B) Main Effect of Environment\n") +
  ylab("Residence (Trials)\n") +
  xlab("\nEnvironment") +
  stat_summary(fun = "mean", color = "white", position = position_nudge(x = 0.1)) +  # Adjust position for mean dots inside the boxplot
  theme(axis.text = element_text(size = 14)) +
  ylim(2, 19)

EnvironmentPlot


#Overharvesting per Patch and Environment
sumrepdat_env_patch <- summarySE(aggPatchEnv, measurevar = "Overharvest",
                                 groupvars=c("Env_Type","Patch_Type"))
overharvest_patch_env <- ggplot(aggPatchEnv, aes(x = Patch_Type, y = Overharvest, fill = Env_Type)) +
  geom_flat_violin(aes(),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Patch_Type)-.15, y = Overharvest, colour = Env_Type),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = Patch_Type, y = Overharvest, fill = Env_Type),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat_env_patch, aes(x = as.numeric(Patch_Type)+.1, y = Overharvest_mean, group = Env_Type, colour = Env_Type), linetype = 3)+
  geom_point(data = sumrepdat_env_patch, aes(x = as.numeric(Patch_Type)+.1, y = Overharvest_mean, group = Env_Type, colour = Env_Type), shape = 18) +
  geom_errorbar(data = sumrepdat_env_patch, aes(x = as.numeric(Patch_Type)+.1, y = Overharvest_mean, group = Env_Type, colour = Env_Type, ymin = Overharvest_mean-sem, ymax = Overharvest_mean+sem), width = .05)+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  theme_minimal()+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12), axis.title.x = element_text(margin = margin(t = 15)))

overharvest_patch_env <- overharvest_patch_env +
  labs(x = "Patch Type", fill = "Environment Type", color = "Environment Type")

overharvest_patch_env



#Analysis including randoms slopes for tonic pupil

model3_tonic_pupil <- lmer(Overharvest ~ tonic_patch_avg + (tonic_patch_avg | Lab/Participant), data = OverharvestData)

#Remove the lab because of singularity issues
model3.1_tonic_pupil <- lmer(Overharvest ~ tonic_patch_avg + (tonic_patch_avg | Participant), data = OverharvestData)

model4_tonic_pupil <- lmer(Overharvest ~ tonic_firsttrial_avg + (tonic_firsttrial_avg|Lab/Participant), OverharvestData)

model4.1_tonic_pupil <- lmer(Overharvest ~ tonic_firsttrial_avg + (tonic_firsttrial_avg|Participant), OverharvestData)

##Calculate the results for each Lab
#Lab HHU
model_HHU <- lmer(Overharvest ~ tonic_patch_avg*Env_Type*Patch_Type + (1|Participant), data = subset(OverharvestData, Lab == "HHU"))
model_HHU2 <- lmer(Overharvest ~ tonic_firsttrial_avg + (1|Participant), data = subset(OverharvestData, Lab == "HHU"))
model_HHU3 <- lmer(Residence ~ Env_Type*Patch_Type + (1|Participant), data = subset(OverharvestData, Lab == "HHU"))

#Lab UGent
model_UGent <- lmer(Overharvest ~ tonic_patch_avg*Env_Type*Patch_Type + (1|Participant), data = subset(OverharvestData, Lab == "UGent"))
model_UGent2 <- lmer(Overharvest ~ tonic_firsttrial_avg + (1|Participant), data = subset(OverharvestData, Lab == "UGent"))
model_UGent3 <- lmer(Residence ~ Env_Type*Patch_Type + (1|Participant), data = subset(OverharvestData, Lab == "UGent"))

##Graph for each estimate per Lab

library(ggplot2)
library(broom.mixed)


# Extract the fixed effects estimates for tonic_patch_avg from the models for each lab
model_HHU_estimates <- tidy(model_HHU, effects = "fixed") %>%
  filter(term == "tonic_patch_avg")
model_UGent_estimates <- tidy(model_UGent, effects = "fixed") %>%
  filter(term == "tonic_patch_avg")

combined_estimates <- rbind(
  data.frame(Lab = "HHU", model_HHU_estimates),
  data.frame(Lab = "UGent", model_UGent_estimates)
)

# Plot the tonic pupil estimates per lab as horizontal lines with error bars
ggplot(combined_estimates, aes(x = estimate, y = Lab, color = Lab)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error),
                 height = 0.1, position = position_dodge(width = 0.5)) +
  labs(y = NULL, x = "Estimates",
       title = "Effect of tonic pupil on Overharvesting") +
  theme_minimal()

#Between-subject Anova 
subset_data <- aggregate(Overharvest ~ Participant + Patch_Type + Env_Type + tonic_patch_avg, data = OverharvestData, FUN = mean)
between_anova <- aov(Overharvest ~ Patch_Type + Env_Type + tonic_patch_avg, data = subset_data)
summary(between_anova)
subset_data <- aggregate(Overharvest ~ Participant + Patch_Type + Env_Type + First_Trial_Avg, data = OverharvestData, FUN = mean)
between_anova1 <- aov(Overharvest ~ Patch_Type + Env_Type + First_Trial_Avg, data = subset_data)
summary(between_anova1)


##Visualizing mixed-effects models

knitr::opts_chunk$set(echo = TRUE)

library(tidyverse) 
library(cowplot) 
library(lme4) 
library(sjPlot) 
library(sjmisc) 
library(effects)
library(sjstats)
library(insight)


##Plotting effect sizes


sjPlot::plot_model(model2_tonic_pupil, axis.labels=c("Tonic Pupil"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of tonic pupil on Overharvesting")

##full model 

sjPlot::plot_model(model3_tonic_fullmodel, terms = c("tonic_patch_avg", "Patch_Type", "Env_Type"), 
                   
                   show.values = TRUE, show.p = TRUE, show.est = TRUE,
                   title = "")

plot + ggplot2::labs(x = "Tonic Pupil")

# Generate the effects object
eff <- allEffects(model3_tonic_fullmodel)

# Plot the effects
plot(eff, show.values = TRUE, show.p = TRUE, show.est = TRUE, xlab="Tonic Pupil",
     main = "")


## Calculate VIF Values 
model3_tonic_fullmodel <- lmer(Overharvest ~ tonic_patch_avg * Patch_Type * Env_Type + (1 | Participant), OverharvestData)

vif_values <- vif(model3_tonic_fullmodel)

