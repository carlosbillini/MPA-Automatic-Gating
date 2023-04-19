## Gating Analyzing tool ##

# READ ME #

# Intro
# This code is meant to analyze 23 gates and the manual gating to later pick
# witch of all of the gates makes more sense to simulate the manual gating.

# UPDATE: Instead of picking 1 gate we are able to pick a range of gates based
# on this analyzes.

# Table of Content
  # - Libraries
  # - UPLOAD DATA
    # - Global data
    # - Data to Analyze
      # - Data with 23 Gates
      # - Data with 1 Gate
  # - PROCESS DATA #
    # - Processing data for all gates in only controls
    # - Processing data for all gates with the average
    # - Speed Growth for AVG
  # - PROCESS FOR FINAL GRAPH ## 
    # - Processing data for data 1 Gate
    # - Processing data for data 23 Gate
  # - LOOK AT MY GRAPHS!!



# Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(tidyverse)
library(matrixStats)

# UPLOAD DATA #
  # Global data
MatrixLib <- read_csv("GitHub/MPA-DataAnalisys/Screen_Analysis/MatrixLib/MatrixLib.csv") %>%
  select(symbol_raw, symbol, matrix_col, matrix_row)

newrow <- c(1:23)

  # Data to analyze
    # Data with 23 Gates

### AZ_01
 allgates_onlycontrol <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ01.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
                   slice(342) %>% 
                   select(-1)

 allgates_all <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ01.csv", 
                                 col_types = cols(`Well Type` = col_skip()), 
                                 skip = 1) 

 allgates_allavg <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ01.csv", 
                            col_types = cols(`Well Type` = col_skip()), 
                            skip = 1) %>% 
                            select(-1)

#----------------------------------------------------------
### AZ_03
#allgates_onlycontrol <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ03.csv", 
#                                 col_types = cols(`Well Type` = col_skip()), 
#                                 skip = 1) %>% 
#  slice(342) %>% 
#  select(-1)

#allgates_all <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ03.csv", 
#                         col_types = cols(`Well Type` = col_skip()), 
#                         skip = 1)

#allgates_allavg <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ03.csv", 
#                         col_types = cols(`Well Type` = col_skip()), 
#                         skip = 1) %>% 
#                         select(-1)

#------------------------------------------------------------

### AZ_04
#allgates_onlycontrol <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ04.csv", 
#                                 col_types = cols(`Well Type` = col_skip()), 
#                                 skip = 1) %>% 
#  slice(342) %>% 
#  select(-1)

#allgates_all <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ04.csv", 
#                         col_types = cols(`Well Type` = col_skip()), 
#                         skip = 1)

#allgates_allavg <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ04.csv", 
#                         col_types = cols(`Well Type` = col_skip()), 
#                         skip = 1) %>% 
#                         select(-1)

#------------------------------------------------------------

### AZ_07
#allgates_onlycontrol <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_PARA01.csv", 
#                                 col_types = cols(`Well Type` = col_skip()), 
#                                 skip = 1) %>% 
#  slice(342) %>% 
#  select(-1)

#allgates_all <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_PARA01.csv", 
#                         col_types = cols(`Well Type` = col_skip()), 
#                         skip = 1)

#allgates_allavg <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_PARA01.csv", 
#                            col_types = cols(`Well Type` = col_skip()), 
#                            skip = 1) %>% 
#  select(-1)

#------------------------------------------------------------












    # Data with 1 Gate

### AZ_01
 OG_gate <- read_csv("Z:/_Employee Folders/Carlos Billini/ForCyt_Data/making locked templates/AZ 01/Manual_Gating/HX1489 AZ01 100722 KO Data.csv", 
                skip = 1) %>%
  select(matrix_row = 1,  Hits = 3)

#----------------------------------------------------------
### AZ_03
#OG_gate <- read_csv("Z:/_Employee Folders/Carlos Billini/ForCyt_Data/making locked templates/AZ 03/Manual_Gating/HX1491 AZ03 100722 KO Data.csv", 
#                    skip = 1) %>%
#  select(matrix_row = 1,  Hits = 3)

#----------------------------------------------------------
### AZ_04
#OG_gate <- read_csv("Z:/_Employee Folders/Carlos Billini/ForCyt_Data/making locked templates/AZ 04/Manual_Gating/HX1492 AZ04 100722 KO Data.csv", 
#                    skip = 1) %>%
#  select(matrix_row = 1,  Hits = 3)

#----------------------------------------------------------
### AZ_07
#OG_gate <- read_csv("Z:/_Employee Folders/Carlos Billini/ForCyt_Data/making locked templates/PARA 01/Manual_Gating/HX1411 PAR01 070822 KO Data Spikes Normalized.csv", 
#                    skip = 1) %>%
#  select(matrix_row = 1,  Hits = 3)

#----------------------------------------------------------














# PROCESS DATA #
  # Processing data for all gates in only controls
allgates_onlycontrol <- rbind(allgates_onlycontrol, newrow)
allgates_onlycontrol <- as.data.frame(t(allgates_onlycontrol)) %>%
  select(Cntr2 = 1, Gates = 2)

speedGrowth_onlycontrol <- abs(diff(allgates_onlycontrol$Cntr2))
speedGrowth_onlycontrol <- cbind(newrow, speedGrowth_onlycontrol)
speedGrowth_onlycontrol <- data.frame(speedGrowth_onlycontrol) %>%
  select(GatesD = 1, Diff2 = 2) %>% 
  slice(-23) 

  # Processing data for all gates with the average

allgates_all1 <- data.frame(colMeans(allgates_allavg[sapply(allgates_allavg, is.numeric)])) %>% 
  rename(MeansGates = colMeans.allgates_allavg.sapply.allgates_allavg..is.numeric... )

allgates_all1$SDGates <- sapply(allgates_allavg, sd)

allgates_all1 <- cbind(allgates_all1, newrow)

  # Speed Growth for AVG

newrow1 <- c(1:22)

allgates_all2 <- abs(diff(allgates_all1$MeansGates))

allgates_all2 <- cbind(newrow1, allgates_all2)

allgates_all2 <- data.frame(allgates_all2) %>%
  select(GatesD = 1, Diff = 2)

  ## PROCESS FOR FINAL GRAPH ## 
  # Processing data for data 1 Gate
OG_gate$S_N = OG_gate$Hits / mean(OG_gate$Hits, trim = 0.1)

OG_gate <- OG_gate %>% select(1, 3)

OG_gate2 <- MatrixLib %>%
  inner_join(OG_gate) %>%
  rename(S_N.x = S_N) %>%
  inner_join(OG_gate,
             by = c("matrix_col" = "matrix_row")) %>%
  select(symbol_raw, symbol, matrix_col, matrix_row, S_N.x, S_N.y = S_N)


Manual_2SD <- filter(OG_gate2, 
              S_N.y >= sd(OG_gate2$S_N.y) * 2 + mean(OG_gate2$S_N.y, trim = 0.1) & 
              S_N.x >= sd(OG_gate2$S_N.x) * 2 + mean(OG_gate2$S_N.x, trim = 0.1))

Manual_2.5SD <- filter(OG_gate2, 
              S_N.y >= sd(OG_gate2$S_N.y) * 2.5 + mean(OG_gate2$S_N.y, trim = 0.1) & 
              S_N.x >= sd(OG_gate2$S_N.x) * 2.5 + mean(OG_gate2$S_N.x, trim = 0.1))

Manual_3SD <- filter(OG_gate2, 
              S_N.y >= sd(OG_gate2$S_N.y) * 3 + mean(OG_gate2$S_N.y, trim = 0.1) & 
              S_N.x >= sd(OG_gate2$S_N.x) * 3 + mean(OG_gate2$S_N.x, trim = 0.1))


Adf <- data.frame(sd2 = nrow(Manual_2SD),
                  sd2.5 = nrow(Manual_2.5SD),
                  sd3 = nrow(Manual_3SD),
                  exGate = 1)

  # Processing data for data 23 Gate

for(i in 1:24){
  colnames(allgates_all)[i+1] = paste0("gate", i) 
}

for(i in 1:23){
  allgates_all[[paste0('S_N', i)]] <- allgates_all[[paste0("gate", i)]] / mean(allgates_all[[paste0("gate", i)]], trim = 0.1)
}

allgates_all <- allgates_all %>% select(-(2:24))

gt2 <- MatrixLib %>%
  inner_join(allgates_all,
             by = c("matrix_row" = "Well ID")) %>% 
  inner_join(allgates_all,
             by = c("matrix_col" = "Well ID"))



for(i in 1:23){
  
  gtflt <- gt2 %>% select(xloc = i+4, yloc = i+27)
  
  
  gtsd1 <- filter(gtflt, 
                  xloc >= 
                    sd(gtflt$xloc)* 2 + 
                    mean(gtflt$xloc, trim = 0.1) &
                    yloc >= 
                    sd(gtflt$yloc)* 2 + 
                    mean(gtflt$yloc, trim = 0.1)  
  )
  
  gtsd2 <- filter(gtflt,
                  xloc >= 
                    sd(gtflt$xloc) * 2.5 + 
                    mean(gtflt$xloc, trim = 0.1) &
                    yloc >= 
                    sd(gtflt$yloc) * 2.5 + 
                    mean(gtflt$yloc, trim = 0.1)  
  )
  
  gtsd3 <- filter(gtflt,
                  xloc >= 
                    sd(gtflt$xloc) * 3 + 
                    mean(gtflt$xloc, trim = 0.1) &
                    yloc >= 
                    sd(gtflt$yloc) * 3 + 
                    mean(gtflt$yloc, trim = 0.1)  
  )
  
  gt3 <- data.frame(sd1 = nrow(gtsd1),
                    sd2 = nrow(gtsd2),
                    sd3 = nrow(gtsd3),
                    exGate = i + 1)
  
  
  Adf[nrow(Adf) + 1,] <- gt3
}




# LOOK AT MY GRAPHS!!


ag_oc <- ggplot(allgates_onlycontrol, aes(x=Gates)) + 
  geom_line(aes(y = Cntr2), color = "#000000", size = 1) + 
  xlab("Gates") + ylab("Growth")

sG_oc <- ggplot(speedGrowth_onlycontrol, aes(x=GatesD)) + 
  geom_line(aes(y = Diff2), color = "#000000", size = 1) + 
  xlab("Gates") + ylab("Speed Growth")

aG_awells <- ggplot(allgates_all1, aes(x=newrow)) + 
  geom_line(aes(y = MeansGates), color = "#000000", size = 1.1) + 
  geom_errorbar(aes(ymin = MeansGates-SDGates, ymax = MeansGates+SDGates), color = "#000000", width=.5) +
  geom_point(aes(y = MeansGates), size=2, shape=21, fill="white") + 
  xlab("Gates") + ylab("Avg Growth")

sG_awells <- ggplot(allgates_all2, aes(x=GatesD)) + 
  geom_line(aes(y = Diff), color = "#000000", size = 1) + 
  xlab("Gates") + ylab("Speed Growth")




endresult <- ggplot(Adf, aes(x = exGate)) + 
  geom_point(aes(y = sd2), color = "#000000", size = 2) + 
  geom_point(aes(y = sd2.5), color = "#ADD8E6", size = 2) +
  geom_point(aes(y = sd3), color = "#E69F00", size = 2) + 
  xlab("Gates") + ylab("Above Threshold")


ggarrange(ag_oc, sG_oc, aG_awells, sG_awells, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
