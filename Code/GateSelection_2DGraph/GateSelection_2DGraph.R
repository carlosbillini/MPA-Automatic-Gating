# READ ME

# Intro:
#  

# Index

# - Libraries
# - Upload Data
  # - Global
  # - Data
# - Gate Selection
  # - Data Processing
    # - Processing data for all gates in only controls
    # - Processing data for all gates with the average
    # - Speed Growth for AVG
  # - Plots
# - Recommended Gates

# - 2D Graph
  # - Process 2D Graph
  # - Spike wells
  # - Define SD Threshold
  # - Simple 2D Graph
  # - In depth Hits

data1 <-  read_csv("Z:/_Employee Folders/Carlos Billini/BroluCarlosGates.csv", 
                  skip = 1) 

dummy <-  data.frame(WellID = "P22")

allgates_onlycontrol <-   right_join(data1, dummy,
                                     by = c('Well ID' = 'WellID')) %>% 
  select(-(1:2))






# Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(tidyverse)
library(matrixStats)
library(hrbrthemes)
library(viridis)
library(plotly)
library(ggarchery)
# - Upload Data
  # - Global
MatrixLib <- read_csv("GitHub/MPA-DataAnalisys/Screen_Analysis/MatrixLib/MatrixLib.csv") %>%
             select(symbol_raw, symbol, matrix_col, matrix_row)
newrow <- c(1:23) # Number of gates in data (Change if needed)


  # - Data

data_files <- list.files("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control")  # Identify file names
data_files

for(i in 1:length(data_files)) {                              # Head of for-loop
  assign(paste0("data", i),                                   # Read and store data frames
         read_csv(paste0("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/",
                         data_files[i]), skip = 1))
}




allgates_onlycontrol <- data1 %>% 
  slice(342) %>% 
  select(-(1:2))

allgates_all <- data1 %>% 
  select(-2)

allgates_allavg <- data1 %>% 
  select(-(1:2))





#allgates_onlycontrol <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ01.csv", 
#                                 col_types = cols(`Well Type` = col_skip()), 
#                                 skip = 1) %>% 
#                                 slice(342) %>% 
#                                 select(-1)
#allgates_all <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ01.csv", 
#                         col_types = cols(`Well Type` = col_skip()), 
#                         skip = 1) 
#allgates_allavg <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ01.csv", 
#                            col_types = cols(`Well Type` = col_skip()), 
#                            skip = 1) %>% 
#                            select(-1)

# - Gate Selection
  # - Data Processing
    # - Processing data for all gates in only controls
allgates_onlycontrol <- rbind(allgates_onlycontrol, newrow)
allgates_onlycontrol <- as.data.frame(t(allgates_onlycontrol)) %>%
                        select(Cntr2 = 1, Gates = 2)

speedGrowth_onlycontrol <- abs(diff(allgates_onlycontrol$Cntr2))
speedGrowth_onlycontrol <- cbind(newrow, speedGrowth_onlycontrol)
speedGrowth_onlycontrol <- data.frame(speedGrowth_onlycontrol) %>%
                           select(GatesD = 1, Diff2 = 2) %>% 
                           slice(-23) 

    # - Processing data for all gates with the average
allgates_all1 <- data.frame(colMeans(allgates_allavg[sapply(allgates_allavg, is.numeric)], na.rm = TRUE)) %>% 
  rename(MeansGates = 1 )

allgates_all1$SDGates <- sapply(allgates_allavg, sd, na.rm = TRUE)

allgates_all1 <- cbind(allgates_all1, newrow)

    # - Speed Growth for AVG
newrow1 <- c(1:22)

allgates_all2 <- abs(diff(allgates_all1$MeansGates))

allgates_all2 <- cbind(newrow1, allgates_all2)

allgates_all2 <- data.frame(allgates_all2) %>%
  select(GatesD = 1, Diff = 2)


#testgate <- cbind(testgate, diff4 = allgates_all2$Diff )
#testgate <- allgates_all2



  # - Plots
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



ggarrange(ag_oc, sG_oc, aG_awells, sG_awells, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


# - Recommended Gates


while (sum(speedGrowth_onlycontrol$Diff2) > 0.001) {
speedGrowth_onlycontrol <- speedGrowth_onlycontrol %>% slice(-1)
}

RecGates <- speedGrowth_onlycontrol %>% slice(1) %>% select(1)

#RecGates <- filter(speedGrowth_onlycontrol,
#                   Diff2 <= 0.001) %>% 
#  slice(1) %>% 
#  select(1)

while (sum(allgates_all2$Diff) > .1) {
  allgates_all2 <- allgates_all2 %>% slice(-1)
}

RecGates_all <- allgates_all2 %>% slice(1) %>% 
  select(1)

#RecGates_all <- filter(allgates_all2,
#                   Diff <= 0.01) %>% 
#  slice(1) %>% 
#  select(1)


RecGates_all[nrow(RecGates_all) + 1,] <- RecGates

GateNumber <- ceiling(mean(RecGates_all$GatesD))

GateNumberD <- c(GateNumber, GateNumber + 1) #change to + 0, +1
GateNumberU <- c(GateNumber + 4, GateNumber + 5) #change to + 4, +5


SelectedGates <- allgates_all %>% 
  select(1, GateNumberD + 1, GateNumberU + 1) %>% 
  slice(-344)


# - 2D Graph
  # - Process 2D Graph
for(i in 1:4){
  colnames(SelectedGates)[i+1] = paste0("gate", i) 
}

for(i in 1:4){
  SelectedGates[[paste0('S_N', i)]] <- SelectedGates[[paste0("gate", i)]] / mean(SelectedGates[[paste0("gate", i)]], trim = 0.1)
}

SelectedGates <- SelectedGates %>% select(-(2:5))

SelectedGates2 <- MatrixLib %>%
  inner_join(SelectedGates,
             by = c("matrix_row" = "Well ID")) %>% 
  inner_join(SelectedGates,
             by = c("matrix_col" = "Well ID"))

for(i in 1:4){
  colnames(SelectedGates2)[i+4] = paste0("S_N", i, ".y") 
  colnames(SelectedGates2)[i+8] = paste0("S_N", i, ".x") 
}


SelectedGates2$S_N.y <-  rowMeans(SelectedGates2[,5:8])
SelectedGates2$S_N.x <-  rowMeans(SelectedGates2[,9:12])

AvgSelectedGate <- SelectedGates2 %>% select(1:4, 13, 14)



# - Spike wells

AbvAvg <- sum(sd(AvgSelectedGate$S_N.y), sd(AvgSelectedGate$S_N.x)) / 2

if (AvgSelectedGate[999,5] > 10 || AvgSelectedGate[999,6] > 10 ) {
  Spikewell <- max(AvgSelectedGate[999,5:6]) * 2 / min(AvgSelectedGate[999,5:6]) 
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == min(AvgSelectedGate[999,5:6])] <- 2 + AbvAvg *3
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == max(AvgSelectedGate[999,5:6])] <- Spikewell + AbvAvg *3
}


if (AvgSelectedGate[1999,5] > 10 || AvgSelectedGate[1999,6] > 10 ) {
  Spikewell <- max(AvgSelectedGate[1999,5:6]) * 2 / min(AvgSelectedGate[1999,5:6]) 
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == min(AvgSelectedGate[1999,5:6])] <- 2 + AbvAvg *3
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == max(AvgSelectedGate[1999,5:6])] <- Spikewell + AbvAvg *3
}

if (AvgSelectedGate[2999,5] > 10 || AvgSelectedGate[2999,6] > 10 ) {
  Spikewell <- max(AvgSelectedGate[2999,5:6]) * 2 / min(AvgSelectedGate[2999,5:6]) 
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == min(AvgSelectedGate[2999,5:6])] <- 2 + AbvAvg *3
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == max(AvgSelectedGate[2999,5:6])] <- Spikewell + AbvAvg *3
}

if (AvgSelectedGate[3999,5] > 10 || AvgSelectedGate[3999,6] > 10 ) {
  Spikewell <- max(AvgSelectedGate[3999,5:6]) * 2 / min(AvgSelectedGate[3999,5:6]) 
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == min(AvgSelectedGate[3999,5:6])] <- 2 + AbvAvg *3
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == max(AvgSelectedGate[3999,5:6])] <- Spikewell + AbvAvg *3
}

if (AvgSelectedGate[4999,5] > 10 || AvgSelectedGate[4999,6] > 10 ) {
  Spikewell <- max(AvgSelectedGate[4999,5:6]) * 2 / min(AvgSelectedGate[4999,5:6]) 
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == min(AvgSelectedGate[4999,5:6])] <- 2 + AbvAvg *3
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == max(AvgSelectedGate[4999,5:6])] <- Spikewell + AbvAvg *3
}

if (AvgSelectedGate[5999,5] > 10 || AvgSelectedGate[5999,6] > 10 ) {
  Spikewell <- max(AvgSelectedGate[5999,5:6]) * 2 / min(AvgSelectedGate[5999,5:6]) 
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == min(AvgSelectedGate[5999,5:6])] <- 2 + AbvAvg *3
  AvgSelectedGate[, 5:6] [AvgSelectedGate[, 5:6] == max(AvgSelectedGate[5999,5:6])] <- Spikewell + AbvAvg *3
}


  # - Define SD Threshold

  # - Simple 2D Graph



##### Analysis - 2D Scatter Plot #####

x <- 2.5  # STD Multiplier 

rm(Manual_SD)


Threshold.y <- sd(AvgSelectedGate$S_N.y) * x + mean(AvgSelectedGate$S_N.y, trim = 0.1)  
Threshold.x <- sd(AvgSelectedGate$S_N.x) * x + mean(AvgSelectedGate$S_N.x, trim = 0.1)

options(ggrepel.max.overlaps = Inf)

theplot = ggplot(AvgSelectedGate, aes(x=S_N.x, y=S_N.y, label=symbol)) + 
  geom_label_repel(aes(label = ifelse(S_N.y>Threshold.y & S_N.x>Threshold.x, as.character(symbol),'')),
                   box.padding   = 1.35, 
                   point.padding = 1.5,
                   segment.color = 'grey50')  +
  geom_point(color = dplyr::case_when(AvgSelectedGate$S_N.y > Threshold.y & AvgSelectedGate$S_N.x > Threshold.x ~ 
                                        "#d95f02", TRUE ~ 
                                        "#7570b3"), 
             size = dplyr::case_when(AvgSelectedGate$S_N.y > Threshold.y & AvgSelectedGate$S_N.x > Threshold.x ~ 
                                       4, TRUE ~ 
                                       2), 
             alpha = dplyr::case_when(AvgSelectedGate$S_N.y > Threshold.y & AvgSelectedGate$S_N.x > Threshold.x ~ 
                                        1, TRUE ~ 
                                        .2))  +
  geom_vline(aes(xintercept = Threshold.x),
             color="blue", 
             linetype="dashed", 
             size=1,
             alpha = .5) +
geom_hline(aes(yintercept = Threshold.y),
           color="blue", 
           linetype="dashed", 
           size=1,
           alpha = .5)

theplot


  # - In depth Hits
  
Manual_SD <- filter(SelectedGates2, 
                     S_N.y >= sd(SelectedGates2$S_N.y) * x + mean(SelectedGates2$S_N.y, trim = 0.1) & 
                       S_N.x >= sd(SelectedGates2$S_N.x) * x + mean(SelectedGates2$S_N.x, trim = 0.1))




Manual_SD <- transform(Manual_SD, row_stdev=apply(Manual_SD, 1, sd, na.rm=TRUE))

ggplot(data = Manual_SD, aes(x=S_N.x, y=S_N.y, size = row_stdev, color=symbol), color = 'blue') + 
  geom_point(alpha=0.7) +
  scale_size(range = c(4, 15), name="SD") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  geom_label_repel(aes(label = as.character(symbol), size = 1), force = 10) +
  theme(legend.position = "none") +
  geom_segment(aes(x=S_N.x, y=S_N.y, xend=S_N4.x-x, yend=S_N4.y-x), 
               arrow = arrow(length=unit(.5, 'cm')), 
               size = .5)



ggplot(data = Manual_SD, aes(x=S_N1.x, y=S_N1.y, color=symbol)) + 
  geom_point(alpha=0.7) +
  geom_label_repel(aes(label = as.character(symbol), size = .5), force = 10) +
  theme(legend.position = "none") +
  geom_segment(aes(x=S_N1.x, y=S_N1.y, xend=S_N2.x, yend=S_N2.y), 
                    arrow = arrow(
                      type = "closed",
                      ends = "last", 
                      angle = 20,
                      length = unit(0.2, "inches"))) + 
  geom_segment(aes(x=S_N2.x, y=S_N2.y, xend=S_N3.x, yend=S_N3.y), 
               arrow = arrow(
                 type = "closed",
                 ends = "last", 
                 angle = 20,
                 length = unit(0.2, "inches")), 
               size = .5) +
  geom_segment(aes(x=S_N3.x, y=S_N3.y, xend=S_N4.x, yend=S_N4.y), 
               arrow = arrow(
                 type = "closed",
                 ends = "last", 
                 angle = 20,
                 length = unit(0.2, "inches")), 
               size = .5)


trythis <- data.frame(symbol = Manual_SD$symbol,
                      SNx = c(Manual_SD$S_N1.x, Manual_SD$S_N2.x, Manual_SD$S_N3.x, Manual_SD$S_N4.x),
                      SNy = c(Manual_SD$S_N1.y, Manual_SD$S_N2.y, Manual_SD$S_N3.y, Manual_SD$S_N4.y)
                      )

ggplot(data=trythis, aes(x=SNx, y=SNy, group=symbol, color=symbol)) +
  geom_line()+
  geom_point()

