# READ ME

# Intro
# Data used is the gates that takes 100% to .1% of the cells
# This code finalizes 4 graphs where it considered only the dummy cells 
  # Graph A & B - Looks into the % of cells in each of the gates both of several
  # of the customer projects and the average. 
  # Discussion:
  # Graph C & D - Looks into the change of the graphs before to look into the 
  # speed of change.
  # Discussion:

# Index
  # - Libraries
  # - Read all data
  # - 23 Gates Data
  # - DATA SET 1 
  # - DATA SET 2
  # - DATA SET 3
  # - DATA SET 4
  # - DATA SET 5
  # - DATA SET 6
  # - Merge all data
  # - Graphs
    # - Graph with all
      # - SD of rows
    # - PLOT OF AVG AND SD
    # - MEASURE DIFFERENCE PER GATE GRAPH
      # - SD of rows 
    # - PLOT OF AVG AND SD



# Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(tidyverse)
library(matrixStats)

# Read all data #

#alldf <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/dataset 1.csv", 
#                      col_types = cols(`Well Type` = col_skip(), 
#                                       ...8 = col_skip()), skip = 1)
#alldf <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Gatein10s.csv", 
#                      col_types = cols(`Well Type` = col_skip()), 
#                      skip = 1)

#MatrixLib <- read_csv("C:/Users/cbillini/Desktop/Stat-Tinker/R Workspace/2D Plot Screen/DATA test/MatrixLib.csv") %>% 
#  select(symbol_raw, symbol, matrix_col, matrix_row)

newrow <- c(1:23)

# 23 Gates Data #
# DATA SET 1 #

alldf1 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_SEE1.csv", 
                  col_types = cols(`Well Type` = col_skip()), 
                  skip = 1) %>% 
  slice(342) %>% 
  select(-1)
  
# Fill name

alldf1 <- rev(alldf1)
alldf1 <- rbind(alldf1, newrow)
alldf1 <- as.data.frame(t(alldf1)) %>%
  select(Cntrl = 1, Gates = 2)

# Diference between gates #

dif1 <- diff(alldf1$Cntrl) 
dif1 <- cbind(newrow, dif1)
dif1 <- data.frame(dif1) %>%
  select(GatesD = 1, Diff1 = 2)
dif1
# DATA SET 2 #  

alldf2 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ01.csv", 
                  col_types = cols(`Well Type` = col_skip()), 
                  skip = 1) %>% 
  slice(342) %>% 
  select(-1)

alldf2 <- rev(alldf2)
alldf2 <- rbind(alldf2, newrow)
alldf2 <- as.data.frame(t(alldf2)) %>%
  select(Cntr2 = 1, Gates = 2)

dif2 <- diff(alldf2$Cntr2) 
dif2 <- cbind(newrow, dif2)
dif2 <- data.frame(dif2) %>%
  select(GatesD = 1, Diff2 = 2)

# DATA SET 3 #

alldf3 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ03.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
  slice(342) %>% 
  select(-1)

alldf3 <- rev(alldf3)
alldf3 <- rbind(alldf3, newrow)
alldf3 <- as.data.frame(t(alldf3)) %>%
  select(Cntr3 = 1, Gates = 2)

dif3 <- diff(alldf3$Cntr3) 
dif3 <- cbind(newrow, dif3)
dif3 <- data.frame(dif3) %>%
  select(GatesD = 1, Diff3 = 2)

# DATA SET 4 #

alldf4 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ04.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
  slice(342) %>% 
  select(-1)

alldf4 <- rev(alldf4)
alldf4 <- rbind(alldf4, newrow)
alldf4 <- as.data.frame(t(alldf4)) %>%
  select(Cntr4 = 1, Gates = 2)

dif4 <- diff(alldf4$Cntr4) 
dif4 <- cbind(newrow, dif4)
dif4 <- data.frame(dif4) %>%
  select(GatesD = 1, Diff4 = 2)

# DATA SET 5 #

alldf5 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ07.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
  slice(342) %>% 
  select(-1)

alldf5 <- rev(alldf5)
alldf5 <- rbind(alldf5, newrow)
alldf5 <- as.data.frame(t(alldf5)) %>%
  select(Cntr5 = 1, Gates = 2)

dif5 <- diff(alldf5$Cntr5) 
dif5 <- cbind(newrow, dif5)
dif5 <- data.frame(dif5) %>%
  select(GatesD = 1, Diff5 = 2)

# DATA SET 6 #

alldf6 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_PARA01.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
  slice(342) %>% 
  select(-1)

alldf6 <- rev(alldf6)
alldf6 <- rbind(alldf6, newrow)
alldf6 <- as.data.frame(t(alldf6)) %>%
  select(Cntr6 = 1, Gates = 2)

dif6 <- diff(alldf6$Cntr6) 
dif6 <- cbind(newrow, dif6)
dif6 <- data.frame(dif6) %>%
  select(GatesD = 1, Diff6 = 2)

# Merge all data

alldif <- list(dif1, dif2, dif3, dif4, dif5, dif6) %>%
  reduce(full_join, by='GatesD') %>% 
  slice(-23)
  
#df <- merge(alldf1, alldf2, by = "Gates")

df <- list(alldf1, alldf2, alldf3, alldf4, alldf5, alldf6) %>%
  reduce(full_join, by='Gates')

# GRAPHS
# Graph with all

#ggplot(alldf2, aes(x=Gates)) + 
#  geom_line(aes(y = Cntr2), color = "#000000", size = 1)

#ggplot(dif2, aes(x=GatesD)) + 
#  geom_line(aes(y = Diff2), color = "#000000", size = 1)

## colors "#0072B2", "#D55E00", "#CC79A7"
## style of line # linetype = "twodash"

plt1 <- ggplot(df, aes(x=Gates)) + 
  geom_line(aes(y = Cntrl), color = "#000000", size = 1) + 
  geom_line(aes(y = Cntr2), color = "#E69F00", size = 1) +
  geom_line(aes(y = Cntr3), color = "#56B4E9", size = 1) +
  geom_line(aes(y = Cntr4), color = "#009E73", size = 1) +
  geom_line(aes(y = Cntr5), color = "#F0E442", size = 1) +
  geom_line(aes(y = Cntr6), color = "#0072B2", size = 1) +
  ggtitle("Control/Dummy per Gate") +
  labs(y = "% Cell", x = "Gates")

# SD of rows 

sddf <- df
sddf$row_std = rowSds(as.matrix(sddf[,c(-2)]))
sddf$row_avg = rowMeans(as.matrix(sddf[,c(-2,-8)])) 
sddf <- sddf  %>%  select(2,8,9)

## PLOT OF AVG AND SD ##

plt2 <- ggplot(sddf, aes(x=Gates)) + 
  geom_line(aes(y = row_avg), color = "#000000", size = 1.1) + 
  geom_errorbar(aes(ymin = row_avg-row_std, ymax = row_avg+row_std), color = "#000000", width=.5) +
  geom_point(aes(y = row_avg), size=2, shape=21, fill="white") +
  ggtitle("Avg Control/Dummy per Gate") +
  labs(y = "% Cell", x = "Gates")


## MEASURE DIFFERENCE PER GATE GRAPH ##



plt3 <- ggplot(alldif, aes(x=GatesD)) + 
  geom_line(aes(y = Diff1), color = "#000000", size = 1) + 
  geom_line(aes(y = Diff2), color = "#E69F00", size = 1) +
  geom_line(aes(y = Diff3), color = "#56B4E9", size = 1) +
  geom_line(aes(y = Diff4), color = "#009E73", size = 1) +
  geom_line(aes(y = Diff5), color = "#F0E442", size = 1) +
  geom_line(aes(y = Diff6), color = "#0072B2", size = 1) +
  ggtitle("Growth Change in Control/Dummy per Gate") +
  labs(y = "Speed of Change", x = "Gates")

# SD of rows 

sddif <- alldif
sddif$row_std = rowSds(as.matrix(sddif[,c(-1)]))
sddif$row_avg = rowMeans(as.matrix(sddif[,c(-1,-8)])) 
sddif <- sddif  %>%  select(1,8,9)

## PLOT OF AVG AND SD ##

plt4 <- ggplot(sddif, aes(x=GatesD)) + 
  geom_line(aes(y = row_avg), color = "#000000", size = 1.1) + 
  geom_errorbar(aes(ymin = row_avg-row_std, ymax = row_avg+row_std), color = "#000000", width=.5) +
  geom_point(aes(y = row_avg), size=2, shape=21, fill="white") +
  ggtitle("Avg Growth Change in Control/Dummy per Gate") +
  labs(y = "Speed of Change", x = "Gates")




ggarrange(plt1, plt2, plt3, plt4,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)














