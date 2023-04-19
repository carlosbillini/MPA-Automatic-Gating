# READ ME

# Intro
  # This code makes 2 graphs
  # One of them makes the classic 2D graph but with several gates (4 Gates) at the same time
  # The other one does the graph but only the ones above 3SD from the average
    # Additionally, it (manually) draws an arrow from the lowest to the highest. 
    

# Index
  # - Libraries
  # - Read all data
  # - Organize data
  # - Selects 4 gates manually
  # - Threshold line
  # - Plot



# Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(tidyverse)


# Read all data #

alldf <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Growth_Control/Gate_20000step_AZ01.csv", 
                      col_types = cols(`Well Type` = col_skip(), 
                                       ...8 = col_skip()), skip = 1)

MatrixLib <- read_csv("C:/Users/cbillini/Desktop/Stat-Tinker/R Workspace/2D Plot Screen/DATA test/MatrixLib.csv") %>% 
  select(symbol_raw, symbol, matrix_col, matrix_row)

# Organize data #
  # Selects 4 gates manually

SelectedGates <- alldf %>% select(1, 11, 12, 15, 16)


for(i in 1:5){
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


# Threshold line


Manual_3SD <- filter(SelectedGates2, 
                     S_N1.y >= sd(SelectedGates2$S_N1.y) * 3 + mean(SelectedGates2$S_N1.y, trim = 0.1) & 
                       S_N1.x >= sd(SelectedGates2$S_N1.x) * 3 + mean(SelectedGates2$S_N1.x, trim = 0.1))

ggplot() + 
  geom_point(data = Manual_3SD, aes(x=S_N1.x, y=S_N1.y), color = 'blue', size = 3) +
  geom_point(data = Manual_3SD, aes(x=S_N2.x, y=S_N2.y), color = 'red', size = 3) +
  geom_point(data = Manual_3SD, aes(x=S_N3.x, y=S_N3.y), color = 'green', size = 3) +
  geom_point(data = Manual_3SD, aes(x=S_N4.x, y=S_N4.y), color = 'orange', size = 3) +
  geom_segment(aes(x=14.62, y=11.32, xend=38.36, yend=28.29), arrow = arrow(length=unit(.5, 'cm'))) +
  geom_segment(aes(x=7.374282, y=11.321783, xend=10.34003, yend=28.299504), arrow = arrow(length=unit(.5, 'cm'))) +
  geom_segment(aes(x=6.790305, y=8.101092, xend=18.13227, yend=20.034027), arrow = arrow(length=unit(.5, 'cm'))) +
  geom_segment(aes(x=6.790305, y=7.456589, xend=18.13227, yend=7.585318), arrow = arrow(length=unit(.5, 'cm'))) +
  geom_point(aes(x=25.7,y=19.2, colour="red"), size = 10)

xmean <- Manual_3SD[,5:8] 
xmean$mean <- rowMeans(xmean)
xmean <- xmean %>% select(5)

ymean <- Manual_3SD[,9:12]
ymean$mean <- rowMeans(ymean)
ymean <- ymean %>% select(5)


# Plot
mplot <- ggplot() + 
  geom_point(data = SelectedGates2, aes(x=S_N1.x, y=S_N1.y), color = 'blue', size = 2) +
  geom_point(data = SelectedGates2, aes(x=S_N2.x, y=S_N2.y), color = 'red', size = 2) +
  geom_point(data = SelectedGates2, aes(x=S_N3.x, y=S_N3.y), color = 'green', size = 2) +
  geom_point(data = SelectedGates2, aes(x=S_N4.x, y=S_N4.y), color = 'orange', size = 2)






