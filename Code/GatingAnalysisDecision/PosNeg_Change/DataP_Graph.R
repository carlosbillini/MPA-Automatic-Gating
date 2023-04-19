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

alldf1 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Gate100to0_23G.csv", 
                  col_types = cols(`Well Type` = col_skip()), 
                  skip = 1) %>% 
  slice(c(342,344)) %>% 
  select(-1)
  
# Fill name

alldf1 <- alldf1[2,] - alldf1[1,]
alldf1 <- rbind(alldf1, newrow)
alldf1 <- as.data.frame(t(alldf1)) %>%
  select(Delta = 1, Gates = 2)

# DATA SET 2 #  

alldf2 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Gate100to0_23G_AZ01.csv", 
                  col_types = cols(`Well Type` = col_skip()), 
                  skip = 1) %>% 
  slice(c(342,344)) %>% 
  select(-1)

alldf2 <- alldf2[2,] - alldf2[1,]
alldf2 <- rbind(alldf2, newrow)
alldf2 <- as.data.frame(t(alldf2)) %>%
  select(Delta2 = 1, Gates = 2)

# DATA SET 3 #

alldf3 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Gate100to0_23G_AZ03.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
  slice(c(342,344)) %>% 
  select(-1)

alldf3 <- alldf3[2,] - alldf3[1,]
alldf3 <- rbind(alldf3, newrow)
alldf3 <- as.data.frame(t(alldf3)) %>%
  select(Delta3 = 1, Gates = 2)

# DATA SET 4 #

alldf4 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Gate100to0_23G_AZ04.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
  slice(c(342,344)) %>% 
  select(-1)

alldf4 <- alldf4[2,] - alldf4[1,]
alldf4 <- rbind(alldf4, newrow)
alldf4 <- as.data.frame(t(alldf4)) %>%
  select(Delta4 = 1, Gates = 2)

# DATA SET 5 #

alldf5 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Gate100to0_23G_AZ07.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
  slice(c(342,344)) %>% 
  select(-1)

alldf5 <- alldf5[2,] - alldf5[1,]
alldf5 <- rbind(alldf5, newrow)
alldf5 <- as.data.frame(t(alldf5)) %>%
  select(Delta5 = 1, Gates = 2)

# DATA SET 6 #

alldf6 <- read_csv("~/GitHub/MPA-DNA-tool/Gating/Dataset/Gate100to0_23G_PARA01.csv", 
                   col_types = cols(`Well Type` = col_skip()), 
                   skip = 1) %>% 
  slice(c(342,344)) %>% 
  select(-1)

alldf6 <- alldf6[2,] - alldf6[1,]
alldf6 <- rbind(alldf6, newrow)
alldf6 <- as.data.frame(t(alldf6)) %>%
  select(Delta6 = 1, Gates = 2)

# Merge all data

#df <- merge(alldf1, alldf2, by = "Gates")

df <- list(alldf1, alldf2, alldf3, alldf4, alldf5, alldf6) %>%
  reduce(full_join, by='Gates')

# Graph with all

## colors "#0072B2", "#D55E00", "#CC79A7"
## style of line # linetype = "twodash"

ggplot(df, aes(x=Gates)) + 
  geom_line(aes(y = Delta), color = "#000000", size = 1) + 
  geom_line(aes(y = Delta2), color = "#E69F00", size = 1) +
  geom_line(aes(y = Delta3), color = "#56B4E9", size = 1) +
  geom_line(aes(y = Delta4), color = "#009E73", size = 1) +
  geom_line(aes(y = Delta5), color = "#F0E442", size = 1) +
  geom_line(aes(y = Delta6), color = "#0072B2", size = 1)



# SD of rows 

sddf <- df
sddf$row_std = rowSds(as.matrix(sddf[,c(-2)]))
sddf$row_avg = rowMeans(as.matrix(sddf[,c(-2,-8)])) 
sddf <- sddf  %>%  select(2,8,9)

## PLOT OF AVG AND SD ##

ggplot(sddf, aes(x=Gates)) + 
  geom_line(aes(y = row_avg), color = "#000000", size = 1.1) + 
  geom_errorbar(aes(ymin = row_avg-row_std, ymax = row_avg+row_std), color = "#000000", width=.5) +
  geom_point(aes(y = row_avg), size=2, shape=21, fill="white")







# Select Controls

PosNegCn <- alldf1[c(342,344),c(2:24)]
PosNegCn <- PosNegCn[2,] - PosNegCn[1,]
#PosNegCn <- PosNegCn %>% select(G1 = 1, G2 = 2, G3 = 3, G4 = 4, G5 = 5)

newrow <- c(1:24)
df <- rbind(PosNegCn, newrow)

df1 <- as.data.frame(t(df1))
df1 <- df1 %>% select(Delta = 1, Gates = 2)

ggplot(df1, aes(x=Gates, y=Delta)) + geom_line()









# Organize data #

adf <- alldf %>% select(matrix_row = 1,  Hits = 2)
bdf <- alldf %>% select(matrix_row = 1,  Hits = 3)
cdf <- alldf %>% select(matrix_row = 1,  Hits = 4)
ddf <- alldf %>% select(matrix_row = 1,  Hits = 5)
edf <- alldf %>% select(matrix_row = 1,  Hits = 6)

# Add column that measure the change
## adf ##

adf1 <- adf %>% mutate(Change = abs(mean(Hits) - Hits))
adf2 <- adf1 %>% 
  full_join(MatrixLib) %>%
  full_join(adf1,
            by = c("matrix_col" = "matrix_row")) %>%
  rename(G1.x = Change.x) %>%
  rename(G1.y = Change.y) %>%
  select(symbol_raw, symbol, matrix_col, matrix_row, G1.x, G1.y)
  
## bdf ##

bdf1 <- bdf %>% mutate(Change = abs(mean(Hits) - Hits))
bdf2 <- bdf1 %>% 
  full_join(MatrixLib) %>%
  full_join(bdf1,
            by = c("matrix_col" = "matrix_row")) %>%
  rename(G2.x = Change.x) %>%
  rename(G2.y = Change.y) %>%
  select(symbol_raw, symbol, matrix_col, matrix_row, G2.x, G2.y)

## cdf ##

cdf1 <- cdf %>% mutate(Change = abs(mean(Hits) - Hits))
cdf2 <- cdf1 %>% 
  full_join(MatrixLib) %>%
  full_join(cdf1,
            by = c("matrix_col" = "matrix_row")) %>%
  rename(G3.x = Change.x) %>%
  rename(G3.y = Change.y) %>%
  select(symbol_raw, symbol, matrix_col, matrix_row, G3.x, G3.y)

## ddf ##

ddf1 <- ddf %>% mutate(Change = abs(mean(Hits) - Hits))
ddf2 <- ddf1 %>% 
  full_join(MatrixLib) %>%
  full_join(ddf1,
            by = c("matrix_col" = "matrix_row")) %>%
  rename(G4.x = Change.x) %>%
  rename(G4.y = Change.y) %>%
  select(symbol_raw, symbol, matrix_col, matrix_row, G4.x, G4.y)

## edf ##

edf1 <- edf %>% mutate(Change = abs(mean(Hits) - Hits))
edf2 <- edf1 %>% 
  full_join(MatrixLib) %>%
  full_join(edf1,
            by = c("matrix_col" = "matrix_row")) %>%
  rename(G5.x = Change.x) %>%
  rename(G5.y = Change.y) %>%
  select(symbol_raw, symbol, matrix_col, matrix_row, G5.x, G5.y)

## DELETE DF ##

rm(adf1, bdf1, cdf1, ddf1, edf1, adf, bdf, cdf, ddf, edf)

# Make the labels overlap if they need too

options(ggrepel.max.overlaps = Inf)

##### Analysis - 2D Scatter Plot #####
## PLOT MUTIPLE DF ##


mplot <- ggplot() + 
  geom_point(data = adf2, aes(x=G1.x, y=G1.y), color = 'blue') +
  geom_point(data = bdf2, aes(x=G2.x, y=G2.y), color = 'red') +
  geom_point(data = cdf2, aes(x=G3.x, y=G3.y), color = 'green') +
  geom_point(data = ddf2, aes(x=G4.x, y=G4.y), color = 'orange') +
  geom_point(data = edf2, aes(x=G5.x, y=G5.y), color = 'brown')

mplot




### PLOT FILTERING 50% DOWN ###

adf3 <- filter(adf2, G1.x > g1x & G1.y > g1y)
bdf3 <- filter(bdf2, G2.x > g2x & G2.y > g2y)
cdf3 <- filter(cdf2, G3.x > g3x & G3.y > g3y)
ddf3 <- filter(ddf2, G4.x > g4x & G4.y > g4y)
edf3 <- filter(edf2, G5.x > g5x & G5.y > g5y)

ggplot() + 
  geom_point(data = adf3, aes(x=G1.x, y=G1.y), color = 'blue') +
  geom_point(data = bdf3, aes(x=G2.x, y=G2.y), color = 'red') +
  geom_point(data = cdf3, aes(x=G3.x, y=G3.y), color = 'green') +
  geom_point(data = ddf3, aes(x=G4.x, y=G4.y), color = 'orange') +
  geom_point(data = edf3, aes(x=G5.x, y=G5.y), color = 'brown')



### Plot average per gate

g1x <- mean(adf2$G1.x, na.rm = TRUE)
g2x <- mean(bdf2$G2.x, na.rm = TRUE)
g3x <- mean(cdf2$G3.x, na.rm = TRUE)
g4x <- mean(ddf2$G4.x, na.rm = TRUE)
g5x <- mean(edf2$G5.x, na.rm = TRUE)

g1y <- mean(adf2$G1.y, na.rm = TRUE)
g2y <- mean(bdf2$G2.y, na.rm = TRUE)
g3y <- mean(cdf2$G3.y, na.rm = TRUE)
g4y <- mean(ddf2$G4.y, na.rm = TRUE)
g5y <- mean(edf2$G5.y, na.rm = TRUE)

 

GX <- c(g1x, g2x, g3x, g4x, g5x)
GY <- c(g1y, g2y, g3y, g4y, g5y)

gtschange <- data.frame(GX, GY)

ggplot() + geom_point(data = gtschange, aes(x=GX, y=GY), color = 'blue')

## MAX - MIN ##

gm1x <- 
  max(adf2$G1.x, na.rm = TRUE) - min(adf2$G1.x, na.rm = TRUE)

