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

adf <- alldf %>% select(matrix_row = 1,  Hits = 2)
bdf <- alldf %>% select(matrix_row = 1,  Hits = 3)
cdf <- alldf %>% select(matrix_row = 1,  Hits = 4)
ddf <- alldf %>% select(matrix_row = 1,  Hits = 5)
edf <- alldf %>% select(matrix_row = 1,  Hits = 6)

# Add column that measure the change
## adf ##

adf1 <- adf %>% mutate(Change = Hits / mean(Hits, trim = 0.1))
adf2 <- adf1 %>% 
  full_join(MatrixLib) %>%
  full_join(adf1,
            by = c("matrix_col" = "matrix_row")) %>%
  rename(G1.x = Change.x) %>%
  rename(G1.y = Change.y) %>%
  select(symbol_raw, symbol, matrix_col, matrix_row, G1.x, G1.y)
  
## bdf ##

bdf1 <- bdf %>% mutate(Change = Hits / mean(Hits, trim = 0.1))
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

