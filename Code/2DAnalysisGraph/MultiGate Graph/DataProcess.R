#Calling the data frames

library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(tidyverse)
# Upload 

##### SAFE Screen #####

df1 <- read_csv("C:/Users/cbillini/Desktop/Stat-Tinker/R Workspace/2D Plot Screen/DATA test/Screen_NM24(1)_HEK_101822_CB.csv", 
                skip = 1)

##### Matrix Library #####

MatrixLib <- read_csv("C:/Users/cbillini/Desktop/Stat-Tinker/R Workspace/2D Plot Screen/DATA test/MatrixLib.csv") %>% 
                      select(symbol_raw, symbol, matrix_col, matrix_row)

## Data set 1 ##                      

alldf <- read_csv("C:/Users/cbillini/Desktop/Stat-Tinker/R Workspace/Gating/Dataset/dataset 1.csv", 
                               col_types = cols(...8 = col_skip()), 
                               skip = 1)

# Graph 1 #

af1 = alldf %>% select(matrix_row = 1,  Hits = 3)
af1o = af1 %>% filter((Hits - mean(Hits)) / sd(Hits) < 2)
af2 = af1 %>% mutate(S_N = Hits / mean(af1o$Hits)) %>%
select(S_N, matrix_row)
af3 = af2 %>%
full_join(MatrixLib) %>%
      rename(S_N.x = S_N) %>%
full_join(af1,
         by = c("matrix_col" = "matrix_row"))%>%
select(symbol_raw, symbol, matrix_col, matrix_row, S_N.x, S_N.y = Hits)

rm(af1, af1o, af2)

# Graph 2 #

bf1 = alldf %>% select(matrix_row = 1,  Hits = 4)
bf1o = bf1 %>% filter((Hits - mean(Hits)) / sd(Hits) < 2)
bf2 = bf1 %>% mutate(S_N = Hits / mean(bf1o$Hits)) %>%
  select(S_N, matrix_row)
bf3 = bf2 %>%
  full_join(MatrixLib) %>%
  rename(S_N.x = S_N) %>%
  full_join(bf1,
            by = c("matrix_col" = "matrix_row"))%>%
  select(symbol_raw, symbol, matrix_col, matrix_row, S_N.x, S_N.y = Hits)

rm(bf1, bf1o, bf2)

# Graph 3 #

cf1 = alldf %>% select(matrix_row = 1,  Hits = 5)
cf1o = cf1 %>% filter((Hits - mean(Hits)) / sd(Hits) < 2)
cf2 = cf1 %>% mutate(S_N = Hits / mean(cf1o$Hits)) %>%
  select(S_N, matrix_row)
cf3 = cf2 %>%
  full_join(MatrixLib) %>%
  rename(S_N.x = S_N) %>%
  full_join(cf1,
            by = c("matrix_col" = "matrix_row"))%>%
  select(symbol_raw, symbol, matrix_col, matrix_row, S_N.x, S_N.y = Hits)

rm(cf1, cf1o, cf2)

# Graph 4 #

df1 = alldf %>% select(matrix_row = 1,  Hits = 6)

ef1 = alldf %>% select(matrix_row = 1,  Hits = 7)




# def variables for formulas

meanofhits = mean(df1$`Hits`)
sdofhits = sd(df1$`Hits`)

# create column with outliers

df1$Outliers1 = df1$`Hits` - meanofhits
df1$Outliers2 = df1$Outliers1/ sdofhits

# filter outliers with more than 2

df2o = filter(df1, Outliers2 < 2)
df2 = df1
# 2D analysis

avg.df2o = mean(df2o$`Hits`) #add a variable
df2$S_N = df2$`Hits` / avg.df2o

# Merger data set with Matrix Library

df2$matrix_row = df2$`ID`
df2$matrix_col = df2$`ID`
df2 = df2 %>% select(S_N, matrix_row, matrix_col)
MatrixLib = MatrixLib %>% select(symbol_raw, symbol, matrix_col, matrix_row)
df3 = merge(MatrixLib,df2, by='matrix_row')
df3 = df3 %>% select(symbol_raw, symbol, matrix_row, S_N, matrix_col.x)
df3 = rename(df3, "matrix_col" = "matrix_col.x")
df4 = df3 %>% select(symbol_raw, symbol, matrix_col)
df5 = merge(df2,df4, by='matrix_col')
df5 = df5 %>% select(symbol_raw, symbol, matrix_col, S_N)
df3 = df3 %>% select(symbol_raw, symbol, matrix_row, S_N)
df6 = merge(df3,df5, by='symbol_raw')
df6 = rename(df6, 
             symbol = symbol.x,
             S_N.x = S_N.y,
             S_N.y = S_N.x)

# Order column

df6 = df6 %>% select(symbol_raw,symbol,matrix_col,matrix_row,S_N.x,S_N.y)

# Find the Threshold lines

Noise.x = mean(df6$S_N.x)
Noise.y = mean(df6$S_N.y)

sd.x = sd(df6$S_N.x)
sd.y = sd(df6$S_N.y)

# Make the labels overlap if they need too

options(ggrepel.max.overlaps = Inf)

##### Analysis - 2D Scatter Plot #####

#theplot = ggplot(df6, aes(x=S_N.x, y=S_N.y, label=symbol)) + 
#  geom_label_repel(aes(label = ifelse(S_N.y>5 & S_N.x>5,as.character(symbol),'')),
#                   box.padding   = 1.35, 
#                   point.padding = 1.5,
#                   segment.color = 'grey50')  +
#  geom_point(color = dplyr::case_when(df6$S_N.x > 5 & df6$S_N.y > 5 ~ "#d95f02",
#                                      TRUE ~ "#7570b3"), size = 2, alpha = 0.8)
#
#theplot


##### Delete data frames #####

rm(df1, df2, df3, df4, df5)
rm(avg.df2o, meanofhits, sdofhits)