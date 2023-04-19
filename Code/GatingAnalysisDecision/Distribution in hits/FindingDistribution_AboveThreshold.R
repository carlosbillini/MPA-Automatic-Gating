# READ ME

# Intro 
# Look for the distribution for the number of hits between 2SD and 3SD
# Purpose: A consider variable to select gating that is close to the average 
# number of hits. 


# Index
  # - Libraries
  # - Data
    # - Global data
    # - AZ_01
  # - Processing data for data 1 Gate
  # - ADD SEVERAL DATA FROM FOLDER 
  # - PROCESS FOR FINAL GRAPH
  # - plots
    # - Histograms for Hits 
    # - Density Distribution for Hits 

# Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(tidyverse)
library(matrixStats)
library(qqplotr)
library(data.table)

# Data
# Global data
MatrixLib <- read_csv("GitHub/MPA-DataAnalisys/Screen_Analysis/MatrixLib/MatrixLib.csv") %>%
  select(symbol_raw, symbol, matrix_col, matrix_row)

### AZ_01
OG_gate <- read_csv("Z:/_Employee Folders/Carlos Billini/ForCyt_Data/making locked templates/AZ 01/Manual_Gating/HX1489 AZ01 100722 KO Data.csv", 
                    skip = 1) %>%
  select(matrix_row = 1,  Hits = 3)


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


ABC <- data.frame(sd2 = nrow(Manual_2SD),
                  sd2.5 = nrow(Manual_2.5SD),
                  sd3 = nrow(Manual_3SD))

rm(OG_gate,OG_gate2,Manual_2.5SD,Manual_2SD,Manual_3SD)


#### ADD SEVERAL DATA FROM FOLDER ####

data_files <- list.files("Z:/_Employee Folders/Carlos Billini/ForCyt_Data/AllManualGates")  # Identify file names
data_files

for(i in 1:length(data_files)) {                              # Head of for-loop
  assign(paste0("data", i),                                   # Read and store data frames
         read_csv(paste0("Z:/_Employee Folders/Carlos Billini/ForCyt_Data/AllManualGates/",
                          data_files[i]), skip = 1) %>%
           select("Well ID" = 1,  Hits = 3))
}


#for(i in 1:(length(data_files)-1)) {
#  alldata <- data1 %>% inner_join(paste0("data", i+1),
#                                  by = "Well ID")
#}

alldata <- data1 %>% inner_join(data2,
                                by = "Well ID") %>% 
  inner_join(data3,
             by = "Well ID") %>%
  inner_join(data4,
             by = "Well ID") %>%
  inner_join(data5,
             by = "Well ID") %>%
  inner_join(data6,
             by = "Well ID") %>%
  inner_join(data7,
             by = "Well ID") %>% 
  inner_join(data8,
             by = "Well ID") %>%
  inner_join(data9,
             by = "Well ID") %>%
  inner_join(data10,
             by = "Well ID") %>%
  inner_join(data11,
             by = "Well ID") %>%
  inner_join(data12,
             by = "Well ID") %>%
  inner_join(data13,
             by = "Well ID") %>%
  inner_join(data14,
             by = "Well ID") %>%
  inner_join(data15,
             by = "Well ID") %>%
  inner_join(data16,
             by = "Well ID") %>%
  inner_join(data17,
             by = "Well ID")
  
for(i in 1:length(data_files)){
  colnames(alldata)[i+1] = paste0("Hits", i) 
}  


## PROCESS FOR FINAL GRAPH ## 


for(i in 1:length(data_files)){
  alldata[[paste0('S_N', i)]] <- alldata[[paste0("Hits", i)]] / mean(alldata[[paste0("Hits", i)]], trim = 0.1)
}



alldata <- alldata %>% select(-(2:(1+length(data_files))))

gt2 <- MatrixLib %>%
  inner_join(alldata,
             by = c("matrix_row" = "Well ID")) %>% 
  inner_join(alldata,
             by = c("matrix_col" = "Well ID"))



for(i in 1:length(data_files)){
  
  gtflt <- gt2 %>% select(xloc = i+4, yloc = i+length(data_files)+4)
  
  
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
  
  
  ABC[nrow(ABC) + 1,] <- gt3
}

ABC <- ABC %>% slice(-1)
rm(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,gt2,gt3,gtflt,gtsd1,gtsd2,gtsd3)

# plots
  # Histograms for Hits 

hist1 <- ggplot(ABC, aes(x=sd2)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  geom_vline(aes(xintercept=mean(sd2)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram for Hist above 2SD") +
  labs(y = "Density", x = "Hits")


hist2 <- ggplot(ABC, aes(x=sd2.5)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  geom_vline(aes(xintercept=mean(sd2.5)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram for Hist above 2.5SD") +
  labs(y = "Density", x = "Hits")


hist3 <- ggplot(ABC, aes(x=sd3)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  geom_vline(aes(xintercept=mean(sd3)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram for Hist above 3SD") +
  labs(y = "Density", x = "Hits")


ggarrange(hist1, hist2, hist3,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

  # Density Distribution for Hits 


densityplot1 <- ggplot(ABC, aes(x=sd2)) + 
  geom_density(color="darkblue",linetype="dashed", fill="lightblue") +
  geom_vline(aes(xintercept=mean(sd2)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram for Hist above 2SD") +
  labs(y = "Density", x = "Hits")

densityplot2 <- ggplot(ABC, aes(x=sd2.5)) + 
  geom_density(color="darkblue",linetype="dashed", fill="lightblue") +
  geom_vline(aes(xintercept=mean(sd2.5)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram for Hist above 2.5SD") +
  labs(y = "Density", x = "Hits")

densityplot3 <- ggplot(ABC, aes(x=sd3)) + 
  geom_density(color="darkblue",linetype="dashed", fill="lightblue") +
  geom_vline(aes(xintercept=mean(sd3)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram for Hist above 3SD") +
  labs(y = "Density", x = "Hits")


ggarrange(densityplot1, densityplot2, densityplot3,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)