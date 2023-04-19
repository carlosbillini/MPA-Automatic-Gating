# READ ME

# Intro 
  # Look for the distribution in the manual gating as coordinates used
  # in the customer projects


# Index
  # - Libraries
  # - Data
  # - plots



# Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(tidyverse)
library(matrixStats)
library(qqplotr)

# Data
GatingCoordinateExamples <- read_csv("GitHub/MPA-DNA-tool/Gating/Dataset/HowManyGates/GatingCoordinateExamples.csv", 
                                     col_types = cols(Customer = col_skip()))


#final = as.numeric(unlist(GatingCoordinateExamples))
#class(GatingCoordinateExamples)



#plots

hist <- ggplot(GatingCoordinateExamples, aes(x=Coordinates)) + 
  geom_histogram(color="black", fill="gray", bins = 20) +
  geom_vline(aes(xintercept=mean(Coordinates)),
             color="blue", linetype="dashed", size=1) +
  geom_density() +
  ggtitle("Histogram for Coordinates") +
  labs(y = "Density", x = "Coordinates") 


QQplot <- ggplot(GatingCoordinateExamples, aes(sample = Coordinates)) +
  stat_qq_point(size = 2,color = "red") +
  stat_qq_line(color="green") +
  xlab("x-axis") + 
  ylab("y-axis") +
  ggtitle("Q-Q Plot") +
  labs(y = "Coordinates", x = "Dunno")


densityplot <- ggplot(GatingCoordinateExamples, aes(x=Coordinates)) + 
  geom_density(color="darkblue",linetype="dashed", fill="lightblue") +
  geom_vline(aes(xintercept=mean(Coordinates)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Distribution") +
  labs(y = "Density", x = "Coordinates") 


ggarrange(hist, densityplot,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)