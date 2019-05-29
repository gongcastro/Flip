# CROSS-LABS FAMILIARITY VS. NOVELTY PREFERENCE IN LEARNING STUDIES
# effect of prior headturn studies on direction of preference (familiar vs. novel) in subsequent experiment 
# comparison across 3 datasets : syntax_wisconsin, syntax_barcelona, jenny's old dataset
# dataset 1 name (only two syntax studies) : ALL_SYNTAX_corr2.csv
# dataset 2 name (two syntax + jenny's old) : ALL_SYNTAX_corr3.csv

# load packages
library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)

# syntax_wisconsin & syntax_barcelona 
# import data 
# change the variable "DiffScore" into numeric 
data<- read.csv("/Users/ChiaraSantolin/Desktop/PostDoc/ALL_SYNTAX_corr2.csv", header=TRUE, sep=",", dec=",")
data
data$DiffScore <- as.numeric(as.character(data$DiffScore))
data

# run Pearson correlation between DiffScore and NumHT (number of heaturn studies before running syntax)
cor.test (data$DiffScore, data$NumHT, type = "pearson")

# syntax_wisconsin, syntax_barcelona & jenny's old dataset
# import data 
# change the variable "DiffScore" into numeric 
data<- read.csv("/Users/ChiaraSantolin/Desktop/PostDoc/ALL_SYNTAX_corr3.csv", header=TRUE, sep=",", dec=",")
data
data$DiffScore <- as.numeric(as.character(data$DiffScore))
data

# run Pearson correlation between DiffScore and NumHT (number of heaturn studies before running syntax)
# this is non significant; Jenny's old dataset includes babies that are not supposed to be there; 
# we also dont know babies' age, and type of study 
cor.test (data$DiffScore, data$NumHT, type = "pearson")
