#ERP_Variability.R
#ERP_Variability.Rproj
#Created by RLS on 05/30/20 
#Last updated by RLS on 05/30/20
#Google Drive/WELL Lab/R/ERP_Variability_Plots/ 
#
## This script was created to make a data visualization displaying subject variability for ERPs per an editor's request: 
# "Failing that, would it be possible to add a figure that plots, e.g., N2 and SP amplitude 
#  by subject, so that readers can get a sense of how much inter-subject variability there was?"
#   As one of the concerns with a small sample size is, of course, the question of how representative
#   of the population the sample is, a nice quantification 
#   of the inter-subject variability might be reassuring."
# 
# Plots data overlaid for each condition (congruent/incongruent) for each subject faceted by 
# color for group. 
#----------------------------------------------------------------------------------------------------------
#Open up necessary packages 
library (tidyverse)
library (plyr)
library (readr)
library (ggplot2)
library (ggpubr)
library(readxl)
library(stringr)

#----------------------------------------------------------------------------------------------------------
#
#Load in files from datapath 
#need to figure out how to write a loop on reading in all folders in a single file! 
#ALSO check out the code that Ian has already written for this on GITHUB
#THINK MORE ABOUT HOW TO ORGANIZE DATASET AND WHAT FINAL PLOTS NEED TO LOOK LIKE!
##
##
# Set working directory for Bilinguals Congruent
setwd("/Users/rsilton/Google Drive/WELL LAB /R/ERP_Variability_Plots/EMSE Files for Becky/Bilinguals Congruent")

# Create file list for the folder
file_list <- list.files(path = wd)

# Read in a single file...
Bilinguals_Congruent <- read_excel("1009 congruent.xlsx")

#Create average of electodes for N200 (A25, A26, A29, A30, A31, B23, B26, B27, B28, B28, B30)
# A25 = column 27
# A26 = column 28
# A29 = column 31
# A30 = column 32
# A31 = column 33
# B23 = column 57
# B26 = column 60
# B27 = column 61
# B28 = column 62
# B29??? = column 63
# B30 = column 64
# 
# Create dataframe for electrode channels of interest OR figure out how to average across channnels of interest
# THIS IS HOW YOU REFERENCE A COLUMN: Bilinguals_Congruent$A25
Bilinguals_Congruent_N200 <- data.frame()
# 
Bilinguals_Congruent_N200 <- Bilinguals_Congruent$A25
Bilinguals_Congruent_N200 <- Bilinguals_Congruent$A26

#Create average of electrodes for N450 ???
  Bilinguals_Congruent$A25

#Create average of electrodes for SP (A15, A24, B20, B21)
#A15 = column 17 
#A24 = column 26
#B20 = column 54
#B21 = column 55

# Plot data
ggplot (data=Bilinguals_Congruent)



# Create Bilingual data frame
#Bilinguals_Congruent_dataset <- data.frame()

#for (i in 1:length(file_list))
#{Bilinguals_Congruent_dataset <- read_excel(file_list[i])}





#
#Average data for electrodes of interest for each condition
#
# Plot averaged electrode data for each subject 