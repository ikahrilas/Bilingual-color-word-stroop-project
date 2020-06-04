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
#----------------------------------------------------------------------------------------------------------
#Open up necessary packages 
library (tidyverse)
library (plyr)
library (readr)
library (ggplot2)
library (ggpubr)
###For additional information about readr:
###https://r4ds.had.co.nz/data-import.html
###https://readr.tidyverse.org/articles/readr.html
###https://readr.tidyverse.org/
###https://cran.r-project.org/web/packages/readr/index.html
#----------------------------------------------------------------------------------------------------------
#
#Load in files from datapath 
#need to figure out how to write a loop on reading in all folders in a single file! 
#ALSO check out the code that Ian has already written for this on GITHUB
##Load data using readr (read_csv)
ESCW_RT <- read_csv("ES_CW_behaviour.csv")
View(ESCW_RT)
#
# Merge data
#
#Average data for electrodes of interest for each condition
#
# Plot averaged electrode data for each subject 