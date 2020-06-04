#ERP_Variability.R
#ERP_Variability.Rproj
#Created by RLS on 05/30/20 
#Last updated by RLS on 06/03/20
#Google Drive/WELL Lab/R/ERP_Variability_Plots/ 
#
## This script was created to make a data visualization displaying subject variability for ERPs per an editor's request: 
# "Failing that, would it be possible to add a figure that plots, e.g., N2 and SP amplitude 
#  by subject, so that readers can get a sense of how much inter-subject variability there was?"
#   As one of the concerns with a small sample size is, of course, the question of how representative
#   of the population the sample is, a nice quantification 
#   of the inter-subject variability might be reassuring."
# 
# This script plots data overlaid for each condition (congruent/incongruent/switch) for each subject faceted by 
# color for group. 3 different ERPs: N200, N450, SP. N200/N450 were selected for congruent/incongruent trials only. 
# SP was selected for switch trials only. 
# 
# N200 electrodes = (A25, A26, A29, A30, A31, B23, B26, B27, B28, B29, B30), note this may change yet...N200 Time window = 210 - 300ms (this may also change)
# N450 electrodes = (A25, B21, B22, B28) N450 Time window = ???
# SP electrodes = (A15, A24, B20, B21) SP Time Window = 400 - 800ms

#----------------------------------------------------------------------------------------------------------
#Open up necessary packages 
library (tidyverse)
library(readxl)
library(here)
library(glue)

# vector of folder path names
folders <- str_subset(list.files(here("data")), "Time File", negate = TRUE)
folder_paths <- glue("/Users/ian/tmp/Bilingual-color-word-stroop-project/data/{folders}/")

# concenate all folder paths
all_files <- c(glue("{folder_paths[1]}{list.files(folder_paths[1])}"),
               glue("{folder_paths[2]}{list.files(folder_paths[2])}"),
               glue("{folder_paths[3]}{list.files(folder_paths[3])}"),
               glue("{folder_paths[4]}{list.files(folder_paths[4])}"),
               glue("{folder_paths[5]}{list.files(folder_paths[5])}"),
               glue("{folder_paths[6]}{list.files(folder_paths[6])}"),
               glue("{folder_paths[7]}{list.files(folder_paths[7])}"),
               glue("{folder_paths[8]}{list.files(folder_paths[8])}"),
               glue("{folder_paths[9]}{list.files(folder_paths[9])}"),
               glue("{folder_paths[10]}{list.files(folder_paths[10])}"),
               glue("{folder_paths[11]}{list.files(folder_paths[11])}"),
               glue("{folder_paths[12]}{list.files(folder_paths[12])}")
               )

# pre-allocate space
dat <- as_tibble(matrix(data = NA_real_, nrow = 167232, ncol = 70))

# read in all data and make variables for participant id number and condition name
dat <- all_files %>% 
  map_dfr(~ {
    read_excel(.x) %>% 
      mutate(pid = as.numeric(str_extract(.x, "[0-9]+")),
             name = basename(dirname(.x)))
  })
