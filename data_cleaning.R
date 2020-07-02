# load packages
library(tidyverse)
library(haven)

# read in data
dat <- read_sav("data/Analyses/dat_6.26.2020.sav")
glimpse(dat)

# restructure dataset to long form
## deal with N200
N200 <- dat %>% 
  select(PID, Group, contains("N200"))

N200_amp <- N200 %>% 
  select(PID, Group, contains("amp")) %>% 
  select(PID, Group, !contains("new")) %>% 
  select(PID, Group, !contains("updated")) %>% 
  pivot_longer(cols = N200_Congruent_MeanAmp:N200_Switch_N_MeanAmp,
               names_to = "condition",
               values_to = "N200_mean_amp") %>% 
  mutate(condition = str_remove(condition, "N200_"),
         condition = str_remove(condition, "_MeanAmp"),
         condition = tolower(condition),
         condition = str_replace_all(condition, "switch_n", "switch_no"),
         condition = str_replace_all(condition, "switch_y", "switch_yes"))

N200_amp_updated <- N200 %>% 
  select(PID, Group, contains("amp")) %>% 
  select(PID, Group, !contains("new")) %>% 
  select(PID, Group, contains("updated")) %>% 
  pivot_longer(cols = Updated_N200_Congruent_Amp:Updated_N200_Switch_Yes_Amp,
               names_to = "condition",
               values_to = "N200_updated_mean_amp") %>% 
  mutate(condition = str_remove(condition, "Updated_N200_"),
         condition = str_remove(condition, "_Amp"),
         condition = tolower(condition))

N200_amp_new <- N200 %>% 
  select(PID, Group, contains("amp")) %>% 
  select(PID, Group, contains("new")) %>% 
  select(PID, Group, !contains("updated")) %>% 
  pivot_longer(cols = N200_NEW_Congruent_MeanAMP:N200_NEW_Incongruent_MeanAmp,
               names_to = "condition",
               values_to = "N200_new_mean_amp") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n200_new_"),
         condition = str_remove(condition, "_meanamp"))

N200_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>% 
  select(PID, Group, !contains("frac")) %>% 
  select(PID, Group, !contains("update")) %>% 
  pivot_longer(cols = N200_Pure_Congruent_LAT:N200_Switch_N_LAT,
               names_to = "condition",
               values_to = "N200_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n200_"),
         condition = str_remove(condition, "_lat"),
         condition = str_replace_all(condition, "switch_y", "switch_yes"),
         condition = str_replace_all(condition, "switch_n", "switch_no"))

N200_updated_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>% 
  select(PID, Group, !contains("frac")) %>% 
  select(PID, Group, contains("update")) %>% 
  pivot_longer(cols = Updated_N200_Congruent_LAT:Updated_N200_Switch_Yes_LAT,
               names_to = "condition",
               values_to = "N200_updated_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "update_n200_"),
         condition = str_remove(condition, "updated_n200_"),
         condition = str_remove(condition, "_lat"))

N200_frac_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>% 
  select(PID, Group, contains("frac")) %>% 
  select(PID, Group, !contains("new")) %>% 
  pivot_longer(cols = c("N200_Congruent_FracLat", "N200_Incongruent_FracLat"),
               names_to = "condition",
               values_to = "N200_frac_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n200_"),
         condition = str_remove(condition, "_fraclat"))

N200_new_frac_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>% 
  select(PID, Group, contains("frac")) %>% 
  select(PID, Group, contains("new")) %>% 
  pivot_longer(cols = c("N200_NEW_Congruent_FracLat", "N200_New_Incongruent_FracLat"),
               names_to = "condition",
               values_to = "N200_new_frac_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n200_new_"),
         condition = str_remove(condition, "_fraclat"))

## join all N200 data together
N200_long <- left_join(N200_amp, N200_amp_updated, by = c("PID", "Group", "condition")) %>% 
  left_join(., N200_amp_new, by = c("PID", "Group", "condition")) %>% 
  left_join(., N200_lat, by = c("PID", "Group", "condition")) %>% 
  left_join(., N200_updated_lat, by = c("PID", "Group", "condition")) %>% 
  left_join(., N200_updated_lat, by = c("PID", "Group", "condition")) %>%
  left_join(., N200_frac_lat, by = c("PID", "Group", "condition")) %>% 
  left_join(.,N200_new_frac_lat, by = c("PID", "Group", "condition"))

# deal with N450 data
N450 <- dat %>% 
  select(PID, Group, contains("N450"))

N200_amp <- N200 %>% 
  select(PID, Group, contains("amp")) %>% 
  select(PID, Group, !contains("new")) %>% 
  select(PID, Group, !contains("updated")) %>% 
  pivot_longer(cols = N200_Congruent_MeanAmp:N200_Switch_N_MeanAmp,
               names_to = "condition",
               values_to = "N200_mean_amp") %>% 
  mutate(condition = str_remove(condition, "N200_"),
         condition = str_remove(condition, "_MeanAmp"),
         condition = tolower(condition),
         condition = str_replace_all(condition, "switch_n", "switch_no"),
         condition = str_replace_all(condition, "switch_y", "switch_yes"))

N200_amp_updated <- N200 %>% 
  select(PID, Group, contains("amp")) %>% 
  select(PID, Group, !contains("new")) %>% 
  select(PID, Group, contains("updated")) %>% 
  pivot_longer(cols = Updated_N200_Congruent_Amp:Updated_N200_Switch_Yes_Amp,
               names_to = "condition",
               values_to = "N200_updated_mean_amp") %>% 
  mutate(condition = str_remove(condition, "Updated_N200_"),
         condition = str_remove(condition, "_Amp"),
         condition = tolower(condition))

N200_amp_new <- N200 %>% 
  select(PID, Group, contains("amp")) %>% 
  select(PID, Group, contains("new")) %>% 
  select(PID, Group, !contains("updated")) %>% 
  pivot_longer(cols = N200_NEW_Congruent_MeanAMP:N200_NEW_Incongruent_MeanAmp,
               names_to = "condition",
               values_to = "N200_new_mean_amp") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n200_new_"),
         condition = str_remove(condition, "_meanamp"))

N200_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>% 
  select(PID, Group, !contains("frac")) %>% 
  select(PID, Group, !contains("update")) %>% 
  pivot_longer(cols = N200_Pure_Congruent_LAT:N200_Switch_N_LAT,
               names_to = "condition",
               values_to = "N200_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n200_"),
         condition = str_remove(condition, "_lat"),
         condition = str_replace_all(condition, "switch_y", "switch_yes"),
         condition = str_replace_all(condition, "switch_n", "switch_no"))

N200_updated_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>% 
  select(PID, Group, !contains("frac")) %>% 
  select(PID, Group, contains("update")) %>% 
  pivot_longer(cols = Updated_N200_Congruent_LAT:Updated_N200_Switch_Yes_LAT,
               names_to = "condition",
               values_to = "N200_updated_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "update_n200_"),
         condition = str_remove(condition, "updated_n200_"),
         condition = str_remove(condition, "_lat"))

N200_frac_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>% 
  select(PID, Group, contains("frac")) %>% 
  select(PID, Group, !contains("new")) %>% 
  pivot_longer(cols = c("N200_Congruent_FracLat", "N200_Incongruent_FracLat"),
               names_to = "condition",
               values_to = "N200_frac_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n200_"),
         condition = str_remove(condition, "_fraclat"))

N200_new_frac_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>% 
  select(PID, Group, contains("frac")) %>% 
  select(PID, Group, contains("new")) %>% 
  pivot_longer(cols = c("N200_NEW_Congruent_FracLat", "N200_New_Incongruent_FracLat"),
               names_to = "condition",
               values_to = "N200_new_frac_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n200_new_"),
         condition = str_remove(condition, "_fraclat"))

## join all N200 data together
N200_long <- left_join(N200_amp, N200_amp_updated, by = c("PID", "Group", "condition")) %>% 
  left_join(., N200_amp_new, by = c("PID", "Group", "condition")) %>% 
  left_join(., N200_lat, by = c("PID", "Group", "condition")) %>% 
  left_join(., N200_updated_lat, by = c("PID", "Group", "condition")) %>% 
  left_join(., N200_updated_lat, by = c("PID", "Group", "condition")) %>%
  left_join(., N200_frac_lat, by = c("PID", "Group", "condition")) %>% 
  left_join(.,N200_new_frac_lat, by = c("PID", "Group", "condition"))
