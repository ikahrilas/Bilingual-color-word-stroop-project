# load packages
library(tidyverse)
library(haven)

# read in data
dat <- read_sav("data/Analyses/dat_6.26.2020.sav")
glimpse(dat)

# retain only those variables that are relevant to analyses
dat_int <- dat %>% 
  select(PID, 
         Group, 
         contains("Updated_SP"), 
         contains("Updated_N200"),
         (contains("N450") & contains("small", ignore.case = TRUE)),
         Update_N200_Mix_Congruent_LAT,
         Update_N200_Mix_Incongruent_LAT,
         Switch_Yes_ACC:Incongruent_RT
         )

# restructure data frame to long form
## deal with accuracy scores
acc <- dat_int %>% 
  select(PID, Group, contains("ACC"))

acc_long <- acc %>% 
  pivot_longer(cols = Switch_Yes_ACC:Incongruent_ACC,
               names_to = "condition",
               values_to = "accuracy") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "_acc"))

## deal with response time scores
rt <- dat_int %>% 
  select(PID, Group, contains("RT"))

rt_long <- rt %>% 
  pivot_longer(cols = Switch_Yes_RT:Incongruent_RT,
               names_to = "condition",
               values_to = "response_time") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "_rt"))

## deal with N200
N200 <- dat_int %>% 
  select(PID, Group, contains("N200"))

N200_amp <- N200 %>% 
  select(PID, Group, contains("amp", ignore.case = TRUE)) %>% 
  pivot_longer(cols = Updated_N200_Congruent_Amp:Updated_N200_Switch_Yes_Amp,
               names_to = "condition",
               values_to = "N200_mean_amp") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "updated_n200_"),
         condition = str_remove(condition, "_amp"))

N200_lat <- N200 %>% 
  select(PID, Group, contains("lat")) %>%
  pivot_longer(cols = Updated_N200_Congruent_LAT :Update_N200_Mix_Incongruent_LAT,
               names_to = "condition",
               values_to = "N200_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "updated_n200_"),
         condition = str_remove(condition, "update_n200_"),
         condition = str_remove(condition, "_lat"))

## join all N200 data together
N200_long <- full_join(N200_amp, N200_lat, by = c("PID", "Group", "condition"))

# deal with N450 data
N450 <- dat_int %>% 
  select(PID, Group, contains("N450"))

N450_amp <- N450 %>% 
  select(PID, Group, contains("mp")) %>%
  pivot_longer(cols = N450_Congruent_Small_Amp:N450_SwitchNo_Small_Amp,
               names_to = "condition",
               values_to = "N450_mean_amp") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n450_"),
         condition = str_remove(condition, "_small_"),
         condition = str_remove(condition, "amp"),
         condition = str_remove(condition, "mp"),
         condition = str_replace_all(condition, "switchyes", "switch_yes"),
         condition = str_replace_all(condition, "switchno", "switch_no")
         )

N450_latency <- N450 %>% 
  select(PID, Group, contains("lat")) %>% 
  pivot_longer(cols = N450_Congruent_Small_Lat:N450_SwitchN_Small_LAT,
               names_to = "condition",
               values_to = "N450_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "n450_"),
         condition = str_remove(condition, "_small_lat"),
         condition = str_replace_all(condition, "switchy", "switch_yes"),
         condition = str_replace_all(condition, "switchn", "switch_no"),
         condition = str_replace_all(condition, "mixedcongruent", "mix_congruent"),
         condition = str_replace_all(condition, "mixedingongruent", "mix_incongruent"))

## join all N450 data together
N450_long <- full_join(N450_amp, N450_latency, by = c("PID", "Group", "condition"))

# deal with SP - for now, just run intended analysis of updated SP
SP <- dat_int %>% 
  select(PID, Group, contains("SP"))

SP_updated_amp <- SP %>% 
  select(PID, Group, contains("amp")) %>%
  pivot_longer(cols = Updated_SP_Congruent_Amp:Updated_SP_Switch_Yes_Amp,
               names_to = "condition",
               values_to = "SP_mean_amp") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "updated_sp_"),
         condition = str_remove(condition, "_amp"),
         condition = str_replace(condition, "mixed", "mix"))

SP_latency <- SP %>% 
  select(PID, Group, contains("lat")) %>% 
  pivot_longer(cols = Updated_SP_Congruent_LAT:Updated_SP_Switch_Yes_LAT,
               names_to = "condition",
               values_to = "SP_latency") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "updated_sp_"),
         condition = str_remove(condition, "_lat"),
         condition = str_replace(condition, "mixed", "mix"))

# merge SP data together
SP_long <- full_join(SP_updated_amp, SP_latency, by = c("PID", "Group", "condition"))

# merge all data together
dat_long <- N200_long %>% 
  full_join(N450_long, by = c("PID", "Group", "condition")) %>% 
  full_join(SP_long, by = c("PID", "Group", "condition")) %>% 
  full_join(acc_long, by = c("PID", "Group", "condition")) %>% 
  full_join(rt_long, by = c("PID", "Group", "condition"))

# read in demographic/SES data for 2nd round of revisions
dat_dem <- read_sav("data/All Demographics.sav")

# retain family income and pid variables
income <- dat_dem %>% 
  select(`ID#`, Income) %>% 
  rename(PID = `ID#`) %>% 
  na_if(999)

# merge SES data with rest of dataset
dat_long <- full_join(income, dat_long, by = "PID")

# turn "mixed" into "mix" for condition types
dat_long$condition <- str_replace(dat_long$condition, "mixed", "mix")

# write long dataset to workspace
write_csv(dat_long, "data/Analyses/dat_long.csv")
