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
         )

# restructure data frame to long form
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
         condition = str_replace_all(condition, "mixedcongruent", "mixed_congruent"),
         condition = str_replace_all(condition, "mixedingongruent", "mixed_incongruent"))

## join all N450 data together
N450_long <- full_join(N450_amp, N450_latency, by = c("PID", "Group", "condition"))

# deal with SP - for now, just run intended analysis of updated SP
SP <- dat_int %>% 
  select(PID, Group, contains("SP"))

SP_updated_amp <- SP %>% 
  select(PID, Group, contains("amp")) %>%
  select(PID, Group, contains("update")) %>% 
  select(PID, Group, !contains("small")) %>% 
  select(PID, Group, !contains("large")) %>% 
  pivot_longer(cols = Updated_SP_Congruent_Amp:Updated_SP_Switch_Yes_Amp,
               names_to = "condition",
               values_to = "SP_updated") %>% 
  mutate(condition = tolower(condition),
         condition = str_remove(condition, "updated_sp_"),
         condition = str_remove(condition, "_amp"))

library(rstatix)

SP_updated_amp <- SP_updated_amp %>% 
  mutate(Group = as.factor(Group),
         condition = as.factor(condition))

mod <- lmer(SP_updated ~ Group*condition + (1|PID), data = SP_updated_amp %>% filter(condition %in% c("congruent", "incongruent")))
aov_mod <- anova(mod)

mod %>% 
  emmeans(~ Group * condition) %>% 
  contrast("pairwise", by = "Group") %>% 
  summary(by = NULL, adjust = "mvt")


sidcontrast(emmeans(mod, "condition"))

aov_mod <- aov(SP_updated ~ Group + condition + Group*condition + Error(PID/condition), data = SP_updated_amp %>% filter(condition %in% c("congruent", "incongruent")))

poo <- SP_updated_amp %>% 
         filter(Group == 2)

pairwise.t.test(poo$SP_updated, poo$condition, paired = TRUE, p.adjust.method = "fdr")
