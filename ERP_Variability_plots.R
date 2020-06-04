#ERP_Variability.R
#ERP_Variability.Rproj
#Created by RLS on 05/30/20 
#Last updated by IJK on 06/04/20
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
# N200 electrodes = A13, B14, B11
# N450 electrodes = (A25, B21, B22, B28) N450 Time window = ???
# SP electrodes = (A15, A24, B20, B21) SP Time Window = 400 - 800ms

#----------------------------------------------------------------------------------------------------------
# load packages
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
#read in time variable
time <- read_excel(here("data", "time_var.xlsx")) %>% 
  rename(ms = Time)

# read in all data and make variables for participant id number and condition name
dat <- all_files %>% 
  map_dfr(~ {
    read_excel(.x) %>% 
      mutate(pid = as.numeric(str_extract(.x, "[0-9]+")),
             name = basename(dirname(.x))) %>% 
      select(-`871`, -`68`) %>% 
      bind_cols(., time)
  })
# clean up electrode names
names(dat) <- gsub("_.*", "", names(dat))

# create group and trial_type variables
dat <- dat %>% 
  mutate(group = if_else(str_detect(name, "Bilingual"), "Bilingual", "Monolingual"),
         trial_type = sub(".*? ", "", dat$name))

# define vectors of electrodes
N200_elec <- c("A13", "B14", "B11") # 210 - 310 ms
N450_elec <- c("A25", "B21", "B22", "B28") # ???
SP_elec <- c("A15", "A24", "B20", "B21") # 400 - 800ms

plot_fun <- function(trials, elec, component_name) {
dat %>% 
  filter(trial_type %in% trials) %>% 
  select(pid, trial_type, group, ms, all_of(elec)) %>%
  pivot_longer(., cols = all_of(elec), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, group, trial_type, ms) %>% 
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  group_by(group, trial_type, ms) %>% 
  mutate(avg_mv = mean(mv, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(ms, mv, group = pid), alpha = 0.3) +
  geom_line(aes(ms, avg_mv), color = "red", size = 1.2) +
  facet_grid(vars(trial_type), vars(group)) +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time (ms)",
#       y = expression(paste("Amplitude ( ",mu,"V)")),
       y = "Amplitude",
       title = paste(component_name, "Variability Waveform Plots")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 16),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))
}

#' Use pmap to iterate plotting function over list of parameters.
#+ iterate and plot
plots <- pmap(list(trials = list(c("Congruent", "Incongruent"),
                                 c("Mixed Congruent", "Mixed Incongruent"),
                                 c("Congruent", "Incongruent"),
                                 c("Mixed Congruent", "Mixed Incongruent"),
                                 c("Switch Yes", "Switch No")),
                   elec = list(N200_elec,
                               N200_elec,
                               N450_elec,
                               N450_elec,
                               SP_elec),
                   component_name = c("N200",
                                      "N200",
                                      "N450",
                                      "N450",
                                      "SP")
                   ),
              .f = plot_fun)
  
#' save images to workspace
#+ save the images
map2(plots, c("N200 congruent and incongruent",
              "N200 mixed congruent and incongruent",
              "N450 congruent and incongruent",
              "N450 mixed congruent and incongruent",
              "SP switch yes and no"), ~ {
  ggsave(plot = .x, filename = here("images", paste0(.y, ".png")), device = "png", width = 8, height = 5, scale = 1.5)
})
