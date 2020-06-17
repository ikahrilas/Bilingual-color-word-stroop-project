#ERP_Variability.R
#ERP_Variability.Rproj
#Created by RLS on 05/30/20 
#Last updated by IJK on 06/05/20
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
# SP electrodes = (A15, A45, A25, B20, B21, B22, B28) SP Time Window = 400 - 800ms

#----------------------------------------------------------------------------------------------------------
# load packages
library (tidyverse)
library(readxl)
library(here)
library(glue)
library(patchwork)
library(emmeans)
library(lmerTest)

# vector of folder path names
folders <- str_subset(list.files(here("data")), "Time File", negate = TRUE)
folder_paths <- glue("/Users/ian/tmp/Bilingual-color-word-stroop-project/data/{folders}/")

# concatenate all folder paths
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

# create group and trial_type variables, convert amplitue to mV
dat <- dat %>% 
  mutate(group = if_else(str_detect(name, "Bilingual"), "Bilingual", "Monolingual"),
         trial_type = sub(".*? ", "", dat$name),
         across(.cols = A1:EXG2, .fns = ~.x * 10^6))

# define vectors of electrodes
N200_elec <- c("A13", "B14", "B11") # 210 - 320 ms
N450_elec <- c("A25", "B21", "B22", "B28") # 400 - 500 mss
SP_elec <- c("A15", "A24", "A25", "B20", "B21", "B22", "B28") # 400 - 800ms

plot_fun <- function(trials, elec, component_name, time_min, time_max) {
dat %>% 
  filter(trial_type %in% trials,
         between(ms, -200, 1000)) %>% 
  select(pid, trial_type, group, ms, all_of(elec)) %>%
  pivot_longer(., cols = all_of(elec), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, group, trial_type, ms) %>% 
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  group_by(group, trial_type, ms) %>% 
  mutate(avg_mv = mean(mv, na.rm = TRUE),
         color = if_else(str_detect(trial_type, "Congruent") | str_detect(trial_type, "No"), "green", "red")) %>% 
  ggplot() +
  geom_line(aes(ms, mv, group = pid), alpha = 0.3) +
  geom_line(aes(ms, avg_mv, color = color), size = 1.2) +
  scale_color_manual(breaks = c("green", "red"),
                     values=c("green", "red")) +
  facet_grid(vars(trial_type), vars(group)) +
  theme_classic() +
  scale_x_continuous(breaks=c(-200, 0, 200, 400, 600, 800, 1000)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("rect", xmin = time_min, xmax = time_max, ymin = -Inf, ymax = Inf, alpha = .15) +
  labs(x = "Time (ms)",
       y = expression(paste("Amplitude ( ",mu,"V)")),
       title = paste(component_name, "Waveform Variability Plots")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 16),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        legend.position = "none")
}

#' Use pmap to iterate plotting function over list of parameters.
plots <- pmap(list(trials = list(c("Congruent", "Incongruent"),
                                 c("Mixed Congruent", "Mixed Incongruent"),
                                 c("Switch Yes", "Switch No"),
                                 c("Congruent", "Incongruent"),
                                 c("Mixed Congruent", "Mixed Incongruent"),
                                 c("Switch Yes", "Switch No"),
                                 c("Congruent", "Incongruent"),
                                 c("Mixed Congruent", "Mixed Incongruent"),
                                 c("Switch Yes", "Switch No")),
                   elec = list(N200_elec,
                               N200_elec,
                               N200_elec,
                               N450_elec,
                               N450_elec,
                               N450_elec,
                               SP_elec,
                               SP_elec,
                               SP_elec),
                   component_name = c("N200",
                                      "N200",
                                      "N200",
                                      "N450",
                                      "N450",
                                      "N450",
                                      "SP",
                                      "SP",
                                      "SP"),
                   time_min = c(210,
                                210,
                                210,
                                400,
                                400,
                                400,
                                400,
                                400,
                                400),
                   time_max = c(320,
                                320,
                                320,
                                500,
                                500,
                                500,
                                800,
                                800,
                                800)
                   ),
              .f = plot_fun)
  
# save images to workspace
map2(plots, c("N200 pure",
              "N200 mixed",
              "N200 switching",
              "N450 pure",
              "N450 mixed",
              "N450 switching",
              "SP pure",
              "SP mixed",
              "SP switching"), ~ {
  ggsave(plot = .x, filename = here("images", paste0(.y, ".png")), device = "png", width = 8, height = 5, scale = 1.5)
})


dat %>% 
  filter(trial_type %in% c("Congruent", "Incongruent"),
         between(ms, -200, 1000)) %>% 
  select(pid, trial_type, group, ms, all_of(N450_elec)) %>%
  pivot_longer(., cols = all_of(N450_elec), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, group, trial_type, ms) %>% 
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  group_by(group, trial_type, ms) %>% 
  mutate(avg_mv = mean(mv, na.rm = TRUE),
         color = if_else(str_detect(trial_type, "Congruent") | str_detect(trial_type, "No"), "green", "red")) %>% 
  ggplot() +
  #geom_line(aes(ms, mv, group = pid), alpha = 0.3) +
  geom_line(aes(ms, avg_mv, color = trial_type), size = 1.2) +
  geom_ribbon(aes(x = ms, ymin = avg_mv - sd(mv)/sqrt(16), ymax = avg_mv + sd(mv)/sqrt(16), group = trial_type), alpha = 0.3) +
  scale_color_manual(breaks = c("Congruent", "Incongruent"),
                     values=c("blue", "red")) +
  facet_wrap(~ group, ncol = 1) +
  theme_classic() +
  scale_x_continuous(breaks=c(-200, 0, 200, 400, 600, 800, 1000)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_segment(x = 400, xend = 500, y = 6.75, yend = 6.75) +
  annotate("rect", fill = "purple", xmin = 400, xmax = 500, ymin = -Inf, ymax = 6.75, alpha = .15) +
  annotate(geom = "text", x = 450, y = 7.25, label = "italic(p) < .05", parse = TRUE) +
  labs(x = "Time (ms)",
       y = expression(paste("Amplitude (",mu,"V)"))) +
  guides(color = guide_legend(title = "Condition")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 16),
        strip.text.x = element_text(size = 16, hjust = 0),
        strip.text.y = element_text(size = 16, hjust = 0))

dat %>% 
  filter(trial_type %in% c("Congruent", "Incongruent"),
         between(ms, -200, 1000)) %>% 
  select(pid, trial_type, group, ms, all_of(N450_elec)) %>%
  pivot_longer(., cols = all_of(N450_elec), names_to = "electrode", values_to = "mv") %>% 
  group_by(trial_type, group, ms) %>% 
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  pivot_wider(names_from = trial_type, values_from = mv, names_glue = "{trial_type}_mv") %>% 
  mutate(diff_mv = Congruent_mv - Incongruent_mv) %>% 
  ggplot() +
  geom_line(aes(ms, diff_mv, color = group), size = 1.2) +
  geom_ribbon(aes(x = ms, ymin = diff_mv - sd(diff_mv)/sqrt(16), ymax = diff_mv + sd(diff_mv)/sqrt(16), group = group), alpha = 0.3) +
  scale_color_manual(breaks = c("Monolingual", "Bilingual"),
                     values=c("blue", "red")) +
  theme_classic() +
  scale_x_continuous(breaks=c(-200, 0, 200, 400, 600, 800, 1000)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_segment(x = 400, xend = 500, y = 2, yend = 2) +
  annotate("rect", fill = "purple", xmin = 400, xmax = 500, ymin = -Inf, ymax = 2, alpha = .15) +
  annotate(geom = "text", x = 450, y = 2.15, label = "italic(p) < .05", parse = TRUE) +
  labs(x = "Time (ms)",
       y = expression(paste("Amplitude (",mu,"V)"))) +
  guides(color = guide_legend(title = "Congruent - Incongruent")) +
  ggtitle("Interaction") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"),
        plot.title = element_text(hjust = 0),
        title = element_text(size = 16),
        strip.text.x = element_text(size = 16, hjust = 0),
        strip.text.y = element_text(size = 16, hjust = 0))
  
#---- field trip style plots
### to do: find exact standard errors and p values!
field_trip_plot <- function(trials, elec, component_name, time_min, time_max, height_1, height_2,
                            height_3, std_error_1, std_error_2, std_error_3, p_1, p_2, p_3) {
mono <- dat %>% 
  filter(trial_type %in% trials,
         group == "Monolingual",
         between(ms, -200, 1000)) %>% 
  select(pid, trial_type, group, ms, all_of(elec)) %>%
  pivot_longer(., cols = all_of(elec), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, group, trial_type, ms) %>% 
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  group_by(group, trial_type, ms) %>% 
  mutate(avg_mv = mean(mv, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(ms, avg_mv, color = trial_type), size = 1.2) +
  geom_ribbon(aes(x = ms, ymin = avg_mv - std_error_1, ymax = avg_mv + std_error_1, group = trial_type), alpha = 0.3) +
  scale_color_manual(breaks = trials,
                     values=c("blue", "red")) +
  theme_classic() +
  scale_x_continuous(breaks=c(-200, 0, 200, 400, 600, 800, 1000)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_segment(x = time_min, xend = time_max, y = height_1, yend = height_1) +
  annotate("rect", fill = "purple", xmin = time_min, xmax = time_max, ymin = -Inf, ymax = height_1, alpha = .15) +
  annotate(geom = "text", x = (time_min + time_max) / 2, y = height_1 + .75, label = paste("italic(p)", p_1), parse = TRUE) +
  labs(x = "Time (ms)",
       y = expression(paste("Amplitude (",mu,"V)"))) +
  guides(color = guide_legend(title = "Condition")) +
  ggtitle("Monolingual") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "line"),
        legend.position = c(0.9, 1.2),
        plot.title = element_text(hjust = 0),
        title = element_text(size = 12),
        strip.text.x = element_text(size = 12, hjust = 0),
        strip.text.y = element_text(size = 12, hjust = 0))

bi <- dat %>% 
  filter(trial_type %in% trials,
         group == "Bilingual",
         between(ms, -200, 1000)) %>% 
  select(pid, trial_type, group, ms, all_of(elec)) %>%
  pivot_longer(., cols = all_of(elec), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, group, trial_type, ms) %>% 
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  group_by(group, trial_type, ms) %>% 
  mutate(avg_mv = mean(mv, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(ms, avg_mv, color = trial_type), size = 1.2) +
  geom_ribbon(aes(x = ms, ymin = avg_mv - std_error_2, ymax = avg_mv + std_error_2, group = trial_type), alpha = 0.3) +
  scale_color_manual(breaks = trials,
                     values=c("blue", "red")) +
  theme_classic() +
  scale_x_continuous(breaks=c(-200, 0, 200, 400, 600, 800, 1000)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_segment(x = time_min, xend = time_max, y = height_1, yend = height_1) +
  annotate("rect", fill = "purple", xmin = time_min, xmax = time_max, ymin = -Inf, ymax = height_1, alpha = .15) +
  annotate(geom = "text", x = (time_min + time_max) / 2, y = height_1 + .75, label = paste("italic(p)", p_2), parse = TRUE) +
  labs(x = "Time (ms)",
       y = expression(paste("Amplitude (",mu,"V)"))) +
  guides(color = guide_legend(title = "Condition")) +
  ggtitle("Bilingual") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "line"),
        legend.position = c(0.9, 1.2),
        plot.title = element_text(hjust = 0),
        title = element_text(size = 12),
        strip.text.x = element_text(size = 12, hjust = 0),
        strip.text.y = element_text(size = 12, hjust = 0))

int <- dat %>% 
  filter(trial_type %in% trials,
         between(ms, -200, 1000)) %>% 
  select(pid, trial_type, group, ms, all_of(elec)) %>%
  pivot_longer(., cols = all_of(elec), names_to = "electrode", values_to = "mv") %>% 
  group_by(trial_type, group, ms) %>% 
  summarize(mv = mean(mv, na.rm = TRUE)) %>% 
  pivot_wider(names_from = trial_type, values_from = mv) %>% 
  mutate(diff_mv = get(trials[1]) - get(trials[2])) %>% 
  ggplot() +
  geom_line(aes(ms, diff_mv, color = group), size = 1.2) +
  geom_ribbon(aes(x = ms, ymin = diff_mv - std_error_3, ymax = diff_mv + std_error_3, group = group), alpha = 0.3) +
  scale_color_manual(breaks = c("Monolingual", "Bilingual"),
                     values=c("green", "purple")) +
  theme_classic() +
  scale_x_continuous(breaks=c(-200, 0, 200, 400, 600, 800, 1000)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_segment(x = time_min, xend = time_max, y = height_3, yend = height_3) +
  annotate("rect", fill = "purple", xmin = time_min, xmax = time_max, ymin = -Inf, ymax = 5, alpha = .15) +
  annotate(geom = "text", x = (time_min + time_max) / 2, y = height_3 + .90, label = paste("italic(p)", p_3), parse = TRUE) +
  labs(x = "Time (ms)",
       y = expression(paste("Amplitude (",mu,"V)"))) +
  guides(color = guide_legend(title = paste(trials[1], "-", trials[2]))) +
  ggtitle("Interaction") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "line"),
        legend.position = c(0.80, 1.15),
        plot.title = element_text(hjust = 0),
        title = element_text(size = 12),
        strip.text.x = element_text(size = 12, hjust = 0),
        strip.text.y = element_text(size = 12, hjust = 0))

mono <- mono + ylim(-2, height_1 + 1)
bi <- bi + ylim(-2, height_1 + 1)
int <- int + ylim(-4, height_1 + 1)
(mono / bi  / int ) + plot_annotation(title = paste("Average", component_name, "Waveforms"),
                                    theme = theme(plot.title = element_text(hjust = 0.5,
                                                                            size = 14)),
                                    tag_levels = "A")
}

mod <- lmer(SP ~ group*trial_type + (1|pid), dat = dat_analysis %>% filter(trial_type %in% c("Mixed Congruent", "Mixed Incongruent")))
summary(mod)
anova(mod)
(emmeans(mod, pairwise ~ trial_type | group))

sp_mixed <- field_trip_plot(trials = c("Mixed Congruent", "Mixed Incongruent"), 
                                              elec = SP_elec, 
                                              component_name = "SP", 
                                              time_min = 400, 
                                              time_max = 800,
                                              height_1 = 7,
                                              height_2 = 7,
                                              height_3 = 5,
                                              std_error_1 = .354,
                                              std_error_2 = .354,
                                              std_error_3 = .501,
                                              p_1 = "== .239",
                                              p_2 = "== .397",
                                              p_3 = "== .810")

ggsave(plot = sp_mixed, filename = here("images", "field trip plots", paste0("SP_mixed", ".png")), 
       device = "png", width = 5, height = 5, scale = 1.5)
#---- box plots
# prep data
N200 <- dat %>%
  select(all_of(N200_elec), pid:trial_type) %>% 
  filter(between(ms, 210, 320)) %>%
  pivot_longer(., cols = all_of(N200_elec), names_to = "electrode", values_to = "mv") %>% 
  group_by(pid, group, trial_type) %>% 
  summarize(N200 = mean(mv, na.rm = TRUE))
N450 <- dat %>%
  select(all_of(N450_elec), pid:trial_type) %>% 
  filter(between(ms, 400, 500)) %>%
  pivot_longer(., cols = all_of(N450_elec), names_to = "electrode", values_to = "mv") %>% 
  group_by(pid, group, trial_type) %>% 
  summarize(N450 = mean(mv, na.rm = TRUE))
SP <- dat %>%
  select(all_of(SP_elec), pid:trial_type) %>% 
  filter(between(ms, 400, 800)) %>%
  pivot_longer(., cols = all_of(SP_elec), names_to = "electrode", values_to = "mv") %>% 
  group_by(pid, group, trial_type) %>% 
  summarize(SP = mean(mv, na.rm = TRUE))

# recode all trial_types to conflict vs. no conflict
dat_analysis <- full_join(N200, N450, by = c("pid", "group", "trial_type")) %>% 
  left_join(SP, by = c("pid", "group", "trial_type"))


full_dat <- full_join(N200, N450, by = c("pid", "group", "trial_type")) %>% 
  left_join(SP, by = c("pid", "group", "trial_type")) %>% 
  pivot_longer(cols = c(N200, N450, SP), names_to = "Component", values_to = "mv") %>% 
  pivot_wider(names_from = trial_type, values_from = mv, names_glue = "{trial_type}_mv") %>% 
  mutate(con_inc_diff = Congruent_mv - Incongruent_mv,
         mixed_diff = `Mixed Congruent_mv` - `Mixed Incongruent_mv`,
         switch_diff = `Switch No_mv` - `Switch Yes_mv`)

# boxplots
full_dat %>%
  mutate(group = factor(group, levels = c("Monolingual", "Bilingual"))) %>% 
  ggplot(aes(x = Component, y = con_inc_diff, fill = Component)) +
  geom_boxplot() +
  geom_point() +
  geom_line(aes(group = pid), alpha = 0.3) +
  labs(y = "Congruent - Incongruent") +
  theme(legend.position = "none") + 
  facet_wrap(~group, ncol = 1) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, hjust = 0.5),
        strip.text.y = element_text(size = 12, hjust = 0.5))

ggsave(filename = here("images", "boxplots", paste0("con_inc", ".png")), 
       device = "png", width = 3, height = 5, scale = 1.5)

full_dat %>%
  mutate(group = factor(group, levels = c("Monolingual", "Bilingual"))) %>% 
  ggplot(aes(x = Component, y = mixed_diff, fill = Component)) +
  geom_boxplot() +
  geom_point() +
  geom_line(aes(group = pid), alpha = 0.3) +
  labs(y = "Mixed Congruent - Mixed Incongruent") +
  theme(legend.position = "none") + 
  facet_wrap(~group, ncol = 1) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, hjust = 0.5),
        strip.text.y = element_text(size = 12, hjust = 0.5))

ggsave(filename = here("images", "boxplots", paste0("mixed", ".png")), 
       device = "png", width = 3, height = 5, scale = 1.5)

full_dat %>%
  mutate(group = factor(group, levels = c("Monolingual", "Bilingual"))) %>% 
  ggplot(aes(x = Component, y = switch_diff, fill = Component)) +
  geom_boxplot() +
  geom_point() +
  geom_line(aes(group = pid), alpha = 0.3) +
  labs(y = "Switch No - Switch Yes") +
  theme(legend.position = "none") + 
  facet_wrap(~group, ncol = 1) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, hjust = 0.5),
        strip.text.y = element_text(size = 12, hjust = 0.5))

ggsave(filename = here("images", "boxplots", paste0("switch", ".png")), 
       device = "png", width = 3, height = 5, scale = 1.5)
