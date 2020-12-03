# set up
library(tidyverse)
library(lmerTest)
library(emmeans)
library(effectsize)

# read in long data
dat <- read_csv("data/Analyses/dat_long.csv")
dat$Income <- factor(dat$Income, levels = c("$10,000 to $20,000", 
                                                  "$30,000 to $50,000",
                                                  "$50,000 to $70,000",
                                                  "$70,000 to $90,000",
                                                  "$90,000 to $110,000",
                                                  "$110,000 to $130,000",
                                                  "$130,000 and above"),
                        exclude = NA)

# N450 analysis
n450_amp_mod <- lmer(N450_mean_amp ~ Group*condition + Income + (1|PID),
                     data = dat %>% 
                     filter(condition %in% c("congruent", "incongruent")))

## anova for F statistic
n450_aov <- anova(n450_amp_mod)
eta_squared(n450_aov)

## post-hoc comparisons
n450_amp_mod %>% 
  emmeans(~ Group * condition) %>% 
  contrast("pairwise", by = "Group") %>% 
  summary(by = NULL, adjust = "holm")

# SP
sp_amp_mod <- lmer(SP_mean_amp ~ Group*condition + (1|PID),
                     data = dat %>% 
                     filter(condition %in% c("congruent", "incongruent")))
## anova for F statistic
sp_aov <- anova(sp_amp_mod)
## post-hoc comparisons
sp_amp_mod %>% 
  emmeans(~ Group * condition) %>% 
  contrast("pairwise", by = "Group") %>% 
  summary(by = NULL, adjust = "holm")

# N200 amp analysis
n200_lat_mod <- lmer(N200_mean_amp ~ Group*condition + (1|PID),
                     data = dat %>% 
                     filter(condition %in% c("switch_no", "switch_yes")))
## anova for F statistic
n200_aov <- anova(n200_lat_mod)
## post-hoc comparisons
n200_lat_mod %>% 
  emmeans(~ Group * condition) %>% 
  contrast("pairwise", by = "Group") %>% 
  summary(by = NULL, adjust = "holm")
