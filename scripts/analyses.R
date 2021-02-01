# set up
library(checkpoint)
library(tidyverse)
library(lmerTest)
library(effectsize)
library(MKinfer)
library(emmeans)

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
# turn income variable into numeric variable and center to be entered as a covariate
# dat <- dat %>% 
#   mutate(Income = as.numeric(Income),
#          Income = Income - mean(Income, na.rm = TRUE)) %>% 
#   rename(income = Income)

##############
###ANALYSES###
##############

# comparison of income between bilingual and monolingual groups
bi_income <- dat[!duplicated(dat$PID),] %>% 
  filter(Group == 2) %>% 
  select(Income) %>% 
  pull()

mono_income <- dat[!duplicated(dat$PID),] %>% 
  filter(Group == 1) %>% 
  select(Income) %>% 
  pull()

## bootstrapped t test since distribution is wacky!
ses_mod <- boot.t.test(x = as.numeric(mono_income), 
            y = as.numeric(bi_income),
            R = 10000)
### there are significant differences in income between the two groups
ses_mod

# accuracy blocked trials
acc_mod <- lmer(accuracy ~ Group*condition + as.numeric(Income) + (1|PID),
                     data = dat %>% 
                     filter(condition %in% c("congruent", "incongruent")))

anova(acc_mod, type = 1) # type 1 appears to be what Laura ran in the initial analyses
eta_squared(anova(acc_mod, type = 1))

emmeans(acc_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "holm")

# response time blocked trials
rt_mod <- lmer(response_time ~ Group*condition + as.numeric(Income) + (1|PID),
                data = dat %>% 
                  filter(condition %in% c("congruent", "incongruent")))

anova(rt_mod, type = 1)
eta_squared(anova(rt_mod, type = 1))

# accuracy mixed trials
acc_mix_mod <- lmer(accuracy ~ Group*condition + as.numeric(Income) + (1|PID),
                data = dat %>% 
                  filter(condition %in% c("mix_congruent", "mix_incongruent")))

anova(acc_mix_mod, type = 1)
eta_squared(anova(acc_mix_mod, type = 1))

emmeans(acc_mix_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "holm")

# response time mixed trials
rt_mixed_mod <- lmer(response_time ~ Group*condition + as.numeric(Income) + (1|PID),
               data = dat %>% 
                 filter(condition %in% c("mix_congruent", "mix_incongruent")))

anova(rt_mixed_mod, type = 1)
eta_squared(anova(rt_mixed_mod, type = 1))

# accuracy switch
acc_switch_mod <- lmer(accuracy ~ Group*condition + as.numeric(Income) + (1|PID),
                    data = dat %>% 
                      filter(condition %in% c("switch_no", "switch_yes")))

anova(acc_switch_mod, type = 1)
eta_squared(anova(acc_switch_mod, type = 1))

emmeans(acc_mix_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "holm")

# response time switch
rt_switch_mod <- lmer(response_time ~ Group*condition + as.numeric(Income) + (1|PID),
                       data = dat %>% 
                         filter(condition %in% c("switch_no", "switch_yes")))

anova(rt_switch_mod, type = 1)
eta_squared(anova(rt_switch_mod, type = 1))

emmeans(acc_mix_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "holm")


# old code that used to work
# acc_mod %>% 
#   emmeans(~ Group) %>% 
#   contrast("pairwise", by = "Group") %>% 
#   summary(by = NULL, adjust = "holm")


# N450 analysis
n450_amp_mod <- lmer(N450_mean_amp ~ Group*condition + as.numeric(Income) + (1|PID),
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
