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
                                            "$20,000 to $30,000",
                                            "$30,000 to $50,000",
                                            "$50,000 to $70,000",
                                            "$70,000 to $90,000",
                                            "$90,000 to $110,000",
                                            "$110,000 to $130,000",
                                            "$130,000 and above"),
                        exclude = NA)
# turn income variable into numeric variable and center to be entered as a covariate
# dat <- dat %>% 
#   mutate(Income = Income,
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

#########################
# behavioral data #
#########################

# accuracy blocked trials
acc_mod <- lmer(accuracy ~ Group*condition + Income + (1|PID),
                     data = dat %>% 
                     filter(condition %in% c("congruent", "incongruent")))

summary(acc_mod)

anova(acc_mod, type = 1) # type 1 appears to be what Laura ran in the initial analyses
eta_squared(anova(acc_mod, type = 1))

emmeans(acc_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "holm")

# response time blocked trials
rt_mod <- lmer(response_time ~ Group*condition + Income + (1|PID),
                data = dat %>% 
                  filter(condition %in% c("congruent", "incongruent")))

anova(rt_mod, type = 1)
eta_squared(anova(rt_mod, type = 1))

# accuracy mixed trials
acc_mix_mod <- lmer(accuracy ~ Group*condition + Income + (1|PID),
                data = dat %>% 
                  filter(condition %in% c("mix_congruent", "mix_incongruent")))

summary(acc_mix_mod)

anova(acc_mix_mod, type = 1)
eta_squared(anova(acc_mix_mod, type = 1))

emmeans(acc_mix_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Income)

emmeans(acc_mix_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "holm")

# response time mixed trials
rt_mixed_mod <- lmer(response_time ~ Group*condition + Income + (1|PID),
               data = dat %>% 
                 filter(condition %in% c("mix_congruent", "mix_incongruent")))

anova(rt_mixed_mod, type = 1)
eta_squared(anova(rt_mixed_mod, type = 1))

# accuracy switch
acc_switch_mod <- lmer(accuracy ~ Group*condition + Income + (1|PID),
                    data = dat %>% 
                      filter(condition %in% c("switch_no", "switch_yes")))

anova(acc_switch_mod, type = 1)
eta_squared(anova(acc_switch_mod, type = 1))

# response time switch
rt_switch_mod <- lmer(response_time ~ Group*condition + Income + (1|PID),
                       data = dat %>% 
                         filter(condition %in% c("switch_no", "switch_yes")))

anova(rt_switch_mod, type = 1)
eta_squared(anova(rt_switch_mod, type = 1))

emmeans(acc_mix_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "holm")


####################################################################################
# EEG data ## ordered according to how they are presented in results section
####################################################################################

# N200 amplitude pure blocks
n200_amp_pure_mod <- lmer(N200_mean_amp ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                          filter(condition %in% c("congruent", "incongruent")))

anova(n200_amp_pure_mod, type = 1)
eta_squared(anova(n200_amp_pure_mod, type = 1))

# N200 latency pure blocks
n200_lat_pure_mod <- lmer(N200_latency ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                            filter(condition %in% c("congruent", "incongruent")))

anova(n200_lat_pure_mod, type = 1)
eta_squared(anova(n200_lat_pure_mod, type = 1))

# N450 amplitude pure blocks
n450_amp_pure_mod <- lmer(N450_mean_amp ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                            filter(condition %in% c("congruent", "incongruent")))

anova(n450_amp_pure_mod, type = 1)
eta_squared(anova(n450_amp_pure_mod, type = 1))

## follow up post hoc tests
emmeans(n450_amp_pure_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "holm") %>% 
  summary(by = NULL)

# N450 latency pure blocks
n450_lat_pure_mod <- lmer(N450_latency ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                            filter(condition %in% c("congruent", "incongruent")))

anova(n450_lat_pure_mod, type = 1)
eta_squared(anova(n450_lat_pure_mod, type = 1))

# SP amplitude pure blocks
sp_lat_pure_mod <- lmer(SP_mean_amp ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                            filter(condition %in% c("congruent", "incongruent")))

anova(sp_lat_pure_mod, type = 1)
eta_squared(anova(sp_lat_pure_mod, type = 1))

## follow up post hoc tests
emmeans(sp_lat_pure_mod, data = filter(dat, condition %in% c("congruent", "incongruent")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "Holm") %>% 
  summary(by = NULL)

# N200 amplitude mixed trials
n200_amp_mix_mod <- lmer(N200_mean_amp ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                            filter(condition %in% c("mix_congruent", "mix_incongruent")))

anova(n200_amp_mix_mod, type = 1)
eta_squared(anova(n200_amp_mix_mod, type = 1))

# N200 latency mixed trials
n200_lat_mix_mod <- lmer(N200_latency ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                            filter(condition %in% c("mix_congruent", "mix_incongruent")))

anova(n200_lat_mix_mod, type = 1)
eta_squared(anova(n200_lat_mix_mod, type = 1))

# N450 amplitude mixed trials
n450_amp_mix_mod <- lmer(N450_mean_amp ~ Group*condition + Income + (1|PID),
                         data = dat %>% 
                           filter(condition %in% c("mix_congruent", "mix_incongruent")))

anova(n450_amp_mix_mod, type = 1)
eta_squared(anova(n450_amp_mix_mod, type = 1))

# N450 latency mixed trials
n450_lat_mix_mod <- lmer(N450_latency ~ Group*condition + Income + (1|PID),
                         data = dat %>% 
                           filter(condition %in% c("mix_congruent", "mix_incongruent")))

anova(n450_lat_mix_mod, type = 1)
eta_squared(anova(n450_lat_mix_mod, type = 1))

# SP amplitude mixed trials
sp_amp_mix_mod <- lmer(SP_mean_amp ~ Group*condition + Income + (1|PID),
                        data = dat %>% 
                          filter(condition %in% c("mix_congruent", "mix_incongruent")))

anova(sp_amp_mix_mod, type = 1)
eta_squared(anova(sp_amp_mix_mod, type = 1))

# N200 amplitude switch blocks
n200_amp_switch_mod <- lmer(N200_mean_amp ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                            filter(condition %in% c("switch_no", "switch_yes")))

anova(n200_amp_switch_mod, type = 1)
eta_squared(anova(n200_amp_switch_mod, type = 1))

## follow up post hoc tests
emmeans(n200_amp_switch_mod, data = filter(dat, condition %in% c("switch_no", "switch_yes")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "Holm") %>% 
  summary(by = NULL)


# N200 latency switch blocks
n200_lat_switch_mod <- lmer(N200_latency ~ Group*condition + Income + (1|PID),
                          data = dat %>% 
                            filter(condition %in% c("switch_no", "switch_yes")))

anova(n200_lat_switch_mod, type = 1)
eta_squared(anova(n200_lat_switch_mod, type = 1))



# N450 amplitude switch blocks
n450_amp_switch_mod <- lmer(N450_mean_amp ~ Group*condition + Income + (1|PID),
                            data = dat %>% 
                              filter(condition %in% c("switch_no", "switch_yes")))

anova(n450_amp_switch_mod, type = 1)
eta_squared(anova(n450_amp_switch_mod, type = 1))

## follow up post hoc tests
emmeans(n450_amp_switch_mod, data = filter(dat, condition %in% c("switch_no", "switch_yes")), ~ Group*condition) %>% 
  contrast("pairwise", by = "condition", adjust = "Holm") %>% 
  summary(by = NULL)


# N450 latency switch blocks
n450_lat_switch_mod <- lmer(N450_latency ~ Group*condition + Income + (1|PID),
                            data = dat %>% 
                              filter(condition %in% c("switch_no", "switch_yes")))

anova(n450_lat_switch_mod, type = 1)
eta_squared(anova(n450_lat_switch_mod, type = 1))


# SP amplitude switch trials
sp_amp_switch_mod <- lmer(SP_mean_amp ~ Group*condition + Income + (1|PID),
                       data = dat %>% 
                         filter(condition %in% c("switch_no", "switch_yes")))

anova(sp_amp_switch_mod, type = 1)
eta_squared(anova(sp_amp_switch_mod, type = 1))

