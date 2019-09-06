# Flip
# Gonzalo García-Castro* and Chiara Santolin, *gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ###################################################################

# load packages
library(magrittr)    # for using pipes
library(readxl)      # for importing Excel files
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(nlme)        # for Linear-Mixed models 
library(faux)        # for power analysis
library(broom.mixed) # for tidy mices model summary
library(ggplot2)     # for visualising data
library(ggalt)       # for plotting lollipops
library(patchwork)   # for attaching plots together
library(googledrive) # for downloading data from Google Drive
library(patchwork)   # for arranging plots

# set experimental parameters
alpha <- 0.05 # significance criterion
power <- 0.80 # desired statistical power
sesoi <- 0.50 # smallest effect size of interest

#### import data #############################################################

#drive_download("data/data_flip", path = "data/", overwrite = TRUE) 
data <-
  read_xlsx("data/data_flip.xlsx") %>%                         # import data
  mutate(
    group = as.factor(case_when(hpp > 0 ~ "more",              # group participants into one or several HPP
                                TRUE ~ "first")), 
    total = novel+familiar,                                    # total looking time (ms)
    difference = novel-familiar,                               # difference score (ms, uncorrected)
    preference = (novel)/(novel + familiar)*100,               # proportion of looking to novel object (%)
    direction = as.factor(case_when(preference>50 ~ "novelty", # direction of preference
                                    TRUE          ~ "familiarity")) 
  )          

#### analyse data ############################################################

# fit models
m0 <- nlme::lme(preference ~ 1, random = ~1|study, data = data, method = "ML", control = list(opt = "optim"))
m1 <- nlme::lme(preference ~ hpp, random = ~1|study, data = data, method = "ML", control = list(opt = "optim"))
m2 <- nlme::lme(preference ~ hpp, random = ~hpp|study, data = data, method = "ML", control = list(opt = "optim"))

# significance testing
anova <- anova(m0, m1, m2)

# confidence intervals
intervals <- intervals(m1, 0.95)

# assumptions
residuals <- residuals(m1)
hist(residuals)
shapiro.test(residuals)
predicted0 <- predict(m0)
predicted1 <- predict(m1)
predicted2 <- predict(m2)
predicted.fixed <- predict(lm(preference~hpp, data = data))

#### visualise data ##########################################################

# only fixed effects
ggplot(data, aes(x = hpp, y = preference)) +
  geom_jitter(alpha = 0.8, size = 2, width = 0.1, aes(shape = study)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "red") +
  stat_summary(aes(y = predicted.fixed), fun.y = mean, geom = "line", colour = "red") +
  geom_hline(yintercept = 50, linetype = "dotted") +
  labs(title = "No random effects (just HPP)", x = "Nº HPP", y = "Novelty preference (%)", shape = "Study") +
  theme(text = element_text(size = 12),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  ggsave("figures/preference-fixed.png") +
  # m0: random intercepts
  ggplot(data, aes(x = hpp, y = preference, shape = study, group = study)) +
  geom_jitter(alpha = 0.8, size = 2, width = 0.1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "red") +
  stat_summary(aes(y = predicted0), fun.y = mean, geom = "line", colour = "red") +
  geom_hline(yintercept = 50, linetype = "dotted") +
  labs(title = "Random intercepts", x = "Nº HPP", y = "Novelty preference (%)", shape = "Study") +
  theme(text = element_text(size = 12),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank()) +
  ggsave("figures/preference-intercepts.png") +
  # m1: random intercepts + HPP
  ggplot(data, aes(x = hpp, y = preference, shape = study, group = study)) +
  geom_jitter(alpha = 0.8, size = 2, width = 0.1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "red") +
  stat_summary(aes(y = predicted1), fun.y = mean, geom = "line", colour = "red") +
  geom_hline(yintercept = 50, linetype = "dotted") +
  labs(title = "Random intercepts + HPP", x = "Nº HPP", y = "Novelty preference (%)", shape = "Study") +
  theme(text = element_text(size = 12),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        panel.grid.major.x = element_blank()) +
  ggsave("figures/preference-intercepts-hpp.png") +
  # m2: random intercepts + HPP + random slopes
  ggplot(data, aes(x = hpp, y = preference, shape = study, group = study)) +
  geom_jitter(alpha = 0.8, size = 2, width = 0.1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "red") +
  stat_summary(aes(y = predicted2, group = study), fun.y = mean, geom = "line", colour = "red") +
  geom_hline(yintercept = 50, linetype = "dotted") +
  labs(title = "Random intercepts + HPP + random slopes", x = "Nº HPP", y = "Novelty preference (%)", shape = "Study") +
  theme(text = element_text(size = 12),
        axis.text = element_text(colour = "black"),
        axis.title.y = element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank()) +
  ggsave("figures/preference-intercepts-hpp-slopes.png") +
  # layout
  plot_layout(ncol = 2, nrow = 2) +
  ggsave("figures/all.png", height = 6.25, width = 9.5)
  
# normality assumption
ggplot(data = as.table(residuals)) +
  geom_density(aes(x = residuals)) +
  labs(x = "Residuals", y = "Probability density", title = "Distribution of residuals") +
  ggplot(data = as.table(residuals), aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(colour = "red") +
  labs(x = "Theoretical normal distribution", y = "Oberseved residuals", title = "QQ plot") +
  plot_layout(nrow = 1)

  #### export data #####################################
  capture.output(anova, file="data/anova-lmm.txt") 
  