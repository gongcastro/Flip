# Flip
# Gonzalo García-Castro* and Chiara Santolin, *gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ###################################################################

# load packages
library(magrittr)  # for using pipes
library(readxl)    # for importing Excel files
library(dplyr)     # for manipulating data
library(tidyr)     # for reshaping datasets
library(ggplot2)   # for visualising data
library(ggalt)     # for plotting lollipops
library(lmerTest)  # for linear-mixed models
library(patchwork) # for attaching plots together
library(googledrive)

#### import data #############################################################

drive_download("data/data_flip", path = "data/", overwrite = TRUE) 
data <-
  read_xlsx("data/data_flip.xlsx") %>%                # import data
  mutate(group = case_when(hpp > 0 ~ "more",          # group participants into one or several HPP
                           TRUE    ~ "first"),        
         total = novel+familiar,                      # total looking time (ms)
         difference = novel-familiar,                 # difference score (ms, uncorrected)
         preference = (novel)/(novel + familiar)*100, # proportion of looking to novel object (%)
         log_preference = log10(preference))          # log10 of preference

# same but subseting by study (Saffran and Santolin)
data.santolin <-
  read_xlsx("data/data_flip.xlsx") %>%
  mutate(group = case_when(hpp > 0 ~ "more",
                           TRUE    ~ "first"),
         total = novel+familiar,
         difference = novel-familiar,
         preference = (novel)/(novel + familiar)*100,
         log_preference = log1p(preference)) %>%
  filter(study == "santolin")

data.saffran <-
  read_xlsx("data/data_flip.xlsx") %>%
  mutate(group = case_when(hpp > 0 ~ "more",
                           TRUE    ~ "first"),
         total = novel+familiar,
         difference = novel-familiar,
         preference = (novel)/(novel + familiar)*100,
         log_preference = log1p(preference)) %>%
  filter(study == "saffran")
  
#### analyse data ############################################################

# overall
fit <- lm(preference ~ hpp, data = data) # estimate coefficient (# of hpp as predictor of preference score)
anova <- anova(fit)                      # perform F-tests on coefficient
residuals <- fit$residuals               # get residuals
normality <- shapiro.test(residuals)     # check normality assumption

# same bu subsetting by study
# santolin
fit.santolin <- lm(preference ~ hpp, data = data.santolin)
anova.santolin <- anova(fit.santolin)
residuals.santolin <- fit.santolin$residuals
normality.santolin <- shapiro.test(residuals.santolin)

# saffran
fit.saffran <- lm(preference ~ hpp, data = data.saffran)
anova.saffran <- anova(fit.saffran)
residuals.saffran <- fit.saffran$residuals
normality.saffran <- shapiro.test(residuals.saffran)

#### visualise data ##########################################################

# first vs  subsequent
ggplot(data, aes(x = hpp, y = preference)) +
  geom_jitter(alpha = 0.5, width = 0.1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "black", size = 1) +
  stat_summary(aes(y = fitted(fit)), fun.y = mean, geom = "line", colour = "black", size = 1) +
  geom_hline(yintercept = 50, linetype = "dotted") +
  labs(x = "Nº HPP", y = "Novelty preference (%) \n(Novel / Novel + Familiar) x 100", colour = "Number of HPP",
       title = "Novelty preference by number of previous HPP tests",
       subtitle = "Mean and 95% CI") +
  theme(text = element_text(size = 15),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        panel.grid.major.x = element_blank()) +
  facet_wrap(.~study) +
  ggsave("figures/preference.png")

# normality assumption
ggplot(data = as.table(residuals)) +
  geom_density(aes(x = residuals)) +
  labs(x = "Residuals", y = "Probability density", title = "Distribution of residuals") +
  ggplot(data = as.table(residuals), aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(colour = "red") +
  labs(x = "Theoretical normal distribution", y = "Oberseved residuals", title = "QQ plot") +
  plot_layout(nrow = 1)
