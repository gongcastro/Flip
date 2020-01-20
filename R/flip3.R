#### Import, process, analyse, and export data #####################
# 1-3 HPP version

#### set up ########################################################

# load packages
library(magrittr)    # for using pipes
library(tibble)      # for tidy datasets
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(forcats)     # for recoding factors
library(ggplot2)     # for visualising data
library(lme4)        # for Linear-Mixed Effects Models (LMEM)
library(car)         # for ANOVA
library(effects)     # for predicting
library(patchwork)   # for arranging plots
library(here)        # for locating files

# set parameters
set.seed(100) # for reproducible confidence interval bootstrapping

#### import and process data ##############################################
data <- read.csv(
  file             = here("Data","00_data-raw.csv"),
  header           = TRUE,
  sep              = ",",
  dec              = ".",
  stringsAsFactors = TRUE
) %>%
  as_tibble() %>%
  pivot_longer(
    cols      = c("Familiar", "Novel"),
    names_to  = "Item",
    values_to = "LookingTime"
  ) %>%
  mutate(
    LogLookingTime = log(LookingTime),
    Item = fct_relevel(Item, "Novel", after = 1), # make familiar items the baseline
    HPPCenter = (HPP - mean(HPP)),
    Study = factor(case_when(
      Study == "Santolin" & Location == "Barcelona" ~ "Santolin, Saffran & Sebastian-Galles (2019)",
      Study == "Santolin" & Location == "Wisconsin" ~ "Santolin & Saffran (2019)",
      Study ==  "Saffran & Wilson"                  ~ "Saffran & Wilson (2003)",
      Study == "SaffranHauser1"                     ~ "Saffran et al. (2008)"))
  ) %>%
  filter(HPP < 4) # get only participants with 1 to 3 HPP studies

#### Linear Mixed-Effects Model #####################################

# 1. Fit maximal model: random by-participant and by-study intercepts and by-study HPP slope
model <- lmer(
  LookingTime ~         # response variable
    Item * HPP +        # fixed effects ("*" means "include the interaction")
    (1 + Item | Participant) + # by-Participant random intercept
    (1 + HPP*Item | Study),  # by-study random intercept and HPP random slope
  data = data,          # indicate dataset
  REML = TRUE           # fit using REML
) 
# model fails to converge
summary(model) # model summary

### 2. Drop by-participant Item random slopes
model2 <- lmer(
  LookingTime ~      # response variable
    Item * HPP +        # fixed effects ("*" means "include the interaction")
    (1 | Participant) + # by-Participant random intercept
    (1 + HPP*Item | Study),  # by-study random intercept and HPP random slope
  data = data,          # indicate dataset
  REML = TRUE           # fit using REML
) 
# model fails to converge
summary(model2) # model summary
### 3. Drop by-study Item random slopes
model3 <- lmer(
  LogLookingTime ~      # response variable
    Item * HPP +        # fixed effects ("*" means "include the interaction")
    (1 | Participant) + # by-Participant random intercept
    (1 + HPP | Study),  # by-study random intercept and HPP random slope
  data = data,          # indicate dataset
  REML = TRUE           # fit using REML
) 
# model fails to converge
summary(model3) # model summary
### 3. Drop by-study HPP random slopes
model4 <- lmer(
  LogLookingTime ~      # response variable
    Item * HPP +        # fixed effects ("*" means "include the interaction")
    (1 | Participant) + # by-Participant random intercept
    (1 | Study),  # by-study random intercept and HPP random slope
  data = data,          # indicate dataset
  REML = TRUE           # fit using REML
) 
# model fails to converge
summary(model4)    # model summary
isSingular(model4) # no singular fit
##### summary of the model ########################################

# extract coefficients from model
coefs <- summary(model4) %$% 
  coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(term = c("(Intercept)", "Item", "HPP", "Item:HPP")) %>% 
  select(., Term = term, Coefficient = Estimate, SEM = `Std. Error`)

# confidence intervals
confints <- confint.merMod( # calculate confidence intervals
  model4,  
  method = "boot",
  level = 0.95
) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Term") %>%
  slice(4:7) %>%
  mutate(Term = c("(Intercept)", "Item", "HPP", "Item:HPP"))

#### null-hypothesis testing #######################################
anova <- Anova(model4, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F test) using Satterthwaite for df
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  right_join(., coefs, by = "Term") %>% # join outcome with coefficients
  left_join(., confints, by = "Term") %>% # join outcome with confidence intervals
  mutate(ci1 = round(`2.5 %`, 2),
         ci2 = round(`97.5 %`, 2)) %>%
  unite(col = "CI95", ci1, ci2, sep = ", ") %>% # make a string with the lower and upper CI
  rename(ci1 = `2.5 %`,
         ci2 = `97.5 %`,
         p = `Pr(>F)`) %>%
  select(Term, `F`, Df, Df.res, Coefficient, SEM, ci1, ci2, CI95, Coefficient, Df.res, p)

#### predictions for plotting interaction graph ###################
effects <- effect(term = "Item*HPP", mod = model4) %>%
  as.data.frame() %>%
  mutate(fit.exp = exp(fit))

# check for multicollinearity
multicollinearity <- vif(model4) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(vif = ".") %>%
  mutate(term = c("Item", "HPP", "Item * HPP"),
         tolerance = 1/vif) 

# fitted model (predictions and residuals): raw and log-transformed looking times
fitted <- model4 %>%
  fortify() %>%
  mutate(Study = as.character(Study),
         Study = ifelse(Study=="Santolin, Saffran & Sebastian-Galles (2019)",
                        "Santolin, Saffran &\nSebastian-Galles (2019)", Study)
  )

#### visualise data ########################################################

# study-wise looking times by test item
data %>%
  group_by(Study, Item) %>%
  summarise(Mean = mean(LookingTime, na.rm = TRUE),
            SD = sd(LookingTime),
            n = n(),
            SEM = SD/sqrt(n)) %>%
  rename(LookingTime = Mean) %>%
  ungroup() %>%
  mutate(Study = as.character(Study),
         Study = ifelse(Study=="Santolin, Saffran & Sebastian-Galles (2019)",
                        "Santolin, Saffran &\nSebastian-Galles (2019)", Study)) %>%
  ggplot(aes(Item, LookingTime, fill = Item)) +
  facet_wrap(~Study,nrow=1) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = Item, ymin = LookingTime-SEM, ymax = LookingTime+SEM),
                width = 0.40, size = 0.75) +
  geom_segment(aes(x = 1, xend = 2, y = mean(data$LookingTime+1800), yend = mean(data$LookingTime+1800)), size = 0.5) +
  geom_text(aes(x = 1.5, y = mean(data$LookingTime+2000)), label = "*", size = 10) +
  labs(x = "Test item", y = "Looking time (ms)") +
  scale_fill_grey(start = 0.25, end = 0.75) +
  scale_y_continuous(limits = c(0, 10000)) +
  theme(
    panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 20),
    axis.text          = element_text(colour = "black"),
    legend.position    = "none",
    strip.background = element_rect(fill = "transparent", colour = "transparent")
  ) +
  ggsave(here("Figures", "HPP3", "01_lookingtimes-study-3.png"), height = 6, width = 12)

# looking times against HPP
ggplot(data, aes(x = Item, y = LookingTime, fill = Item)) +
  facet_wrap(~HPP, nrow = 1, strip.position = "bottom") +
  stat_summary(fun.y = mean, geom = "bar", size = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, size = 0.75) +
  labs(x = "HPP", y = "Looking time (ms)") +
  scale_colour_grey(start = 0.25, end = 0.75) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(
    panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 20),
    axis.text          = element_text(colour = "black"),
    legend.position    = "none",
    strip.placement = "outside" 
  ) +
  ggsave(here("Figures", "HPP3", "02_lookingtimes-hpp-3.png"), height = 5, width = 12)

# coefficients
ggplot(data = filter(anova, Term != "(Intercept)"), aes(Term, Coefficient)) +
  geom_linerange(aes(x = Term, ymin = ci1, ymax = ci2), alpha = 0.5, size = 10) +
  geom_point(size = 5, colour = "black") +
  geom_errorbar(aes(ymax = Coefficient+SEM, ymin = Coefficient-SEM), size = 1.5, width = 0, colour = "black") +
  geom_hline(yintercept = 0) +
  labs(x = "Term", y = "Coefficient") +
  coord_flip() +
  theme(
    panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor.x = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 15),
    axis.text          = element_text(colour = "black"),
    legend.position    = "none"
  ) +
  ggsave(here("Figures", "HPP3", "03_coefficients-3.png"), height = 4,width=10)

# add interaction plot 
ggplot(effects, aes(x = HPP, y = fit, linetype = Item)) +
  geom_line(size = 1.25) +
  labs(x = "HPP",
       y = "Looking time (ms)",
       colour = "Test item",
       fill = "Test item",
       shape = "Test item") +
  scale_linetype_discrete() +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(1, 3, by = 1)) +
  theme(
    panel.grid         = element_line(colour = "grey", linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 25),
    axis.text          = element_text(colour = "black"),
    legend.position    = c(0.25, 0.15),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "transparent")
  ) +
  ggsave(here("Figures", "HPP3", "0_interaction-3.png"), width = 10, height = 5)

# model assumptions: normality
ggplot(fitted, aes(sample = .resid)) +
  geom_qq(colour = "grey") +
  geom_qq_line(colour = "black", size = 1) +
  labs(x = "Theoretical", y = "Sample", colour = "Trial") +
  theme(
    panel.grid.major.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 15),
    axis.text          = element_text(colour = "black"),
    legend.position    = "right"
  ) +
  ggplot(fitted, aes(sample = .resid)) +
  facet_wrap(~Study) +
  geom_qq(colour = "grey") +
  geom_qq_line(size = 1, colour = "black") +
  labs(x = "Theoretical", y = "Sample", colour = "Study") +
  theme(
    panel.grid.major.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 15),
    axis.text          = element_text(colour = "black"),
    legend.position    = "none"
  ) +
  ggplot(fitted, aes(x = .resid)) +
  geom_density(fill = "transparent") +
  labs(x = "Residuals", y = "Density") +
  theme(
    panel.grid.major.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 12),
    axis.text          = element_text(colour = "black"),
    legend.position    = "none"
  ) +
  ggplot(fitted, aes(x = .resid)) +
  facet_wrap(~Study) +
  geom_density(fill = "transparent") +
  labs(x = "Residuals", y = "Density", colour = "Study",
       title = "Distribution of residuals: Cheking for normality") +
  theme(
    panel.grid.major.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 15),
    axis.text          = element_text(colour = "black"),
    legend.position    = "none"
  ) +
  plot_layout(nrow = 2, ncol = 2) +
  ggsave(here("Figures", "HPP3", "04_model-assumptions-normality-3.png"),width = 10,height = 5)

# model assumptions: homoskedasticity
data.frame(
  studentised_residual = rstudent(model3),
  fitted = fitted(model3),
  study = data$Study
) %>%
  mutate(
    study = as.character(study),
    study = ifelse(study=="Santolin, Saffran & Sebastian-Galles (2019)",
                   "Santolin, Saffran &\nSebastian-Galles (2019)", study)
  ) %>%
  ggplot(aes(fitted, studentised_residual)) +
  facet_wrap(~study) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_smooth(method = "lm", fullrange = TRUE, alpha = 0.5, colour = "black") +
  labs(x = "Predicted", y = "Studentised residuals", colour = "Study", fill = "Study") +
  theme(
    panel.grid.major.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 15),
    axis.text          = element_text(colour = "black"),
    legend.position    = "right"
  ) +
  ggsave(here("Figures", "HPP3", "04_model-assumptions-homoskedasticity-3.png"), width=10, height=5)

#### export results ########################################################
write.table(data, here("Data", "HPP3", "01_data-processed-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(anova, here("Data", "HPP3", "02_results-lmem-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(effects, here("Data", "HPP3", "02_results-effects-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(multicollinearity, here("Data", "HPP3", "02_results-multicollinearity-3.csv"), sep = ",", dec = ".", row.names = FALSE)


