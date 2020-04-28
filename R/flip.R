#### Import, process, analyse, and export data #####################

#### 1. Set up ########################################################

# load packages
library(magrittr)    # for using pipes
library(tibble)      # for tidy datasets
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(forcats)     # for recoding factors
library(ggplot2)     # for visualising data
library(lme4)        # for Linear-Mixed Effects Models (LMEM)
library(car)         # for ANOVA
library(ggeffects)   # for predicting
library(patchwork)   # for arranging plots
library(here)        # for locating files

# set parameters
set.seed(100) # for reproducible confidence interval bootstrapping

#### 2. Import and process data ##############################################
data <- read.csv(
  file             = here("Data","00_data-raw.csv"),
  header           = TRUE,
  sep              = ",",
  dec              = ".",
  stringsAsFactors = TRUE
) %>%
  as_tibble() %>%
  pivot_longer( # change to long format
    cols      = c("Familiar", "Novel"),
    names_to  = "Item",
    values_to = "LookingTime"
  ) %>%
  mutate(
    HPP        = as.numeric(HPP), # make HPP an integer
    Item       = ifelse(Item=="Familiar", 0, 1), # item dummy coding, familiar items as baseline
    ItemCenter = ifelse(Item==0, -0.5, 0.5),     # item effect coding
    ItemNovel  = ifelse(Item==1, 0, -1),         # item dummy coding, novel items as baseline
    Study = factor(case_when( # recode variable with more informative labels
      Study == "Santolin" & Location == "Barcelona" ~ "Santolin, Saffran & Sebastian-Galles (2019)",
      Study == "Santolin" & Location == "Wisconsin" ~ "Santolin & Saffran (2019)",
      Study ==  "Saffran & Wilson"                  ~ "Saffran & Wilson (2003)",
      Study == "SaffranHauser1"                     ~ "Saffran et al. (2008)",
      TRUE                                          ~ ""))
  ) 

#### 3. Linear Mixed-Effects Model #####################################
### 3.1. Fit maximal model: random by-participant and by-study intercepts and by-study HPP and Item slopes
model <- lmer(
  LookingTime ~              # response variable
    Item * HPP +             # fixed effects ("*" means "include the interaction")
    (1 + Item*HPP | Study/Participant),  # by-study random intercept and HPP random slope
  data = data,               # indicate dataset
  REML = TRUE                # fit using REML
) 
# model fails to converge
### 3.2.a. Drop Item-by-study random slopes (Item dummy-coding, baseline on familiar trials)
model2 <- lmer(
  LookingTime ~                    # response variable
    Item*HPP +                     # fixed effects ("*" means "include the interaction")
    (1 + HPP | Study/Participant), # by-study random intercept and HPP random slope
  data = data,                     # indicate dataset
  REML = TRUE                      # fit using REML
) 
# model fails to converge
### 3.3.a. We drop the random slopes term to avoid singular fit
model3 <- lmer(
  LookingTime ~              # response variable
    Item*HPP +               # fixed effects ("*" means "include the interaction")
    (1 | Study/Participant), # by-study random intercept
  data = data,               # indicate dataset
  REML = TRUE                # fit using REML
) 
summary(model3)          # model summary
### 3.3.b. Fit the same LMEM with effect-coding on Item
model3.effect <- lmer(
  LookingTime ~              # response variable
    ItemCenter*HPP +         # fixed effects ("*" means "include the interaction")
    (1 | Study/Participant), # by-study random intercept
  data = data,               # indicate dataset
  REML = TRUE                # fit using REML
) 
summary(model3.effect)   # model summary
### 3.3.c. Fit the same LMEM with dummy-coding Item on, baseline on novel trials
model3.novel <- lmer(
  LookingTime ~              # response variable
    ItemNovel*HPP +          # fixed effects ("*" means "include the interaction")
    (1 | Study/Participant), # by-study random intercept
  data = data,               # indicate dataset
  REML = TRUE                # fit using REML
) 
# Cholesky factor on this model is singular, but this should not impact coefficients fixed effects
summary(model3.novel)    # model summary

##### 4. Extract coefficients ########################################

# 4.a. Extract coefficients from model (Item dummy-coded, baseline on familiar trials)
coefs <- summary(model3) %$% 
  coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(term = c("(Intercept)", "Item", "HPP", "Item:HPP")) %>% 
  select(., Term = term, Coefficient = Estimate, SEM = `Std. Error`)
# 4.b. Extract coefficients from model (Item effect coded)
coefs.effect <- summary(model3.effect) %$% 
  coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(term = c("(Intercept)", "ItemCenter", "HPP", "ItemCenter:HPP")) %>% 
  select(., Term = term, Coefficient = Estimate, SEM = `Std. Error`)
# 4.c. Extract coefficients from model (Item dummy-coded, baseline on novel trials)
coefs.novel <- summary(model3.novel) %$% 
  coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(term = c("(Intercept)", "ItemNovel", "HPP", "ItemNovel:HPP")) %>% 
  select(., Term = term, Coefficient = Estimate, SEM = `Std. Error`)

#### 5. Compute bootstrapped confidence intervals ###############################
# 5.a. Confidence intervals (Item dummy-coded, baseline on familiar trials)
confints <- confint.merMod( # calculate confidence intervals
  model3,  
  method = "boot",
  level = 0.95
) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Term") %>%
  slice(4:7) %>%
  mutate(Term = c("(Intercept)", "Item", "HPP", "Item:HPP"))
# 5.b. Confidence intervals (Item effect-coded)
confints.effect <- confint.merMod( # calculate confidence intervals
  model3.effect,  
  method = "boot",
  level = 0.95
) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Term") %>%
  slice(4:7) %>%
  mutate(Term = c("(Intercept)", "ItemCenter", "HPP", "ItemCenter:HPP"))
# 5.c. Confidence intervals (Item dummy-coded, baseline on novel trials)
confints.novel <- confint.merMod( # calculate confidence intervals
  model3.novel,  
  method = "boot",
  level = 0.95
) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Term") %>% 
  slice(4:7) %>%
  mutate(Term = c("(Intercept)", "ItemNovel", "HPP", "ItemNovel:HPP"))

#### 6. ANOVA ##########################################################################
# 6.a. F-tests on fixed effects' coefficients (Item dummy-coded, baseline on familiar trials)
anova <- Anova(model3, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  right_join(., coefs, by = "Term") %>% # join outcome with coefficients
  left_join(., confints, by = "Term") %>% # join outcome with confidence intervals
  mutate(ci1 = round(`2.5 %`, 1),
         ci2 = round(`97.5 %`, 1)) %>%
  unite(col = "CI95", ci1, ci2, sep = ", ") %>% # make a string with the lower and upper CI
  mutate(CI95 = paste0("[", CI95, "]")) %>%
  rename(ci1 = `2.5 %`,
         ci2 = `97.5 %`,
         p = `Pr(>F)`) %>%
  select(Term, `F`, Df, Df.res, Coefficient, SEM, ci1, ci2, CI95, Coefficient, Df.res, p)
# 6.b. F-tests on fixed effects' coefficients (Item effect-coded)
anova.effect <- Anova(model3.effect, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  right_join(., coefs.effect, by = "Term") %>% # join outcome with coefficients
  left_join(., confints.effect, by = "Term") %>% # join outcome with confidence intervals
  mutate(ci1 = round(`2.5 %`, 1),
         ci2 = round(`97.5 %`, 1)) %>%
  unite(col = "CI95", ci1, ci2, sep = ", ") %>% # make a string with the lower and upper CI
  mutate(CI95 = paste0("[", CI95, "]")) %>%
  rename(ci1 = `2.5 %`,
         ci2 = `97.5 %`,
         p = `Pr(>F)`) %>%
  select(Term, `F`, Df, Df.res, Coefficient, SEM, ci1, ci2, CI95, Coefficient, Df.res, p)
# 6.c. F-tests on fixed effects' coefficients (Item dummy-coded, baseline on novel trials)
anova.novel <- Anova(model3.novel, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  right_join(., coefs.novel, by = "Term") %>% # join outcome with coefficients
  left_join(., confints.novel, by = "Term") %>% # join outcome with confidence intervals
  mutate(ci1 = round(`2.5 %`, 1),
         ci2 = round(`97.5 %`, 1)) %>%
  unite(col = "CI95", ci1, ci2, sep = ", ") %>% # make a string with the lower and upper CI
  mutate(CI95 = paste0("[", CI95, "]")) %>%
  rename(ci1 = `2.5 %`,
         ci2 = `97.5 %`,
         p = `Pr(>F)`) %>%
  select(Term, `F`, Df, Df.res, Coefficient, SEM, ci1, ci2, CI95, Coefficient, Df.res, p)

#### 7. Get predicted means ###############################################################
# 7.a. Predictions for plotting interaction graph (Item dummy-coded, baseline at familiar trials)
effects <- ggpredict(model3, terms = c("HPP", "Item")) %>%
  rename(Item = group, HPP = x) %>%
  mutate(Item = ifelse(Item==0, "Familiar", "Novel"))

# 7.b. Predictions for plotting interaction graph (Item effect-coded)
effects.effect <- ggpredict(model3.effect, terms = c("HPP", "ItemCenter")) %>%
  rename(Item = group, HPP = x) %>%
  mutate(Item = ifelse(Item==-0.5, "Familiar", "Novel"))

# 7.c. Predictions for plotting interaction graph (Item dummy-coded, baseline at novel trials)
effects.novel <- ggpredict(model3.novel, terms = c("HPP", "ItemNovel")) %>%
  rename(ItemNovel = group, HPP = x) %>%
  mutate(ItemNovel = ifelse(ItemNovel==0, "Novel", "Familiar"))

#### 8. Check other assumptions ################################################################
# 8.a. Check for multicollinearity (Item dummy-coded, baseline at familiar trials)
multicollinearity <- vif(model3) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(vif = ".") %>%
  mutate(term = c("Item", "HPP", "ItemCenter * HPP"),
         tolerance = 1/vif) 
# 8.b. Check for multicollinearity (Item effect-coded)
multicollinearity.effect <- vif(model3.effect) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(vif = ".") %>%
  mutate(term = c("ItemCenter", "HPP", "ItemCenter * HPP"),
         tolerance = 1/vif)
# 8.c. Check for multicollinearity (Item dummy-coded, baseline at novel trials)
multicollinearity.novel <- vif(model3.novel) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(vif = ".") %>%
  mutate(term = c("ItemNovel", "HPP", "ItemNovel * HPP"),
         tolerance = 1/vif) 

#### 9. Gather info from model #######################################################
# 9.a. Fitted model (predictions and residuals): looking times (Item effect-coded)
fitted <- model3 %>%
  fortify() %>%
  mutate(Item = ifelse(Item==0, "Familiar", "Novel"),
         Study = as.character(Study),
         Study = ifelse(Study=="Santolin, Saffran & Sebastian-Galles (2019)",
                        "Santolin, Saffran &\nSebastian-Galles (2019)", Study)
  )
# 9.b. Fitted model (predictions and residuals): looking times (Item effect-coded)
fitted.effect <- model3.effect %>%
  fortify() %>%
  mutate(Item = ifelse(Item==-0.5, "Familiar", "Novel"),
         Study = as.character(Study),
         Study = ifelse(Study=="Santolin, Saffran & Sebastian-Galles (2019)",
                        "Santolin, Saffran &\nSebastian-Galles (2019)", Study)
  )
# 9.c. Fitted model (predictions and residuals): looking times (Item dummy-coded, baseline on novel trials)
fitted.novel <- model3.novel %>%
  fortify() %>%
  mutate(Item = ifelse(Item==0, "Novel", "Familiar"),
         Study = as.character(Study),
         Study = ifelse(Study=="Santolin, Saffran & Sebastian-Galles (2019)",
                        "Santolin, Saffran &\nSebastian-Galles (2019)", Study)
  )

#### 10. Visualise data ########################################################
# 10.1 Study-wise looking times by test item
data %>%
  group_by(Study, Item) %>%
  summarise(Mean = mean(LookingTime, na.rm = TRUE),
            SD = sd(LookingTime),
            n = n(),
            SEM = SD/sqrt(n)) %>%
  rename(LookingTime = Mean) %>%
  ungroup() %>%
  mutate(
    Item = ifelse(Item==0, "Familiar", "Novel"),
    Study = as.character(Study),
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
  ggsave(here("Figures", "01_lookingtimes-study.png"), height = 6, width = 12)

# 10.2. Looking times against HPP
data %>%
  mutate(Item = ifelse(Item==0, "Familiar", "Novel")) %>%
  ggplot(aes(x = Item, y = LookingTime, fill = Item)) +
  facet_wrap(~HPP, nrow = 1, strip.position = "bottom") +
  stat_summary(fun.y = mean, geom = "bar", size = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, size = 0.75) +
  labs(x = "HPP Visits", y = "Looking time (ms)") +
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
  ggsave(here("Figures", "02_lookingtimes-hpp.png"), height = 5, width = 12)

# 10.3. Looking times against HPP
data %>%
  mutate(Item = ifelse(Item==0, "Familiar", "Novel")) %>%
  group_by(Study, Item, HPP, Participant) %>%
  summarise(LookingTime = mean(LookingTime)) %>%
  ggplot(aes(x = HPP, y = LookingTime, linetype = Item)) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) +
  labs(x = "HPP Visits", y = "Looking time (ms)") +
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
    legend.position    = c(0.25, 0.1),
    legend.direction = "horizontal",
    strip.placement = "outside" 
  ) +
  ggsave(here("Figures", "02_lookingtimes-hpp.png"), height = 5, width = 12)

# 10.4. Predictions
ggplot(effects, aes(x = HPP, y = predicted, shape = Item, fill = Item)) +
  geom_ribbon(aes(x = HPP, ymin = conf.low, ymax = conf.high), colour = NA, alpha = 0.5) +
  geom_line(size = 0.75) +
  geom_point(size = 5) +
  labs(x = "HPP visits",
       y = "Looking time (ms)\n",
       colour = "Test item",
       fill = "Test item",
       shape = "Test item") +
  scale_fill_grey(start = 0.25, end = 0.75) +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  theme(
    panel.grid         = element_line(colour = "grey", linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 20),
    axis.text          = element_text(colour = "black"),
    axis.title         = element_text(size = 15), 
    legend.position    = c(0.3, 0.05),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "transparent")
  ) +
  ggsave(here("Figures", "03_interaction.png"), width = 7, height = 5) +
  ggsave(here("Figures", "03_interaction.pdf"), width = 7, height = 5)


# 10.6. Model assumptions: normality
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
  ggsave(here("Figures", "04_model-assumptions-normality.png"),width = 10,height = 5)

# 10.8. Model assumptions: homoskedasticity
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
  ggsave(here("Figures", "04_model-assumptions-homoskedasticity.png"),width=10,height=5)

#### 11. Export results ########################################################
# 11.a. Export results from Item-dummy-coded model (baseline on familiar trials)
write.table(data, here("Data", "01_data-processed.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(anova, here("Data", "02_results-lmem.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(effects, here("Data", "02_results-effects.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(fitted, here("Data", "02_results-fitted.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(multicollinearity, here("Data", "02_results-multicollinearity.csv"), sep = ",", dec = ".", row.names = FALSE)
# 11.b. Export results from Item-effect-coded model
write.table(data, here("Data", "Effect-coding", "01_data-processed-effect.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(anova.effect, here("Data", "Effect-coding", "02_results-lmem-effect.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(effects.effect, here("Data", "Effect-coding", "02_results-effects-effect.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(fitted.effect, here("Data", "Effect-coding", "02_results-fitted-effect.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(multicollinearity.effect, here("Data", "Effect-coding", "02_results-multicollinearity-effect.csv"), sep = ",", dec = ".", row.names = FALSE)
# 11.c. Export results from Item-dummy-coded model (baseline on novel trials)
write.table(data, here("Data", "Dummy-coding-Novel", "01_data-processed-novel.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(anova.novel, here("Data", "Dummy-coding-Novel", "02_results-lmem-novel.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(effects.novel, here("Data", "Dummy-coding-Novel", "02_results-effects-novel.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(fitted.novel, here("Data", "Dummy-coding-Novel", "02_results-fitted-novel.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(multicollinearity.novel, here("Data", "Dummy-coding-Novel", "02_results-multicollinearity-novel.csv"), sep = ",", dec = ".", row.names = FALSE)

