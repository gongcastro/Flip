#### Import, process, analyse, and export data #####################
# 1-6 HPP version

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
library(purrr)       # for functional programming
library(effects)     # for predicting
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
  pivot_longer(
    cols      = c("Familiar", "Novel"),
    names_to  = "Item",
    values_to = "LookingTime"
  ) %>%
  mutate(
    LogLookingTime = log(LookingTime),
    Item = fct_relevel(Item, "Novel", after = 1),     # Item dummy coding, familiar items as baseline
    ItemCenter = ifelse(Item=="Familiar", -0.5, 0.5), # Item effect coding
    ItemNovel = ifelse(Item=="Novel", 0, -1),         # Item dummy coding, novel items as baseline
    Study = factor(case_when(Study == "Santolin" & Location == "Barcelona" ~ "Santolin, Saffran & Sebastian-Galles (2019)",
                      Study == "Santolin" & Location == "Wisconsin" ~ "Santolin & Saffran (2019)",
                      Study ==  "Saffran & Wilson"                  ~ "Saffran & Wilson (2003)",
                      Study == "SaffranHauser1"                     ~ "Saffran et al. (2008)",
                      TRUE                                          ~ ""))
  )

# import results from Bayesian LMEM
model.bayesian <- read.csv(here("Data", "02_lmem-bayesian.csv"), header = TRUE, stringsAsFactors = FALSE, sep = ",") %>%
  select(1:5) %>%
  mutate(Term = c("(Intercept)", "Item", "HPP", "Item:HPP"))
colnames(model.bayesian) <- c("Term", "Estimate", "SE", "ci.low", "ci.upp")
model.bayesian <- model.bayesian %>%
  rename_at(2:5, ~paste0(., "_bayesian"))

#### 3. Linear Mixed-Effects Model #####################################
### 3.1. Fit maximal model: random by-participant and by-study intercepts and by-study HPP slope
model <- lmer(
  LookingTime ~         # response variable
    Item * HPP +        # fixed effects ("*" means "include the interaction")
    (1 | Participant) + # by-Participant random intercept
    (1 + Item*HPP | Study),  # by-study random intercept and HPP random slope
  data = data,          # indicate dataset
  REML = TRUE           # fit using REML
) 
summary(model)                 # model summary: correlation parameter (by-study intercepts and HPP slopes) is at boundary (-1)
shapiro.test(residuals(model)) # check for non-normality: strong evidence against normality of residuals
hist(residuals(model))         # histogram of residuals
### 3.2.a. Fit the same LMEM on log-transformed looking times (Item dummy-coding, baseline on familiar trials)
model.log <- lmer(
  LogLookingTime ~       # response variable
    Item*HPP +           # fixed effects ("*" means "include the interaction")
    (1 | Participant) +  # by-Participant random intercept
    (1 + HPP | Study),   # by-study random intercept and HPP random slope
  data = data,           # indicate dataset
  REML = TRUE            # fit using REML
) 
# Cholesky factor on this model is singular, but this should not impact coefficients fixed effects
summary(model.log)                               # model summary
chf.log        <- getME(model.log,"Tlist")[[2]]  # Cholesky factor
rowlengths.log <- sqrt(rowSums(chf.log*chf.log)) # unconditional correlation matrix
svd.log        <- svd(chf.log, nv = 0)$u         # singular value decomposition - Near-singular matrix
shapiro.test(residuals(model.log))               # check for non-normality: no evidence against normality of residuals
hist(residuals(model.log))                       # histogram of residuals
### 3.2.b. Fit the same LMEM on log-transformed looking times (Item effect-coding)
model.log.effect <- lmer(
  LogLookingTime ~       # response variable
    ItemCenter*HPP +           # fixed effects ("*" means "include the interaction")
    (1 | Participant) +  # by-Participant random intercept
    (1 + HPP | Study),   # by-study random intercept and HPP random slope
  data = data,           # indicate dataset
  REML = TRUE            # fit using REML
) 
# Cholesky factor on this model is singular, but this should not impact coefficients fixed effects
summary(model.log.effect)                                             # model summary
chf.log.effect        <- getME(model.log.effect,"Tlist")[[2]]         # Cholesky factor
rowlengths.log.effect <- sqrt(rowSums(chf.log.effect*chf.log.effect)) # unconditional correlation matrix
svd.log.effect        <- svd(chf.log.effect, nv = 0)$u                # singular value decomposition - Near-singular matrix
shapiro.test(residuals(model.log.effect))                             # check for non-normality: no evidence against normality of residuals
hist(residuals(model.log.effect))                                     # histogram of residuals

### 3.2.c. Fit the same LMEM on log-transformed looking times (Item dummy-coding, baseline on novel trials)
model.log.novel <- lmer(
  LogLookingTime ~       # response variable
    ItemNovel*HPP +           # fixed effects ("*" means "include the interaction")
    (1 | Participant) +  # by-Participant random intercept
    (1 + HPP | Study),   # by-study random intercept and HPP random slope
  data = data,           # indicate dataset
  REML = TRUE            # fit using REML
) 
# Cholesky factor on this model is singular, but this should not impact coefficients fixed effects
summary(model.log.novel)                                           # model summary
chf.log.novel        <- getME(model.log.novel,"Tlist")[[2]]        # Cholesky factor
rowlengths.log.novel <- sqrt(rowSums(chf.log.novel*chf.log.novel)) # unconditional correlation matrix
svd.log.novel        <- svd(chf.log.novel, nv = 0)$u               # singular value decomposition - Near-singular matrix
shapiro.test(residuals(model.log.novel))                           # check for non-normality: no evidence against normality of residuals
hist(residuals(model.log.novel))                                   # histogram of residuals

##### 4. Extract coefficients ########################################

# 4.a. Extract coefficients from model (Item dummy-coded, baseline on familiar trials)
coefs <- summary(model.log) %$% 
  coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(term = c("(Intercept)", "Item", "HPP", "Item:HPP")) %>% 
  select(., Term = term, Coefficient = Estimate, SEM = `Std. Error`)
# 4.b. Extract coefficients from model (Item effect coded)
coefs.effect <- summary(model.log.effect) %$% 
  coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(term = c("(Intercept)", "ItemCenter", "HPP", "ItemCenter:HPP")) %>% 
  select(., Term = term, Coefficient = Estimate, SEM = `Std. Error`)
# 4.c. Extract coefficients from model (Item dummy-coded, baseline on novel trials)
coefs.novel <- summary(model.log.novel) %$% 
  coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(term = c("(Intercept)", "ItemNovel", "HPP", "ItemNovel:HPP")) %>% 
  select(., Term = term, Coefficient = Estimate, SEM = `Std. Error`)

#### 5. Compute bootstrapped confidence intervals ###############################

# 5.a. Confidence intervals (Item dummy-coded, baseline on familiar trials)
confints <- confint.merMod( # calculate confidence intervals
  model.log,  
  method = "boot",
  level = 0.95
) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Term") %>%
  slice(6:9) %>%
  mutate(Term = c("(Intercept)", "Item", "HPP", "Item:HPP"))
# 5.b. Confidence intervals (Item effect-coded)
confints.effect <- confint.merMod( # calculate confidence intervals
  model.log.effect,  
  method = "boot",
  level = 0.95
) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Term") %>%
  slice(6:9) %>%
  mutate(Term = c("(Intercept)", "ItemCenter", "HPP", "ItemCenter:HPP"))
# 5.c. Confidence intervals (Item dummy-coded, baseline on novel trials)
confints.novel <- confint.merMod( # calculate confidence intervals
  model.log.novel,  
  method = "boot",
  level = 0.95
) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Term") %>% 
  slice(6:9) %>%
  mutate(Term = c("(Intercept)", "ItemNovel", "HPP", "ItemNovel:HPP"))

#### 6. ANOVA ##########################################################################
# 6.a. F-tests on fixed effects' coefficients (Item dummy-coded, baseline on familiar trials)
anova <- Anova(model.log, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
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
  select(Term, `F`, Df, Df.res, Coefficient, SEM, ci1, ci2, CI95, Coefficient, Df.res, p) %>%
  left_join(., model.bayesian, by = "Term")
# 6.b. F-tests on fixed effects' coefficients (Item effect-coded)
anova.effect <- Anova(model.log.effect, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  right_join(., coefs.effect, by = "Term") %>% # join outcome with coefficients
  left_join(., confints.effect, by = "Term") %>% # join outcome with confidence intervals
  mutate(ci1 = round(`2.5 %`, 2),
         ci2 = round(`97.5 %`, 2)) %>%
  unite(col = "CI95", ci1, ci2, sep = ", ") %>% # make a string with the lower and upper CI
  rename(ci1 = `2.5 %`,
         ci2 = `97.5 %`,
         p = `Pr(>F)`) %>%
  select(Term, `F`, Df, Df.res, Coefficient, SEM, ci1, ci2, CI95, Coefficient, Df.res, p) %>%
  left_join(., model.bayesian, by = "Term")
# 6.c. F-tests on fixed effects' coefficients (Item dummy-coded, baseline on novel trials)
anova.novel <- Anova(model.log.novel, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  right_join(., coefs.novel, by = "Term") %>% # join outcome with coefficients
  left_join(., confints.novel, by = "Term") %>% # join outcome with confidence intervals
  mutate(ci1 = round(`2.5 %`, 2),
         ci2 = round(`97.5 %`, 2)) %>%
  unite(col = "CI95", ci1, ci2, sep = ", ") %>% # make a string with the lower and upper CI
  rename(ci1 = `2.5 %`,
         ci2 = `97.5 %`,
         p = `Pr(>F)`) %>%
  select(Term, `F`, Df, Df.res, Coefficient, SEM, ci1, ci2, CI95, Coefficient, Df.res, p) %>%
  left_join(., model.bayesian, by = "Term")

#### 7. Get predicted values ###############################################################
# 7.a. Predictions for plotting interaction graph (Item dummy-coded, baseline at familiar trials)
effects <- effect(term = "Item*HPP", mod = model.log) %>%
  as.data.frame() %>%
  mutate(fit.exp = exp(fit))
# 7.b. Predictions for plotting interaction graph (Item effect-coded)
effects.effect <- effect(term = "ItemCenter*HPP", mod = model.log.effect) %>%
  as.data.frame() %>%
  mutate(fit.exp = exp(fit))
# 7.c. Predictions for plotting interaction graph (Item dummy-coded, baseline at novel trials)
effects.novel <- effect(term = "ItemNovel*HPP", mod = model.log.novel) %>%
  as.data.frame() %>%
  mutate(fit.exp = exp(fit))

#### 8. Check other assumptions ################################################################
# 8.a. Check for multicollinearity (Item dummy-coded, baseline at familiar trials)
multicollinearity <- vif(model.log) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(vif = ".") %>%
  mutate(term = c("Item", "HPP", "ItemCenter * HPP"),
         tolerance = 1/vif) 
# 8.b. Check for multicollinearity (Item effect-coded)
multicollinearity.effect <- vif(model.log.effect) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(vif = ".") %>%
  mutate(term = c("ItemCenter", "HPP", "ItemCenter * HPP"),
         tolerance = 1/vif)
# 8.c. Check for multicollinearity (Item dummy-coded, baseline at novel trials)
multicollinearity.novel <- vif(model.log.novel) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(vif = ".") %>%
  mutate(term = c("ItemNovel", "HPP", "ItemNovel * HPP"),
         tolerance = 1/vif) 

#### 9. Gather info from model #######################################################
# 9.a. Fitted model (predictions and residuals): raw and log-transformed looking times (Item dummy-coded, baseline on familiar trials)
fitted <- map(list(model, model.log), ~fortify(.)) %>%
  set_names(c("Raw", "Log-transformed")) %>%
  bind_rows(.id = "Model") %>%
  mutate(Model = fct_inorder(Model),
         .fittedExp = exp(.fitted),
         Study = as.character(Study),
         Study = ifelse(Study=="Santolin, Saffran & Sebastian-Galles (2019)",
                        "Santolin, Saffran &\nSebastian-Galles (2019)", Study)
  )
# 9.b. Fitted model (predictions and residuals): raw and log-transformed looking times (Item effect-coded)
fitted.effect <- map(list(model, model.log.effect), ~fortify(.)) %>%
  set_names(c("Raw", "Log-transformed")) %>%
  bind_rows(.id = "Model") %>%
  mutate(Model = fct_inorder(Model),
         .fittedExp = exp(.fitted),
         Study = as.character(Study),
         Study = ifelse(Study=="Santolin, Saffran & Sebastian-Galles (2019)",
                        "Santolin, Saffran &\nSebastian-Galles (2019)", Study)
  )
# 9.c. Fitted model (predictions and residuals): raw and log-transformed looking times (Item dummy-coded, baseline on novel trials)
fitted.novel <- map(list(model, model.log.novel), ~fortify(.)) %>%
  set_names(c("Raw", "Log-transformed")) %>%
  bind_rows(.id = "Model") %>%
  mutate(Model = fct_inorder(Model),
         .fittedExp = exp(.fitted),
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
  ggsave(here("Figures", "01_lookingtimes-study.png"), height = 6, width = 12)

# 10.2. Looking times against HPP
ggplot(data, aes(x = Item, y = LookingTime, fill = Item)) +
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
data %>% group_by(Study, HPP, Participant) %>%
  summarise(LookingTime = mean(LookingTime)) %>%
  ggplot(aes(x = HPP, y = LookingTime)) +
  stat_summary(fun.y = mean, geom = "line", size = 0.5) +
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

# 10.4. Coefficients
ggplot(data = filter(anova, Term != "(Intercept)"), aes(Term, Coefficient)) +
  geom_linerange(aes(x = Term, ymin = ci1, ymax = ci2), alpha = 0.5, size = 10) +
  geom_point(size = 5, colour = "black") +
  geom_errorbar(aes(ymax = Coefficient+SEM, ymin = Coefficient-SEM), size = 1.5, width = 0, colour = "black") +
  geom_errorbar(aes(ymax = ci.upp_bayesian, ymin = ci.low_bayesian), size = 1, width = 0, colour = "grey", position = position_nudge(x = 0.25)) +
  geom_point(aes(y = Estimate_bayesian), size = 3, colour = "black", shape = 5, position = position_nudge(x = 0.25)) +
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
  ggsave(here("Figures", "03_coefficients.png"), height = 4,width=10)

# 10.5. Interaction plot 
ggplot(effects, aes(x = HPP, y = fit.exp, linetype = Item)) +
  geom_line(size = 1.25) +
  labs(x = "HPP visits",
       y = "Looking time (ms)",
       colour = "Test item",
       fill = "Test item",
       shape = "Test item") +
  scale_linetype_discrete() +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  theme(
    panel.grid         = element_line(colour = "grey", linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 25),
    axis.text          = element_text(colour = "black"),
    legend.position    = c(0.25, 0.15),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "transparent")
  ) +
  ggsave(here("Figures", "03_interaction.png"), width = 10, height = 5)

# 10.6. Model assumptions: normality
ggplot(filter(fitted, Model=="Log-transformed"), aes(sample = .resid)) +
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
  ggplot(filter(fitted, Model=="Log-transformed"), aes(sample = .resid)) +
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
  ggplot(filter(fitted, Model=="Log-transformed"), aes(x = .resid)) +
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
  ggplot(filter(fitted, Model=="Log-transformed"), aes(x = .resid)) +
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

# 10.7. Model assumptions: normality (raw vs. log-transformed)
ggplot(fitted, aes(sample = .resid)) +
  facet_wrap(~Model, scales = "free_y") +
  geom_qq(alpha = 0.7, colour = "grey") +
  geom_qq_line(colour = "black", size = 1) +
  labs(x = "Theoretical distribution", y = "Empirical distribution", colour = "Trial") +
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
  ggplot(fitted, aes(x = .resid)) +
  facet_wrap(~Model, scales = "free") +
  geom_density(fill = "transparent") +
  labs(x = "Residuals", y = "Density", colour = "Study") +
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
  plot_layout(nrow = 2) +
  ggsave(here("Figures", "04_model-assumptions-normality-rawlog.png"))

# 10.8. Model assumptions: homoskedasticity
data.frame(
  studentised_residual = rstudent(model.log),
  fitted = fitted(model.log),
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

