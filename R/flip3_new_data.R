#### Import, process, analyse, and export data #####################
# 1-3 HPP version

#### set up ########################################################

# load packages
library(tibble)      # for tidy datasets
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(forcats)     # for recoding factors
library(ggplot2)     # for visualising data
library(lmerTest)    # for p-values in LMEM
library(lme4)        # for Linear-Mixed Effects Models (LMEM)
library(car)         # for ANOVA
library(effects)     # for predicting
library(broom.mixed) # for tidy lmer output
library(rlang)       # for calling arguments in functions
library(patchwork)   # for arranging plots
library(here)        # for locating files

#### import and process data ##############################################
data <- read.csv(
  file             = here("Data", "additional_data","00_data-raw_with_SaffranHauser.csv"),
  header           = TRUE,
  sep              = ",",
  dec              = ".",
  stringsAsFactors = TRUE
) %>%
  filter(HPP < 4) %>%
  as_tibble() %>%
  pivot_longer(
    cols      = c("Familiar", "Novel"),
    names_to  = "Item",
    values_to = "LookingTime"
  ) %>%
  mutate(
    Item = fct_relevel(Item, "Novel", after = 1), # make familiar items the baseline
    HPPCenter = (HPP - mean(HPP)),
    Study = factor(Study, levels = c("Santolin", "Saffran & Wilson", "SaffranHauser1")),
    Study2 = case_when(Study == "Santolin" & Location == "Barcelona" ~ "Replication study",
                       Study == "Santolin" & Location == "Wisconsin" ~ "Santolin & Saffran (2019)",
                       Study  ==  "Saffran & Wilson"                 ~ "Saffran & Wilson (2003)",
                       Study == "SaffranHauser1"                     ~ "Saffran et al. (2008)",
                       TRUE ~ ""
    )
  )


#### Linear Mixed-Effects Model #####################################

# 1. maximal model: random intercepts and slopes for all random effects
model1 <- lmer(
  LookingTime ~                # response variable
    Item * HPP +               # fixed effects ("*" means "include the interaction")
    (1 + HPP | Participant) +  # by-Participant random intercept and HPP random slope
    (1 + HPP | Study),         # by-study random intercept and HPP random slope
  data = data,                 # indicate dataset
  REML = TRUE                  # fit using REML
) 

# 2. drop random slope of HPP on participants
model2 <- lmer(
  LookingTime ~               # response variable
    Item * HPP +  # fixed effects ("*" means "include the interaction")
    (1 | Participant) +       # by-participant random intercept
    (1 + HPP | Study),  # by-study random intercept and HPP random slope
  data = data,                # indicate dataset
  REML = TRUE                 # fit using REML
) 

# 3. random intercepts by participant and study (no random slopes)
model3 <- lmer(
  LookingTime ~              # response variable
    Item * HPP + # fixed effects ("*" means "include the interaction")
    (1 | Participant) +      # by-participant random intercept
    (1 | Study),  # by-study random intercept and HPP random slope
  data = data,               # indicate dataset
  REML = TRUE               # fit using REML
) 

##### summary of the model ########################################
summary <- summary(model3) %$% # extract coefficients from model
  coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  mutate(term = c("(Intercept)", # rename terms to join datasets 
                  "Item",
                  "HPP",
                  "Item:HPP")) %>% 
  select(., Term = term, Coefficient = Estimate, SEM = `Std. Error`)

# confidence intervals
# set seed here for  confidence interval bootstrapping (otherwise will vary slightly from run to run)
set.seed(100)
confidence.intervals <- confint.merMod( # calculate confidence intervals
  model3,  
  method = "boot",
  level = 0.95
) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Term") %>%
  slice(4:9) %>%
  mutate(Term = c("(Intercept)" , # rename terms to join datasets 
                  "Item",
                  "HPP",
                  "Item:HPP"))

#### null-hypothesis testing #######################################
anova <- Anova(model3, type = "III", test.statistic = "F") %>% # perform type III ANOVA Kenward-Roger F-tests
  as.data.frame() %>%
  rownames_to_column("Term") %>%
  right_join(., summary, by = "Term") %>% # join outcome with coefficients
  left_join(., confidence.intervals, by = "Term") %>% # join outcome with confidence intervals
  mutate(ci1 = round(`2.5 %`, 2),
         ci2 = round(`97.5 %`, 2)) %>%
  unite(col = "CI95", ci1, ci2, sep = ", ") %>% # make a string with the lower and upper CI
  rename(ci1 = `2.5 %`,
         ci2 = `97.5 %`,
         p = `Pr(>F)`) %>%
  select(Term, `F`, Df, Df.res, Coefficient, SEM, ci1, ci2, CI95, Coefficient, Df.res, p)

# predictions for plotting interaction graph
effects <- effect(term = "Item*HPP", mod = model3) %>%
  as.data.frame()

# posterior predictive sampling
iqrvec      <- sapply(simulate(model3, 1000), IQR)
obsval      <- IQR(data$LookingTime)
post.pred.p <- mean(obsval >= c(obsval, iqrvec))

# check for non-normality
normality <- shapiro.test(residuals(model3)) # residuals are non-normal

# check for multicollinearity
multicollinearity <- vif(model3) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(vif = ".") %>%
  mutate(term = c("Item", "HPP", "Item * HPP"),
         tolerance = 1/vif) 

#### visualise data ########################################################

# study-wise looking times by test item
data %>%
  group_by(Study2, Item) %>%
  summarise(Mean = mean(LookingTime, na.rm = TRUE),
            SD = sd(LookingTime),
            n = n(),
            SEM = SD/sqrt(n)) %>%
  rename(LookingTime = Mean) %>%
  ggplot(aes(Item, LookingTime, fill = Item)) +
  facet_wrap(~Study2,nrow=1) +
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
  ggsave(here("Figures/HPP3", "01_lookingtimes-study-wSaffranHauser1.png"), height = 5, width = 12)

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
    text               = element_text(colour = "black", size = 25),
    axis.text          = element_text(colour = "black"),
    legend.position    = "none",
    strip.placement = "outside" 
  ) +
  ggsave(here("Figures/HPP3", "02_lookingtimes-hpp-wSaffranHauser.png"), height = 5, width = 12)

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
    text               = element_text(colour = "black", size = 25),
    axis.text          = element_text(colour = "black"),
    legend.position    = "none"
  ) +
  ggsave(here("Figures/HPP3", "03_coefficients-wSaffranHauser.png"), height = 4,width=10)

# add interaction plot 
ggplot(effects, aes(x = HPP, y = fit, linetype = Item)) +
  geom_line(size = 1.25) +
  labs(x = "HPP",
       y = "Looking time (ms)",
       colour = "Test item",
       fill = "Test item",
       shape = "Test item") +
  scale_linetype_discrete() +
  scale_y_continuous(limits = c(6000, 7250), breaks = seq(6000, 7250, by = 250)) +
  scale_x_continuous(breaks = seq(1, 3, by = 1)) +
  theme(
    panel.grid = element_line(colour = "grey", linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 25),
    axis.text          = element_text(colour = "black"),
    legend.position    = c(0.25, 0.15),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "transparent")
  ) +
  ggsave(here("Figures/HPP3", "03_interaction-wSaffranHauser.png"), width = 10, height = 5)

# model assumptions: normality
ggplot(data = fortify(model3), aes(sample = .resid)) +
  geom_qq(alpha = 0.3) +
  geom_qq_line(colour = "black", size = 1) +
  labs(x = "Theoretical", y = "Sample", colour = "Trial") +
  theme(
    panel.grid.major.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 25),
    axis.text          = element_text(colour = "black"),
    legend.position    = "right"
  ) +
  ggplot(data = fortify(model3), aes(sample = .resid)) +
  facet_wrap(~Study) +
  geom_qq(alpha = 0.3) +
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
  ggplot(data = fortify(model3), aes(x = .resid)) +
  geom_density(fill = "transparent") +
  labs(x = "Residuals", y = "Density") +
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
  ggplot(data = fortify(model3), aes(x = .resid)) +
  facet_wrap(~Study) +
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
  plot_layout(nrow = 2, ncol = 2) +
  ggsave(here("Figures/HPP3", "04_model-assumptions-normality-wSaffranHauser.png"),width=10,height=5)

# model assumptions: homoskedasticity
data.frame(
  studentised_residual = rstudent(model3),
  fitted = fitted(model3),
  study = data$Study
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
  ggsave(here("Figures/HPP3", "04_model-assumptions-homoskedasticity-wSaffranHauser.png"),width=10,height=5)

#### export results ########################################################
write.table(data, here("Data/HPP3", "01_data-processed-3-wSaffranHauser.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(anova, here("Data/HPP3", "02_results-lmem-3-wSaffranHauser.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(effects, here("Data/HPP3", "02_results-effects-3-wSaffranHauser.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(multicollinearity, here("Data/HPP3", "02_results-multicollinearity-3-wSaffranHauser.csv"), sep = ",", dec = ".", row.names = FALSE)
write(post.pred.p, here("Data/HPP3", "02_results-posterior-3-wSaffranHauser.txt"))


