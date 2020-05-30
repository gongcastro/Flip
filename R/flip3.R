#### Import, process, analyse, and export data #####################

# HPP3

#### 1. Set up ########################################################

# load packages
library(magrittr)    # for using pipes
library(tibble)      # for tidy datasets
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(forcats)     # for recoding factors
library(janitor)     # for cleaning column names
library(lme4)        # for Linear-Mixed Effects Models (LMEM)
library(AICcmodavg)  # for computing SE
library(car)         # for ANOVA
library(broom.mixed) # for extracting coefficients
library(ggeffects)   # for predicting
library(ggplot2)     # for visualising data
library(patchwork)   # for arranging plots
library(here)        # for locating files

# load functions
source(here("R", "functions.R"))

# set parameters
set.seed(100) # for reproducible confidence interval bootstrapping

#### 2. Import and process data ##############################################
dat <- read.csv(here("Data","00_data-raw.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  pivot_longer(cols = c("familiar", "novel"), names_to  = "item", values_to = "looking_time") %>% # change to long format
  mutate(item        = ifelse(item=="familiar", 0, 1), # item dummy coding, familiar items as baseline
         item_center = ifelse(item==0, -0.5, 0.5),     # item effect coding
         item_novel  = ifelse(item==1, 0, -1),         # item dummy coding, novel items as baseline
         study = factor(case_when( # recode variable with more informative labels
           study == "Santolin" & location == "Barcelona" ~ "Santolin, Saffran & Sebastian-Galles (2019)",
           study == "Santolin" & location == "Wisconsin" ~ "Santolin & Saffran (2019)",
           study ==  "Saffran & Wilson"                  ~ "Saffran & Wilson (2003)",
           study == "SaffranHauser1"                     ~ "Saffran et al. (2008)",
           TRUE                                          ~ NA_character_))) %>%
  filter(hpp < 4)

# aggregate data
dat_aggregated <- dat %>%
  mutate(item = ifelse(item==0, "Familiar","Novel")) %>%
  group_by(hpp, item) %>%
  summarise(N = n(),
            mean_looking_time = mean(looking_time),
            sem = sd(looking_time)/sqrt(N),
            ci = qt(0.975, N-1)*sem,
            ci_lower = mean_looking_time-ci,
            ci_upper = mean_looking_time+ci)

# compute novelty preference
dat_diff <- dat %>%
  group_by(participant, location, study, hpp_prior, hpp, total_studies) %>%
  summarize(novelty_preference = looking_time[item==1]-looking_time[item==0])

# collapsing within HPP
dat_diff_hpp <- dat_diff %>%
  group_by(hpp) %>%
  summarise(N = n(),
            mean_novelty_preference = mean(novelty_preference),
            ci = qt(0.975, N-1)*sd(novelty_preference)/sqrt(N),
            ci_lower = mean_novelty_preference-ci,
            ci_upper = mean_novelty_preference+ci)

#### 3. Linear Mixed-Effects Model #####################################
model1        <- lmer(looking_time ~ item*hpp + (1 + item*hpp | study/participant), data = dat)  # all random intercepts and slopes (maximal, doesn't converge)
model2        <- lmer(looking_time ~ item*hpp + (1 + hpp | study/participant), data = dat) # drop Item-by-study random slopes (doesn't converge)
model3        <- lmer(looking_time ~ item*hpp + (1 | study/participant), data = dat) # drop random slopes to avoid
model3.effect <- update(model3, . ~ . -item*hpp + item_center*hpp) # effect coding
model3.novel  <- update(model3, . ~ . -item*hpp + item_novel*hpp) # baseline on novel trials (singular Cholesky factor)

##### 4. Extract coefficients ########################################
coefs        <- tidy(model3) %>% filter(effect=="fixed") %>% select(term, estimate, std.error) %>% clean_names()
coefs.effect <- tidy(model3.effect) %>% filter(effect=="fixed") %>% select(term, estimate, std.error) %>% clean_names()
coefs.novel  <- tidy(model3.novel) %>% filter(effect=="fixed") %>% select(term, estimate, std.error) %>% clean_names()

#### 5. Compute bootstrapped confidence intervals ###############################
confints <- confint.merMod(model3, method = "boot") %>%
  as.data.frame() %>%
  rownames_to_column(var = "term") %>%
  slice(4:7) %>%
  mutate(term = c("(Intercept)", "item", "hpp", "item:hpp")) %>%
  clean_names()

confints.effect <- confint.merMod(model3.effect, method = "boot") %>%
  as.data.frame() %>%
  rownames_to_column(var = "term") %>%
  slice(4:7) %>%
  mutate(term = c("(Intercept)", "item_center", "hpp", "item_center:hpp")) %>%
  clean_names()

confints.novel <- confint.merMod(model3.novel, method = "boot") %>%
  as.data.frame() %>%
  rownames_to_column(var = "term") %>%
  slice(4:7) %>%
  mutate(term = c("(Intercept)", "item_novel", "hpp", "item_novel:hpp")) %>%
  clean_names()

#### 6. ANOVA ##########################################################################
anova <- Anova(model3, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
  as.data.frame() %>%
  rownames_to_column("term") %>%
  right_join(., coefs, by = "term") %>% # join outcome with coefficients
  left_join(., confints, by = "term") %>% # join outcome with confidence intervals
  rename(ci1 = `x2_5_percent`, ci2 = x97_5_percent, p = `Pr(>F)`) %>%
  clean_names() %>%
  mutate(ci1 = round(ci1, 1), ci2 = round(ci2, 1)) %>%
  unite("ci95", ci1, ci2, sep = ", ", remove = FALSE) %>% # make a string with the lower and upper CI
  mutate(ci95 = paste0("[", ci95, "]")) %>%
  select(term, estimate, std_error, ci1, ci2, ci95, f, df, df_res, p)

anova.effect <- Anova(model3.effect, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
  as.data.frame() %>%
  rownames_to_column("term") %>%
  right_join(., coefs.effect, by = "term") %>% # join outcome with coefficients
  left_join(., confints.effect, by = "term") %>% # join outcome with confidence intervals
  rename(ci1 = `x2_5_percent`, ci2 = x97_5_percent, p = `Pr(>F)`) %>%
  clean_names() %>%
  mutate(ci1 = round(ci1, 1), ci2 = round(ci2, 1)) %>%
  unite("ci95", ci1, ci2, sep = ", ", remove = FALSE) %>% # make a string with the lower and upper CI
  mutate(ci95 = paste0("[", ci95, "]")) %>%
  select(term, estimate, std_error, ci1, ci2, ci95, f, df, df_res, p)

anova.novel <- Anova(model3.novel, type = "III", test.statistic = "F") %>% # perform type III ANOVA (KF F-test)
  as.data.frame() %>%
  rownames_to_column("term") %>%
  right_join(., coefs.novel, by = "term") %>% # join outcome with coefficients
  left_join(., confints.novel, by = "term") %>% # join outcome with confidence intervals
  rename(ci1 = `x2_5_percent`, ci2 = x97_5_percent, p = `Pr(>F)`) %>%
  clean_names() %>%
  mutate(ci1 = round(ci1, 1), ci2 = round(ci2, 1)) %>%
  unite("ci95", ci1, ci2, sep = ", ", remove = FALSE) %>% # make a string with the lower and upper CI
  mutate(ci95 = paste0("[", ci95, "]")) %>%
  select(term, estimate, std_error, ci1, ci2, ci95, f, df, df_res, p)


#### 7. Get predicted means ###############################################################
predictions <- expand.grid(hpp = seq(0.9, 6.1, 0.1), item = c(0, 1)) %>%
  as_tibble() %>%
  mutate(fit = predictSE(model3, .)$fit,
         std_error = predictSE(model3, .)$se,
         item = ifelse(item==0, "Familiar", "Novel"))

predictions.effect <- expand.grid(hpp = seq(0.9, 6.1, 0.1), item_center = c(0, 1)) %>%
  as_tibble() %>%
  mutate(fit = predictSE(model3.effect, .)$fit,
         std_error = predictSE(model3.effect, .)$se,
         item_center = ifelse(item_center==-0.5, "Familiar", "Novel"))             

predictions.novel <- expand.grid(hpp = seq(0.9, 6.1, 0.1), item_novel = c(0, 1)) %>%
  as_tibble() %>%
  mutate(fit = predictSE(model3.novel, .)$fit,
         std_error = predictSE(model3.novel, .)$se,
         item_novel = ifelse(item_novel==1, "Familiar", "Novel"))

#### 9. Visualise data ########################################################

# 9.1 Study-wise looking times by test item
study_data <- dat %>%
  group_by(study, item) %>%
  summarise(mean = mean(looking_time, na.rm = TRUE),
            sd = sd(looking_time),
            n = n(),
            std_error = sd/sqrt(n)) %>%
  rename(looking_time = mean) %>%
  ungroup() %>%
  mutate(item = ifelse(item==0, "Familiar", "Novel"),
         study = as.character(study),
         study = ifelse(study=="Santolin, Saffran & Sebastian-Galles (2019)",
                        "Santolin, Saffran &\nSebastian-Galles (2019)", study))

ggplot(study_data, aes(item, looking_time, fill = item)) +
  facet_wrap(~study,nrow=1) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = item, ymin = looking_time-std_error, ymax = looking_time+std_error),
                width = 0.40, size = 0.75) +
  geom_segment(aes(x = 1, xend = 2, y = mean(dat$looking_time+1800), yend = mean(dat$looking_time+1800)), size = 0.5) +
  geom_text(aes(x = 1.5, y = mean(dat$looking_time+2000)), label = "*", size = 10) +
  labs(x = "Test item", y = "Looking time (ms)") +
  scale_fill_grey(start = 0, end = 0.50) +
  scale_y_continuous(limits = c(0, 10000), labels = add_big_mark) +
  theme_custom +
  theme(legend.position = "none") +
  ggsave(here("Figures", "HPP3", "01_lookingtimes-study-3.png"), height = 6, width = 12)

# 9.2. Novelty preference
ggplot(dat_diff_hpp, aes(hpp, mean_novelty_preference)) +
  geom_hline(yintercept = 0, size = 1, linetype = "dotted") +
  geom_smooth(data = dat_diff, aes(hpp, novelty_preference), formula = "y ~ x",
              colour = "black", size = 1.2, method = "lm", alpha = 0.25) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0) +
  geom_point(aes(size = N))+
  xlab("HPP visits")+
  scale_x_continuous(breaks = seq(1, 3))+
  scale_y_continuous(breaks = seq(-1000, 4000, 1000), labels = add_big_mark)+
  ylab("Difference in Looking Time (ms)\n(Novel - Familiar)")+
  theme_custom +
  theme(legend.position = "top") +
  
  # 9.3. Predictions
  ggplot(predictions, aes(hpp, fit, fill = item, shape = item, linetype = item)) +
  geom_ribbon(aes(ymin = fit-std_error, ymax = fit+std_error), colour = NA, alpha = 0.5) +
  geom_line(size = 1) +
  geom_point(data = dat_aggregated, aes(x = hpp, y = mean_looking_time),
             size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(data = dat_aggregated, aes(x = hpp, y = mean_looking_time, ymax = mean_looking_time+sem, ymin = mean_looking_time-sem),
                width = 0, position = position_dodge(width = 0.3), linetype = "solid") +
  labs(x = "HPP visits", y = "Looking time (ms)\n",
       colour = "Test item", fill = "Test item", linetype = "Test item", shape = "Test item") +
  scale_y_continuous(labels = add_big_mark) +
  scale_x_continuous(breaks = seq(1, 3, by = 1)) +
  scale_fill_grey() +
  theme_custom +
  theme(legend.position = "top", 
        legend.direction = "horizontal") +
  plot_layout(guides = "keep") +
  plot_annotation(tag_levels = "A") +
  ggsave(here("Figures", "HPP3", "02_interaction-3.png"), width = 15) +
  ggsave(here("Figures", "HPP3", "02_interaction-3.pdf"), width = 15) 

#### 10. Export results ########################################################
# 10.a. Export results from Item-dummy-coded model (baseline on familiar trials)
write.table(data, here("Data", "HPP3", "01_data-processed-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(anova, here("Data", "HPP3", "02_results-lmem-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(predictions, here("Data", "HPP3", "02_results-effects-3.csv"), sep = ",", dec = ".", row.names = FALSE)
# 11.b. Export results from Item-effect-coded model
write.table(data, here("Data", "HPP3", "Effect-coding", "01_data-processed-effect-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(anova.effect, here("Data", "HPP3", "Effect-coding", "02_results-lmem-effect-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(predictions.effect, here("Data", "HPP3", "Effect-coding", "02_results-effects-effect-3.csv"), sep = ",", dec = ".", row.names = FALSE)
# 11.c. Export results from Item-dummy-coded model (baseline on novel trials)
write.table(data, here("Data", "HPP3", "Dummy-coding-Novel", "01_data-processed-novel-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(anova.novel, here("Data", "HPP3", "Dummy-coding-Novel", "02_results-lmem-novel-3.csv"), sep = ",", dec = ".", row.names = FALSE)
write.table(predictions.novel, here("Data", "HPP3", "Dummy-coding-Novel", "02_results-effects-novel-3.csv"), sep = ",", dec = ".", row.names = FALSE)

