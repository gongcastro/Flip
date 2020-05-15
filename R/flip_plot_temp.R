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

#import data
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


#### NEW SUGGESTED PLOTTING CODE BELOW ####

#### Fam/ Novelty Preference Plot ####

#summarize effect by participant
data_diff <- data %>%
  group_by(Participant,Location,Study,HPPprior,HPP,totalStudies) %>%
  summarize(novelty_preference = LookingTime[Item==1]-LookingTime[Item==0])

#checking dfs on correlation test
cor.test(data_diff$HPP,data_diff$totalStudies)

#collapsing within HPP
data_diff_byHPP <- data_diff %>%
  group_by(HPP) %>%
  summarize(
    N=n(),
    mean_novelty_preference=mean(novelty_preference),
    ci=qt(0.975, N-1)*sd(novelty_preference)/sqrt(N),
    ci_lower=mean_novelty_preference-ci,
    ci_upper=mean_novelty_preference+ci
            )
  
library(cowplot) #just using for default aesthetics
theme_set(theme_cowplot())
p1 <- ggplot(data_diff_byHPP,aes(HPP,mean_novelty_preference))+
  geom_hline(yintercept=0,size=1.5, linetype="dashed")+
  geom_smooth(data=data_diff,aes(HPP,novelty_preference),size=1.2,method="lm",color="#225ea8",fill="#225ea8",alpha=0.25)+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),width=0,color="#e34a33")+
  geom_point(aes(size=N),color="#e34a33",fill="#e34a33")+
  xlab("HPP visits")+
  scale_x_continuous(breaks=seq(1,6))+
  scale_y_continuous(breaks=seq(-1000,4000,1000))+
  ylab("Difference in Looking Time (ms)\n(Novel - Familiar)")+
  theme(legend.position=c(0.8,0.15))
ggsave(here("Figures", "temp_difference_looking_hpp.pdf"), height = 6, width = 6)

#### Plotting the model ####

#first, compute means for fam and novel trials by HPP
average_looking_byHPP <- data %>%
  group_by(HPP,Item) %>%
  summarize(
    N=n(),
    mean_looking_time=mean(LookingTime),
    se = sd(LookingTime)/sqrt(N),
    ci=qt(0.975, N-1)*se,
    ci_lower=mean_looking_time-ci,
    ci_upper=mean_looking_time+ci
  ) %>%
  mutate(Item = ifelse(Item==0, "Familiar", "Novel"))

# fit main model from paper
model3 <- lmer(
  LookingTime ~              # response variable
    Item*HPP +               # fixed effects ("*" means "include the interaction")
    (1 | Study/Participant), # by-study random intercept
  data = data,               # indicate dataset
  REML = TRUE                # fit using REML
) 
summary(model3)

#using predictSE from the AICcmodavg package to get standard error bands from lmer model (not quite the right SEs, but probably ok)
library(AICcmodavg)
#create the data frame to make predictions about (w/ slightly extended boundaries due to mean jittering, see plot below)
pX <- expand.grid(HPP = seq(0.9,6.1,0.1),Item=c(0,1))
#plot predictions from model3 for data frame pX w/ SEs
pY <- predictSE(model3,pX)

#clumsily build data frame for plotting
pX$LookingTime <- pY$fit
pX$se <- pY$se.fit
pX <-pX%>%
  mutate(Item = ifelse(Item==0, "Familiar", "Novel"))

#note the use of stat="identity" in geom_smooth
p2 <- ggplot(data = pX, aes(x = HPP, y = LookingTime,
                          colour = Item, shape = Item,fill = Item, linetype = Item)) +
  geom_smooth(aes(ymin=LookingTime-se,ymax=LookingTime+se),stat="identity",  alpha = 0.25)+
  geom_point(data=average_looking_byHPP,aes(x=HPP,y=mean_looking_time),position=position_dodge(0.3),size=3)+
  geom_errorbar(data=average_looking_byHPP,aes(x=HPP,y=mean_looking_time,ymin=mean_looking_time-se,ymax=mean_looking_time+se),width=0,position=position_dodge(0.3),linetype="solid")+
  scale_x_continuous(breaks=seq(1,6),limits=c(0.8,6.2))+
  scale_color_brewer(palette="Set1")+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position=c(0.2,0.2))+
  labs(x = "HPP visits", y = "Looking time (ms)\n",
       colour = "Test item", fill = "Test item", linetype = "Test item", shape = "Test item")
ggsave(here("Figures", "temp_interaction_with_means+ses.pdf"), height = 6, width = 6)

#plot together with patchwork
library(patchwork)
p1+p2+plot_annotation(tag_levels = 'A')
ggsave(here("Figures", "temp_diff_and_interaction.pdf"), height = 6, width = 13)
