#### Import, process, analyse, and export data #####################
# 1-6 HPP version - Bayesian approach

#### set up ########################################################

# load packages
library(magrittr)  # for using pipes
library(tibble)    # for more informative data sets
library(dplyr)     # for manipulating variables
library(tidyr)     # for reshaping datasets
library(stringr)   # for working with character strings
library(forcats)   # for working with factors
library(brms)      # for Bayesian LMEM
library(ggplot2)   # for visualising data
library(here)      # for locating files

# set parameters
set.seed(100) # for reproducibility

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
    Study = factor(case_when(Study == "Santolin" & Location == "Barcelona" ~ "Santolin, Saffran & Sebastian-Galles (2019)",
                             Study == "Santolin" & Location == "Wisconsin" ~ "Santolin & Saffran (2019)",
                             Study ==  "Saffran & Wilson"                  ~ "Saffran & Wilson (2003)",
                             Study == "SaffranHauser1"                     ~ "Saffran et al. (2008)"))
  )

#### Linear Mixed-Effects Model #####################################
model <- brm(
  LogLookingTime ~
    HPP * Item +
    (1 | Participant) +
    (1 + HPP| Study),
  prior = set_prior(prior = "normal(0, 3)", class = "b"),
  data = data,
  chains = 20,
  iter = 1000, 
  algorithm = "sampling",
  save_model = here("R", "flip-bayesian")
)

summary <- summary(model)
coefs <- summary$fixed %>% as.data.frame() %>% rownames_to_column("Term")

#### export data ########################################################
write.table(coefs, here("Data", "02_lmem-bayesian.csv"), sep = ",", dec = ".", row.names = FALSE)
