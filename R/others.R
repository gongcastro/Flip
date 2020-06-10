# Extra figures and tables

#### 1. Set up ########################################################

# load packages
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(ggplot2)     # for visualising data
library(correlation) # for computing the Pearson correlation
library(papaja)      # for formatting p-values
library(janitor)     # for cleaning column namaes
library(here)        # for locating files

# import functions
source(here("R", "functions.R"))

#### 2. Import data #####################################################
dat <- read.csv(file = here("Data","00_data-raw.csv"), header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  pivot_longer(cols      = c("familiar", "novel"),  # change to long format
               names_to  = "item",
               values_to = "looking_time") %>%
  mutate(item        = ifelse(item=="Familiar", 0, 1), # item dummy coding, familiar items as baseline
         item_center = ifelse(item==0, -0.5, 0.5),     # item effect coding
         item_novel  = ifelse(item==1, 0, -1),         # item dummy coding, novel items as baseline
         study       = factor(case_when( # recode variable with more informative labels
           study == "Santolin" & location == "Barcelona" ~ "Santolin, Saffran & Sebastian-Galles (2019)",
           study == "Santolin" & location == "Wisconsin" ~ "Santolin & Saffran (2019)",
           study ==  "Saffran & Wilson"                  ~ "Saffran & Wilson (2003)",
           study == "SaffranHauser1"                     ~ "Saffran et al. (2008)",
           TRUE                                          ~ ""))) %>%
  distinct(participant, .keep_all = TRUE)

# HPP vs total studies
visits <- dat %>% group_by(hpp, total_studies) %>% summarise(counts = n())

corr <- cor_test(dat, x = "total_studies", y = "hpp", method = "pearson") %>%
  as.data.frame() %>%
  clean_names()

ggplot(visits, aes(x = total_studies, y = hpp, size = counts)) +
  geom_smooth(method = "lm", formula = "y ~ x", colour = "black", show.legend = FALSE) +
  annotate(x = 3.5, y = 7, geom = "text",
           label = paste0("r(", corr$df, ") = ", round(corr$r, 2), ", p = ", printp(corr$p), ", R2 = ", round(corr$r^2, 2), ", 95% CI = [", round(corr$CI_low, 2), ", ", round(corr$CI_high, 2), "]"),
           size = 4.5) +
  geom_point() +
  labs(x = "Total Visits", y = "HPP Visits\n", size = "Number of\nparticipants") +
  scale_size(breaks = seq(0, 30, by = 5)) +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  theme_custom +
  theme(legend.position    = c(0.6, 0.1),
        legend.direction = "horizontal") +
  ggsave(here("Figures", "00_visits.png"), height = 7, width = 7) +
  ggsave(here("Figures", "00_visits.pdf"), height = 7, width = 7)

#### export data ###################################
write.table(corr, here("Data", "04_total-vs-hpp.csv"), sep = ",", dec = ".", row.names = FALSE)


