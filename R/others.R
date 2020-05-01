# Extra figures and tables

#### 1. Set up ########################################################

# load packages
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(ggplot2)     # for visualising data
library(correlation) # for computing the Pearson correlation
library(papaja)      # for formatting p-values
library(here)        # for locating files

#### 2. Import data #####################################################
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
  rename(TotalStudies = totalStudies) %>%
  mutate(
    HPP          = as.numeric(HPP), # make HPP an integer
    Item         = ifelse(Item=="Familiar", 0, 1), # item dummy coding, familiar items as baseline
    ItemCenter   = ifelse(Item==0, -0.5, 0.5),     # item effect coding
    ItemNovel    = ifelse(Item==1, 0, -1),         # item dummy coding, novel items as baseline
    Study        = factor(case_when( # recode variable with more informative labels
      Study == "Santolin" & Location == "Barcelona" ~ "Santolin, Saffran & Sebastian-Galles (2019)",
      Study == "Santolin" & Location == "Wisconsin" ~ "Santolin & Saffran (2019)",
      Study ==  "Saffran & Wilson"                  ~ "Saffran & Wilson (2003)",
      Study == "SaffranHauser1"                     ~ "Saffran et al. (2008)",
      TRUE                                          ~ ""))
  ) 

# HPP vs total studies
visits <- data %>%
  group_by(HPP, TotalStudies) %>%
  summarise(Counts = n())

corr <- cor_test(data, x = "TotalStudies", y = "HPP", method = "pearson")

ggplot(visits, aes(x = TotalStudies, y = HPP, size = Counts)) +
  geom_smooth(method = "lm", formula = "y ~ x", colour = "black", show.legend = FALSE) +
  annotate(x = 3.5, y = 7, geom = "text",
           label = paste0("r(", corr$df, ") = ", round(corr$r, 2), ", p = ", printp(corr$p),
                          ", 95% CI = [", round(corr$CI_low, 2), ", ", round(corr$CI_high, 2), "]"),
           size = 4.5) +
  geom_point(alpha = 0.5) +
  labs(x = "Total Visits", y = "HPP Visits", size = "Number of\nparticipants") +
  scale_size(breaks = seq(0, 30, by = 5)) +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  theme(
    panel.grid.major.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.y = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.major.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.grid.minor.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
    panel.background   = element_rect(fill = "white", colour = "grey"),
    text               = element_text(colour = "black", size = 15),
    axis.text          = element_text(colour = "black"),
    legend.position    = c(0.7, 0.1),
    legend.direction = "horizontal"
    ) +
  ggsave(here("Figures", "00_visits.png"), height = 6, width = 6) +
  ggsave(here("Figures", "00_visits.pdf"), height = 6, width = 6)


