# Flip
# Gonzalo García-Castro* and Chiara Santolin, *gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

# set up --------------------------------------------------------------------

# load packages
library(magrittr) # for using pipes
library(readxl)   # for importing Excel files
library(dplyr)    # for manipulating data
library(tidyr)    # for reshaping datasets
library(ggplot2)  # for visualising data

# import data ---------------------------------------------------------------
data <-
  read_xlsx("data/data_flip.xlsx") %>%
  mutate(group = case_when(hpp > 0 ~ "more",
                           TRUE    ~ "first"),
         total = familiar+unfamiliar,
         preference = familiar/(familiar+unfamiliar)*100) %>%
  gather(key = "trial_type", "looking_time", -c(baby, location, hpp, total, preference, group))

# visualise data ------------------------------------------------------------
ggplot(data, aes(x = group, y = preference, colour = hpp)) +
  geom_point(alpha = 0.5, size = 3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "red", size = 1.25) +
  geom_hline(yintercept = 50, linetype = "dotted") +
  labs(x = "Group", y = "Familiarity preference corrected by total looking time\nFamiliar/ (Familiar + Unfamiliar)*100)", colour = "Number of HPP",
       title = "Familiarity preference by number of previous HPP tests",
       subtitle = "Red dots and whiskers represent the mean\nand bootstraped 95% confidence interval, respectively") +
  facet_wrap(.~location) +
  theme(text = element_text(size = 15),
        legend.position = "top",
        panel.grid.major.x = element_blank()) +
  ggsave("figures/preference.png")
  

