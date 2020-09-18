#### Merge results from subsamples

#### set up #####################################################

# load package
library(dplyr)
library(purrr)
library(here)
library(ggplot2)

# load functions
source(here("R", "functions.R"))

#### import data ################################################
anova2 <- read.csv(here("Data", "HPP2", "02_results-lmem-2.csv"), stringsAsFactors = FALSE)      
anova3 <- read.csv(here("Data", "HPP3", "02_results-lmem-3.csv"), stringsAsFactors = FALSE)      
anova4 <- read.csv(here("Data", "HPP4", "02_results-lmem-4.csv"), stringsAsFactors = FALSE)      
anova5 <- read.csv(here("Data", "HPP5", "02_results-lmem-5.csv"), stringsAsFactors = FALSE) 
anova6 <- read.csv(here("Data", "02_results-lmem.csv"), stringsAsFactors = FALSE)      

#### merge data #################################################
anova <- list(anova2, anova3, anova4, anova5, anova6) %>%
  set_names(paste0("HPP ", c("1-2", "1-3", "1-4", "1-5", "1-6"))) %>%
  bind_rows(.id = "subset")

#### visualise data #############################################
anova %>%
  filter(term != "(Intercept)") %>%
  mutate(term = case_when(term == "item" ~ "Item",
                          term=="hpp" ~ "HPP",
                          term=="item:hpp" ~ "Item \U000D7 HPP",
                          TRUE ~ NA_character_),
         term = factor(term, ordered = TRUE),
         is6 = subset=="HPP6") %>%
  ggplot(aes(subset, estimate, shape = is6, colour = subset, fill = estimate)) +
  facet_wrap(~term, ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = ci1, ymax = ci2), alpha = 0.4, width = 0, size = 3) +
  geom_errorbar(aes(ymin = estimate-std_error, ymax = estimate+std_error), width = 0) +
  geom_point(size = 2) +
  labs(x = "Term", y = "Coefficient\n") +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  theme_custom +
  theme(legend.position = "none",
        text = element_text(size = 8),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  ggsave(here("Figures", "05_anova-merged.png"), height = 2.5) +
  ggsave(here("Figures", "05_anova-merged.pdf"), height = 2.5)


#### export data ################################################
write.table(anova, here("Data", "03_anova-merged.csv"), sep = ",", dec = ".", row.names = FALSE)
