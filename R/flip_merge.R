#### Merge results from subsamples

#### set up #####################################################

# load package
library(dplyr)
library(purrr)

#### import data ################################################
anova2 <- read.csv(here("Data", "HPP2", "02_results-lmem-2.csv"))      
anova3 <- read.csv(here("Data", "HPP3", "02_results-lmem-3.csv"))      
anova4 <- read.csv(here("Data", "HPP4", "02_results-lmem-4.csv"))      
anova5 <- read.csv(here("Data", "HPP5", "02_results-lmem-5.csv")) 
anova6 <- read.csv(here("Data", "02_results-lmem.csv"))      

#### merge data #################################################
anova <- list(anova2, anova3, anova4, anova5, anova6) %>%
  set_names(paste0("HPP", c(2:6))) %>%
  bind_rows(.id = "Subset")

#### visualise data #############################################
anova %>%
  filter(Term != "(Intercept)") %>%
  mutate(Term = factor(Term, ordered = TRUE),
         is6 = Subset=="HPP6") %>%
  ggplot(aes(Subset, Coefficient, shape = is6)) +
  facet_wrap(~Term, ncol = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = ci1, ymax = ci2), alpha = 0.4, colour = "black", width = 0, size = 6) +
  geom_errorbar(aes(ymin = Coefficient-SEM, ymax = Coefficient+SEM), colour = "black", width = 0) +
  geom_point(size = 3) +
  labs(x = "Term", y = "Coefficient") +
  coord_flip() +
  theme(
    text = element_text(size = 20, colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title = element_blank(),
    panel.grid = element_line(linetype = "dotted", colour = "grey"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    legend.position = "none"
  ) +
  ggsave(here("Figures", "05_anova-merged.png"), height = 7)

#### export data ################################################
write.table(anova, here("Data", "03_anova-merged.csv"), sep = ",", dec = ".", row.names = FALSE)