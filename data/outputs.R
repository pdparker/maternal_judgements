# Packages ####
library(tidyverse)
library(here)
library(glue)
library(gt)
library(tidylog)
# load apa theme for gt tables
source(here::here("templates", "apa_style.R") )
# load M and B theme for plots
source(here::here("templates", "theme_MandB.R") )

# Tables ####
#Skeleton
table2 <- data %>%
  select() %>%
  gt() %>%
  apa_theme()
# Adjust expand
gtsave(data = table2, path = "figures", filename = "table2.png",expand = 10)

# Figures ####
plot1 <- data %>%
  ggplot() +
  theme_MandB()
#plots must be saved at 300dpi
ggsave(plot = plot1,filename = here::here("figures", "plots1.png"),dpi = 300)
