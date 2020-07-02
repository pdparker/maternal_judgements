# Packages ####
library(tidyverse)
library(here)
library(glue)
library(gt) # For table construction
library(tidylog)
# Run cleaning script
source(here::here("code","data_manipulation.R"))
# load apa theme for gt tables
source(here::here("templates", "apa_style.R") )


# Produce Table 1 ####
#Skeleton
table1 <- data %>%
  select() %>%
  gt() %>%
  apa_theme()

# Save table as img for uploading to paper
gtsave(data = table1, path = "figures", filename = "table1.png")
