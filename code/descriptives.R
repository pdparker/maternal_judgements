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
# Get a single imputation for simple descriptives
tmp <- child_data_imp$imputations[[1]]
table1 <- tmp %>%
  select(gender,indig, lang, geo) %>%
  gt() %>%
  apa_theme()

# Save table as img for uploading to paper
gtsave(data = table1, path = "figures", filename = "table1.png")

