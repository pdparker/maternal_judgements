# Packages ####
library(tidyverse)
library(here)
library(glue)
library(tidylog)
# Optional
#library(purrr) # for running models in a loop
#library(furrr) # for running models in a loop in parallel
# Uncomment package based on planned models
library(brms) # Easy implementation of Bayes. Most general package for most situations
#library(lme4) # Multilevel models. Suggest to use brms instead
#library(srvyr) # For complex survey designs
#library(lavaan) # For SEM models
#library(metafor) # For meta-analysis (metaSEM) is another option
#library(mice) # for imputed data
# Run cleaning script
source(here::here("code","data_manipulation.R"))
date <- Sys.Date()
# Run Models
# Often may want to run model 
model <-
out <- 
# Save data model results
save(model,out, file = here::here("data", glue("{date}_model.RData")))



