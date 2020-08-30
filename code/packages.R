# Packages ####
library(drake) # Make files for R
library(here) # For relative paths
library(tidyverse) # For general manipulation of data
library(dataMaid) # For documentation
library(glue) # For joining strings
library(forcats) # For manipulating categorical variables
library(tidylog) # For getting logs of data manipulation
library(readit) # For reading stata, sas, etc files
library(purrr) # For manipulate lists
library(dataMaid) # For producing data documentation
library(fs) # For file and directory manipulation
library(Amelia) # For imputation
library(brms) # For main models
library(quantreg) # For quantile regression
library(survey) # For descriptives and baic models
library(mitools) # for pasting together imputations
library(patchwork) # For combining plots
library(fuzzyjoin) # For regular expression joins
library(janitor) # For cleaning names
library(stringr) # For string manipulation
library(ggtext) # For adding CSS and markdown to plots
library(gggibbous) # For moon plots