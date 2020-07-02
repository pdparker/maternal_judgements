# Packages ####
# Data Manipulation Base Packages
library(here) # for relative paths
library(tidyverse) # for general manipulation of data
library(REDCapR) # for reading in data from RedCap
library(janitor) # for some cleaning
library(dataMaid) # for documentation
library(glue) # for joining strings
library(forcats) # For manipulating categorical variables
library(tidylog) # for getting logs of data manipulation
# You might also want to use - uncomment if you want them
#library(lubridate) # If there are dates
#library(purrr) # for manipulating lists. Useful if there are more than one data file
#library(fuzzyjoin) # when manipulating messy string data
#library(conflicted) # if you want to be careful with common function names
# Other packages you might want to add

# Redcap set-up ####
redcap_token <- #Token needs to be requested and goes here
#Set Date for later use
date <- Sys.Date()

# Load data ####
data <- read_redcap() #Set batch size to 1000 or more so this is quick.
#Cleaning script ####
# Generally: Categorical variables should be factors using character values (e.g., 'boys')
# Generally: Count/ordinal variables should be as.ordered
# Generally: Dates should be as.Date
data %>%
  select() %>%
  mutate()

# Produce Documentation ####
# Commented out so that it does not run on source
# makeCodeBook(data,file = here("documentation", glue("{date}_codebook.Rmd")))
#makeDataReport(data,file = here("documentation", glue("{date}_report.Rmd")))

