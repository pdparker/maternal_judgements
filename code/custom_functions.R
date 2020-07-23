# Packages ####
# Data Manipulation Base Packages
library(here) # for relative paths
library(tidyverse) # for general manipulation of data
library(dataMaid) # for documentation
library(glue) # for joining strings
library(forcats) # For manipulating categorical variables
library(tidylog) # for getting logs of data manipulation
library(readit) # read stata, sas, etc files
library(purrr) # manipulate lists
library(dataMaid) # for producing data documentation
library(fs) # file and directory manipulation
library(Amelia) # for imputation
library(brms) # For main models
library(quantreg) #for quantile regression
library(survey) # For descriptives and baic models
library(mitools) # for pasting together imputations

date <- Sys.Date()


# Custom functions ####
z <- function(z){ (z - mean(z,na.rm=TRUE))/(sd(z,na.rm=TRUE))}

div <- function(x,num = 100) {x/num}


table_map <- function(table, rows = 2, cols = 5, margin=NULL){
  map(table, prop.table, margin=margin) %>%
    rbind.data.frame() %>% 
    rowMeans()  %>% 
    matrix(., nrow=rows, ncol=cols) %>% 
    provideDimnames() %>%
    as_data_frame() %>% 
    rownames_to_column() %>%
    tidylog::gather(variable, value, -rowname) %>% 
    tidylog::spread(rowname, value) %>%
    mutate(variable = c("much worse","worse","average", "better", "much better"))
}
    
#remove log made on same day as this is called by source
path <- dir_info(here("log"),regexp = ".txt") %>%
  mutate(path = str_extract(path,"[0-9]{4}-[0-9]{2}-[0-9]{2}") ) 

if(path$path == date){
  file_delete(here("log",glue("{path$path}_log.txt")))
}  

log_to_file <- function(text) cat(text, file = here("log",glue("{date}_log.txt")), sep = "\n", append = TRUE)
options("tidylog.display" = list(message, log_to_file))

