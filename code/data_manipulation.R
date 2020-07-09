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
library(readit) # read stata, sas, etc files
library(purrr) # manipulate lists
library(dataMaid) # for producing data documentation
library(fs) # file and directory manipulation
library(Amelia) # for imputation
library(survey) # for dealing with weights and strata
options(survey.lonely.psu="remove")
date <- Sys.Date()

# Custom functions ####
z <- function(z){ (z - mean(z,na.rm=TRUE))/(sd(z,na.rm=TRUE))}

div <- function(x,num = 100) {x/num}
#remove log made on same day as this is called by source
path <- dir_info(here("log"),regexp = ".txt") %>%
  mutate(path = str_extract(path,"[0-9]{4}-[0-9]{2}-[0-9]{2}") ) 
  
if(path$path == date){
  file_delete(here("log",glue("{path$path}_log.txt")))
}  



# Cloudstor Read & Manipulation ####
# age 4 data ####
capture.output(data_age_4 <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk4.sas7bdat") %>%
  select(cid = hicid, ses = csep, geo = csos,
        indig1 =zf12m2, indig2 = zf12cm, gender = zf02m1,
        iq = cppvt, lang = cf11m2, parent_gender = zf02m2) %>%
  mutate(geo = ifelse(geo < 1, 'urban', 'rural'),
         lang = ifelse(lang == '1201', 'eng', 'other'),
         indig = case_when(
           indig1 < 0 ~ NA_character_,
           indig2 < 0 ~ NA_character_,
           indig1 > 1 ~ 'indig',
           indig2 > 1 ~ 'indig',
           TRUE ~ 'nonIndig'
         ) %>% factor,
         cohort = 'K',
         gender = ifelse(gender == 1, "boy", "girl"),
         parent_gender = ifelse(parent_gender == 1, "father", "mother")
  ) %>%
    select(-indig1,-indig2),
  file = here("log",glue("{date}_log.txt")), append = FALSE, type = "message")

# age 8 data ####
capture.output(data_age_8 <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk8.sas7bdat") %>%
  select(cid = hicid, y3_grade = epc06a1,y3_state = estate,y3_weight = eweight,
         y3_stratum = stratum, y3_math.interest = epc58b2, y3_math.judgement = elc08a2a, 
         y3_read.interest = epc58b8, y3_read.judgement = elc08a1a) %>%
  # Reverse score math interest and set to ordinal
  mutate(y3_math.interest = case_when(
    y3_math.interest == 1 ~ 3,
    y3_math.interest == 2 ~ 2,
    y3_math.interest == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # Reverse score reading interest and set to ordinal
  y3_read.interest = case_when(
    y3_read.interest == 1 ~ 3,
    y3_read.interest == 2 ~ 2,
    y3_read.interest == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # reverse score parents math judgements and set to ordinal
  y3_math.judgement = case_when(
    y3_math.judgement == 1 ~ 5,
    y3_math.judgement == 2 ~ 4,
    y3_math.judgement == 3 ~ 3,
    y3_math.judgement == 4 ~ 2,
    y3_math.judgement == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # Reverse score parents reading judgements and set to ordinal
  y3_read.judgement = case_when(
    y3_read.judgement == 1 ~ 5,
    y3_read.judgement == 2 ~ 4,
    y3_read.judgement == 3 ~ 3,
    y3_read.judgement == 4 ~ 2,
    y3_read.judgement == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'K'),
  file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")

# age 10 data #####
capture.output(data_age_10 <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk10.sas7bdat") %>%
  select(cid = hicid, y5_grade = fpc06a1, y5_math.interest = fpc58b2, 
         y5_math.judgement = flc08a2a,
         y5_read.interest = fpc58b8, y5_read.judgement = flc08a1a) %>% 
  # Reverse score math interest and set to ordinal
  mutate(y5_math.interest = case_when(
    y5_math.interest == 1 ~ 3,
    y5_math.interest == 2 ~ 2,
    y5_math.interest == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # Reverse score reading interest and set to ordinal
  y5_read.interest = case_when(
    y5_read.interest == 1 ~ 3,
    y5_read.interest == 2 ~ 2,
    y5_read.interest == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # reverse score parents math judgements and set to ordinal
  y5_math.judgement = case_when(
    y5_math.judgement == 1 ~ 5,
    y5_math.judgement == 2 ~ 4,
    y5_math.judgement == 3 ~ 3,
    y5_math.judgement == 4 ~ 2,
    y5_math.judgement == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # reverse score parents reading judgements and set to ordinal
  y5_read.judgement = case_when(
    y5_read.judgement == 1 ~ 5,
    y5_read.judgement == 2 ~ 4,
    y5_read.judgement == 3 ~ 3,
    y5_read.judgement == 4 ~ 2,
    y5_read.judgement == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'K'
  ),
  file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")
# age 12 data ####
capture.output(data_age_12 <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk12.sas7bdat") %>%
  select(cid = hicid, y7_math.interest = gpc58b2, y7_math.judgement = glc08a2a,
         y7_read.interest = gpc58b8, y7_read.judgement = glc08a1a) %>% 
  # Reverse score math interest and set to ordinal
  mutate(y7_math.interest = case_when(
    y7_math.interest == 1 ~ 3,
    y7_math.interest == 2 ~ 2,
    y7_math.interest == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # Reverse score reading interest and set to ordinal
  y7_read.interest = case_when(
    y7_read.interest == 1 ~ 3,
    y7_read.interest == 2 ~ 2,
    y7_read.interest == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # reverse score parents math judgements and set to ordinal
  y7_math.judgement = case_when(
    y7_math.judgement == 1 ~ 5,
    y7_math.judgement == 2 ~ 4,
    y7_math.judgement == 3 ~ 3,
    y7_math.judgement == 4 ~ 2,
    y7_math.judgement == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  # reverse score parents reading judgements and set to ordinal
  y7_read.judgement = case_when(
    y7_read.judgement == 1 ~ 5,
    y7_read.judgement == 2 ~ 4,
    y7_read.judgement == 3 ~ 3,
    y7_read.judgement == 4 ~ 2,
    y7_read.judgement == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'K'
  ),
  file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")

# Administrative data: child ####
capture.output(child_admin <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/NAPLAN/lsacnaplan.sas7bdat") %>%
  # only working on the K cohort
  filter(cohort == 'K') %>%
  select(cid = hicid, y3_math = y3num, y5_math = y5num, y7_math = y7num,
         y3_read = y3read, y5_read = y5read, y7_read = y7read,
         y3_status = y3status, y5_status = y5status, y7_status = y7status) %>%
  mutate(y3_math = replace(y3_math, y3_math < 0, NA),
         y5_math = replace(y5_math, y5_math < 0, NA),
         y7_math = replace(y7_math, y7_math < 0, NA),
         y3_read = replace(y3_read, y3_read < 0, NA),
         y5_read = replace(y5_read, y5_read < 0, NA),
         y7_read = replace(y7_read, y7_read < 0, NA)
  ), 
  file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")
# Administrative data: school ####
capture.output(school_admin_2008 <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2008 & HICID %in% data_age_4$cid) %>%
  select(cid = HICID, y3_sid = School_ID,
         y3_math.sch = y3.n_SN_Mean.nAPLANScore,
         y3_read.sch = y3_R_SN_Mean.nAPLANScore), 
  file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")

capture.output(school_admin_2010 <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
                 filter(calendar_year == 2010 & HICID %in% data_age_4$cid) %>%
                 select(cid = HICID, y5_sid = School_ID,
                        y5_math.sch = y5.n_SN_Mean.nAPLANScore,
                        y5_read.sch = y5_R_SN_Mean.nAPLANScore), 
               file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")

capture.output(school_admin_2012 <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
                 filter(calendar_year == 2012 & HICID %in% data_age_4$cid) %>%
                 select(cid = HICID, y7_sid = School_ID,
                        y7_math.sch = y7.n_SN_Mean.nAPLANScore,
                        y7_read.sch = y7_R_SN_Mean.nAPLANScore), 
               file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")
# Merge child survey data with administrative data####
capture.output(
  child_data <- reduce(list(data_age_4,data_age_8,data_age_10,data_age_12,
                            child_admin,school_admin_2008, school_admin_2010, school_admin_2012),
                       left_join, by = "cid") %>%
    #focus on children with known qualities at age 8 (i.e., remove home schooled children) and drop fathers
    filter(y3_grade == 19 & !is.na(y3_stratum) & !is.na(y3_sid) & y3_status != 4 & parent_gender == 'mother') %>%
    select(-starts_with("cohort")),
  file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")

# Produce Documentation ####
# Commented out so that it does not run on source
# dataMaid::makeCodebook(child_data,file = here("documentation", glue("{date}_codebook.Rmd")), replace=TRUE)


# Imputations & subsequent manipulation ####
# as.data.frame added because Amelian does not like tidyverse
set.seed(42)
child_data_imp <- amelia(as.data.frame(child_data),m = 10, idvars = c('cid', 'y3_sid', 'y5_sid', 'y7_sid',
                                                                          'y3_weight', 'parent_gender',
                                                                          'y3_state', 'y3_stratum',
                                                                      "y3_grade", "y5_grade",
                                                                      "y3_status","y5_status","y7_status"),
                               noms = c('geo', 'indig', 'gender', 'lang'),
                               ords = c('y3_math.judgement', 'y3_math.interest', 'y5_math.judgement', 'y5_math.interest', 'y7_math.interest', 'y7_math.judgement',
                                        'y3_read.judgement', 'y3_read.interest', 'y5_read.judgement', 'y5_read.interest', 'y7_read.interest', 'y7_read.judgement')
)

# Commented out as I dont wont to run this on source
# png(filename = here("figures", "missmap.png"))
# missmap(child_data_imp)
# dev.off()

# Manipulate to create within variables ####
tmp <- child_data_imp$imputations
class(tmp) <- "list"
tmp <- bind_rows(tmp, .id = "imputation") %>%
  mutate(across(ends_with('sch'), .fns = div)) %>%
  mutate(across(ends_with('math'), .fns = div)) %>%
  mutate(across(ends_with('read'), .fns = div)) %>%
  mutate(
    #Within centered math and reading
    y3_math_w = y3_math - (y3_math+y5_math+y7_math)/3,
    y5_math_w = y5_math - (y3_math+y5_math+y7_math)/3,
    y7_math_w = y7_math - (y3_math+y5_math+y7_math)/3,
    y3_read_w = y3_read - (y3_read+y5_read+y7_read)/3,
    y5_read_w = y5_read - (y3_read+y5_read+y7_read)/3,
    y7_read_w = y7_read - (y3_read+y5_read+y7_read)/3) %>%
  mutate(across(ends_with("interest"), list(n = as.numeric), .names = "{col}.{fn}")) %>%
  mutate(across(ends_with("judgement"), list(n = as.numeric), .names = "{col}.{fn}")) 

child_data_imp <-  mitools::imputationList(tmp %>% group_split(imputation))

# Save as survey object ####
child_data_svy <- svydesign(ids = ~y3_sid,strata = ~y3_stratum, weights = ~y3_weight, nest = TRUE, 
                      data = child_data_imp)


# stack all waves
capture.output(child_data_long <- tmp %>%
  dplyr::select(-ends_with("sid"),-ends_with("_w"), -ends_with("status")) %>%
  group_by(imputation) %>%
  pivot_longer(
    cols = y3_grade:y7_read.judgement.n,
    names_to = c("year", ".value"),
    names_sep = "_"
  ) %>%
  ungroup %>%
  group_by(cid) %>%
  arrange(year) %>%
  mutate(across(math.interest:read, list(l = dplyr::lag(.)), .names =  "{col}.{fn}")) %>%
  ungroup(),
  file = here("log",glue("{date}_log.txt")), append = TRUE, type = "message")



child_data_imp_long <-  mitools::imputationList(child_data_long %>% group_split(imputation))


 

