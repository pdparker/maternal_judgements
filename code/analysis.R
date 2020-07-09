# Packages ####
library(tidyverse)
library(here)
library(glue)
library(tidylog)
library(broom)
library(mitools)
library(VGAM)
library(ordinal)
# Optional
#library(purrr) # for running models in a loop
#library(furrr) # for running models in a loop in parallel
# Uncomment package based on planned models
#library(brms) # Easy implementation of Bayes. Most general package for most situations
#library(lme4) # Multilevel models. Suggest to use brms instead
library(srvyr) # For complex survey designs
#library(lavaan) # For SEM models
#library(metafor) # For meta-analysis (metaSEM) is another option
#library(mice) # for imputed data
# Run cleaning script
source(here::here("code","data_manipulation.R"))
date <- Sys.Date()

#####             Warning: Don't forget that the NAPLAN scores have been devided by 100. Make sure to multiple coefs by 100 to put back on NAPLAN scale     #####

# Math achievement ####
m_math_w <- plm(math ~  lag(math) + lag(math.interest.n)+lag(math.judgement.n), data = child_data_imp_long$imputations[[10]],index = c("cid"), model = "within")
m_math_r <- plm(math ~  lag(math) + lag(math.interest.n)+lag(math.judgement.n), data = child_data_imp_long$imputations[[10]],index = c("cid"), model = "random")
tidy(m_math_r, conf.int = TRUE)
tidy(m_math_w, conf.int = TRUE)
phtest(m_math_w, m_math_r)
# Reading achievement ####
m_read_w <- plm(read ~  lag(read) + lag(read.interest.n)+lag(read.judgement.n), data = child_data_imp_long$imputations[[10]],index = c("cid"), model = "within")
m_read_r <- plm(read ~  lag(read) + lag(read.interest.n)+lag(read.judgement.n), data = child_data_imp_long$imputations[[10]],index = c("cid"), model = "random")
tidy(m_read_r, conf.int = TRUE)
tidy(m_read_w, conf.int = TRUE)
phtest(m_read_w, m_read_r)
# Math Interest ####
m_math_w <- plm(math.interest.n ~  lag(math) + lag(math.interest.n)+lag(math.judgement.n), data = child_data_imp_long$imputations[[10]],index = c("cid"), model = "within")
m_math_r <- plm(math.interest.n ~  lag(math) + lag(math.interest.n)+lag(math.judgement.n), data = child_data_imp_long$imputations[[10]],index = c("cid"), model = "random")
tidy(m_math_r, conf.int = TRUE)
tidy(m_math_w, conf.int = TRUE)
phtest(m_math_w, m_math_r)
# Reading interest ####
m_read_w <- plm(read.interest.n ~  lag(read) + lag(read.interest.n)+lag(read.judgement.n), data = child_data_imp_long$imputations[[10]],index = c("cid"), model = "within")
m_read_r <- plm(read.interest.n ~  lag(read) + lag(read.interest.n)+lag(read.judgement.n), data = child_data_imp_long$imputations[[10]],index = c("cid"), model = "random")
tidy(m_read_r, conf.int = TRUE)
tidy(m_read_w, conf.int = TRUE)
phtest(m_read_w, m_read_r)

# Ordinal models
m_math_w <- vglm(math.interest ~  math + math.judgement.n+factor(cid), data = child_data_imp_long$imputations[[10]], family = "cumulative")

summary(m_math_w)
# Save data model results
save(model,out, file = here::here("data", glue("{date}_model.RData")))



