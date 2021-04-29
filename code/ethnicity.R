library(tidyverse)
library(readit)
library(here)
library(drake)
data_age_4 = readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk4.sas7bdat")
loadd(data)
data = data %>% select(cid)
tmp = data_age_4 %>%
  right_join(data, by = c('hicid'= 'cid')) 


tmp %>% 
  group_by(zf09m1) %>%
  count(sort = TRUE)

tmp %>% 
  group_by(zf09m2) %>%
  count(sort = TRUE) %>%
  ungroup() %>%
  mutate(perc = n/sum(n)*100)

tmp %>% 
  group_by(cf11m1) %>%
  count(sort = TRUE) %>%
  ungroup() %>%
  mutate(perc = n/sum(n))





2570/2664
