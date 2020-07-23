# Packages ####
library(tidyverse)
library(here)
library(glue)
library(tidylog)
library(broom)
library(mitools)
library(brms)
library(plm) 
library(sjstats)
# Run cleaning script
source(here::here("code","data_manipulation.R"))
# Set up data ####
# Need a single imputation to run Bayes models
tmp <- child_data_imp$imputations[[1]]
# Get a year 5 dataset
tmp_3 <- tmp %>%
  filter(!is.na(y3_sid))
# Get a year 5 dataset
tmp_5 <- tmp %>%
  filter(!is.na(y5_sid))
# Get a year 7 dataset
tmp_7 <- tmp %>%
  filter(!is.na(y7_sid))
# Cluster set-up ####

# Mediation ####
#Year 3
m1 <- bf(y3_math.judgement ~ gender,
         family = cumulative(link = "logit", threshold = "flexible"))
m2 <- bf(y3_math ~ as.numeric(y3_math.judgement) + gender)
m3 <- brm(m1 + m2 + set_rescor(FALSE), chains = 2, data = tmp_3, cores = 2)
summary(m3)
#Longitudinal
m4 <- bf(math.judgement ~ gender + math.judgement.l + math.l,
         family = cumulative(link = "logit", threshold = "flexible"))
m5 <- bf(math ~ math.judgement + gender + math.judgement.l + math.l)
m6 <- brm(m4 + m5 + set_rescor(FALSE), chains = 2, data = child_data_long, cores = 2)
summary(m6)

#Year 5
m7 <- bf(y5_math.judgement ~ gender, family = cumulative(link = "logit", threshold = "flexible"))
m8 <- bf(y5_math ~ as.numeric(y5_math.judgement) + gender)
m9 <- brm(m7 + m8 + set_rescor(FALSE), chains = 2, data = tmp_5, cores = 2)
summary(m9)


#####             Warning: Don't forget that the NAPLAN scores have been devided by 100. Make sure to multiple coefs by 100 to put back on NAPLAN scale     #####

# Maternal Judgements - Year 3 ####
M1 <- brm(y3_math.judgement ~ I(y3_math-y3_math.sch) + iq + ses+
            geo + gender + indig + lang, data = tmp, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y3_math.judgement ~ cs(I(y3_math-y3_math.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = tmp, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

# Maternal Judgements - Year 5 ####
M1 <- brm(y5_math.judgement ~ I(y5_math-y5_math.sch) + iq + ses+
            geo + gender + indig + lang, data = tmp, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y5_math.judgement ~ cs(I(y5_math-y5_math.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = tmp, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

# Maternal Judgements - Year 3 ####
M1 <- brm(y3_read.judgement ~ I(y3_read-y3_read.sch) + iq + ses+
            geo + gender + indig + lang, data = tmp, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y3_read.judgement ~ cs(I(y3_read-y3_read.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = tmp, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

# Maternal Judgements - Year 5 ####
M1 <- brm(y5_read.judgement ~ I(y5_read-y5_read.sch) + iq + ses+
            geo + gender + indig + lang, data = tmp, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y5_read.judgement ~ cs(I(y5_read-y5_read.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = tmp, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

# Assumption Longitudinal Models
# matheracy Year 5
M1 <- brm(y5_math ~  mo(y3_math.judgement) + mo(y3_math.interest) + y3_math +
            y3_math.sch + iq + ses + geo + gender + indig + lang,
          data = tmp_5, chains = 2, cores = 2)
summary(M1)
M2 <- brm(y5_math ~  y3_math.judgement.n + y3_math.interest.n +
            y3_math + y3_math.sch + iq + ses + geo + gender +
            indig + lang,
          data = tmp_5, chains = 2, cores = 2)

summary(M2)
loo(M1,M2)
M3 <- brm(y5_math ~  y3_math.judgement.n + y3_math.interest.n +
            y3_math + y3_math.sch + iq + ses+ geo + gender +
            indig + lang + (1|y5SId),
          data = tmp_5, chains = 4, iter = 4000)
summary(M3)
loo(M2,M3)
# matheracy Year 7
M1 <- brm(y7math ~  mo(y5_math.judgement) + mo(y5_math.interest) + y5_math +
            y5_math.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_7, chains = 2, cores = 2)
summary(M1)
M2 <- brm(y7math ~  y5_math.judgement.n + y5_math.interest.n +
            y5_math + y5_math.sch + iq + ses+ geo + gender +
            indig + lang,
          data = tmp_7, chains = 2, cores = 2)
summary(M2)
loo(M1,M2)
M3 <- brm(y7math ~  y5_math.judgement.n + y5_math.interest.n +
            y5_math + y5_math.sch + iq + ses+ geo + gender +
            indig + lang + (1|y7SId),
          data = tmp_7, chains = 4, iter = 4000)
summary(M3)
loo(M2,M3)

# Reading Year 5
M1 <- brm(y5_read ~  mo(y3_read.judgement) + mo(y3_read.interest) + y3_read +
            y3_read.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_5, chains = 2, cores = 2)
summary(M1)
M2 <- brm(y5_read ~  y3_read.judgement.n + y3_read.interest.n +
            y3_read + y3_read.sch + iq + ses+ geo + gender +
            indig + lang,
          data = tmp_5, chains = 2, cores = 2)
summary(M2)
loo(M1,M2)
M3 <- brm(y5_read ~  y3_read.judgement.n + y3_read.interest.n +
            I(y3_read/100) + I(y3_read.sch/100) + iq + ses+ geo + gender +
            indig + lang + (1|y5SId),
          data = tmp_5, chains = 4, iter = 4000)
summary(M3)
loo(M2,M3)
# Reading Year 7
M1 <- brm(y7_read ~  mo(y5_read.judgement) + mo(y5_read.interest) + y5_read +
            y5_read.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_7, chains = 2, cores = 2)
summary(M1)
M2 <- brm(y7_read ~  y5_read.judgement.n + y5_read.interest.n +
            y5_read + y5_read.sch + iq + ses+ geo + gender +
            indig + lang,
          data = tmp_7, chains = 2, cores = 2)
summary(M2)
loo(M1,M2)
M3 <- brm(y7_read ~  y5_read.judgement.n + y5_read.interest.n +
            y5_read + y5_read.sch + iq + ses+ geo + gender +
            indig + lang + (1|y7SId),
          data = tmp_7, chains = 4, iter = 4000)
summary(M3)
loo(M2,M3)


# Reading Year 7
M1 <- brm(y7_read ~  mo(y5_read.judgement) + mo(y5_read.interest) + y5_read +
            y5_read.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_7, chains = 2, cores = 2)
summary(M1)
M2 <- brm(y7_read ~  y5_read.judgement.n + y5_read.interest.n +
            y5_read + y5_read.sch + iq + ses+ geo + gender +
            indig + lang,
          data = tmp_7, chains = 2, cores = 2)
summary(M2)
loo(M1,M2)
M3 <- brm(y7_read ~  y5_read.judgement.n + y5_read.interest.n +
            y5_read + y5_read.sch + iq + ses+ geo + gender +
            indig + lang + (1|y7SId),
          data = tmp_7, chains = 4, iter = 4000)
summary(M3)
loo(M2,M3)

# Math Year 7
M1 <- brm(y7_math.interest ~  mo(y5_math.judgement) + mo(y5_math.interest) + y5_math +
            y5_math.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_7, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y7_math.interest ~  y5_math.judgement.n + y5_math.interest.n + y5_math +
            y5_math.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_7, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)
M3 <- brm(y7_math.interest ~  y5_math.judgement.n + y5_math.interest.n + y5_math +
            y5_math.sch + iq + ses+ geo + gender + indig + lang + (1|y7SId),
          data = tmp_7, chains = 4, iter = 4000, family = acat, cores = 2)
summary(M3)
loo(M2,M3)

# Math Year 5
M1 <- brm(y5_math.interest ~  mo(y3_math.judgement) + mo(y3_math.interest) + y3_math +
            y3_math.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_5, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y5_math.interest ~  y3_math.judgement.n + y3_math.interest.n + y3_math +
              y3_math.sch + iq + ses + geo + gender + indig + lang,
            data = tmp_5, chains = 2, family = acat, cores = 2)

M2_a <- brm(y5_math.interest ~  cs(y3_math.judgement.n) + cs(y3_math.interest.n) + cs(y3_math) +
            cs(y3_math.sch) + cs(iq) + cs(ses)+ cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = tmp_5, chains = 2, family = acat, cores = 2)
summary(M2)
summary(M2_a)
loo(M1,M2)
loo(M2_a,M2)
M3 <- brm(y5_math.interest ~  y3_math.judgement.n + y3_math.interest.n + y3_math +
            y3_math.sch + iq + ses+ geo + gender + indig + lang + (1|y5SId),
          data = tmp_5, chains = 4, iter = 4000, family = acat, cores = 2)
summary(M3)
loo(M2,M3)


# Math Year 7
M1 <- brm(y5_math.interest ~  mo(y3_math.judgement) + mo(y3_math.interest) + y3_math +
            y3_math.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_5, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y7_math.interest ~  y5_math.judgement.n + y5_math.interest.n + y5_math +
            y5_math.sch + iq + ses + geo + gender + indig + lang,
          data = tmp_5, chains = 2, family = acat, cores = 2)

M2_a <- brm(y7_math.interest ~  cs(y5_math.judgement.n) + cs(y5_math.interest.n) + cs(y5_math) +
              cs(y5_math.sch) + cs(iq) + cs(ses)+ cs(geo) + cs(gender) + cs(indig) + cs(lang),
            data = tmp_5, chains = 2, family = acat, cores = 2)
summary(M2)
summary(M2_a)
loo(M1,M2)
loo(M2_a,M2)
M3 <- brm(y5_math.interest ~  y3_math.judgement.n + y3_math.interest.n + y3_math +
            y3_math.sch + iq + ses+ geo + gender + indig + lang + (1|y5SId),
          data = tmp_5, chains = 4, iter = 4000, family = acat, cores = 2)
summary(M3)
loo(M2,M3)

# Read Year 5
M1 <- brm(y5_read.interest ~  mo(y3_read.judgement) + mo(y3_read.interest) +
            y3_read + y3_read.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_5, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y5_read.interest ~  y3_read.judgement.n + y3_read.interest.n + y3_read +
            y3_read.sch + iq + ses+ geo + gender + indig + lang,
          data = tmp_5, chains = 2, family = acat, cores = 2)
M2_a <- brm(y5_read.interest ~  cs(y3_read.judgement.n) + cs(y3_read.interest.n) + cs(y3_read) +
            cs(y3_read.sch) + cs(iq) + cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = tmp_5, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)
loo(M2_a,M2)
M3 <- brm(y5_read.interest ~  y3_read.judgement.n + y3_read.interest.n + y3_read +
            y3_read.sch + iq + ses+ geo + gender + indig + lang + (1|y5SId),
          data = tmp_5, chains = 4, iter = 4000, family = acat, cores = 2)
summary(M3)
loo(M2,M3)

# Math Year 7
M1 <- brm(y7read.interest ~  mo(y5_read.judgement) + mo(y5_read.interest) + I(y5_read/100) +
            I(y5_read.sch/100) + iq + ses+ geo + gender + indig + lang,
          data = tmp_7, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y7_read.interest ~  y5_read.judgement + y5_read.interest + y5_read +
            y5_read.sch + iq + ses + geo + gender + indig + lang,
          data = tmp_7, chains = 2, family = acat, core = 2)
M2_a <- brm(y7_read.interest ~  cs(y5_read.judgement) + cs(y5_read.interest) + cs(y5_read) +
            cs(y5_read.sch) + cs(iq) + cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = tmp_7, chains = 2, family = acat, core = 2)
summary(M2)
loo(M1,M2)
loo(M2_a,M2)
M3 <- brm(y7read.interest ~  y5_read.judgement + y5_read.interest + I(y5_read/100) +
            I(y5_read.sch/100) + iq + ses+ geo + gender + indig + lang + (1|y7SId),
          data = tmp_7, chains = 4, iter = 4000, family = acat, cores = 2)
summary(M3)
loo(M2,M3)

# Within Person Models ####
# Math achievement ####
m_math_w <- plm(math ~  lag(math) + lag(math.interest.n)+lag(math.judgement.n), data = child_data_imp_long$imputations[[1]],index = c("cid"), model = "within")
m_math_r <- plm(math ~  lag(math) + lag(math.interest.n)+lag(math.judgement.n), data = child_data_imp_long$imputations[[1]],index = c("cid"), model = "random")
tidy(m_math_r, conf.int = TRUE)
tidy(m_math_w, conf.int = TRUE)
phtest(m_math_w, m_math_r)
# Reading achievement ####
m_read_w <- plm(read ~  lag(read) + lag(read.interest.n)+lag(read.judgement.n), data = child_data_imp_long$imputations[[1]],index = c("cid"), model = "within")
m_read_r <- plm(read ~  lag(read) + lag(read.interest.n)+lag(read.judgement.n), data = child_data_imp_long$imputations[[1]],index = c("cid"), model = "random")
tidy(m_read_r, conf.int = TRUE)
tidy(m_read_w, conf.int = TRUE)
phtest(m_read_w, m_read_r)
# Math Interest ####
m_math_w <- plm(math.interest.n ~  lag(math) + lag(math.interest.n)+lag(math.judgement.n), data = child_data_imp_long$imputations[[1]],index = c("cid"), model = "within")
m_math_r <- plm(math.interest.n ~  lag(math) + lag(math.interest.n)+lag(math.judgement.n), data = child_data_imp_long$imputations[[1]],index = c("cid"), model = "random")
tidy(m_math_r, conf.int = TRUE)
tidy(m_math_w, conf.int = TRUE)
phtest(m_math_w, m_math_r)
# Reading interest ####
m_read_w <- plm(read.interest.n ~  lag(read) + lag(read.interest.n)+lag(read.judgement.n), data = child_data_imp_long$imputations[[1]],index = c("cid"), model = "within")
m_read_r <- plm(read.interest.n ~  lag(read) + lag(read.interest.n)+lag(read.judgement.n), data = child_data_imp_long$imputations[[1]],index = c("cid"), model = "random")
tidy(m_read_r, conf.int = TRUE)
tidy(m_read_w, conf.int = TRUE)
phtest(m_read_w, m_read_r)



# Longitudinal Models ####
library(ordinal)
tmp <- child_data_imp_long$imputations[[1]]

m_math_w <- brm(math.interest~math.l+math.interest.l+math.judgement.l+
                         (1|cid) + (1|sid), data = tmp, cores = 4, chains = 4,
                family = cumulative(link = "logit", threshold = "flexible"),
                control = list(adapt_delta = 0.95))
summary(m_math_w)
conditional_effects(m_math_w, effects = "math.judgement.l", categorical = TRUE)


m_read_w <- brm(read.interest~read.l+read.interest.l+read.judgement.l+
                  (1|cid) + (1|sid), data = tmp, cores = 4, chains = 4,
                family = cumulative(link = "logit", threshold = "flexible"),
                control = list(adapt_delta = 0.95))
summary(m_read_w)
conditional_effects(m_read_w, effects= "read.judgement.l", categorical = TRUE)

m_math_w <- brm(math.interest~math.l+math.interest.l+math.judgement.l+
                  (1|cid) + (1|sid), data = tmp, cores = 4, chains = 4,
                family = cumulative(link = "logit", threshold = "flexible"),
                control = list(adapt_delta = 0.95))
summary(m_math_w)
conditional_effects(m_math_w, effects = "math.judgement.l", categorical = TRUE)


m_read_a <- brm(read~read.l+read.interest.l+read.judgement.l+
                  (1|cid) + (1|sid), data = tmp, cores = 4, chains = 4,
                control = list(adapt_delta = 0.95))
summary(m_read_a)
conditional_effects(m_read_a, effects= "read.judgement.l")

m_math_a <- brm_multiple(math~math.l+math.interest.l+math.judgement.l+
                  (1|cid) + (1|sid), data = as.list(child_data_imp_long$imputations),
                  cores = 4, chains = 4,
                control = list(adapt_delta = 0.95))
summary(m_math_a)
a <- conditional_effects(m_math_a, effects = "math.judgement.l")

?brm_multiple
