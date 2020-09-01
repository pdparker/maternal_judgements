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

loadd(data_imp_mod)

supplement_data <- function(data = data_imp_mod){
  # Set up data ####
  # Need a single imputation to run Bayes models
  single_imp <- data$child_data_imp[[1]][[1]]
  # Single longitudinal imputation
  single_imp_long <- data$child_data_imp_long[[1]][[1]]
  # Get a year 5 dataset
  imp_y3 <- single_imp %>%
    filter(!is.na(y3_sid))
  # Get a year 5 dataset
  imp_y5 <- single_imp %>%
    filter(!is.na(y5_sid))
  # Get a year 7 dataset
  imp_y7 <- single_imp %>%
    filter(!is.na(y7_sid))
  
  return(list(single_imp = single_imp,single_imp_long = single_imp_long,
              imp_y3 = imp_y3, imp_y5 = imp_y5, imp_y7 = imp_y7)
         )
}


# Data set-up ####
sup <- supplement_data()
#####             Warning: Don't forget that the NAPLAN scores have been devided by 100. Make sure to multiple coefs by 100 to put back on NAPLAN scale     #####

# Maternal Judgements - Year 3: Math ####
M1 <- brm(y3_math.judgement ~ I(y3_math-y3_math.sch) + iq + ses+
            geo + gender + indig + lang, data = sup$imp_y3, chains = 2, family = acat, cores = 2)
summary(M1)

M2 <- brm(y3_math.judgement ~ cs(I(y3_math-y3_math.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = sup$imp_y3, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

M3 <- brm(y3_math.judgement ~ I(y3_math-y3_math.sch) + iq + ses+ (1|y3_sid) +
            geo + gender + indig + lang, data = sup$imp_y3,
            chains = 2, family = acat, cores = 2)
summary(M3)
loo(M1,M3)

# Maternal Judgements - Year 5: Math ####
M1 <- brm(y5_math.judgement ~ I(y5_math-y5_math.sch) + iq + ses+
            geo + gender + indig + lang, data = sup$imp_y5, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y5_math.judgement ~ cs(I(y5_math-y5_math.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = sup$imp_y5, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

M3 <- brm(y5_math.judgement ~ I(y5_math-y5_math.sch) + iq + ses+ (1|y5_sid) +
            geo + gender + indig + lang, data = sup$imp_y5, chains = 2, family = acat, cores = 2)
summary(M3)
loo(M1,M3)
# Maternal Judgements - Year 7: Math ####
M1 <- brm(y7_math.judgement ~ I(y7_math-y7_math.sch) + iq + ses+
            geo + gender + indig + lang, data = sup$imp_y7, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y7_math.judgement ~ cs(I(y7_math-y7_math.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = sup$imp_y7, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

M3 <- brm(y7_math.judgement ~ I(y7_math-y7_math.sch) + iq + ses+ (1|y7_sid) +
            geo + gender + indig + lang, data = sup$imp_y7, chains = 2, family = acat, cores = 2)
summary(M3)
loo(M1,M3)

# Maternal Judgements - Year 3 ####
M1 <- brm(y3_read.judgement ~ I(y3_read-y3_read.sch) + iq + ses+
            geo + gender + indig + lang, 
          data = sup$imp_y3, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y3_read.judgement ~ cs(I(y3_read-y3_read.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = sup$imp_y3, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

M3 <- brm(y3_read.judgement ~ I(y3_read-y3_read.sch) + iq + ses+  (1|y3_sid) +
            geo + gender + indig + lang, 
          data = sup$imp_y3, chains = 2, family = acat, cores = 2)
summary(M3)
loo(M1,M3)

# Maternal Judgements - Year 5 ####
M1 <- brm(y5_read.judgement ~ I(y5_read-y5_read.sch) + iq + ses+
            geo + gender + indig + lang,
          data = sup$imp_y5, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y5_read.judgement ~ cs(I(y5_read-y5_read.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = sup$imp_y5, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

M3 <- brm(y5_read.judgement ~ I(y5_read-y5_read.sch) + iq + ses+ (1|y5_sid) +
            geo + gender + indig + lang,
          data = sup$imp_y5, chains = 2, family = acat, cores = 2)
summary(M3)
loo(M1,M3)

# Maternal Judgements - Year 7 ####
M1 <- brm(y7_read.judgement ~ I(y7_read-y7_read.sch) + iq + ses+
            geo + gender + indig + lang,
          data = sup$imp_y7, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y7_read.judgement ~ cs(I(y7_read-y7_read.sch)) + cs(iq) +
            cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = sup$imp_y7, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)

M3 <- brm(y7_read.judgement ~ I(y7_read-y7_read.sch) + iq + ses+ (1|y7_sid) +
            geo + gender + indig + lang,
          data = sup$imp_y7, chains = 2, family = acat, cores = 2)
summary(M3)
loo(M1,M3)

# Assumption Longitudinal Models
# matheracy Year 5
M1 <- brm(y5_math ~  mo(y3_math.judgement) + mo(y3_math.interest) + y3_math +
            y3_math.sch + iq + ses + geo + gender + indig + lang,
          data = sup$single_imp, chains = 2, cores = 2)
summary(M1)
M2 <- brm(y5_math ~  y3_math.judgement.n + y3_math.interest.n +
            y3_math + y3_math.sch + iq + ses + geo + gender +
            indig + lang,
          data = sup$single_imp, chains = 2, cores = 2)

summary(M2)
loo(M1,M2)
M3 <- brm(y5_math ~  y3_math.judgement.n + y3_math.interest.n +
            y3_math + y3_math.sch + iq + ses+ geo + gender +
            indig + lang + (1|y5SId),
          data = sup$single_imp, chains = 4, iter = 4000)
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
          data = sup$single_imp, chains = 2, cores = 2)
summary(M1)
M2 <- brm(y5_read ~  y3_read.judgement.n + y3_read.interest.n +
            y3_read + y3_read.sch + iq + ses+ geo + gender +
            indig + lang,
          data = sup$single_imp, chains = 2, cores = 2)
summary(M2)
loo(M1,M2)
M3 <- brm(y5_read ~  y3_read.judgement.n + y3_read.interest.n +
            I(y3_read/100) + I(y3_read.sch/100) + iq + ses+ geo + gender +
            indig + lang + (1|y5SId),
          data = sup$single_imp, chains = 4, iter = 4000)
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
          data = sup$single_imp, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y5_math.interest ~  y3_math.judgement.n + y3_math.interest.n + y3_math +
              y3_math.sch + iq + ses + geo + gender + indig + lang,
            data = sup$single_imp, chains = 2, family = acat, cores = 2)

M2_a <- brm(y5_math.interest ~  cs(y3_math.judgement.n) + cs(y3_math.interest.n) + cs(y3_math) +
            cs(y3_math.sch) + cs(iq) + cs(ses)+ cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = sup$single_imp, chains = 2, family = acat, cores = 2)
summary(M2)
summary(M2_a)
loo(M1,M2)
loo(M2_a,M2)
M3 <- brm(y5_math.interest ~  y3_math.judgement.n + y3_math.interest.n + y3_math +
            y3_math.sch + iq + ses+ geo + gender + indig + lang + (1|y5SId),
          data = sup$single_imp, chains = 4, iter = 4000, family = acat, cores = 2)
summary(M3)
loo(M2,M3)

# Math Year 7
M1 <- brm(y5_math.interest ~  mo(y3_math.judgement) + mo(y3_math.interest) + y3_math +
            y3_math.sch + iq + ses+ geo + gender + indig + lang,
          data = sup$single_imp, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y7_math.interest ~  y5_math.judgement.n + y5_math.interest.n + y5_math +
            y5_math.sch + iq + ses + geo + gender + indig + lang,
          data = sup$single_imp, chains = 2, family = acat, cores = 2)

M2_a <- brm(y7_math.interest ~  cs(y5_math.judgement.n) + cs(y5_math.interest.n) + cs(y5_math) +
              cs(y5_math.sch) + cs(iq) + cs(ses)+ cs(geo) + cs(gender) + cs(indig) + cs(lang),
            data = sup$single_imp, chains = 2, family = acat, cores = 2)
summary(M2)
summary(M2_a)
loo(M1,M2)
loo(M2_a,M2)
M3 <- brm(y5_math.interest ~  y3_math.judgement.n + y3_math.interest.n + y3_math +
            y3_math.sch + iq + ses+ geo + gender + indig + lang + (1|y5SId),
          data = sup$single_imp, chains = 4, iter = 4000, family = acat, cores = 2)
summary(M3)
loo(M2,M3)

# Read Year 5
M1 <- brm(y5_read.interest ~  mo(y3_read.judgement) + mo(y3_read.interest) +
            y3_read + y3_read.sch + iq + ses+ geo + gender + indig + lang,
          data = sup$single_imp, chains = 2, family = acat, cores = 2)
summary(M1)
M2 <- brm(y5_read.interest ~  y3_read.judgement.n + y3_read.interest.n + y3_read +
            y3_read.sch + iq + ses+ geo + gender + indig + lang,
          data = sup$single_imp, chains = 2, family = acat, cores = 2)
M2_a <- brm(y5_read.interest ~  cs(y3_read.judgement.n) + cs(y3_read.interest.n) + cs(y3_read) +
            cs(y3_read.sch) + cs(iq) + cs(ses) + cs(geo) + cs(gender) + cs(indig) + cs(lang),
          data = sup$single_imp, chains = 2, family = acat, cores = 2)
summary(M2)
loo(M1,M2)
loo(M2_a,M2)
M3 <- brm(y5_read.interest ~  y3_read.judgement.n + y3_read.interest.n + y3_read +
            y3_read.sch + iq + ses+ geo + gender + indig + lang + (1|y5SId),
          data = sup$single_imp, chains = 4, iter = 4000, family = acat, cores = 2)
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
# Longitudinal Models ####

m_math_w <- brm(math.interest~math.l+math.interest.l+math.judgement.l+
                         (1|cid) + (1|sid), data = sup$single_imp_long, cores = 4, chains = 4,
                family = cumulative(link = "logit", threshold = "flexible"),
                control = list(adapt_delta = 0.95))
summary(m_math_w)
conditional_effects(m_math_w, effects = "math.judgement.l", categorical = TRUE)


m_read_w <- brm(read.interest~read.l+read.interest.l+read.judgement.l+
                  (1|cid) + (1|sid), data = sup$single_imp_long, cores = 4, chains = 4,
                family = cumulative(link = "logit", threshold = "flexible"),
                control = list(adapt_delta = 0.95))
summary(m_read_w)
conditional_effects(m_read_w, effects= "read.judgement.l", categorical = TRUE)

m_math_w <- brm(math.interest~math.l+math.interest.l+math.judgement.l+
                  (1|cid) + (1|sid), data = sup$single_imp_long, cores = 4, chains = 4,
                family = cumulative(link = "logit", threshold = "flexible"),
                control = list(adapt_delta = 0.95))
summary(m_math_w)
conditional_effects(m_math_w, effects = "math.judgement.l", categorical = TRUE)

