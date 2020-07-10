# Packages ####
library(tidyverse)
library(here)
library(glue)
library(tidylog)
library(broom)
library(modelsummary)
library(mitools)
library(brms)
library(quantreg) 
library(srvyr)
# Run cleaning script
source(here::here("code","data_manipulation.R"))

#  Do Mothers’ Judge their Child’s Ability Accurately? ####
#Correlations - Math
cor_math_3 <- with(child_data_svy, svyglm(scale(I(y3_math-y3_math.sch) ) ~ scale(y3_math.judgement.n))) 
summary(MIcombine(cor_math_3))
cor_math_5 <- with(child_data_svy, svyglm(scale(I(y5_math-y5_math.sch) ) ~ scale(y5_math.judgement.n))) 
summary(MIcombine(cor_math_5)) 

#Correlations - Reading
cor_read_3 <- with(child_data_svy, svyglm(scale(I(y3_read-y3_read.sch) ) ~ scale(y3_read.judgement.n)))
summary(MIcombine(cor_read_3))
cor_read_5 <- with(child_data_svy, svyglm(scale(I(y5_read-y5_read.sch) ) ~ scale(y5_read.judgement)))
summary(MIcombine(cor_read_5))

# Means and Standard Deviations 
##Year 3
###Mean
mean_math_judgement_3 <- with(child_data_svy, svyby(~I(y3_math-y3_math.sch),~y3_math.judgement, svymean))
summary(MIcombine(mean_math_judgement_3))
mean_math_judgement_3 <- with(child_data_svy, svyby(~I(y3_read-y3_read.sch),~y3_read.judgement, svymean))
summary(MIcombine(mean_read_judgement_3))
### SD
sd_math_judgement_3 <- with(child_data_svy, svyby(~I(y3_math-y3_math.sch),~y3_math.judgement, svyvar) )
MIcombine(sd_math_judgement_3)$coef %>% .^.5
sd_read_judgement_3 <- with(child_data_svy, svyby(~I(y3_read-y3_read.sch),~y3_read.judgement, svyvar) )
MIcombine(sd_read_judgement_5)$coef %>% .^.5
## Year 5
### Mean
mean_math_judgement_5 <- with(child_data_svy, svyby(~I(y5_math-y5_math.sch),~y5_math.judgement, svymean))
summary(MIcombine(mean_math_judgement_5))
mean_math_judgement_5 <- with(child_data_svy, svyby(~I(y5_read-y5_read.sch),~y5_read.judgement, svymean))
summary(MIcombine(mean_math_judgement_5))
### SD
sd_math_judgement_5 <- with(child_data_svy, svyby(~I(y5_math-y5_math.sch),~y5_math.judgement, svyvar) )
MIcombine(sd_math_judgement_5)$coef %>% .^.5

sd_read_judgement_5 <- with(child_data_svy, svyby(~I(y5_read-y5_read.sch),~y5_read.judgement, svyvar) )
MIcombine(sd_read_judgement_5)$coef %>% .^.5

# Over estimation bias ####
## Math ach y3
bias_math_3 <- with(child_data_svy, svytable(~I(y3_math>=y3_math.sch) + y3_math.judgement))

map(bias_math_3, prop.table, margin = 1) %>%
  rbind.data.frame() %>%
  rowMeans()*100  %>%
  matrix(., nrow=2, ncol=5)
## Math judgement y3
with(child_data_svy, svytable(~y3_math.judgement, Ntotal=100)) %>%
  rbind.data.frame() %>%
  rowMeans()
## Math ach y5
bias_math_5 <- with(child_data_svy, svytable(~I(y5_math>=y5_math.sch) + y5_math.judgement))

map(bias_math_5, prop.table, margin = 1) %>%
  rbind.data.frame() %>%
  rowMeans()*100  %>%
  matrix(., nrow=2, ncol=5) 
## Math judgement y5
with(child_data_svy, svytable(~y5_math.judgement, Ntotal=100)) %>%
  rbind.data.frame() %>%
  rowMeans() 

# What Systematic Biases are there in Maternal Judgements? ####
quants <- c(.1,.5,.9)
# Quantile Regression
## Reading Y3 
qreg_read_3 <- matrix(NA,nrow=3, ncol=4)
qreg_read_3[,1] <- glue("Quantile {c(0.1,0.5,0.9)}")

for (j in seq_along(quants)){
  res <- list()
  for ( i in 1:10){
    tmp <- child_data_imp$imputations[[i]]
    tmp_svy <- svydesign(ids = ~y3_sid,strata = ~y3_stratum, weights = ~y3_weight, nest = TRUE, 
                       data = tmp)
    tmp_svy<-as.svrepdesign(tmp_svy,type="bootstrap", replicates=100)
    res[[i]] <- withReplicates(tmp_svy, quote(coef(rq(scale(I(y3_read-y3_read.sch))~I(scale(as.numeric(y3_read.judgement))), tau=quants[j], weights=.weights))))
    }
  res <- cbind.data.frame(res)
  ests <- res[,c(T,F)]
  ses <- res[,c(F,T)]
  point <- rowMeans(res)
  se <- rowMeans(ses) + (1+1/10)*(1/(10-1))*apply(ests, 1, var)
  qreg_read_3[j,2:4] <- c(point[2],point[2] - 2*se[2],point[2] + 2*se[2])
}
## Reading Y5 
qreg_read_5 <- matrix(NA,nrow=3, ncol=4)
qreg_read_5[,1] <- glue("Quantile {c(0.1,0.5,0.9)}")

for (j in seq_along(quants)){
  res <- list()
  for ( i in 1:10){
    tmp <- child_data_imp$imputations[[i]]
    tmp_svy <- svydesign(ids = ~y3_sid,strata = ~y3_stratum, weights = ~y3_weight, nest = TRUE, 
                         data = tmp)
    tmp_svy<-as.svrepdesign(tmp_svy,type="bootstrap", replicates=100)
    res[[i]] <- withReplicates(tmp_svy, quote(coef(rq(scale(I(y5_read-y5_read.sch))~I(scale(as.numeric(y5_read.judgement))), tau=quants[j], weights=.weights))))
  }
  res <- cbind.data.frame(res)
  ests <- res[,c(T,F)]
  ses <- res[,c(F,T)]
  point <- rowMeans(res)
  se <- rowMeans(ses) + (1+1/10)*(1/(10-1))*apply(ests, 1, var)
  qreg_read_5[j,2:4] <- c(point[2],point[2] - 2*se[2],point[2] + 2*se[2])
}


## math Y3 
qreg_math_3 <- matrix(NA,nrow=3, ncol=4)
qreg_math_3[,1] <- glue("Quantile {c(0.1,0.5,0.9)}")

for (j in seq_along(quants)){
  res <- list()
  for ( i in 1:10){
    tmp <- child_data_imp$imputations[[i]]
    tmp_svy <- svydesign(ids = ~y3_sid,strata = ~y3_stratum, weights = ~y3_weight, nest = TRUE, 
                         data = tmp)
    tmp_svy<-as.svrepdesign(tmp_svy,type="bootstrap", replicates=100)
    res[[i]] <- withReplicates(tmp_svy, quote(coef(rq(scale(I(y3_math-y3_math.sch))~I(scale(as.numeric(y3_math.judgement))), tau=quants[j], weights=.weights))))
  }
  res <- cbind.data.frame(res)
  ests <- res[,c(T,F)]
  ses <- res[,c(F,T)]
  point <- rowMeans(res)
  se <- rowMeans(ses) + (1+1/10)*(1/(10-1))*apply(ests, 1, var)
  qreg_math_3[j,2:4] <- c(point[2],point[2] - 2*se[2],point[2] + 2*se[2])
}
## math Y5
qreg_math_5 <- matrix(NA,nrow=3, ncol=4)
qreg_math_5[,1] <- glue("Quantile {c(0.1,0.5,0.9)}")

for (j in seq_along(quants)){
  res <- list()
  for ( i in 1:10){
    tmp <- child_data_imp$imputations[[i]]
    tmp_svy <- svydesign(ids = ~y3_sid,strata = ~y3_stratum, weights = ~y3_weight, nest = TRUE, 
                         data = tmp)
    tmp_svy<-as.svrepdesign(tmp_svy,type="bootstrap", replicates=100)
    res[[i]] <- withReplicates(tmp_svy, quote(coef(rq(scale(I(y5_math-y5_math.sch))~I(scale(as.numeric(y5_math.judgement))), tau=quants[j], weights=.weights))))
  }
  res <- cbind.data.frame(res)
  ests <- res[,c(T,F)]
  ses <- res[,c(F,T)]
  point <- rowMeans(res)
  se <- rowMeans(ses) + (1+1/10)*(1/(10-1))*apply(ests, 1, var)
  qreg_math_5[j,2:4] <- c(point[2],point[2] - 2*se[2],point[2] + 2*se[2])
}

# Predictors of Judgements
## Parenting Predictors ----
M1 <- with(child_data_svy, svyolr(y3_read.judgement ~ I(y3_read-y3_read.sch) + scale(iq) + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M1))

summary(MIcombine(M1)) %>%
  as_tibble %>%
  set_names(c("result", "se", "conf.low", "conf.high", "miss")) %>%
  select(-miss) %>%
  mutate(across(everything(), list(e = exp), .names = "{col}.{fn}"))

M1 <- with(child_data_svy, svyolr(y3_math.judgement ~ I((y3_math-y3_math.sch)/100) + scale(iq) + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M1))

M1 <- with(child_data_svy, svyolr(y3_read.judgement ~ I((y3_read-y3_read.sch)/100) + scale(iq) + scale(ses)+ geo + gender + indig + lang +praise+scold+warm+angryParent) )
summary(MIcombine(M1))
M1 <- with(child_data_svy, svyolr(y3_math.judgement ~ I((y3_math-y3_math.sch)/100) + scale(iq) + scale(ses)+ geo + gender + indig + lang+praise+scold+warm+angryParent) )
summary(MIcombine(M1))

M1 <- with(child_data_svy, svyolr(y3_math.judgement ~ I((y3_math-y3_math.sch)/100) + scale(iq) + scale(ses)*gender+ geo*gender + indig*gender + lang*gender) )
summary(MIcombine(M1))
M1 <- with(child_data_svy, svyolr(y3_read.judgement ~ I((y3_read-y3_read.sch)/100) + scale(iq) + scale(ses)*gender+ geo*gender + indig*gender + lang*gender) )
summary(MIcombine(M1))

M1 <- with(child_data_svy, svyolr(y3_math.judgement ~ I((y3_math-y3_math.sch)/100) + scale(iq) + scale(ses)+ geo*gender + indig + lang) )
summary(MIcombine(M1))
M1 <- with(child_data_svy, svyolr(y3_read.judgement ~ I((y3_read-y3_read.sch)/100) + scale(iq) + scale(ses)+ geo*gender + indig + lang) )
summary(MIcombine(M1))

t <- MIcombine(M1)

car::deltaMethod(coef(t), vcov(t), g = 'gendergirl +`geourban:gendergirl`')

M1 <- with(child_data_svy, svyolr(y5_math.judgement ~  I((y5_math-y5_math.sch)/100) + scale(iq) + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M1))


M1 <- with(child_data_svy, svyolr(y5_read.judgement ~  I((y5_read-y5_read.sch)/100) + scale(iq) + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M1))

tmp <-  coef(MIcombine(M1))[10] - coef(MIcombine(M1))[1:7] %*% c(0,0,0,1,1,1,0)
1 - ( 1 / (1+exp(-tmp) ))
#.49
#.61
tmp <-  coef(MIcombine(M1))[10] - coef(MIcombine(M1))[1:7] %*% c(0,0,0,1,0,1,0)
1 - ( 1 / (1+exp(-tmp) ))
#.54
#.57
M1 <- with(child_data_svy, svyolr(y5_math.judgement ~  I((y5_math-y5_math.sch)/100) + scale(iq) + scale(ses)+ geo + gender + indig + lang+praise+scold+warm+angryParent) )
summary(MIcombine(M1))
M1 <- with(child_data_svy, svyolr(y5_read.judgement ~  I((y5_read-y5_read.sch)/100) + scale(iq) + scale(ses)+ geo + gender + indig + lang+praise+scold+warm+angryParent) )
summary(MIcombine(M1))

M1 <- with(child_data_svy, svyolr(y5_math.judgement ~ I((y5_math-y5_math.sch)/100) + scale(iq) + scale(ses)*gender+ geo*gender + indig*gender + lang*gender) )
summary(MIcombine(M1))
M1 <- with(child_data_svy, svyolr(y5_read.judgement ~ I((y5_read-y5_read.sch)/100) + scale(iq) + scale(ses)*gender+ geo*gender + indig*gender + lang*gender) )
summary(MIcombine(M1))

# Predictors of Future achievement ----

# Year 5 by Year 3
M5.1 <- with(child_data_svy, svyglm(I(y5_math/100) ~  I(scale(as.numeric(y3MathSc))) + I(y3_math/100) + I(y3_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.1))
M5.3 <- with(child_data_svy, svyglm(I(y5_math/100) ~  I(as.numeric(y3_math.judgement))+ I(scale(as.numeric(y3MathSc))) + I(y3_math/100) + I(y3_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))
M5.3 <- with(child_data_svy, svyglm(I(y5_read/100) ~  I(as.numeric(y3_read.judgement))+ I(scale(as.numeric(y3_readSc))) + I(y3_read/100) + I(y3_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))
M5.3 <- with(child_data_svy, svyglm(I(y5_read/100) ~  I(poly(as.numeric(y3_read.judgement),3))+ I(scale(as.numeric(y3_readSc))) + I(y3_read/100) + I(y3_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))
#Year 7 by Year 5
M5.1 <- with(child_data_svy, svyglm(I(y7Num/100) ~  I(scale(as.numeric(y5MathSc))) + I(y5_math/100) + I(y5_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M5.1))
M5.3 <- with(child_data_svy, svyglm(I(y7Num/100) ~  I(as.numeric(y5_math.judgement)) + I(scale(as.numeric(y5MathSc))) + I(y5_math/100) + I(y5_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M5.3))

M5.3 <- with(child_data_svy, svyglm(I(y7Read/100) ~  I(as.numeric(y5_read.judgement)) + I(scale(as.numeric(y5_readSc))) + I(y5_read/100) + I(y5_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M5.3))
M5.3 <- with(child_data_svy, svyglm(I(y7Read/100) ~  I(poly(as.numeric(y5_read.judgement),3)) + I(scale(as.numeric(y5_readSc))) + I(y5_read/100) + I(y5_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M5.3))


#Year 7 by Year 5

M5.3 <- with(child_data_svy, svyglm(I(y7Num/100) ~  I(as.numeric(y5_math.judgement)) + I(y3_math/100) + I(y3_math.sch/100) + I(scale(as.numeric(y5MathSc))) + I(y5_math/100) + I(y5_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M5.3))

M5.3 <- with(child_data_svy, svyglm(I(y7Read/100) ~  I(as.numeric(y5_read.judgement))  + I(y3_read/100) + I(y3_read.sch/100)+ I(scale(as.numeric(y5_readSc))) + I(y5_read/100) + I(y5_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang) )
summary(MIcombine(M5.3))


# Do Mothers Judgements Stimulate Positive Gains in Academic Outcomes? ####
# Prediction of liking of math ---
M5.1 <- with(child_data_svy, svyolr(y5MathSc ~  I(scale(as.numeric(y3MathSc))) + I(y3_math/100) + I(y3_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.1)) 
M5.3 <- with(child_data_svy, svyolr(y5MathSc ~  I(scale(as.numeric(y3_math.judgement))) + I(scale(as.numeric(y3MathSc))) + I(y3_math/100) + I(y3_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))
M5.3 <- with(child_data_svy, svyolr(y5_readSc ~  I(scale(as.numeric(y3_read.judgement))) + I(scale(as.numeric(y3_readSc))) + I(y3_read/100) + I(y3_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))
M5.3 <- with(child_data_svy, svyolr(y5_readSc ~  I(poly(scale(as.numeric(y3_read.judgement)),3)) + I(scale(as.numeric(y3_readSc))) + I(y3_read/100) + I(y3_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))

M5.3 <- with(child_data_svy, svyolr(y7MathSc ~  I(scale(as.numeric(y5_math.judgement))) + I(scale(as.numeric(y5MathSc))) + I(y5_math/100) + I(y5_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))
M5.3 <- with(child_data_svy, svyolr(y7ReadSc ~  I(scale(as.numeric(y5_read.judgement))) + I(scale(as.numeric(y5_readSc))) + I(y5_read/100) + I(y5_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))
M5.3 <- with(child_data_svy, svyolr(y7ReadSc ~  I(poly(scale(as.numeric(y5_read.judgement)),3)) + I(scale(as.numeric(y5_readSc))) + I(y5_read/100) + I(y5_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))

M5.3 <- with(child_data_svy, svyolr(y7MathSc ~  I(scale(as.numeric(y5_math.judgement))) + I(y3_math/100) + I(y3_math.sch/100) + I(scale(as.numeric(y5MathSc))) + I(y5_math/100) + I(y5_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))
M5.3 <- with(child_data_svy, svyolr(y7ReadSc ~  I(scale(as.numeric(y5_read.judgement))) + I(scale(as.numeric(y5_readSc))) + I(y3_read/100) + I(y3_read.sch/100) + I(y5_read/100) + I(y5_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang ) )
summary(MIcombine(M5.3))


# Prediction of schooling ----
#Year 7 by Year 5
M5.3 <- with(child_data_svy, svyglm(I(scale(y7NumSch)) ~  I(scale(as.numeric(y5_math.judgement))) + I(scale(as.numeric(y5MathSc))) + I(y5_math/100) + I(y5_math.sch/100) + iq + scale(ses)+ geo + gender + indig + lang,
                              subset=!y3State %in% 4:5) )
summary(MIcombine(M5.3))

M5.3 <- with(child_data_svy, svyglm(I(scale(y7ReadSch)) ~  I(scale(as.numeric(y5_read.judgement))) + I(scale(as.numeric(y5_readSc))) + I(y5_read/100) + I(y5_read.sch/100) + iq + scale(ses)+ geo + gender + indig + lang,
                              subset=!y3State %in% 4:5) )
summary(MIcombine(M5.3))

