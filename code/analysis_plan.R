cor_fun <- function(d_s){
  #correlation - Math
  cor_math_3 <- with(d_s, svyglm(scale(I(y3_math-y3_math.sch) ) ~ scale(y3_math.judgement.n))) 
  cor_math_3 <- suppressMessages(summary(MIcombine(cor_math_3))) %>% as_tibble(rownames = "par")
  cor_math_5 <- with(d_s, svyglm(scale(I(y5_math-y5_math.sch) ) ~ scale(y5_math.judgement.n))) 
  cor_math_5 <- suppressMessages(summary(MIcombine(cor_math_5)) ) %>% as_tibble(rownames = "par")
  cor_math_7 <- with(d_s, svyglm(scale(I(y7_math-y7_math.sch) ) ~ scale(y7_math.judgement.n))) 
  cor_math_7 <- suppressMessages(summary(MIcombine(cor_math_7)) ) %>% as_tibble(rownames = "par")
  
  #Correlations - Reading
  cor_read_3 <- with(d_s, svyglm(scale(I(y3_read-y3_read.sch) ) ~ scale(y3_read.judgement.n)))
  cor_read_3 <- suppressMessages(summary(MIcombine(cor_read_3)) ) %>% as_tibble(rownames = "par")
  cor_read_5 <- with(d_s, svyglm(scale(I(y5_read-y5_read.sch) ) ~ scale(y5_read.judgement.n)))
  cor_read_5 <- suppressMessages(summary(MIcombine(cor_read_5)) ) %>% as_tibble(rownames = "par")
  cor_read_7 <- with(d_s, svyglm(scale(I(y7_read-y7_read.sch) ) ~ scale(y7_read.judgement.n)))
  cor_read_7 <- suppressMessages(summary(MIcombine(cor_read_7)) ) %>% as_tibble(rownames = "par")
  
  cor <- list(math_3=cor_math_3,
              math_5=cor_math_5,
              math_7=cor_math_7,
              read_3=cor_read_3,
              read_5=cor_read_5,
              read_7=cor_read_7) 
  
  cor <- reduce(cor,bind_rows,.id = "model") %>%
    filter(str_detect(par,"scale")) %>%
    mutate(model = names(cor))
  
  return(cor)
}

descript_fun <- function(child_d_svy){
  # Means and Standard Deviations 
  ##Year 3
  ###Mean
  mean_math_judgement_3 <- with(child_d_svy, svyby(~I(y3_math-y3_math.sch),~y3_math.judgement, svymean))
  mean_math_judgement_3 <- summary(MIcombine(mean_math_judgement_3))
  mean_read_judgement_3 <- with(child_d_svy, svyby(~I(y3_read-y3_read.sch),~y3_read.judgement, svymean))
  mean_read_judgement_3 <- summary(MIcombine(mean_read_judgement_3))
  ### SD
  sd_math_judgement_3 <- with(child_d_svy, svyby(~I(y3_math-y3_math.sch),~y3_math.judgement, svyvar) )
  sd_math_judgement_3 <- MIcombine(sd_math_judgement_3)$coef %>% .^.5 %>% as_tibble()
  sd_read_judgement_3 <- with(child_d_svy, svyby(~I(y3_read-y3_read.sch),~y3_read.judgement, svyvar) )
  sd_read_judgement_3 <- MIcombine(sd_read_judgement_3)$coef %>% .^.5 %>% as_tibble()
  ## Year 5
  ### Mean
  mean_math_judgement_5 <- with(child_d_svy, svyby(~I(y5_math-y5_math.sch),~y5_math.judgement, svymean))
  mean_math_judgement_5 <- summary(MIcombine(mean_math_judgement_5))
  mean_read_judgement_5 <- with(child_d_svy, svyby(~I(y5_read-y5_read.sch),~y5_read.judgement, svymean))
  mean_read_judgement_5 <- summary(MIcombine(mean_read_judgement_5))
  ### SD
  sd_math_judgement_5 <- with(child_d_svy, svyby(~I(y5_math-y5_math.sch),~y5_math.judgement, svyvar) )
  sd_math_judgement_5 <- MIcombine(sd_math_judgement_5)$coef %>% .^.5 %>% as_tibble()
  sd_read_judgement_5 <- with(child_d_svy, svyby(~I(y5_read-y5_read.sch),~y5_read.judgement, svyvar) )
  sd_read_judgement_5 <- MIcombine(sd_read_judgement_5)$coef %>% .^.5 %>% as_tibble()
  ## Year 7
  ### Mean
  mean_math_judgement_7 <- with(child_d_svy, svyby(~I(y7_math-y7_math.sch),~y7_math.judgement, svymean))
  mean_math_judgement_7 <- summary(MIcombine(mean_math_judgement_7))
  mean_read_judgement_7 <- with(child_d_svy, svyby(~I(y7_read-y7_read.sch),~y7_read.judgement, svymean))
  mean_read_judgement_7 <- summary(MIcombine(mean_read_judgement_7))
  ### SD
  sd_math_judgement_7 <- with(child_d_svy, svyby(~I(y7_math-y7_math.sch),~y7_math.judgement, svyvar) )
  sd_math_judgement_7 <- MIcombine(sd_math_judgement_7)$coef %>% .^.5 %>% as_tibble()
  sd_read_judgement_7 <- with(child_d_svy, svyby(~I(y7_read-y7_read.sch),~y7_read.judgement, svyvar) )
  sd_read_judgement_7 <- MIcombine(sd_read_judgement_7)$coef %>% .^.5 %>% as_tibble()
  
  ## Math ach y3
  bias_math_3 <- with(child_d_svy, svytable(~I(y3_math>=y3_math.sch) + y3_math.judgement)) %>%
    table_map(margin=1) %>%
    bind_cols(., 
              with(child_d_svy, svytable(~y3_math.judgement)) %>%
                table_map(rows = 1)) %>%
    set_names(c("level","below_avg","above_avg","throw_away","sample_pro")) %>%
    select(level, sample_pro, below_avg, above_avg) %>%
    mutate(across(sample_pro:above_avg, ~`*`(.,100)))
  ## Read ach y3
  bias_read_3 <- with(child_d_svy, svytable(~I(y3_read>=y3_read.sch) + y3_read.judgement)) %>%
    table_map(margin=1) %>%
    bind_cols(., 
              with(child_d_svy, svytable(~y3_read.judgement)) %>%
                table_map(rows = 1)) %>%
    set_names(c("level","below_avg","above_avg","throw_away","sample_pro")) %>%
    select(level, sample_pro, below_avg, above_avg) %>%
    mutate(across(sample_pro:above_avg, ~`*`(.,100)))
  
  ## Math ach y5
  bias_math_5 <- with(child_d_svy, svytable(~I(y5_math>=y5_math.sch) + y5_math.judgement)) %>%
    table_map(margin=1) %>%
    bind_cols(., 
              with(child_d_svy, svytable(~y5_math.judgement)) %>%
                table_map(rows = 1)) %>%
    set_names(c("level","below_avg","above_avg","throw_away","sample_pro")) %>%
    select(level, sample_pro, below_avg, above_avg) %>%
    mutate(across(sample_pro:above_avg, ~`*`(.,100)))
  ## Read ach y5
  bias_read_5 <- with(child_d_svy, svytable(~I(y5_read>=y5_read.sch) + y5_read.judgement)) %>%
    table_map(margin=1) %>%
    bind_cols(., 
              with(child_d_svy, svytable(~y5_read.judgement)) %>%
                table_map(rows = 1)) %>%
    set_names(c("level","below_avg","above_avg","throw_away","sample_pro")) %>%
    select(level, sample_pro, below_avg, above_avg) %>%
    mutate(across(sample_pro:above_avg, ~`*`(.,100)))
  
  ## Math ach y7
  bias_math_7 <- with(child_d_svy, svytable(~I(y7_math>=y7_math.sch) + y7_math.judgement)) %>%
    table_map(margin=1) %>%
    bind_cols(., 
              with(child_d_svy, svytable(~y7_math.judgement)) %>%
                table_map(rows = 1)) %>%
    set_names(c("level","below_avg","above_avg","throw_away","sample_pro")) %>%
    select(level, sample_pro, below_avg, above_avg) %>%
    mutate(across(sample_pro:above_avg, ~`*`(.,100)))
  ## Read ach y7
  bias_read_7 <- with(child_d_svy, svytable(~I(y7_read>=y7_read.sch) + y7_read.judgement)) %>%
    table_map(margin=1) %>%
    bind_cols(., 
              with(child_d_svy, svytable(~y7_read.judgement)) %>%
                table_map(rows = 1)) %>%
    set_names(c("level","below_avg","above_avg","throw_away","sample_pro")) %>%
    select(level, sample_pro, below_avg, above_avg) %>%
    mutate(across(sample_pro:above_avg, ~`*`(.,100)))
  
  

  descript <- list(math_judgement_3 = bind_cols(mean_math_judgement_3,sd_math_judgement_3,bias_math_3),
                   read_judgement_3 = bind_cols(mean_read_judgement_3,sd_read_judgement_3,bias_read_3),
                   math_judgement_5 = bind_cols(mean_math_judgement_5,sd_math_judgement_5,bias_math_5),
                   read_judgement_5 = bind_cols(mean_read_judgement_5,sd_read_judgement_5,bias_read_5),
                   math_judgement_7 = bind_cols(mean_math_judgement_7,sd_math_judgement_7,bias_math_7),
                   read_judgement_7 = bind_cols(mean_read_judgement_7,sd_read_judgement_7,bias_read_7)
  )
  
  descript <- reduce(descript, bind_rows) %>%
    rename(sd = value, mean = results) %>%
    mutate(var = rep(names(descript), each = 5)) %>%
    tidyr::separate(var, into = c("subject", "variable", "year")) %>%
    select(subject,year, level, mean, lower = `(lower`, upper = `upper)`, sd,
           sample_pro, above_avg, below_avg) %>%
    mutate(across(sample_pro:below_avg, ~scales::percent(.,scale = 1, accuracy = 0.1)) ) %>%
    as_tibble()
  
  return(descript)
  
}

quant_reg <- function(d=data_imp_mod, quants = c(.1,.5,.9), imputations = 5,
                     domain = "read", year = 3){
  d <- d$child_data_imp[[1]]
  out <- matrix(NA,nrow=3, ncol=4)
  out[,1] <- glue("Quantile {quants}")
  f1 <- glue("scale(I(y{year}_{domain}-y{year}_{domain}.sch))~I(scale(y{year}_{domain}.judgement.n))")
  
  for (j in seq_along(quants)){
    res <- list()
    for ( i in 1:imputations){
      tmp <- d[[i]]
      tmp_svy <- svydesign(ids = ~y3_sid,strata = ~y3_stratum, weights = ~y3_weight, nest = TRUE, 
                           d = tmp)
      tmp_svy<-as.svrepdesign(tmp_svy,type="bootstrap", replicates=100)
      res[[i]] <- withReplicates(tmp_svy, quote(coef(rq(as.formula(f1),
                                                        tau=quants[j], weights=.weights))))
    }
    res <- cbind.data.frame(res)
    ests <- res[,c(T,F)]
    ses <- res[,c(F,T)]
    point <- rowMeans(res)
    se <- rowMeans(ses) + (1+1/imputations)*(1/(imputations-1))*apply(ests, 1, var)
    out[j,2:4] <- c(point[2],point[2] - 2*se[2],point[2] + 2*se[2])
  }
  return(out)
}


bias_model <- function(d, domain = "read", imps = 5){
  f2 <- glue("{domain}.judgement~I(z({domain}-{domain}.sch))+ scale(iq) + scale(ses)+ geo + gender +
                       indig + lang + (1|cid) + (1|sid)")
  model <- brm_multiple(as.formula(f2),
                     data=as.list(d$child_data_imp_long[[1]]),
                     family=cumulative, cores = imps, chains = 2)
  return(model)
}


bias_svy <- function(data = data_s, year = 3, domain = "read") {
  f1 <- glue("y{year}_{domain}.judgement~I(z(y{year}_{domain}-y{year}_{domain}.sch)) + scale(iq) + scale(ses)+ geo + gender +
                       indig + lang")
  model <- with(data, svyolr(f1) )
  capture.output(out <- summary(MIcombine(model)),file = 'NULL')
  return(as_tibble(out,rownames = "variable"))
}

bias_svy_int <- function(data = data_s, year = 3, domain = "read") {
  f1 <- glue("y{year}_{domain}.judgement~I(z(y{year}_{domain}-y{year}_{domain}.sch)) + scale(iq) + scale(ses)*gender + 
              geo*gender + indig*gender + lang*gender")
  model <- with(data, svyolr(f1) )
  capture.output(out <- summary(MIcombine(model)),file = 'NULL')
  return(as_tibble(out,rownames = "variable"))
}

sch_model <- function(data = data_s, domain = "read"){
  f1 <- glue("y7_{domain}.sch ~
             y5_{domain}.judgement.n + y5_{domain}.interest.n +
             y5_{domain} + y5_{domain}.sch +
             iq + scale(ses)+ geo + gender + indig + lang")
  
  model <- with(data, svyglm(as.formula(f1), subset=!y3_state %in% 4:5) )
  
  capture.output(out <- summary(MIcombine(model)),file = 'NULL')
  return(as_tibble(out,rownames = "variable"))
}


