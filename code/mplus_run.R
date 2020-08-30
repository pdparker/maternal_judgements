#loadd(data)
#loadd(data_imp)

mplus_prepare <- function(d = data_imp, m = 5){
  cat(glue("mplus_data{1:m}.dat"),file = here("data", "mplus_imp.txt"), sep = "\n")
  for (i in 1:m){
    mplus_data <- d$imputations[[i]] %>%
      dplyr::select(contains("weight"),contains("math"),contains("read"),
             gender, ses, geo, indig, lang, y3_sid) %>%
      mutate(gender = ifelse(gender == 'boy', 1, 0),
             geo = ifelse(geo == 'urban', 1, 0),
             indig = ifelse(indig == 'indig', 1, 0),
             lang = ifelse(lang == 'eng', 1, 0)
             ) %>%
      mutate(across(everything(), haven::zap_label)) %>%
      mutate(across(everything(), haven::zap_labels)) %>%
      mutate(across(everything(), as.numeric))

    MplusAutomation::prepareMplusData(df = as.data.frame(mplus_data),
                                      filename = here("data",
                                                      glue("mplus_data{i}.dat")
                                                      )
    )
  }

}

mplus_run <- function(mplus){
  
  MplusAutomation::runModels(target = here::here("code", "mplus") )
  
  mplus <- MplusAutomation::readModels(target = here::here("code", "mplus") )
  
  return(mplus)
}

