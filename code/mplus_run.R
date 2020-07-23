mplus_run <- function(data){

  mplus_data <- data %>%
    select(contains("weight"),contains("math"),contains("read"),
         gender, ses, geo, indig, lang, y3_sid) %>%
    mutate(gender = ifelse(gender == 'boy', 1, 0),
           geo = ifelse(geo == 'urban', 1, 0),
           indig = ifelse(indig == 'indig', 1, 0),
           lang = ifelse(lang == 'eng', 1, 0)) %>%
    mutate(across(everything(), haven::zap_label)) %>%
    mutate(across(everything(), haven::zap_labels)) %>%
    mutate(across(everything(), as.numeric))

  MplusAutomation::prepareMplusData(df = as.data.frame(mplus_data),
                   filename = here("data", "mplus_data.dat"))

# # Read Mplus Models ####
  MplusAutomation::runModels(target = here("code", "mplus") )

  models <- MplusAutomation::readModels(target = here("code", "mplus") )
  
  return(models)
}

