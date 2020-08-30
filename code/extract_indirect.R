

reading <- my_extract_indirect()

# reading %>%
#   filter(str_detect(Path, "Maternal")) %>%
#   group_by(Effect) %>%
#   tidylog::summarise(across(Estimate:`+95 CI`, mean))
# 
# reading %>%
#   filter(str_detect(Path, "Total")) %>%
#   mutate(across(where(is.numeric),exp) )

math <- my_extract_indirect(file = here("code", "mplus","mediation_math.out"),
                            type = 'Math')
