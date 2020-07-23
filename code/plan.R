library(drake)
library(here)
source(here("code", "custom_functions.R"))
source(here("code", "prepare_data.R"))
source(here("code", "analysis_plan.R"))


plan <- drake_plan(
  age_4 =     data_age_4(),
  age_8 =     data_age_8(),
  age_10 =    data_age_10(),
  age_12 =    data_age_12(),
  c_admin =    child_admin(),
  s_admin =   school_admin(age_4),
  data =      child_data(list(age_4,age_8,age_10,
                            age_12,c_admin,s_admin)
                        ),
  data_imp =  child_data_imp(data),
  data_imp_mod = data_mod(data_imp),
  data_s =   data_svy(data_imp_mod),
  cor =      cor_fun(data_s),
  descriptives = descript_fun(data_s),
  quant = target(
    quant_reg(data_imp_mod, domain = dom, year = y),
    transform = cross(
      dom = c("read", "math"),
      y = c(3, 5, 7)
    )
  ),
  bias_mod_svy = target(
    bias_svy(data_s, domain = dom, year = y),
    transform = cross(
      dom = c("read", "math"),
      y = c(3, 5, 7)
    )
  ),
  sch_outcome = target(
    sch_model(data_s, domain = dom),
    transform = cross(dom = c("read", "math") )
  ),
  mplus = mplus_run(data),
  mplus_sum = target(
    mplus %>%
      map(`[[`, "summaries") %>%
      map_df(as_tibble)
  )
)

vis_drake_graph(plan,targets_only = TRUE)
make(plan)


loadd(sch_outcome_read)



