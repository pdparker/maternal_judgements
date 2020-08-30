source(here::here("code", "packages.R"))
source(here("code", "custom_functions.R"))
source(here("code", "prepare_data.R"))
source(here("code", "analysis_plan.R"))
source(here("code", "mplus_run.R"))
source(here("code", "outputs.R"))


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
  bias_mod_svy = target(
    bias_svy(data_s, domain = dom, year = y),
    transform = cross(
      dom = c("read", "math"),
      y = c(3, 5, 7)
    )
  ),
  bias_mod_svy_int = target(
    bias_svy_int(data_s, domain = dom, year = y),
    transform = cross(
      dom = c("read", "math"),
      y = c(3, 5, 7)
    )
  ),
  sch_outcome = target(
    sch_model(data_s, domain = dom),
    transform = cross(dom = c("read", "math") )
  ),
  mplus = mplus_prepare(d = data_imp, m = 5),
  mplus_out = mplus_run(mplus),
  mplus_sum = target(
     mplus_out %>%
       map(`[[`, "summaries") %>%
       map_df(as_tibble)
   ),
  table1 = table1_fun(descriptives),
  table2a = table2_read(bias_mod_svy_read_3,bias_mod_svy_read_5,bias_mod_svy_read_7),
  table2b = table2_math(bias_mod_svy_math_3,bias_mod_svy_math_5,bias_mod_svy_math_7),
  table3a = table3_read(mplus_out),
  table3b = table3_math(mplus_out),
  table4a = table4_read(mplus_out),
  table4b = table4_math(mplus_out),
  plots = plot_data(data),
  figure1 = figure_read(plots),
  figure2 = figure_math(plots)
)

vis_drake_graph(plan,targets_only = TRUE)
make(plan)
#clean()

drake_ggraph(plan, label_nodes = TRUE, targets_only = TRUE)
ggsave(here("figures","dependency.png"), dpi = 300, width = 12, height = 12)



# Old Models 
# quant = target(
#   quant_reg(data_imp_mod, domain = dom, year = y),
#   transform = cross(
#     dom = c("read", "math"),
#     y = c(3, 5, 7)
#   )
# ),


