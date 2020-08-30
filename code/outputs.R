
# Descriptives
#drake::loadd(descriptives)
table1_fun <- function(descriptives) {
  table1 <- descriptives %>%
    mutate(subject = case_when(
      subject == 'math' ~ 'Numeracy',
      subject == 'read' ~ 'Reading'
    )) %>%
    gt %>%
    apa_style() %>%
    fmt_number(
      columns = 4:7
    ) %>%
    tab_header(title = "Table 1",
               subtitle = md("_Maternal Judgements of their Child’s Academic Achievement_")) %>%
    cols_label( `subject` = 'Domain',
                `year` = 'School Year',
                `level` = 'Maternal Judgement',
                `mean` = 'Mean',
                `lower` = '-95% CI',
                `upper` = '+95% CI',
                `sd` = 'SD',
                `sample_pro` = 'Average %',
                `above_avg` = 'Above Avg. %',
                `below_avg` = 'Below Avg. %') %>%
    opt_align_table_header(align = 'left') %>%
    tab_source_note(source_note = md('_Notes._ Avg. = Average.')) %>%
    cols_align(
      align = "right",
      columns = 8:10
    )
  # May need to turn these into arguments.
  gtsave(data = table1, filename = here("figures",'table1.html') )
  webshot2::webshot(here("figures",'table1.html'), here("figures",'table1.png'),
                    zoom = 2, expand = 5,
                    vwidth = 800,
                    vheight = 1000)
  return(cat("Table 1 saved"))
}


# Bias Models #####
#drake::loadd(list=glue('bias_mod_svy_{dom_y}'))
# Table 2 reading ####
table2_read <- function(bias_mod_svy_read_3,bias_mod_svy_read_5,bias_mod_svy_read_7){
  read_bias_model <- bind_rows(bias_mod_svy_read_3,bias_mod_svy_read_5,bias_mod_svy_read_7,
                               .id = 'Model') %>%
    mutate(Model = case_when(
      Model == 1 ~ 'Year 3',
      Model == 2 ~ 'Year 5',
      Model == 3 ~ 'Year 7'
    )) %>%
    mutate(across(results:`upper)`, exp)) %>%
    regex_left_join(variable_rename, by = c(variable = 'variable_regex')) %>%
    slice(c(8:11, 1:7, (8:11)+11, (1:7)+11, (8:11)+22, (1:7)+22)) %>%
    dplyr::select(Model, Predictor = new, `Log-odds` = results, `-95% CI` = `(lower`, `+95% CI` = `upper)`)
  
  table2_read <- read_bias_model %>%
    mutate(across(where(is.numeric), ~round(., 2))) %>%
    pivot_wider(id_cols = Predictor,
                names_from = Model,
                values_from = `Log-odds`:`+95% CI`) %>% 
    dplyr::select(Predictor, `Log-odds_Year 3`, `-95% CI_Year 3`, `+95% CI_Year 3`,
                  `Log-odds_Year 5`, `-95% CI_Year 5`, `+95% CI_Year 5`,
                  `Log-odds_Year 7`, `-95% CI_Year 7`, `+95% CI_Year 7`) %>%
    gt %>%
    apa_style() %>%
    tab_spanner(label = "Year 3", 
                columns = vars(`Log-odds_Year 3`,`-95% CI_Year 3`,`+95% CI_Year 3`)) %>%
    tab_spanner(label = "Year 5", 
                columns = vars(`Log-odds_Year 5`,`-95% CI_Year 5`,`+95% CI_Year 5`)) %>%
    tab_spanner(label = "Year 7", 
                columns = vars(`Log-odds_Year 7`,`-95% CI_Year 7`,`+95% CI_Year 7`)) %>%
    tab_header(title = "Table 2a",
               subtitle = md("_Predictors of Maternal Judgements in Reading_")) %>%
    opt_align_table_header(align = 'left') %>%
    cols_label( `Log-odds_Year 3` = 'Odd Ratio',
                `Log-odds_Year 5` = 'Odd Ratio',
                `Log-odds_Year 7` = 'Odd Ratio',
                `-95% CI_Year 3` = '-95% CI',
                `-95% CI_Year 5` = '-95% CI',
                `-95% CI_Year 7` = '-95% CI',
                `+95% CI_Year 3` = '+95% CI',
                `+95% CI_Year 5` = '+95% CI',
                `+95% CI_Year 7` = '+95% CI'
    )  %>%
    cols_align(
      align = "left",
      columns = vars(Predictor)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(`Log-odds_Year 3`,`-95% CI_Year 3`,`+95% CI_Year 3`),
        rows =  (`-95% CI_Year 3` > 1 | `+95% CI_Year 3` < 1)
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(`Log-odds_Year 5`,`-95% CI_Year 5`,`+95% CI_Year 5`),
        rows =  (`-95% CI_Year 5` > 1 | `+95% CI_Year 5` < 1)
      )
    ) %>%
    tab_style(
      style = cell_text(weight = 'bold'),
      locations = cells_body(
        columns = vars(`Log-odds_Year 7`,`-95% CI_Year 7`,`+95% CI_Year 7`),
        rows =  (`-95% CI_Year 7` > 1 | `+95% CI_Year 7` < 1)
      )
    )  %>%
    tab_source_note(source_note = md('_Notes._ z- indicates that the variables has been z-scored.
                                   Bold text indicates that the 95% confidence interval for that parameter does not cross one.
                                   LOTE = child growing up in a household where a language other than English is spoken.'))
  
  gtsave(data = table2_read, filename = here("figures",'table2_read.html') )
  webshot2::webshot(here("figures",'table2_read.html'), here("figures",'table2_read.png'),
                    zoom = 2, expand = 5,
                    vwidth = 600,
                    vheight = 400)
}

# Table 2 math ####
table2_math <- function(bias_mod_svy_math_3,bias_mod_svy_math_5,bias_mod_svy_math_7){
  math_bias_model <- bind_rows(bias_mod_svy_math_3,bias_mod_svy_math_5,bias_mod_svy_math_7,
                               .id = 'Model') %>%
    mutate(Model = case_when(
      Model == 1 ~ 'Year 3',
      Model == 2 ~ 'Year 5',
      Model == 3 ~ 'Year 7'
    )) %>%
    mutate(across(results:`upper)`, exp)) %>%
    regex_left_join(variable_rename, by = c(variable = 'variable_regex')) %>%
    slice(c(8:11, 1:7, (8:11)+11, (1:7)+11, (8:11)+22, (1:7)+22)) %>%
    dplyr::select(Model, Predictor = new, `Log-odds` = results, `-95% CI` = `(lower`, `+95% CI` = `upper)`)
  
  table2_math <- math_bias_model %>%
    mutate(across(where(is.numeric), ~round(., 2))) %>%
    pivot_wider(id_cols = Predictor,
                names_from = Model,
                values_from = `Log-odds`:`+95% CI`) %>% 
    dplyr::select(Predictor, `Log-odds_Year 3`, `-95% CI_Year 3`, `+95% CI_Year 3`,
                  `Log-odds_Year 5`, `-95% CI_Year 5`, `+95% CI_Year 5`,
                  `Log-odds_Year 7`, `-95% CI_Year 7`, `+95% CI_Year 7`) %>%
    gt %>%
    apa_style() %>%
    tab_spanner(label = "Year 3", 
                columns = vars(`Log-odds_Year 3`,`-95% CI_Year 3`,`+95% CI_Year 3`)) %>%
    tab_spanner(label = "Year 5", 
                columns = vars(`Log-odds_Year 5`,`-95% CI_Year 5`,`+95% CI_Year 5`)) %>%
    tab_spanner(label = "Year 7", 
                columns = vars(`Log-odds_Year 7`,`-95% CI_Year 7`,`+95% CI_Year 7`)) %>%
    tab_header(title = "Table 2b",
               subtitle = md("_Predictors of Maternal Judgements in Numeracy_")) %>%
    opt_align_table_header(align = 'left') %>%
    cols_label( `Log-odds_Year 3` = 'Odds Ratio',
                `Log-odds_Year 5` = 'Odds Ratio',
                `Log-odds_Year 7` = 'Odds Ratio',
                `-95% CI_Year 3` = '-95% CI',
                `-95% CI_Year 5` = '-95% CI',
                `-95% CI_Year 7` = '-95% CI',
                `+95% CI_Year 3` = '+95% CI',
                `+95% CI_Year 5` = '+95% CI',
                `+95% CI_Year 7` = '+95% CI'
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(`Log-odds_Year 3`,`-95% CI_Year 3`,`+95% CI_Year 3`),
        rows =  (`-95% CI_Year 3` > 1 | `+95% CI_Year 3` < 1)
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(`Log-odds_Year 5`,`-95% CI_Year 5`,`+95% CI_Year 5`),
        rows =  (`-95% CI_Year 5` > 1 | `+95% CI_Year 5` < 1)
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(`Log-odds_Year 7`,`-95% CI_Year 7`,`+95% CI_Year 7`),
        rows =  (`-95% CI_Year 7` > 1 | `+95% CI_Year 7` < 1)
      )
    ) %>%
    tab_source_note(source_note = md('_Notes._ z- indicates that the variables has been z-scored.
                                   Bold text indicates that the 95% confidence interval for that parameter does not cross one.
                                   LOTE = child growing up in a household where a language other than English is spoken.'))
  
  gtsave(data = table2_math, filename = here("figures",'table2_math.html') )
  webshot2::webshot(here("figures",'table2_math.html'), here("figures",'table2_math.png'),
                    zoom = 2, expand = 5,
                    vwidth = 600,
                    vheight = 400) 
}
# Table 3 ####
#drake::loadd(mplus_sum)
#drake::loadd(mplus_out)
# Table 3 a ####
table3_read <- function(mplus_out){
  table3_read <- mplus_out$Read_RI.CLPM.out$parameters$ci.stdyx.standardized %>%
    as_tibble() %>%
    filter(str_detect(paramHeader,"ON")) %>%
    mutate(Outcome = str_remove(paramHeader,"\\..+")) %>%
    dplyr::select(Outcome, Predictor = param, 
                  Est = est, `-95 CI` = `low2.5`,`+95 CI` = `up2.5`) %>%
    mutate(Outcome = case_when(
      str_detect(Outcome, "CX") ~ str_replace(Outcome, 'CX', 'Interest Year '),
      str_detect(Outcome, "CY") ~ str_replace(Outcome, 'CY', 'Achievement Year '),
      str_detect(Outcome, "CW") ~ str_replace(Outcome, 'CW', 'Maternal Judgement Year ')
    ),
    Predictor = case_when(
      str_detect(Predictor, "CX") ~ str_replace(Predictor, 'CX', 'Interest Year '),
      str_detect(Predictor, "CY") ~ str_replace(Predictor, 'CY', 'Achievement Year '),
      str_detect(Predictor, "CW") ~ str_replace(Predictor, 'CW', 'Maternal Judgement Year ')
    )
    ) %>%
    gt() %>%
    apa_style() %>%
    tab_header(title = "Table 3a",
               subtitle = md("_RI-CLPM Reading_")) %>%
    opt_align_table_header(align = 'left') %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = everything(),
        rows =  (`-95 CI` * `+95 CI` >= 0)
      )
    ) %>%
    fmt_number(
      columns = 3:5,
      decimals = 2
    ) %>%
    tab_source_note(source_note = md('_Notes._ All results presented in standardized betas.'))
  
  gtsave(data = table3_read, filename = here("figures",'table3_read.html') )
  webshot2::webshot(here("figures",'table3_read.html'), here("figures",'table3_read.png'),
                    zoom = 2, expand = 5,
                    vwidth = 500,
                    vheight = 600)
}
# Table 3 b ####
table3_math <- function(mplus_out){
  table3_math <- mplus_out$Math_RI.CLPM.out$parameters$ci.stdyx.standardized %>%
    as_tibble() %>%
    filter(str_detect(paramHeader,"ON")) %>%
    mutate(Outcome = str_remove(paramHeader,"\\..+")) %>%
    dplyr::select(Outcome, Predictor = param, 
                  Est = est, `-95 CI` = `low2.5`,`+95 CI` = `up2.5`) %>%
    mutate(Outcome = case_when(
      str_detect(Outcome, "CX") ~ str_replace(Outcome, 'CX', 'Interest Year '),
      str_detect(Outcome, "CY") ~ str_replace(Outcome, 'CY', 'Achievement Year '),
      str_detect(Outcome, "CW") ~ str_replace(Outcome, 'CW', 'Maternal Judgement Year ')
    ),
    Predictor = case_when(
      str_detect(Predictor, "CX") ~ str_replace(Predictor, 'CX', 'Interest Year '),
      str_detect(Predictor, "CY") ~ str_replace(Predictor, 'CY', 'Achievement Year '),
      str_detect(Predictor, "CW") ~ str_replace(Predictor, 'CW', 'Maternal Judgement Year ')
    )
    ) %>%
    gt() %>%
    apa_style() %>%
    tab_header(title = "Table 3b",
               subtitle = md("_RI-CLPM Numeracy_")) %>%
    opt_align_table_header(align = 'left') %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = 3:5,
        rows =  (`-95 CI` * `+95 CI` >= 0)
      )
    ) %>%
    fmt_number(
      columns = 3:5,
      decimals = 2
    ) %>%
    tab_source_note(source_note = md('_Notes._ All results presented in standardized betas.'))
  
  gtsave(data = table3_math, filename = here("figures",'table3_math.html') )
  webshot2::webshot(here("figures",'table3_math.html'), here("figures",'table3_math.png'),
                    zoom = 2, expand = 5,
                    vwidth = 500,
                    vheight = 600)
}
# Table 4 ###
# Table 4a ####
table4_read <- function(mplus_out){
  table4_read <- mplus_out$mediation_read.out$parameters$ci.unstandardized %>%
    as_tibble() %>%
    filter(str_detect(paramHeader,"ON")) %>%
    mutate(Outcome = str_remove(paramHeader,"\\..+")) %>%
    dplyr::select(Outcome, Predictor = param, 
                  Est = est, `-95 CI` = `low2.5`,`+95 CI` = `up2.5`) %>% 
    mutate(Outcome = case_when(
      str_detect(Outcome, "JUD") ~ str_replace(Outcome, 'Y(.)_R_JUD', 'Maternal Judgement Year \\1'),
      str_detect(Outcome, "INT") ~ str_replace(Outcome, 'Y(.)_R_INT', 'Interest Year \\1'),
      str_detect(Outcome, "READ") ~ str_replace(Outcome, 'Y(.)_READ', 'Achievement Year \\1')
    ),
    Predictor = case_when(
      str_detect(Predictor, "JUD") ~ str_replace(Predictor, 'Y(.)_R_JUDGE', 'Maternal Judgement Year \\1'),
      str_detect(Predictor, "INT") ~ str_replace(Predictor, 'Y(.)_R_INT', 'Interest Year \\1'),
      str_detect(Predictor, "READ") ~ str_replace(Predictor, 'Y(.)_READ', 'Achievement Year \\1')
    ),
    across(Est:`+95 CI`,.fns = ~case_when(
      str_detect(Outcome, "Interest|Maternal") ~ exp(.),
      TRUE ~ .) 
    )
    )%>%
    drop_na() %>%
    gt() %>%
    apa_style() %>%
    tab_header(title = "Table 4b",
               subtitle = md("_CLPM Reading_")) %>%
    opt_align_table_header(align = 'left') %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = everything(),
        rows =  (`-95 CI` * `+95 CI` >= 0 & str_detect(Outcome, "Ach"))
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = everything(),
        rows =  (`-95 CI` > 1 | `+95 CI` < 1)
      )
    ) %>%
    fmt_number(
      columns = 3:5,
      decimals = 2
    ) %>%
    tab_source_note(source_note = md('_Notes._ When Achievement, Interest, and Judgements are outcomes they are z-scored.
                                   When Interest or Maternal Judgements are outcomes, results are given in Odds ratios.
                                   When Achievement is an outcome results are given in standardized betas.
                                   '))
  
  gtsave(data = table4_read, filename = here("figures",'table4_read.html') )
  webshot2::webshot(here("figures",'table4_read.html'), here("figures",'table4_read.png'),
                    zoom = 2, expand = 5,
                    vwidth = 500,
                    vheight = 600)
}
# Table 4b ####
table4_math <- function(mplus_out){
  table4_math <- mplus_out$mediation_math.out$parameters$ci.stdyx.standardized %>%
    as_tibble() %>%
    filter(str_detect(paramHeader,"ON")) %>%
    mutate(Outcome = str_remove(paramHeader,"\\..+")) %>%
    dplyr::select(Outcome, Predictor = param, 
                  Est = est, `-95 CI` = `low2.5`,`+95 CI` = `up2.5`) %>% 
    mutate(Outcome = case_when(
      str_detect(Outcome, "JUD") ~ str_replace(Outcome, 'Y(.)_M_JUD', 'Maternal Judgement Year \\1'),
      str_detect(Outcome, "INT") ~ str_replace(Outcome, 'Y(.)_M_INT', 'Interest Year \\1'),
      str_detect(Outcome, "MATH") ~ str_replace(Outcome, 'Y(.)_MATH', 'Achievement Year \\1')
    ),
    Predictor = case_when(
      str_detect(Predictor, "JUD") ~ str_replace(Predictor, 'Y(.)_M_JUDGE', 'Maternal Judgement Year \\1'),
      str_detect(Predictor, "INT") ~ str_replace(Predictor, 'Y(.)_M_INT', 'Interest Year \\1'),
      str_detect(Predictor, "MATH") ~ str_replace(Predictor, 'Y(.)_MATH', 'Achievement Year \\1')
    ),
    across(Est:`+95 CI`,.fns = ~case_when(
      str_detect(Outcome, "Interest|Maternal") ~ exp(.),
      TRUE ~ .) 
    )
    ) %>%
    drop_na() %>%
    gt() %>%
    apa_style() %>%
    tab_header(title = "Table 4b",
               subtitle = md("_CLPM Numeracy_")) %>%
    opt_align_table_header(align = 'left') %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = everything(),
        rows =  (`-95 CI` * `+95 CI` >= 0 & str_detect(Outcome, "Ach"))
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = everything(),
        rows =  (`-95 CI` > 1 | `+95 CI` < 1)
      )
    ) %>%
    fmt_number(
      columns = 3:5,
      decimals = 2
    ) %>%
    tab_source_note(source_note = md('_Notes._ When Achievement, Interest, and Judgements are outcomes they are z-scored.
                                   When Interest or Maternal Judgements are outcomes, results are given in Odds ratios.
                                   When Achievement is an outcome results are given in standardized betas.
                                   '))
  
  gtsave(data = table4_math, filename = here("figures",'table4_math.html') )
  webshot2::webshot(here("figures",'table4_math.html'), here("figures",'table4_math.png'),
                    zoom = 2, expand = 5,
                    vwidth = 500,
                    vheight = 600)
}

# Figures ####
plot_data <- function(data){
  plot_data <- data %>%
    dplyr::select(ends_with('judgement'), ends_with('math'),
                  ends_with('sch'), ends_with('read'),cid, gender) %>% 
    mutate(y3_math = y3_math - y3_math.sch,
           y5_math = y5_math - y5_math.sch,
           y7_math = y7_math - y7_math.sch,
           y3_read = y3_read - y3_read.sch,
           y5_read = y5_read - y5_read.sch,
           y7_read = y7_read - y7_read.sch) %>%
    dplyr::select(-ends_with('sch')) %>% 
    mutate(across(y3_math.judgement:cid, as.numeric)) %>% 
    pivot_longer(
      cols = y3_math.judgement:y7_read,
      names_to = c("Grade", "Variable"),
      names_sep = "_"
    ) %>% 
    pivot_wider(
      id_cols = c(cid, Grade, gender),
      names_from = Variable,
      values_from = value
    ) %>%
    mutate(Grade = str_replace(Grade, 'y', 'Year '),
           math.judgement = case_when(
             math.judgement == 1 ~ 'Much Worse',
             math.judgement == 2 ~ 'Worse',
             math.judgement == 3 ~ 'Average',
             math.judgement == 4 ~ 'Better',
             math.judgement == 5 ~ 'Much Better',
           ),
           math.judgement = fct_relevel(math.judgement, c('Much Worse','Worse',
                                                          'Average','Better',
                                                          'Much Better')
           ),
           read.judgement = case_when(
             read.judgement == 1 ~ 'Much Worse',
             read.judgement == 2 ~ 'Worse',
             read.judgement == 3 ~ 'Average',
             read.judgement == 4 ~ 'Better',
             read.judgement == 5 ~ 'Much Better',
           ),
           read.judgement = fct_relevel(read.judgement, c('Much Worse','Worse',
                                                          'Average','Better',
                                                          'Much Better')
           )
    )
  return(plot_data)
}
# Figure 1 ####
# drake::loadd(plot_data)

figure_math <- function(plot_data){
  # Data prep
  gen_data <- plot_data %>%
    mutate(Gender = ifelse(gender == 'boy', 0, 1)) %>%
    group_by(math.judgement, Grade) %>%
    tidylog::summarise(girl_prop = mean(Gender, na.rm=TRUE),
                       boy_prop = mean(1-Gender, na.rm=TRUE),
                       n = n()) %>%
    drop_na() 
  # Data annotation
  ann_one <- data.frame(math = -250,y = 0.006,
                        math.judgement = 1,
                        lab = 
                          "**Objective Performance Distribution**<br>
                      NAPLAN Numeracy<br>
                      (Child Score - School Average Score)",
                        Grade = factor('Year 3',levels = c("Year 3","Year 5","Year 7")))
  
  ann_two <- data.frame(math = 250,y = 0.006,
                        math.judgement = 1,
                        lab = 
                          '**Maternal Judgements**<br>
                      Mothers were asked: _Compared to other children in their class
                      how well do you think your child is progressing in numeracy?_<br>
                      With response options of:
                      <span style="color:#450c54" style="font-style:bold">Much Worse</span>,
                      <span style="color:#31688d" style="font-style:bold">Worse</span>,
                      <span style="color:#21908c" style="font-style:bold">Average</span>,
                      <span style="color:#37b679" style="font-style:bold">Better</span>, and
                      <span style="color:#fde824" style="font-style:bold">Much Better</span>',
                        Grade = factor('Year 7',levels = c("Year 3","Year 5","Year 7")))
  # Part 1 ####         
  p1 <- plot_data %>%
    ggplot (aes(x = math, group = as.factor(math.judgement),
                fill = as.factor(math.judgement))) +
    geom_density(aes(color = as.factor(math.judgement)), alpha = .3) +
    theme_void() +
    facet_wrap(~Grade, ncol = 1) +
    xlim(-250,250) +
    theme(legend.position = 'none',
          axis.text.x = element_text()) +
    geom_textbox(data = ann_one, aes(x=-170,y=.007, label = lab),
                 maxwidth = 50, fill = NA, box.size = NA, size = 2) +
    geom_textbox(data = ann_two, aes(x=180,y=.008, label = lab),
                 maxwidth = 40, fill = NA, box.size = NA, size = 2) +
    scale_fill_manual(values = pal1) +
    scale_color_manual(values = pal1) +
    theme(strip.text = element_text(face = 'bold', hjust = 0.045))
  
  # Part 2 ####
  p2 <- ggplot(gen_data) +
    geom_moon(aes(1, fct_rev(math.judgement), ratio = girl_prop,
                  fill = math.judgement, color = math.judgement),
              alpha = .5) +
    geom_text(aes(1,fct_rev(math.judgement),
                  label = scales::percent(gen_data$girl_prop,accuracy = 0.1)),
              size = 2) +
    theme_void() +
    facet_wrap(~Grade,ncol = 1) +
    labs(tag = 'Proportion girls') +
    theme(
      strip.text.x = element_text(hjust = 1, size = 0, face = 'bold'),
      strip.background = element_blank(),
      legend.position = 'none',
      plot.tag = element_text(angle = 90, size = 10),
      plot.tag.position = "left"
    ) +
    scale_y_discrete(position = "right") +
    scale_fill_manual(values = pal1) +
    scale_color_manual(values = pal1)
  # Combine Plots ####
  p1 + p2 +
    plot_layout(design = 
                  '
              AAAAAB
              AAAAAB
              AAAAAB
              AAAAAB
              AAAAAB
              AAAAAB
              ')
  
  ggsave(here("figures","figure1_numeracy.png"), dpi=300)
  
}


  
# Reading Plot #### 
figure_read <- function(plot_data){
  # Annotations
  ann_one <- data.frame(read = -250,y = 0.006,
                        read.judgement = 1,
                        lab = 
                          "**Objective Performance Distribution**<br>
                      NAPLAN Reading<br>
                      (Child Score - School Average Score)",
                        Grade = factor('Year 3',
                                       levels = c("Year 3","Year 5","Year 7")))
  
  ann_two <- data.frame(read = 250,y = 0.006,
                        read.judgement = 1,
                        lab = 
                          '**Maternal Judgements**<br>
                      Mothers were asked: _Compared to other children in their class
                      how well do you think your child is progressing in reading?_<br>
                      With response options of:
                      <span style="color:#450c54" style="font-style:bold">Much Worse</span>,
                      <span style="color:#31688d" style="font-style:bold">Worse</span>,
                      <span style="color:#21908c" style="font-style:bold">Average</span>,
                      <span style="color:#37b679" style="font-style:bold">Better</span>, and
                      <span style="color:#fde824" style="font-style:bold">Much Better</span>',
                        Grade = factor('Year 7',levels = c("Year 3","Year 5","Year 7")))
  # Data prep
  gen_data <- plot_data %>%
    mutate(Gender = ifelse(gender == 'boy', 0, 1)) %>%
    group_by(read.judgement, Grade) %>%
    tidylog::summarise(girl_prop = mean(Gender, na.rm=TRUE),
                       boy_prop = mean(1-Gender, na.rm=TRUE),
                       n = n()) %>%
    drop_na() 
  # Part 1 ####
  p1 <- plot_data %>%
    ggplot (aes(x = read, group = as.factor(read.judgement),
                fill = as.factor(read.judgement))) +
    geom_density(aes(color = as.factor(read.judgement)), alpha = .3) +
    theme_void() +
    facet_wrap(~Grade, ncol = 1) +
    xlim(-250,250) +
    theme(legend.position = 'none',
          axis.text.x = element_text()) +
    geom_textbox(data = ann_one, aes(x=-170,y=.007, label = lab),
                 maxwidth = 50, fill = NA, box.size = NA, size = 2) +
    geom_textbox(data = ann_two, aes(x=180,y=.008, label = lab),
                 maxwidth = 40, fill = NA, box.size = NA, size = 2) +
    scale_fill_manual(values = pal1) +
    scale_color_manual(values = pal1) +
    theme(strip.text = element_text(face = 'bold', hjust = 0.045))
  # Part 2 ####
  p2 <- ggplot(gen_data) +
    geom_moon(aes(1, fct_rev(read.judgement), ratio = girl_prop,
                  fill = read.judgement, color = read.judgement),
              alpha = .5) +
    geom_text(aes(1,fct_rev(read.judgement),
                  label = scales::percent(gen_data$girl_prop,accuracy = 0.1)),
              size = 2) +
    theme_void() +
    facet_wrap(~Grade,ncol = 1) +
    labs(tag = 'Proportion girls') +
    theme(
      strip.text.x = element_text(hjust = 1, size = 0, face = 'bold'),
      strip.background = element_blank(),
      legend.position = 'none',
      plot.tag = element_text(angle = 90, size = 10),
      plot.tag.position = "left"
    ) +
    scale_y_discrete(position = "right") +
    scale_fill_manual(values = pal1) +
    scale_color_manual(values = pal1)
  # Combine Plots ####
  p1 + p2 +
    plot_layout(design = 
                  '
              AAAAAB
              AAAAAB
              AAAAAB
              AAAAAB
              AAAAAB
              AAAAAB
              ')
  
  ggsave(here("figures","figure1_reading.png"), dpi=300)
  
}







