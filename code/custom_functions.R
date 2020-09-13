
# For naming files
date <- Sys.Date()
# load apa theme for gt tables
source(here::here("templates", "apa_style.R") )
# load M and B theme for plots
#source(here::here("templates", "theme_MandB.R") )

# Custom functions ####
# Tidyverse safe scale
z <- function(z){ (z - mean(z,na.rm=TRUE))/(sd(z,na.rm=TRUE))}
# For using in across
div <- function(x,num = 100) {x/num}
# For constructing custom cross-tabs
table_map <- function(table, rows = 2, cols = 5, margin=NULL){
  map(table, prop.table, margin=margin) %>%
    rbind.data.frame() %>% 
    rowMeans()  %>% 
    matrix(., nrow=rows, ncol=cols) %>% 
    provideDimnames() %>%
    as_data_frame() %>% 
    rownames_to_column() %>%
    tidylog::gather(variable, value, -rowname) %>% 
    tidylog::spread(rowname, value) %>%
    mutate(variable = c("much worse","worse","average", "better", "much better"))
}
#remove log made on same day as this is called by source
path <- dir_info(here("log"),regexp = ".txt") %>%
  mutate(path = str_extract(path,"[0-9]{4}-[0-9]{2}-[0-9]{2}") ) 

if(path$path == date){
  file_delete(here("log",glue("{path$path}_log.txt")))
}  

log_to_file <- function(text) cat(text, file = here("log",glue("{date}_log.txt")), sep = "\n", append = TRUE)
options("tidylog.display" = list(message, log_to_file))
# Custom version of extract indirect from Mplus Automation.
my_extract_indirect <- function(file = here("code", "mplus","mediation_read.out"),
                                type = 'Read') {
  short <- str_extract(type, ".{1}")
  # Read lines
  tmp <- readLines(file)
  top <- grep("CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", tmp)+2
  bottom <- grep("CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", tmp)-2
  # Select Indirect Effects
  tmp2 <- tmp[top:bottom] 
  tmp3 <- tmp2[str_detect(tmp2, ".+")]
  # Turn into data frame
  tmp4 <- str_split(tmp3, "\\s{2,}",simplify = TRUE) %>%
    as_tibble() %>%
    #slice(-1) %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    mutate(x = ifelse(as.character(x)!="", x, NA),
           x = zoo::na.locf(x),
           x = str_to_title(x))
  # Get Paths
  paths <- paste(tmp4$lower_5_percent,collapse = ',') %>%
    str_remove_all(.,"Total,|Total indirect,|^,") %>%
    str_split(., "GENDER", simplify = TRUE) %>%
    str_replace_all(",?Specific indirect [0-9]+,", "Specific Indirect: ") %>%
    str_replace_all(",?Direct", "Direct: ") %>%
    str_replace_all("_", " ") %>%
    str_to_title() %>%
    str_replace_all(glue("{short} Jud"), "Maternal Judgement") %>%
    str_replace_all(glue("{short} Int"), "Interest") %>%
    str_replace_all(glue("{type}"), "Achievement") %>%
    str_replace_all("^,","") %>%
    str_replace_all(",$","") %>%
    str_replace_all(": ,",": ") %>%
    `[`(-19)
  # Split to add in paths
  tmp4 <- 
    tmp4 %>%
    mutate(across(lower_2_5_percent:upper_5_percent_2, as.numeric)) %>%
    drop_na() %>%
    group_split(lower_5_percent)
  # add paths
  tmp4[[1]] <-tmp4[[1]] %>%
    mutate(lower_5_percent = paths)
  # Put back together
  tmp5 <- bind_rows(tmp4) %>% 
    select(x, lower_5_percent, upper_5_percent, lower_5_percent_2, upper_5_percent_2) %>%
    set_names(c("Effect", "Path", "Estimate", "-95 CI", "+95 CI")) 
  
  return(tmp5)
}

# Renaming Tribbles for consistent and informative row names
variable_rename <- tribble(
  ~variable_regex, ~new,
  "I\\(z\\(y._.+ - y._.+.sch\\)\\)", "z-Achievement",
  "^scale\\(ses\\)\\:gendergirl", "Gender X SES",
  "geourban\\:gendergirl", "Gender X Urban",
  ".+\\:geourban","Gender X Urban",
  ".+\\:indignonIndig", "Gender X non-Indigenous",
  ".+\\:langother", "Gender X LOTE",
  "^scale\\(iq\\)$", 'z-Cognitive Ability',
  "^scale\\(ses\\)$", 'z-SES',
  "^geourban$", 'Urban',
  "^gendergirl$", 'Girl',
  "^indignonIndig$", 'non-Indigenous',
  "^langother$", 'LOTE',
  "1\\|2", 'Much Worse | Worse',
  "2\\|3", 'Worse | Average',
  "3\\|4", 'Average | Better',
  "4\\|5",  'Better | Much Better'
)
# For extractions
dom = c("read", "math")
y = c(3, 5, 7)
dom_y <- cross2(dom, y) %>%  map_chr(paste, sep = "_", collapse = "_")

# Color pallet
pal1 <- c('#450c54', '#31688d', '#21908c', '#37b679', '#fde824')

