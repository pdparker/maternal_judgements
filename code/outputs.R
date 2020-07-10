# Packages ####
library(tidyverse)
library(here)
library(glue)
library(gt)
library(tidylog)
library(patchwork)
# load apa theme for gt tables
source(here::here("templates", "apa_style.R") )
# load M and B theme for plots
source(here::here("templates", "theme_MandB.R") )

# Tables ####
#Skeleton
table2 <- data %>%
  select() %>%
  gt() %>%
  apa_theme()
# Adjust expand
gtsave(data = table2, path = "figures", filename = "table2.png",expand = 10)

# Figures ####
tmp <- cDataYr3_imp$imputations$imp1
p1 <- tmp %>%
  ggplot(aes(y3Num - y3NumSch, fill = y3MathScPar, color = y3MathScPar) ) +
  geom_density(alpha = .2) + 
  xlim(-250,250) +
  theme_bw() + 
  ggtitle("Year 3", "Parents perceptions and objective performance")

p2 <- tmp %>%
  ggplot(aes(y5Num - y5NumSch, fill = y5MathScPar, color = y5MathScPar) ) +
  geom_density(alpha = .2) + 
  xlim(-250,250) +
  theme_bw() + 
  ggtitle("Year 5", "Parents perceptions and objective performance")
#plots must be saved at 300dpi
p1 + p2  
ggsave(here("figures", "math_distribution.png"),width = 16, height = 8, dpi = 300) 

p1 <- tmp %>%
  ggplot(aes(y3Read - y3ReadSch, fill = y3ReadScPar, color = y3ReadScPar) ) +
  geom_density(alpha = .2) + 
  xlim(-250,250) +
  theme_tufte() + 
  ggtitle("Year 3", "Parents perceptions and objective performance")

p2 <- tmp %>%
  ggplot(aes(y5Read - y5ReadSch, fill = y5ReadScPar, color = y5ReadScPar) ) +
  geom_density(alpha = .2) + 
  xlim(-250,250) +
  theme_tufte() + 
  ggtitle("Year 5", "Parents perceptions and objective performance")

#plots must be saved at 300dpi
p1 + p2  
ggsave(here("figures", "reading_distribution.png"),width = 16, height = 8, dpi = 300)  


