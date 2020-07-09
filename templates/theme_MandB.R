library(tidyverse)
library(extrafont)


font_import()

theme_MandB <- function (base_size = 11, base_family = "Times New Roman", horizontal = TRUE, dkpanel = FALSE)
{
  bgcolors <- c("#24292D", "#323E4E", "#BED5E0")
  ret <- theme_foundation(base_size = base_size, base_family = base_family) + 
    theme(line = element_line(colour = "#24292D"), rect = element_rect(fill = bgcolors["ebg"], 
                                                                       colour = NA, linetype = 1), text = element_text(colour = "black"), 
          axis.line = element_line(size = rel(0.8)), axis.line.y = element_blank(), 
          axis.text = element_text(size = rel(1)), axis.text.x = element_text(vjust = 0, 
                                                                              margin = margin(t = base_size, unit = "pt"),
                                                                              colour = "#24292D"), 
          axis.text.y = element_text(color = "#24292D",hjust = 0, margin = margin(r = base_size, 
                                                                                  unit = "pt")), axis.ticks = element_line(), axis.ticks.y = element_blank(), 
          axis.title = element_text(color = "#24292D", size = rel(1)),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.ticks.length = unit(-base_size * 0.5, "points"),
          legend.background = element_rect(linetype = 0), 
          legend.spacing = unit(base_size * 1.5, "points"), 
          legend.key = element_rect(linetype = 0), legend.key.size = unit(1.2, 
                                                                          "lines"), legend.key.height = NULL, legend.key.width = NULL, 
          legend.text = element_text(size = rel(1.25)), legend.text.align = NULL, 
          legend.title = element_text(size = rel(1), hjust = 0), 
          legend.title.align = NULL, legend.position = "top", 
          legend.direction = NULL, legend.justification = "center", 
          panel.background = element_rect(linetype = 0), panel.border = element_blank(), 
          panel.grid.major = element_line(colour = "#BED5E0", 
                                          size = rel(1.75)), panel.grid.minor = element_blank(), 
          panel.spacing = unit(0.25, "lines"), strip.background = element_rect(fill = "#BED5E0", 
                                                                               colour = NA, linetype = 0), strip.text = element_text(size = rel(1.25)), 
          strip.text.x = element_text(), strip.text.y = element_text(angle = -90), 
          plot.background = element_rect(fill = "#BED5E0", 
                                         colour = NA), plot.title = element_text(size = rel(1.5), 
                                                                                 hjust = 0, face = "bold"), plot.margin = unit(c(6, 
                                                                                                                                 5, 6, 5) * 2, "points"), complete = TRUE)
  if (horizontal) {
    ret <- ret + theme(panel.grid.major.x = element_blank(),
                       panel.background = element_rect(fill = "white"), 
                       strip.background = element_rect(fill = "#323E4E"),
                       strip.text = element_text(colour = 'white'))
  }
  else {
    ret <- ret + theme(panel.grid.major.y = element_blank())
  }
  if (dkpanel == TRUE) {
    ret <- ret + theme(panel.background = element_rect(fill = "white"), 
                       strip.background = element_rect(fill = "#323E4E"),
                       strip.text = element_text(colour = 'white'))
  }
  ret
}

d <- tibble(
  Subject = rep(c("Math", "Science", "Computing", "Engineering",
                  "Physical Sciences", "Biology","Literacy"), each = 2),
  Construct = rep(c("Self-belief", "Interest"), 7),
  Effect = c(.27,.17,.18,.21,.44,.48,.24,.22,.43,.27,-.03,-.23,-.17,-.32)
)

d %>%
  ggplot(aes (x = reorder(Subject, -Effect), y = Effect, label = Effect)) +
  geom_bar(stat = "identity", fill = "#323E4E") +
  facet_wrap(~Construct) + 
  theme_MandB()