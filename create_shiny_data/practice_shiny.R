# Have different planting dates faceted
# Use ggridges to show distributions

library(ggridges)
library(ggthemes)
library(shiny)
library(tidyverse)
library(maps)
library(shinythemes)


#--for running through script
ccbio <-
  read_csv("create_shiny_data/IA_ccbio-map-raws.csv") %>%
  filter(dot < 183) %>% #--jul-1?
    mutate(subregion = str_to_title(subregion),
           ccbio_lbs = round(ccbio_kgha * 2.2/2.47, 0),
           ccbio_lbs = ifelse(ccbio_lbs <0, 0, ccbio_lbs),
           dop_nice = factor(dop, labels = c("Sep-15", "Oct-7", "Nov-1")),
           dot_nice = factor(dot, labels = c("Apr-1", "Apr-15", "May-1", "May-15", "Jun-1", "Jun-15")),
           dot_nicerev = fct_rev(dot_nice)
           )


ccbio %>% 
  filter(subregion == "Story") %>% 
      ggplot(aes(x = ccbio_lbs, y = dot_nicerev, fill = stat(x), height = ..density..)) + 
        geom_density_ridges_gradient(scale = 2, stat = "density", trim = TRUE) +
        geom_vline(xintercept = 2000, linetype = "dashed", color = "black") +
        scale_fill_viridis_c(option = "C") +
        facet_grid(.~dop_nice, scales = "free") + 
        labs(x = "Cover Crop Biomass (lbs/ac)",
             y = "Termination Date") +
        guides(fill = F) +
        theme(axis.title = element_text(size = rel(2.5)),
              axis.text.x = element_text(size = rel(2.3)),
              axis.text.y = element_text(size = rel(2.5)),
              strip.text = element_text(size = rel(2.5), face = "bold")
              ) +
        theme_bw()
      
      