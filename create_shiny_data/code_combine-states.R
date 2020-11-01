# Gina, combining state's sims for shiny (just IA and IL right now)


library(tidyverse)

ccbioIA <- read_csv("create_shiny_data/IA_ccbio-map.csv")
ccbioIL <- read_csv("create_shiny_data/IL_ccbio-map.csv")


ccbio_map <-
  ccbioIA %>% 
  bind_rows(ccbioIL)

write_csv(ccbio_map, "MidwestRyeCoverCrop/ccbio_map.csv")
