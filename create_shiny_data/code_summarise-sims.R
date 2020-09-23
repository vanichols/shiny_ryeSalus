# Gina, modified the map.R code 9/22/2020
# trying to get Iowa data to make a shiny

library(tidyverse)
library(maps)
library(mapdata)

# Read simulation output
rawdata <- list.files("regionalSim_IA","raw")
term <- data.frame()
term2 <- data.frame()


#--doy 92 = april 1
#--doy 106 = april 15
#--doy 122 = May 1
#--doy 136 = May 15
#--doy 153 = June 1
#--doy 167 = June 15
#--doy 183 = July 1
#--doy 197 = July 15
mydates <- c(92, 106, 122, 136, 153, 167, 183, 197)




# find 20, 50, and 80th percentiles ---------------------------------------


for(i in 1:length(rawdata)){
  
  cat("\r",round(i/length(rawdata),2)*100,"%")
  
  term <- 
    readRDS(file.path("regionalSim",rawdata[i])) %>%
    as_tibble() %>%
    filter(!is.na(CWAD),
           DOY %in% mydates) %>%
    group_by(ExpID) %>%
    separate(ExpID,c("lat","long","year","dop"),sep = "_") %>%
    group_by(lat,long,dop, DOY) %>%
    #summarise(CWAD = list(quantile(CWAD,probs = c(0.2,0.5,0.8)))) %>%
    summarise(CWAD20 = quantile(CWAD, 0.2),
              CWAD50 = quantile(CWAD, 0.5),
              CWAD80 = quantile(CWAD, 0.8)) %>%
    ungroup() %>% 
    mutate(state = substr(rawdata[i], 1, 2),
           lat = as.numeric(gsub("N","",lat)),
           long = -as.numeric(gsub("W","",long))) %>%
    bind_rows(term)
  
}



term %>% write_csv("create_shiny_data/IA_ccbio.csv")
  


# keep raw values to create a distribution - do this directly in the shiny? --------------------------------


for(i in 1:length(rawdata)){
  
  #i <- 1
  
  cat("\r",round(i/length(rawdata),2)*100,"%")
  
  term2 <- 
    readRDS(file.path("regionalSim_IA", rawdata[i])) %>%
    as_tibble() %>%
    filter(!is.na(CWAD),
           DOY %in% mydates) %>%
    separate(ExpID,c("lat","long","year","dop"),sep = "_") %>%
    mutate(
      year = as.numeric(year),
      state = substr(rawdata[i], 1, 2),
      lat = as.numeric(gsub("N","",lat)),
      long = -as.numeric(gsub("W","",long))) %>%
    bind_rows(term2)
  
}


#--split it up so I can push to github
term2$dop %>% unique()

term2 %>% 
  filter(dop == "300") %>% write_rds("create_shiny_data/IA_ccbio-raws-300.rds")

term2 %>% 
  filter(dop == "280") %>% write_rds("create_shiny_data/IA_ccbio-raws-280.rds")

term2 %>% 
  filter(dop == "260") %>% write_rds("create_shiny_data/IA_ccbio-raws-260.rds")
