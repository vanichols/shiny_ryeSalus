# Gina, modified the map.R code 9/22/2020
# trying to get Iowa data to make a shiny
# updated 11/2, making it generalizable to state

library(tidyverse)
library(maps)
library(mapdata)
library(magrittr)



# create quantile database ------------------------------------------------

# find 20, 50, and 80% quantiles

#--doy 92 = april 1
#--doy 106 = april 15
#--doy 122 = May 1
#--doy 136 = May 15
#--doy 153 = June 1
#--doy 167 = June 15
#--doy 183 = July 1
#--doy 197 = July 15
mytermdates <- c(92, 106, 122, 136, 153, 167, 183, 197)
rawdata <- list.files("regionalSim","raw")
myemptydf <- data.frame()

funGetQuants <- function(fun.filelist = rawdata, 
                         fun.state = "IA",
                         fun.mytermdates = mytermdates,
                         fun.quants = myemptydf){

  # testing
  #fun.state <- "IN"
  
  tmp.filelist <- fun.filelist[grepl(paste(fun.state), fun.filelist)]
  
  
  
for(i in 1:length(tmp.filelist)){
  
  #i <- 1
  cat("\r",round(i/length(tmp.filelist),2)*100,"%")
  
  tmp.quants <- 
    readRDS(file.path("regionalSim", tmp.filelist[i])) %>%
    as_tibble() %>%
    filter(!is.na(CWAD),
           DOY %in% mytermdates) %>% #--only keep days in my list
    group_by(ExpID) %>%
    separate(ExpID,c("lat","long","year","dop"),sep = "_") %>%
    group_by(lat, long, dop, DOY) %>%
    #summarise(CWAD = list(quantile(CWAD,probs = c(0.2,0.5,0.8)))) %>%
    summarise(CWAD20 = quantile(CWAD, 0.2),
              CWAD50 = quantile(CWAD, 0.5),
              CWAD80 = quantile(CWAD, 0.8)) %>%
    ungroup() %>% 
    mutate(state = substr(tmp.filelist[i], 1, 2),
           lat = as.numeric(gsub("N","",lat)),
           long = -as.numeric(gsub("W","",long))) 
  
  fun.quants <- 
    fun.quants %>%
    bind_rows(tmp.quants)
  
  i <- i + 1
  
}
  
  return(fun.quants)
  
}

#quants_IN <- funGetQuants(rawdata, "IN")
#quants_IA <- funGetQuants(rawdata, "IA")
#quants_IL <- funGetQuants(rawdata, "IL")

quants_MI <- funGetQuants(rawdata, "MI")
quants_MI %>% write_csv("create_shiny_data/quants/MI_ccbio-quants.csv")

quants_MN <- funGetQuants(rawdata, "MN")
quants_MN %>% write_csv("create_shiny_data/quants/MN_ccbio-quants.csv")

quants_OH <- funGetQuants(rawdata, "OH")
quants_OH %>% write_csv("create_shiny_data/quants/OH_ccbio-quants.csv")

quants_WI <- funGetQuants(rawdata, "WI")
quants_WI %>% write_csv("create_shiny_data/quants/WI_ccbio-quants.csv")

