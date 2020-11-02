# Gina, modified the map.R code 9/22/2020
# trying to get Iowa data to make a shiny
# updated: 11/2 making quants separately from raw data thing, doing more states

library(tidyverse)
library(maps)
library(mapdata)
library(ggridges)



quantIA <- read_csv("create_shiny_data/quants/IA_ccbio-quants.csv")
quantIL <- read_csv("create_shiny_data/quants/IL_ccbio-quants.csv")
quantIN <- read_csv("create_shiny_data/quants/IN_ccbio-quants.csv")

quants <- 
  quantIA %>% 
  bind_rows(quantIL) %>% 
  bind_rows(quantIN)

#--summarised simulations
quants_l <- 
  quants %>%  
  pivot_longer(CWAD20:CWAD80, names_to = "prob", values_to = "ccbio") %>% 
  filter(DOY < 197) #--july 15 is ridiculous


#--Domain for simulations

simdomain <-
  data.frame(state = c("IN","OH","MI",
                       "ND","NE","KS",
                       "IA","MN","WI",
                       "SD","IL","MO")) %>%
  group_by(state) %>%
  do(data = read.csv(paste0("assets/Raster_Counts/",.$state,"_combined_corn_county.csv"))) %>%
  unnest(cols = c(data)) %>%
  separate(key, c("SoilID","lat","long","fips"), sep = "_") %>%
  mutate(Weatherfp = paste0(lat,"_",long)) %>%
  filter(SoilID %in% readRDS("data/SoilIDs.rds")) %>%
  group_by(lat,long,fips) %>%
  summarise(area = sum(Count)*0.09) %>%
  ungroup() %>%
  mutate(lat = as.numeric(gsub("N","",lat)),
         long = -as.numeric(gsub("W","",gsub(".wdb.xml","",long))),
         fips = as.numeric(fips)) %>%
  left_join(county.fips %>%
              separate(polyname,c("region","subregion"), sep = ","))



# Summarize data by county, probabilities
quants_county <- 
  quants_l %>%
  left_join(simdomain) %>%
  #group_by(region, subregion, prob, dop, fips) %>%
  group_by(region, subregion, prob, dop, fips, DOY) %>%
  summarise(area = sum(area),
            ccbio = mean(ccbio)
            ) %>%
  filter(area > 30000) %>%
  ungroup() %>%
  mutate(dop2 = factor(dop, labels = c("Sep-15","Oct-7","Nov-1")),
         DOY2 = factor(DOY, labels = c("Apr-1", "Apr-15", "May-1", "May-15", 
                                       "Jun-1", "Jun-15", "Jul-1")))

#--data to make figure
fig_dat <- 
  map_data("county") %>%
  left_join(quants_county) %>%
  as_tibble() %>% 
  filter(!is.na(prob))

fig_dat %>% select(region) %>% pull() %>% unique()

fig_dat %>% write_csv("MidwestRyeCoverCrop/ccbio_quants-map.csv")

