# Gina, modified the map.R code 9/22/2020
# trying to get Iowa data to make a shiny

library(maps)
library(mapdata)

#--summarised simulations
term <- 
  read_csv("create_shiny_data/IA_ccbio.csv") %>% 
  pivot_longer(CWAD20:CWAD80, names_to = "prob", values_to = "ccbio")

#--raw-ish sims
term2 <- 
  read_rds("create_shiny_data/IA_ccbio-raws.rds")



# Domain for simulations
simdomain <-
  data.frame(state = c("IA")) %>%
  group_by(state) %>%
  do(data = read.csv(paste0("assets/Raster_Counts/",.$state,"_combined_corn_county.csv"))) %>%
  unnest() %>%
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



# probabilities -----------------------------------------------------------

# Summarize data by county, probabilities
term_county <- 
  term %>%
  left_join(simdomain) %>%
  #group_by(region, subregion, prob, dop, fips) %>%
  group_by(region, subregion, prob, dop, fips, DOY) %>%
  summarise(area = sum(area),
            ccbio = mean(ccbio)
            ) %>%
  filter(area > 30000) %>%
  ungroup() %>%
  mutate(dop2 = factor(dop, labels = c("Sep-15","Oct-7","Nov-1")),
         DOY2 = factor(DOY, labels = c("Apr-1", "Apr-15", "May-1", "May-15", "Jun-1", "Jun-15", "Jul-1", "Jul-15")))

term_county

#--data to make figure
fig_dat <- 
  map_data("county") %>%
  left_join(term_county) %>%
  as_tibble() %>% 
  filter(!is.na(prob))

fig_dat %>% write_csv("create_shiny_data/IA_ccbio-map.csv")



# raws, distributions -----------------------------------------------------------

library(ggridges)

term2_county <- 
  term2 %>%
  left_join(simdomain) %>%
  mutate(dop2 = factor(dop, labels = c("Sep-15","Oct-7","Nov-1")),
         DOY2 = factor(DOY, labels = c("Apr-1", "Apr-15", "May-1", "May-15", "Jun-1", "Jun-15", "Jul-1", "Jul-15")),
         DOY3 = fct_rev(DOY2)) %>% 
  filter(!is.na(CWAD))

term2_county %>% write_rds("create_shiny_data/IA_ccbio-map-raws.rds")


# example maps ------------------------------------------------------------


#--practice map, I love it!
term2_county %>% 
  filter(subregion == "story") %>% 
  ggplot(aes(x = CWAD, y = DOY3, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 2) +
  scale_fill_viridis_c(option = "C") +
  #coord_cartesian(xlim = c(0, 6000)) + 
  facet_grid(.~dop2, scales = "free")




fig_dat2 <- 
  fig_dat %>% 
  filter(dop2 == "Sep-15",
         DOY2 == "Jun-1",
         subregion == "adair"
         ) 

fig_dat3 <- 
  fig_dat2 %>% 
  group_by(subregion, prob, dop2, DOY2) %>% 
  summarise(long = mean(long),
            lat = mean(lat),
            ccbio = mean(ccbio, na.rm = T)) %>% 
  distinct()

ggplot() + 
  geom_polygon(data = fig_dat, aes(x=long, y = lat, group = group, fill = ccbio)) + 
  geom_polygon(data = fig_dat2,
               aes(x=long, y = lat, group = group),
               color = "red",
               fill = NA,
               lwd = 2) + 
  geom_label(data = fig_dat3,
               aes(x=long, y = lat, label = ccbio),
               color = "red",
               ) + 
  # geom_polygon(data = state_poly, 
  #              aes(x=long, y = lat, group = group), 
  #              fill = alpha("white",0.1),
  #              colour = "black", lwd = 0.5) + 
  facet_grid(.~prob) + 
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,
                               barheight = 0.8)) +
  theme(legend.position = "top")





  
  ggplot(aes(long, lat, fill=ccbio)) +
  geom_polygon(aes(x=long, y = lat, group = group)) + 
  geom_polygon(data = state_poly, 
              aes(x=long, y = lat, group = group), 
              fill = alpha("white",0.1),
              colour = "black", lwd = 0.5) + 
  facet_grid(.~prob) + 
  scale_fill_viridis_c()
  #scale_fill_viridis_c(direction = -1, breaks = c(100,120,140,160,180), labels = c("<Apr-10","May-1","May-20","Jun-10",">Jun-30")) + 
  coord_equal(ratio = 1.3,
              expand = T) + 
  labs(y = "", x = "", fill = "Earliest termination date with rye biomass > 5.0 Mg/ha") +
  ggthemes::theme_few() + 
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,
                               barheight = 0.8))   

ggsave("figures/map.jpg",width = 8, height = 8)

# Plot only one map
map_data("county")%>%
  left_join(term_county) %>%
  filter(!is.na(DOY), dop == "Oct-7", prob == "80%") %>%
  ggplot(aes(long,lat, fill=DOY)) +
  geom_polygon(aes(x=long, y = lat, group = group, fill = pmin(180,pmax(DOY,100))),colour = "gray10",lwd = 0.3) + 
  geom_polygon(data = state_poly, 
               aes(x=long, y = lat, group = group), 
               fill = alpha("white",0.1),
               colour = "black", lwd = 0.5) + 
  #facet_grid(prob~dop) + 
  scale_fill_viridis_c(direction = -1, breaks = c(100,120,140,160,180), labels = c("<Apr-10","May-1","May-20","Jun-10",">Jun-30")) + 
  coord_equal(ratio = 1.3,
              expand = T) + 
  labs(y = "Latitude", x = "Longitude", fill = "Earliest termination date with rye biomass > 5.0 Mg/ha") +
  ggthemes::theme_few() + 
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,
                               barheight = 0.8))   

ggsave("figures/map_Oct7P80.jpg",width = 5, height = 5)
