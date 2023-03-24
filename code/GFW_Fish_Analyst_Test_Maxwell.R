### GFW Fish Analyst Interview test

## Author: Maxwell Azali

### Date: 24-March-2023


## Clean working environment and detach packages
rm(list = ls())

library(here)

source(here("code", "Detach_Packages.R"))



### Load packages 
library(here)
library(DataExplorer)
library(tidyverse)
library(tidyselect)
library(sf)
library(rgeos)
library(maptools)


### Import data 

dat <- read.csv(here("analyst_testing","data", "analyst_fishing_data.csv"))


### Explore data properties
glimpse(dat)

dat %>%
  create_report(output_file = paste("Data_exploration_report","GFW_Fish_Analyst_Maxwell", sep="_"),
                output_dir = here("Outputs"),
                report_title = "GFW data exploration report",
                y = "fishing_hours")


#---- 
# Data exploration shows 18.6% missing observations across the entire dataset. Year has 45.8% of the observations missing, thus I cannot assume all the data is from 2019. I will only retain rows with 2019 data.
#---

dat_2019 <- dat%>%
  filter(year == 2019)

## Check distribution of fishing class and explore if there are non - fishing vessels included
dat_2019 %>%
  group_by(fishing_class) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

boxplot(dat_2019$fishing_hours)

range(dat_2019$fishing_hours)# Note there low and high values only 8,760 hours a year--> ask why high values and lower cutoff to consider (e.g 0.1 fishing hours?)


## No non-fishing vessels found in data thus can continue subsequent calculations  


#------------
# 1. Calculate the total fishing hours, by gear type, inside both closure areas in 2019.
#------------

# Calculate total fishing hours by gear type per location

total_fishing_per_gear_2019 <- dat_2019 %>%
  group_by(lat_bin, lon_bin, fishing_class) %>%
  summarize(total_fishing_hours_gear = sum(fishing_hours, na.rm = TRUE))%>%
  ungroup()


# Clip fishing hours to the closure areas shapefiles 

# Import shapefiles 
closure1 <- read_sf(here("analyst_testing", "data", "VME-DB_VME_GFCM_1", "closuresPolygon.shp"))
closure2 <- read_sf(here("analyst_testing", "data", "VME-DB_VME_GFCM_4", "other_areas.shp"))

closure1 <- st_as_sf(closure1)
closure2 <- st_as_sf(closure2)

## Check crs of both layers (conclusion both use similar crs)
st_crs(closure1)
st_crs(closure2)

# extract bounds of both shapefiles
closure1_bounds <-st_bbox(closure1)
closure2_bounds <- st_bbox(closure2)


# spatial join the 2019 per gear fishing hours to closure areas

# create sf_ object on 2019 fishing effort

sf_total_fishing_per_gear_2019 <- total_fishing_per_gear_2019 %>%
st_as_sf(coords = c("lon_bin", "lat_bin"),
         crs = st_crs(closure1))%>%
  mutate(lon = st_coordinates(.)[,1],lat = st_coordinates(.)[,2])


# closure 1 spatial join and filter out non intersecting values

closure1_total_fishing_per_gear_2019 <- sf_total_fishing_per_gear_2019 %>%
st_join(closure1, join = st_intersects) %>%
  filter(!is.na(VME_ID))


# closure 2 spatial join and filter out non intersecting values

closure2_total_fishing_per_gear_2019 <- sf_total_fishing_per_gear_2019 %>%
  st_join(closure2, join = st_intersects) %>%
  filter(!is.na(VME_ID))


closure_fishing_hours <- bind_rows(closure1_total_fishing_per_gear_2019,closure2_total_fishing_per_gear_2019)

# summarise total fishing effort (hours) per gear for each closure

closure_fishing_hours_summary <- closure_fishing_hours %>%
  filter(total_fishing_hours_gear >0) %>% # Remove zero fishing activity
  group_by(fishing_class, VME_ID, REG_NAME) %>%
  summarise(fishing_hours = sum(total_fishing_hours_gear, na.rm = TRUE))%>%
  rename(Closure_ID = VME_ID)%>%
    ungroup()



write.csv(closure_fishing_hours_summary, here("Outputs", "Q1_closure_fishing_hours.csv"))



#------------
# 2. Provide a map or two showing the closure areas overlaid with fishing effort for trawlers and longliners.
#---------


# Filter only longline and trawler data for 2019, removing zero fishing activity  this is for mapping fishing effort across entire seascape

longline_trawler <- sf_total_fishing_per_gear_2019 %>%
  filter(total_fishing_hours_gear >0) %>% # Remove zero fishing activity
  filter(grepl("longlines|trawlers", fishing_class))%>%
  as.data.frame()
 

# 2019 Closure data only filter longline and trawler data
longline_trawler_closure <- closure_fishing_hours %>%
  filter(total_fishing_hours_gear >0) %>% # Remove zero fishing activity
  filter(grepl("longlines|trawlers", fishing_class))%>%
  mutate(lon = st_coordinates(.)[,1],lat = st_coordinates(.)[,2])%>%
  as.data.frame()

range(longline_trawler_closure$total_fishing_hours_gear) # To help in setting the fill scale gradient for both closures


## Import world shapefile

world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

## transform world shapefile crs to match GFW closure crs

world <- st_transform(world,crs = st_crs(closure1))

# Post processing neat facet labels for ggplot 

label_facet <- as_labeller(c("drifting_longlines" = "Drifting longlines",
                             "set_longlines" = "Set longlines",
                             "trawlers" = "Trawlers"))



# Make map with all longline and trawler data, zoom in to closure 1 (Fisheries restricted area) overlain and facet by gear

map1 <- longline_trawler %>%
  ggplot()+
  geom_sf(data= world) +
  geom_tile( aes(x=lon, y=lat, fill=total_fishing_hours_gear)) +
  facet_wrap(. ~ fishing_class, labeller = label_facet) +            
scale_fill_gradientn(limits = c(0,860),
  colors = c("#ffffd4", "#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404"),
  breaks = c(0, 300, 600,  900),
  oob = scales::squish) +
  geom_sf(data = closure1, fill= "transparent", lty=1, linewidth = 1) +
  coord_sf(xlim = c(33, 36), ylim = c(30, 32.5), expand = FALSE) +
  theme_bw() + 
  theme(legend.position = "top")+
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location = "topright") +
  labs(x= "Longitude", y= "Latitude", fill= "Fishing hours", title = "2019 Fishing effort (Fisheries Restricted Area)")

map1

ggsave(plot = map1, filename = here("outputs", "Map1_Fish_restricted.pdf"), width = 13.4, height = 8.4)




# Make map with all longline and trawler data, zoom in to closure 2 (Essential fish habitat) overlain and facet by gear


map2 <- longline_trawler %>%
  ggplot()+
  geom_sf(data= world) +
  geom_tile( aes(x=lon, y=lat, fill=total_fishing_hours_gear)) +
  # geom_tile( data= sf_total_fishing_per_gear_2019, aes(x=lon, y=lat, fill=total_fishing_hours_gear)) +
  facet_wrap(. ~ fishing_class, labeller = label_facet) +            
  scale_fill_gradientn(limits = c(0,900),
                       colors = c("#ffffd4", "#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404"),
                       breaks = c(0, 300, 600,  900),
                       oob = scales::squish) +
  geom_sf(data = closure2, fill= "transparent", lty=1, linewidth = 1) +
  coord_sf(xlim = c(4, 6.0), ylim = c(42.5, 44.0), expand = TRUE) +
  theme_bw() + 
  theme(legend.position = "top") +
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location = "topright")+
  labs(x= "Longitude", y= "Latitude", fill= "Fishing hours", title = "2019 Fishing effort (Essential fish habitat)")

map2

ggsave(plot= map2, filename = here("outputs", "Map2_Fish_Essential_habitat.pdf"), width = 13.4, height = 8.4)





# Make map with all longline and trawler data for entire seascape

range(longline_trawler$lon)

range(longline_trawler$lat)


map3 <- longline_trawler %>%
  ggplot()+
  geom_sf(data= world) +
  geom_tile( aes(x=lon, y=lat, fill=total_fishing_hours_gear)) +
  # geom_tile( data= sf_total_fishing_per_gear_2019, aes(x=lon, y=lat, fill=total_fishing_hours_gear)) +
  facet_wrap(. ~ fishing_class, labeller = label_facet) +            
  scale_fill_gradientn(limits = c(0,900),
                       colors = c("#ffffd4", "#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404"),
                       breaks = c(0, 300, 600,  900),
                       oob = scales::squish) +
  geom_sf(data = closure2, fill= "transparent", lty=1, linewidth = 1) +
  geom_sf(data = closure1, fill= "transparent", lty=1, linewidth = 1) +
  coord_sf(xlim = c(2,35), ylim = c(30, 45), expand = TRUE) +
  theme_bw() + 
  theme(legend.position = "top") +
  ggspatial::annotation_scale() +
  ggspatial::annotation_north_arrow(location = "topright")+
  labs(x= "Longitude", y= "Latitude", fill= "Fishing hours", title = "2019 Fishing effort (Entire seascape)")

ggsave(plot= map3, filename = here("outputs", "Map3_Fish_Seascape.pdf"), width = 13.4, height = 8.4)
