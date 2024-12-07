install.packages("leaflet")
install.packages("leaflet.extras")

#load libraries
library(spData)
library(sf)
library(tidyverse)
library(units)
library(leaflet)
library(leaflet.extras)

#load 'world' data from spData package
data(world)

# load 'states' boundaries from spData package
data(us_states)

#plot data with 1st column
plot(world[1])  
plot(us_states[1])

#transform to albers equal area projection
albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#filter world datasat
canada <- world %>%
  filter(name_long == "Canada") %>%
  st_transform(crs = albers) 
  #st_transform(crs = 5070)  >> European Petroleum Survey Group(EPSG code -- NAD/Conus Albers) = 5070

#buffer Canada
canada_buffered <- st_buffer(canada, dist = 10000)

#filter us datasat
new_york <- us_states %>%
  filter(NAME == "New York") %>%
  st_transform(crs = albers)

#create border
border <- st_intersection(canada_buffered, new_york)

#show border area
ggplot() +
  geom_sf(data = new_york, fill = "white", color = "black") +
  geom_sf(data = border, fill = "red", color = "blue") +
  ggtitle("Buffered Canada (10km) in Albers Equal Area Projection") +
  theme_minimal()

#calculate area
border_area <- st_area(border) %>%
set_units(km^2)

# Transform geometries to WGS84 (EPSG:4326) for Leaflet
canada_wgs <- st_transform(canada_buffered, crs = 4326)
new_york_wgs <- st_transform(new_york, crs = 4326)
border_wgs <- st_transform(border, crs = 4326)

# Create a leaflet map
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(data = st_geometry(new_york_wgs), color = "black", weight = 2, fillOpacity = 0.1, popup = "New York") %>%
  addPolygons(data = st_geometry(border_wgs), color = "red", weight = 2, fillOpacity = 0.5, popup = paste("Border area:", round(border_area, 2), "km²")) %>%
  addLegend(position = "bottomright", 
            colors = c("black", "red"), 
            labels = c("New York State", "Border Area"), 
            opacity = 0.3, 
            title = "Legend") %>%
  addScaleBar(position = "bottomleft")


