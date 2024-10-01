library(spData)
library(sf)
library(tidyverse)
library(units)
# library(units) #this one is optional, but can help with unit conversions.

#load 'world' data from spData package
data(world)

# load 'states' boundaries from spData package
data(us_states)

plot(world[1])  #plot if desired
plot(us_states[1]) #plot if desired

#transform to albers equal area projection
albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#filter world datasat
canada <- world %>%
  filter(name_long == "Canada") %>%
  st_transform(crs = albers)

#buffer Canada
canada_buffered <- st_buffer(canada, dist = 10000) %>%

#filter us datasat
new_york <- us_states %>%
  filter(NAME == "New York") %>%
  st_transform(crs = albers)

border <- st_intersection(canada_buffered, new_york)

#show border
ggplot() +
  geom_sf(data = new_york, fill = "white", color = "black") +
  geom_sf(data = border, fill = "red", color = "blue") +
  ggtitle("Buffered Canada (10km) in Albers Equal Area Projection") +
  theme_minimal()

border_area <- st_area(border) %>%
set_units(km^2)



