
# GDAL Bug - https://github.com/jhollist/elevatr/issues/62
library(sf)
library(elevatr)
library(dplyr)

point_elev <- tibble(long = c(-89, -88.5), lat = c(43.5, 44)) %>% 
  sf::st_as_sf(coords = c("long", "lat"), 
               crs = 4326) %>% 
  sf::st_make_grid(square = TRUE, cellsize = 0.1, what = 'centers') %>% 
  sf::st_as_sf() %>% mutate(site_id = paste0('r_', row_number())) %>% 
  elevatr::get_aws_points(z = 9)
