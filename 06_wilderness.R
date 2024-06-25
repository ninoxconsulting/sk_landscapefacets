# wilderness

library(dplyr)
library(terra)
library(sf)

# read in templates 
srast <- rast(file.path("inputs", "sk_rast_template.tif"))
in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))


dis <- st_read(file.path("inputs", "skeena_clip_ce.gpkg" ))
rds <- st_read(file.path("inputs", "skeena_clippoly_roads.gpkg" ))


private <- st_read(file.path("inputs",  "sk_privateland_raw.gpkg"))

"cutblocks"





dis <- dis %>% 
  mutate(type = 1) %>% 
  select(type)

rds <- rds %>% 
  mutate(type = 1) %>% 
  select(type)


dist <- bind_rows(rds, dis)

human <- rasterize(dist, srast, field = "type")

explot(human)

# read in the private lands 









