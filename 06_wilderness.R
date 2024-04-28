# wilderness


library(bcdata)
library(dplyr)
library(terra)
library(sf)


# read in aoi + prepped areas 

#basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
basedata = "inputs"

aoi <- vect(file.path(basedata, "SkeenaRegionBndry.gpkg"))
aoi_sf <- st_as_sf(aoi)

srast = terra::rast(file.path("outputs","facet_rcd_diversity_101c.tif"))


dis <- st_read(file.path("inputs", "skeena_clip_ce.gpkg" ))
rds <- st_read(file.path("inputs", "skeena_clippoly_roads.gpkg" ))


dis <- dis %>% 
  mutate(type = 1) %>% 
  select(type)

rds <- rds %>% 
  mutate(type = 1) %>% 
  select(type)


dist <- bind_rows(rds, dis)

human <- rasterize(dist, srast, field = "type")

explot(human)


## from-to-becomes
m <- c(0, 1, 1,
       1, 2, 2,
       2, 3, 3,
       3, 4, 4,
       4, 6, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(con_rare, rclmat, include.lowest=TRUE)

writeRaster(rc, file.path("outputs", "sk_rarity_conc.tif"))












