# wilderness

library(dplyr)
library(terra)
library(sf)

# read in templates 
srast <- rast(file.path("inputs", "sk_rast_template.tif"))
in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))

dis <- st_read(file.path("inputs", "skeena_clip_ce.gpkg" ))%>%
  filter(CEF_HUMAN_DISTURB_FLAG =="Human Disturb Current 20yr")%>%
  mutate(type = 1) %>% 
  select(type)

rds <- st_read(file.path("inputs", "skeena_clippoly_roads.gpkg" ))%>% 
  mutate(type = 1) %>% 
  select(type)


private <- st_read(file.path("inputs",  "sk_privateland_raw.gpkg"))%>%
  filter(OWNER_TYPE == "Private") %>%
  mutate(type = 1) %>% 
  select(type)


dist <- bind_rows(rds, dis)

dist <- rbind(dist, private)

dist <- vect(dist)
human <- terra::rasterize(dist, srast, field = "type")

plot(human)

h <- subst(human, NA, 0)
h <-  mask(h, srast)


terra::writeRaster(h,file.path("outputs", "sk_wilderness.tif"), overwrite = TRUE)










