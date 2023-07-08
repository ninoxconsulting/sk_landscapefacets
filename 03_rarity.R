#03. Rarity
#selecting the classes with less than 10% total coverage 

library(terra)
library(dplyr)

tdat <- read.csv("landscape_facets_summary.csv")
rare <- tdat %>% 
  select(skeena_lfacet_3005, rare_under_10)

srast = terra::rast("sk_lf_3005.tif")


#Sieve by 5 cells minimum
srast_s <- terra::sieve(srast, 5) 


# create a grid of points and then reclass into rare and not rare 
spts <- terra::as.points(srast, values=TRUE, na.rm=TRUE, na.all=FALSE)

writeVector(spts, "skeena_pts.gpkg")

ssf <- sf::st_as_sf(spts)
ssfa <- cbind(ssf, st_coordinates(ssf))

#convert to raster
spts <- left_join(spts,rare )

spp <- left_join(ssfa, rare)
rare_under_10