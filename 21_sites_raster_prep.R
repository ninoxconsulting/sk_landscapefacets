#21_prep_sites_layers

library(dplyr)
library(terra)
library(sf)


# read in templates 
srast <- rast(file.path("inputs", "sk_rast_template.tif"))
in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))
outputs <- file.path("outputs", "final", "sites", "raw_tiffs")






# read in the wilderness layer / human footprint layer

w <- rast(file.path("outputs", "sk_wilderness_2023.tif"))
names(w)<- "humanfootprint"

writeRaster(w, file.path(outputs, "humanfootprint.tif"), overwrite=TRUE)




# productivity 

gdd <- rast(file.path("inputs", "gdd_sk.tif"))
names(gdd)<- "gdd"
writeRaster(gdd, file.path(outputs, "gdd_c.tif"), overwrite=TRUE)

ndvi <- rast(file.path("outputs", "ndvi_2019_2023_mean_101c.tif"))
names(ndvi)<- "ndvi"

# might need to stack? 
writeRaster(ndvi, file.path(outputs, "ndvi_c.tif"))




# resistence / connectivity 
res <- rast(file.path("inputs", "pither_resistance.tif"))
names(res) = "resistance"

writeRaster(res, file.path(outputs, "resistance_c.tif"))


# microrefugia 
mic <- rast(file.path("inputs", "microrefugia.tif"))
names(mic) = "microrefugia"
writeRaster(mic, file.path(outputs, "microrefugia.tif"))

# macrorefugia
mac <- rast(file.path("inputs", "2080s_macrorefugia.tif"))
names(mac) = "macrorefugia"
writeRaster(mac, file.path(outputs, "macrorefugia.tif"))





#protected _lands 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pro <- rasterize(pro, srast)
names(pro) = "protected_lands"
writeRaster(pro, file.path(outputs, "protected_lands.tif"))



# red and blue sp and federal listed species. 
rb <- st_read(file.path("inputs", "bc_red_blue_sp_raw.gpkg"))
rb <- rasterize(rb, srast)
names(rb) = "red_blue_sp"
writeRaster(rb, file.path(outputs, "red_blue_sp.tif"))

# fed listed species

rb <- st_read(file.path("inputs", "fed_listed_sp_raw.gpkg"))
rb <- rasterize(rb, srast)
names(rb) = "fed_listed_sp"
writeRaster(rb, file.path(outputs, "fed_listed_sp.tif"))


# KBAs & IBAs
#kbam <- st_read(file.path("outputs", "final", "KBA_marine.gpkg"))
#pro <- rasterize(kbam, srast)
#names(pro) = "kba_marine"
#writeRaster(pro, file.path(outputs, "kba_marine.tif"))

kbat <- st_read(file.path("outputs", "final", "KBA_terrestrial.gpkg"))
kbat <- st_transform(kbat, crs=st_crs(srast))
pro <- rasterize(kbat, srast)
names(pro) = "ter_kba"
writeRaster(pro, file.path(outputs, "kba_terrestrial.tif"))


j <- st_read(file.path("outputs", "final", "Jokulhaups_sk.gpkg"))
j <- st_transform(j, crs=st_crs(srast))
pro <- rasterize(j, srast)
names(pro) = "jokulhaups"
writeRaster(pro, file.path(outputs, "jokulhaups.tif"))

#IBA 
ib <- st_read(file.path("outputs", "final", "sk_important_bird_areas.gpkg"))
ib <- st_transform(ib, crs=st_crs(srast))
ib <- rasterize(ib, srast)
names(ib) = "iba"
writeRaster(ib, file.path(outputs, "iba.tif"))




# TAP Old growth 
## intact watersheds - From TAP old growth 
#https://catalogue.data.gov.bc.ca/dataset/b684bdbf-8824-4bc6-8d22-329d9b97c043

tap <- rast( file.path("inputs", "TAP_intact_watershed.tif"))
names(tap) = "tap_intact_watershed"
writeRaster(tap, file.path(outputs, "TAP_intact_watershed.tif"), overwrite = T)

# big trees 
bt <- rast(file.path("inputs", "TAP_bigtrees_raw.tif"))
names(bt) = "tap_bigtrees"
writeRaster(bt, file.path(outputs, "TAP_bigtrees.tif"), overwrite = T)


# ancient forests
bt <- rast(file.path("inputs", "TAP_ancient_forest_raw.tif"))
names(bt) = "tap_ancient_forest"
writeRaster(bt, file.path(outputs, "TAP_ancient_forest.tif"), overwrite = T)










## Landscape terrestrial barcodes 

#sk_barcode? 


# rarity

# continuous measure of rarity
con_rarec <- rast(file.path("outputs","sk_lf_rdc_rarity_101c.tif"))
names(con_rarec)= "ter_rarity"
writeRaster(con_rarec, file.path(outputs, "ter_rarity_c.tif"))

# classed version
con_rareclass <- rast(file.path("outputs", "sk_rarity_conc.tif"))
names(con_rareclass)= "ter_rarity_class"
writeRaster(con_rareclass, file.path(outputs, "ter_rarity_class.tif"))


# diversity - terrestrial 
div= rast(file.path("outputs", "sk_lf_rdc_diversity_101c.tif"))
names(div)= "ter_diversity"
writeRaster(div, file.path(outputs, "ter_diversity_c.tif"))

# classed version 
divc <- rast(file.path("outputs", "sk_diversity_conc.tif"))
names(divc)= "ter_diversity_class"
writeRaster(divc, file.path(outputs, "ter_diversity_class.tif"))



## Aquatic terrestrial barcodes - lakes 

# rarity - continuous lakes 
rar <- rast(file.path("outputs", "sk_lakes_meanbyarea_rarity_101c.tif"))
names(rar )= "aq_lakes_rarity_c"
writeRaster(rar, file.path(outputs, "aq_lakes_rarity_c.tif"))

# rarity - classed lakes 

rc <- rast(file.path("outputs", "sk_lakes_raritybyarea_conc.tif"))
names(rc )= "aq_lakes_rarity_class"
writeRaster(rc, file.path(outputs, "aq_lakes_rarity_class.tif"))


# diversity - continuous lakes
div <- rast(file.path("outputs", "sk_lakes_meanbyarea_div_101c.tif"))
names(div)= "aq_lakes_diversity_c"
writeRaster(div, file.path(outputs, "aq_lakes_diversity_c.tif"))

# diversity - classed lakes
div <- rast(file.path("outputs", "sk_lakes_divarea_conc.tif"))
names(div)= "aq_lakes_diversity_class"
writeRaster(div, file.path(outputs, "aq_lakes_diversity_class.tif"))


## Aquatic terrestrial barcodes - Rivers
# diversity - continuous
div <- rast(file.path("outputs", "sk_rivers_diversity_101c.tif"))
names(div)= "aq_rivers_diversity_c"
writeRaster(div, file.path(outputs, "aq_rivers_diversity_c.tif"))
# diversity - classed
div <- rast(file.path("outputs", "sk_rivers_diversity_conc.tif"))
names(div)= "aq_rivers_diversity_class"
writeRaster(div, file.path(outputs, "aq_rivers_diversity_class.tif"))

# rarity - continuous
rc <- rast(file.path("outputs", "sk_rivers_rarity_mean_101c.tif"))
names(rc)= "aq_rivers_rarity_c"
writeRaster(rc, file.path(outputs, "aq_rivers_rarity_c.tif"), overwrite = T)

# rarity - classed

rc <- rast( file.path("outputs", "sk_rivers_rarity_mean_conc.tif"))
names(rc)= "aq_rivers_rarity_class"
writeRaster(rc, file.path(outputs, "aq_rivers_rarity_class.tif"), overwrite = T)


# wetland density 
wet <-  rast(file.path("inputs", "sk_wetland_density_1km.tif"))
names(wet) = "aq_wetland_density"
writeRaster(wet, file.path(outputs, "aq_wetland_density.tif"))

# lake density

lak <- rast(file.path("inputs", "sk_lake_density_1km.tif"))
names(lak) = "aq_lake_density"
writeRaster(lak, file.path(outputs, "aq_lake_density.tif"))






