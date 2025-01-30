#21_prep_sites_layers

library(dplyr)
library(terra)
library(sf)


# read in templates 
srast <- rast(file.path("inputs", "sk_rast_template.tif"))
srast <- aggregate(srast, fact = 10)
names(srast) <- "template"


in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))
outputs <- file.path("outputs", "final", "sites", "raw_tiffs")

#dim(srast)
#ncell(srast)




# read in the wilderness layer / human footprint layer

w <- rast(file.path("outputs", "final", "sk_wilderness_2023.tif"))
names(w)<- "humanfootprint"
w  <- aggregate(w , fact=10, fun="mean")
writeRaster(w, file.path(outputs, "humanfootprint.tif"), overwrite=TRUE)




# productivity 
gdd <- rast(file.path("inputs", "gdd_sk.tif"))
names(gdd)<- "gdd"
gdd  <- aggregate(gdd , fact=10, fun="mean")
writeRaster(gdd, file.path(outputs, "gdd_c.tif"), overwrite=TRUE)

# ndvi
ndvi <- rast(file.path("outputs", "ndvi_2019_2023_mean_101c.tif"))
names(ndvi)<- "ndvi"
ndvi  <- aggregate(ndvi , fact=10, fun="mean")
# might need to stack? 
writeRaster(ndvi, file.path(outputs, "ndvi_c.tif"), overwrite = TRUE)


#npp 
npp <- rast(file.path("inputs", "npp_skeena_2019_2023.tif"))
names(npp)<- "npp"
npp <- project(npp,srast)
writeRaster(npp, file.path(outputs, "npp.tif"), overwrite = TRUE)


# resistence / connectivity 
res <- rast(file.path("inputs", "pither_resistance.tif"))
names(res) = "resistance"
res <- aggregate(res , fact=10, fun="mean")
writeRaster(res, file.path(outputs, "resistance_c.tif"), overwrite = TRUE)


# microrefugia 
mic <- rast(file.path("inputs", "microrefugia.tif"))
names(mic) = "microrefugia"
mic <- aggregate(mic  , fact=10, fun="mean")
writeRaster(mic, file.path(outputs, "microrefugia.tif"), overwrite = TRUE)

# macrorefugia
mac <- rast(file.path("inputs", "2080s_macrorefugia.tif"))
names(mac) = "macrorefugia"
mac <- aggregate(res , fact=10, fun="mean")
writeRaster(mac, file.path(outputs, "macrorefugia.tif"), overwrite = TRUE)





#protected _lands 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pro <- rasterize(vect(pro), srast)
names(pro) = "protected_lands"
writeRaster(pro, file.path(outputs, "protected_lands.tif"), overwrite = TRUE)





# break this into species Presence/absence

# red and blue sp and federal listed species. 
rb <- st_read(file.path("inputs", "bc_red_blue_sp_raw.gpkg"))

#unique(rb$SCI_NAME)
#unique(rb$EL_TYPE)

sp <- unique(rb$SCI_NAME)

purrr::map(sp, function(x){
  print(x)
  rbb <- rb |> filter(SCI_NAME == x)
  comm_name <- gsub(" ", "_", unique(rbb$ENG_NAME))
  comm_name <- gsub("/", "", comm_name)
  comm_name <- gsub("-", "", comm_name)
  
  
  sciname <- gsub(" ", "_", unique(rbb$SCI_NAME))
  sciname <- gsub("/", "", sciname)
  sciname <- gsub("-", "", sciname)
  
  rbb <- rasterize(rbb, srast)
  names(rbb) = paste0("bc_listed_", comm_name )
  writeRaster(rbb, file.path(outputs, "species", paste0("bc_listed_", sciname, ".tif")), overwrite = TRUE)
  
})


# rb <- rasterize(rb, srast)
# names(rb) = "red_blue_sp"
# writeRaster(rb, file.path(outputs, "red_blue_sp.tif"), overwrite = TRUE)


# fed listed species
rb <- st_read(file.path("inputs", "fed_listed_sp_raw.gpkg"))

sp <- unique(rb$SCIENTIFIC_NAME)

purrr::map(sp, function(x){
  print(x)
  rbb <- rb |> filter(SCIENTIFIC_NAME == x)
  comm_name <- gsub(" ", "_", unique(rbb$COMMON_NAME_ENGLISH))
  sciname <- gsub(" ", "_", unique(rbb$SCIENTIFIC_NAME))
  rbb <- rasterize(rbb, srast)
  names(rbb) = paste0("fed_listed_", comm_name )
  writeRaster(rbb, file.path(outputs, "species", paste0("fed_listed_", sciname, ".tif")), overwrite = TRUE)
  
})




# KBAs & IBAs
#kbam <- st_read(file.path("outputs", "final", "KBA_marine.gpkg"))
#pro <- rasterize(kbam, srast)
#names(pro) = "kba_marine"
#writeRaster(pro, file.path(outputs, "kba_marine.tif"))

kbat <- st_read(file.path("outputs", "final", "KBA_terrestrial.gpkg"))
kbat <- st_transform(kbat, crs=st_crs(srast))
pro <- rasterize(vect(kbat), srast)
names(pro) = "ter_kba"
writeRaster(pro, file.path(outputs, "kba_terrestrial.tif"), overwrite = TRUE)
# not included as so small 



j <- st_read(file.path("outputs", "final", "Jokulhaups_sk.gpkg"))
j <- st_transform(j, crs=st_crs(srast))
pro <- rasterize(j, srast)
names(pro) = "jokulhaups"
writeRaster(pro, file.path(outputs, "jokulhaups.tif"), overwrite = TRUE)

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
tap <- aggregate(tap , fact=10, fun="mean")
writeRaster(tap, file.path(outputs, "TAP_intact_watershed.tif"), overwrite = T)

# big trees 
bt <- rast(file.path("inputs", "TAP_bigtrees_raw.tif"))
names(bt) = "tap_bigtrees"
bt<- aggregate(bt , fact=10, fun="mean")
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
con_rarec<- aggregate(con_rarec , fact=10, fun="mean")
names(con_rarec)= "ter_rarity"
writeRaster(con_rarec, file.path(outputs, "ter_rarity_c.tif"), overwrite = T)

# classed version
con_rareclass <- rast(file.path("outputs", "sk_rarity_conc.tif"))
con_rareclass <-aggregate(con_rareclass , fact=10, fun="mean")
names(con_rareclass)= "ter_rarity_class"
writeRaster(con_rareclass, file.path(outputs, "ter_rarity_class.tif"), overwrite = T)


# diversity - terrestrial 
div= rast(file.path("outputs", "sk_lf_rdc_diversity_101c.tif"))
names(div)= "ter_diversity"
div<- aggregate(div , fact=10, fun="mean")
writeRaster(div, file.path(outputs, "ter_diversity_c.tif"), overwrite = T)

# classed version 
divc <- rast(file.path("outputs", "sk_diversity_conc.tif"))
divc<- aggregate(divc , fact=10, fun="mean")
names(divc)= "ter_diversity_class"
writeRaster(divc, file.path(outputs, "ter_diversity_class.tif"), overwrite = T)



## Aquatic terrestrial barcodes - lakes 

# rarity - continuous lakes 
rar <- rast(file.path("outputs", "sk_lakes_meanbyarea_rarity_101c.tif"))
rar<- aggregate(rar , fact=10, fun="mean")
names(rar )= "aq_lakes_rarity_c"
writeRaster(rar, file.path(outputs, "aq_lakes_rarity_c.tif"), overwrite = T)

# rarity - classed lakes 

rc <- rast(file.path("outputs", "sk_lakes_raritybyarea_conc.tif"))
rc<- aggregate(rc , fact=10, fun="mean")
names(rc )= "aq_lakes_rarity_class"
writeRaster(rc, file.path(outputs, "aq_lakes_rarity_class.tif"), overwrite = T)


# diversity - continuous lakes
div <- rast(file.path("outputs", "sk_lakes_meanbyarea_div_101c.tif"))
div<- aggregate(div , fact=10, fun="mean")
names(div)= "aq_lakes_diversity_c"
writeRaster(div, file.path(outputs, "aq_lakes_diversity_c.tif"), overwrite = T)

# diversity - classed lakes
div <- rast(file.path("outputs", "sk_lakes_divarea_conc.tif"))
div <- aggregate(div , fact=10, fun="mean")
names(div)= "aq_lakes_diversity_class"
writeRaster(div, file.path(outputs, "aq_lakes_diversity_class.tif"), overwrite = T)

# 
# ## Aquatic terrestrial barcodes - Rivers
# # diversity - continuous
# div <- rast(file.path("outputs", "sk_rivers_diversity_101c.tif"))
# names(div)= "aq_rivers_diversity_c"
# writeRaster(div, file.path(outputs, "aq_rivers_diversity_c.tif"))
# # diversity - classed
# div <- rast(file.path("outputs", "sk_rivers_diversity_conc.tif"))
# names(div)= "aq_rivers_diversity_class"
# writeRaster(div, file.path(outputs, "aq_rivers_diversity_class.tif"))
# 
# # rarity - continuous
# rc <- rast(file.path("outputs", "sk_rivers_rarity_mean_101c.tif"))
# names(rc)= "aq_rivers_rarity_c"
# writeRaster(rc, file.path(outputs, "aq_rivers_rarity_c.tif"), overwrite = T)
# 
# # rarity - classed
# 
# rc <- rast( file.path("outputs", "sk_rivers_rarity_mean_conc.tif"))
# names(rc)= "aq_rivers_rarity_class"
# writeRaster(rc, file.path(outputs, "aq_rivers_rarity_class.tif"), overwrite = T)


# wetland density 
wet <-  rast(file.path("inputs", "sk_wetland_density_1km.tif"))
names(wet) = "aq_wetland_density"
writeRaster(wet, file.path(outputs, "aq_wetland_density.tif"))


# lake density

lak <- rast(file.path("inputs", "sk_lake_density_1km.tif"))
names(lak) = "aq_lake_density"
writeRaster(lak, file.path(outputs, "aq_lake_density.tif"))











## check the number of cells 
# ecoregion 

ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg")) |> 
  mutate(ecocode = seq_along(ECOREGION_NAME))

ecv <- vect(ec)

ecr <- rasterize(ecv, srast, field = "ecocode")

# count number of cells per ecoregion

ecr <- ecr |> 
  as.data.frame() |> 
  group_by(ecocode) |> 
  summarise(n = n())

ecdf <- ec %>% st_drop_geometry(ec)

cellno <- left_join(ecr, ecdf)
