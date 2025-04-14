#21_prep_sites_layers

library(dplyr)
library(terra)
library(sf)
library(raster)

# read in templates 
srast <- rast(file.path("inputs", "sk_rast_template.tif"))
outputs <- file.path("outputs", "final", "sites", "raw_tiffs")
# create a raster with the same extent and resolution as the template
# see here for Terra option: https://github.com/rspatial/terra/issues/142

srast <- rast(file.path("inputs", "sk_rast_template.tif"))
srast <- aggregate(srast, fun = "any", na.rm = FALSE, fact = 10)

# create a new raster will all values set to 1
srast <- rast(nrows = nrow(srast), ncols = ncol(srast), xmin = ext(srast)[1], xmax = ext(srast)[2], ymin = ext(srast)[3], ymax = ext(srast)[4], crs = crs(srast))
srast[] <- 1 # convert entire raster to 1
srast <- raster(srast) # convert to raster 

# overlay 
in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))
insp <- as(in_aoi, "Spatial")

# convert the spatial poly to a raster based on cover 
SpP_ras <- raster::rasterize(insp , srast , getCover=TRUE)
SpP_ras[SpP_ras==0] <- NA # convert 0  to NA

# convert back to terra object # this is the percent coverage layer for reference 
SpP_ras1 <- rast(SpP_ras)
writeRaster(SpP_ras1, file.path(outputs, "template_cover.tif"), overwrite=TRUE)

# generate binary based 1km template. 
SpP_ras1[SpP_ras1>0] <- 1
names(SpP_ras1) <- "template"
writeRaster(SpP_ras1, file.path(outputs, "template_1km.tif"), overwrite=TRUE)
srast <- SpP_ras1

#in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))
#outputs <- file.path("outputs", "final", "sites", "raw_tiffs")

#dim(srast)
#ncell(srast)

# read in the wilderness layer / human footprint layer

w <- rast(file.path("outputs", "final", "sk_wilderness_2023.tif"))
names(w)<- "humanfootprint"
w  <- aggregate(w , fact=10, fun="max")
w[is.na(w)] <- 0 
w <- mask(w,srast)
writeRaster(w, file.path(outputs, "humanfootprint1.tif"), overwrite=TRUE)

# second version of the human footprint layer
# w <- rast(file.path("outputs", "final", "sk_wilderness_2023.tif"))
# names(w)<- "humanfootprint"
# wv <- as.polygons(w)
# 
# wwv <- rasterize(wv, srast , "humanfootprint")
# writeRaster(wwv , file.path(outputs, "humanfootprint2.tif"), overwrite=TRUE)
inw <- read_sf(file.path("outputs", "sk_wilderness.gpkg"))
wwwv <- rasterize(inw, srast , "type")
wwwvc <- rasterize(inw, srast, "type",touches = TRUE, cover = TRUE)

writeRaster(wwwv  , file.path(outputs, "humanfootprint3_base.tif"), overwrite=TRUE)
writeRaster(wwwvc  , file.path(outputs, "humanfootprint4_touches.tif"), overwrite=TRUE)







w  <- aggregate(w , fact=10, fun="max")
w[is.na(w)] <- 0 
w <- mask(w,srast)
writeRaster(w, file.path(outputs, "humanfootprint1.tif"), overwrite=TRUE)







# get the CEP dataset 
dis <- st_read(file.path("inputs", "skeena_clip_ce_2023.gpkg" )) |> 
  filter(CEF_HUMAN_DISTURB_FLAG =="Human Disturb Current 20yr") 

# split out the urban areas 
private <- st_read(file.path("inputs",  "sk_privateland_raw.gpkg")) |> 
  filter(OWNER_TYPE == "Private") |> 
  mutate(type = 1) |> 
  select(type)

urban <- dis |> 
  filter(CEF_DISTURB_GROUP == "Urban") |> 
  st_transform(crs = crs(srast)) |> 
  mutate(type = 1) |> 
  select(type)

urban <- bind_rows(private, urban)
urbanr <- terra::rasterize(urban, srast)
urbanr[is.na(urbanr)] <- 0 
urbanr <- mask(urbanr,srast)
names(urbanr) <- "urban"
writeRaster(urbanr, file.path(outputs, "urban.tif"), overwrite = TRUE)


## mining and OGC 

mining <- dis |> 
  filter(CEF_DISTURB_GROUP %in% c("OGC_Infrastructure", "Mining_and_Extraction")) |> 
  st_transform(crs = crs(srast)) |> 
  mutate(type = 1) |> 
  select(type)
mine <- terra::rasterize(mining, srast)
mine[is.na(mine)] <- 0 
mine <- mask(mine,srast)
names(mine) <- "mining_oilgas"
writeRaster(mine, file.path(outputs, "mining_OG.tif"), overwrite = TRUE)


## roads and Rail and infrastructure 
## this is already merged so cant distinguis road types from this layer need to re-exctract info

roads <- st_read(file.path("inputs", "skeena_clippoly_roads_2023_1000buf.gpkg" )) |> 
  select(DRA_ROAD_CLASS, DRA_ROAD_SURFACE) |> 
  mutate(type = 1) |> 
  select(type)

rd<- terra::rasterize(roads , srast)
rd[is.na(rd)] <- 0 
rd <- mask(rd,srast)
names(rd) <- "roads"
writeRaster(rd, file.path(outputs, "roads.tif"), overwrite = TRUE)


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
mac <- aggregate(mac , fact=10, fun="mean")
writeRaster(mac, file.path(outputs, "macrorefugia.tif"), overwrite = TRUE)

#protected _lands 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pro <- rasterize(vect(pro), srast)
names(pro) = "protected_lands"
pro[is.na(pro)] <- 0 
pro <- mask(pro,srast)
writeRaster(pro, file.path(outputs, "protected_lands.tif"), overwrite = TRUE)









# break this into species Presence/absence

# red and blue sp and federal listed species. 
rb <- st_read(file.path("inputs", "bc_red_blue_sp_raw.gpkg"))

#unique(rb$SCI_NAME)
#unique(rb$EL_TYPE)

sp <- unique(rb$SCI_NAME)

sp[62]

xx <- purrr::map(sp, function(x){
  #x <- sp[63]
  print(x)
  
  #x <- sp[63]
  rbb <- rb |> filter(SCI_NAME == x)
  comm_name <- gsub(" ", "_", unique(rbb$ENG_NAME))
  comm_name <- gsub("/", "", comm_name)
  comm_name <- gsub("-", "", comm_name)
  
  
  sciname <- gsub(" ", "_", unique(rbb$SCI_NAME))
  sciname <- gsub("/", "", sciname)
  sciname <- gsub("-", "", sciname)
  
  rbb <- rasterize(rbb, srast)
  
  if(all(values(is.na(rbb)))){
    cli::cli_alert("All values are NA, skipping sp")
  } else {
    
  rbb[is.na(rbb)] <- 0 
  names(rbb) = paste0("bc_listed_", comm_name )
  rbb <- mask(rbb,srast)
  print(length(unique(values(rbb))))
  writeRaster(rbb, file.path(outputs, "species", paste0("bc_listed_", sciname, ".tif")), overwrite = TRUE)
  }
})


# check that there are only 2 values in the raster
"Ptychoramphus aleuticus"
"Arctopoa eminens"
"Melanitta perspicillata"















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
  rbb[is.na(rbb)] <- 0 
  rbb <- mask(rbb,srast)
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
ib[is.na(ib)] <- 0 
ib <- mask(ib,srast)

writeRaster(ib, file.path(outputs, "iba.tif"), overwrite = TRUE)


#stack ib + srast
#xx <- stack(ib, srast)






# TAP Old growth 
## intact watersheds - From TAP old growth 
#https://catalogue.data.gov.bc.ca/dataset/b684bdbf-8824-4bc6-8d22-329d9b97c043

tap <- rast( file.path("inputs", "TAP_intact_watershed.tif"))
names(tap) = "tap_intact_watershed"
tap <- aggregate(tap , fact=10, fun="mean")
tap <- project(tap, srast)
writeRaster(tap, file.path(outputs, "TAP_intact_watershed.tif"), overwrite = T)

# big trees 
bt <- rast(file.path("inputs", "TAP_bigtrees_raw.tif"))
names(bt) = "tap_bigtrees"
bt <- aggregate(bt , fact=10, fun="mean")
#bt <- project(bt, srast)
crs(bt) <- crs(srast)
writeRaster(bt, file.path(outputs, "TAP_bigtrees.tif"), overwrite = T)

#bt + srast


# ancient forests
bt <- rast(file.path("inputs", "TAP_ancient_forest_raw.tif"))
names(bt) = "tap_ancient_forest"
bt <- aggregate(bt , fact=10, fun="mean")
#bt <- project(bt, srast)
crs(bt) <- crs(srast)
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
con_rareclass <-aggregate(con_rareclass , fact=10, fun="max")
names(con_rareclass)= "ter_highveryhigh_rarity"
con_rareclass[is.na(con_rareclass)] <- 0 

# select only class 4 and 5 and convert to binary layer 
m <- c(0, 3, 0, # lowest diversity 
       3, 5, 1) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)

rar_vh <- classify(con_rareclass, rclmat, include.lowest=TRUE)
names(rar_vh) = "ter_rare_hig_vhigh"
writeRaster(rar_vh, file.path(outputs, "ter_rarity45_class.tif"), overwrite = T)



# diversity - terrestrial 
div= rast(file.path("outputs", "sk_lf_rdc_diversity_101c.tif"))
names(div)= "ter_diversity"
div<- aggregate(div , fact=10, fun="mean")
writeRaster(div, file.path(outputs, "ter_diversity_c.tif"), overwrite = T)

# classed version 
divc <- rast(file.path("outputs", "sk_diversity_conc.tif"))
divc<- aggregate(divc , fact=10, fun="mean")
#names(divc)= "ter_diversity_class"
#writeRaster(divc, file.path(outputs, "ter_diversity_class.tif"), overwrite = T)


# select only class 4 and 5 and convert to binary layer 
m <- c(0, 3, 0, # lowest diversity 
       3, 5, 1) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)

div_vh <- classify(divc, rclmat, include.lowest=TRUE)

#plot(div_vh)
#plot(divc)
names(div_vh) = "ter_div_high_vhigh"
div_vh[is.na(div_vh)] <- 0 
div_vh <- mask(div_vh,srast)
writeRaster(div_vh, file.path(outputs, "ter_div45_class.tif"), overwrite = T)



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

writeRaster(wet, file.path(outputs, "aq_wetland_density.tif"), overwrite = T)


# lake density

lak <- rast(file.path("inputs", "sk_lake_density_1km.tif"))
names(lak) = "aq_lake_density"
writeRaster(lak, file.path(outputs, "aq_lake_density.tif"), overwrite = T)











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
