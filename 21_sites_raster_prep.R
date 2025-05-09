#21_prep_sites_layers

library(dplyr)
library(terra)
library(sf)
library(raster)
library(dplyr)
# read in templates 

#outputs <- file.path("outputs", "final", "sites", "raw_tiffs")
outputs <- file.path("outputs", "final", "sites_202505", "raw_tiffs")

# create a raster with the same extent and resolution as the template
# see here for Terra option: https://github.com/rspatial/terra/issues/142

#srast <- rast(file.path("inputs", "sk_rast_template.tif"))
#srast <- aggregate(srast, fun = "any", na.rm = FALSE, fact = 10)

# # create a new raster will all values set to 1
# srast <- rast(nrows = nrow(srast), ncols = ncol(srast), xmin = ext(srast)[1], xmax = ext(srast)[2], ymin = ext(srast)[3], ymax = ext(srast)[4], crs = crs(srast))
# srast[] <- 1 # convert entire raster to 1
# srast <- raster(srast) # convert to raster
# 
# # overlay
# in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))
# insp <- as(in_aoi, "Spatial")
# 
# # convert the spatial poly to a raster based on cover
# SpP_ras <- raster::rasterize(insp , srast , getCover=TRUE)
# SpP_ras[SpP_ras==0] <- NA # convert 0  to NA
# 
# # convert back to terra object # this is the percent coverage layer for reference
# SpP_ras1 <- rast(SpP_ras)
# writeRaster(SpP_ras1, file.path(outputs, "template_cover.tif"), overwrite=TRUE)
# 
# # read in template_cover.tif
# srastc <- rast(file.path(outputs, "template_cover.tif"))
# 
# # generate binary based 1km template usign 0.5 as threshold
# srastc[srastc >= 0.5] <- 1
# srastc[srastc < 0.5] <- NA
# names(srastc) <- "template"
# writeRaster(srastc, file.path(outputs, "template_1km.tif"), overwrite=TRUE)
# srast <- srastc




srast <- rast(file.path(outputs, "template_1km.tif"))


#srast is the base template for all datset extents
#########################################################################



# 1) read in the wilderness layer / human footprint layer

w <- rast(file.path("outputs", "final", "sk_wilderness_2023.tif"))
names(w)<- "humanfootprint"
inw <- read_sf(file.path("outputs", "sk_wilderness.gpkg"))
wwwvc <- rasterize(inw, srast, "type",touches = TRUE, cover = TRUE)
wwwvc[is.na(wwwvc)] <- 0
wwwvc <- mask(wwwvc,srast)
writeRaster(wwwvc  , file.path(outputs, "wilderness_cover.tif"), overwrite=TRUE)


# generate binary based 1km template usign 0.5 as threshold
wwwvc[wwwvc >= 0.5] <- 1
wwwvc[wwwvc < 0.5] <- NA
names(wwwvc) <- "wilderness"
writeRaster(wwwvc  , file.path(outputs, "wilderness.tif"), overwrite=TRUE)

#############################################################################


# get the CEP dataset
dis <- st_read(file.path("inputs", "skeena_clip_ce_2023.gpkg" )) |>
  filter(CEF_HUMAN_DISTURB_FLAG =="Human Disturb Current 20yr")

# split out the urban areas
private <- st_read(file.path("inputs",  "sk_privateland_raw.gpkg")) |>
  filter(OWNER_TYPE == "Private") |>
  mutate(type = 1) |>
  dplyr::select(type)

urban <- dis |>
  filter(CEF_DISTURB_GROUP == "Urban") |>
  st_transform(crs = crs(srast)) |>
  mutate(type = 1) |>
  dplyr::select(type)

urban <- bind_rows(private, urban)
urbanr <- terra::rasterize(urban, srast,touches = TRUE, cover = TRUE)
writeRaster(urbanr, file.path(outputs, "urban_cover.tif"), overwrite = TRUE)

urbanr[urbanr >= 0.5] <- 1
urbanr[urbanr < 0.5] <- NA
#urbanr <- mask(urbanr,srast)
names(urbanr) <- "urban"
writeRaster(urbanr, file.path(outputs, "urban.tif"), overwrite = TRUE)
aa <- rast(file.path(outputs, "urban_cover.tif"))


## mining and OGC

mining <- dis |>
  filter(CEF_DISTURB_GROUP %in% c("OGC_Infrastructure", "Mining_and_Extraction")) |>
  st_transform(crs = crs(srast)) |>
  mutate(type = 1) |>
  dplyr::select(type)

mine <- terra::rasterize(mining, srast,touches = TRUE, cover = TRUE)
names(mine) <- "mining_oilgas"
writeRaster(mine, file.path(outputs, "mining_OG_cover.tif"), overwrite = TRUE)
aa <- rast(file.path(outputs, "mining_OG_cover.tif"))
mine <- mask(aa,srast)
mine[mine <= 0.1] <- 0
mine[is.na(mine)] <- 0
mine <- mask(mine,srast)
writeRaster(mine, file.path(outputs, "mining_og_cover.tif"), overwrite = TRUE)

mine[mine >= 0.5] <- 1
mine[mine < 0.5] <- NA
mine <- mask(mine,srast)
names(mine) <- "mining_oilgas"
writeRaster(mine, file.path(outputs, "mining_OG.tif"), overwrite = TRUE)




## roads and Rail and infrastructure
## this is already merged so cant distinguis road types from this layer need to re-exctract info

roads <- st_read(file.path("inputs", "skeena_clippoly_roads_2023_1000buf.gpkg" )) |>
  dplyr::select(DRA_ROAD_CLASS, DRA_ROAD_SURFACE) |>
  mutate(type = 1) |>
  dplyr::select(type)

rd<- terra::rasterize(roads , srast,touches = TRUE, cover = TRUE)
rd[is.na(rd)] <- 0
rd <- mask(rd,srast)
names(rd) <- "roads"
writeRaster(rd, file.path(outputs, "roads_cover.tif"), overwrite = TRUE)
rd[rd >= 0.5] <- 1
rd[rd < 0.5] <- NA
writeRaster(rd, file.path(outputs, "roads.tif"), overwrite = TRUE)



# ecoregions

ec <- st_read(file.path("inputs", "sk_ecoreg_reduced.gpkg")) |>
  mutate(ecocode = seq_along(ECOREGION_NAME))

ec <- terra::rasterize(ec , srast, "ECOREGION_NAME", touches = TRUE,na.rm = TRUE)
names(ec )<- "ecoregion_name"
ec <- mask(ec ,srast)
writeRaster(ec, file.path(outputs, "ecoregion.tif"), overwrite=TRUE)

# test with one ecoregion
aa <- rast(file.path(outputs, "ecoregion.tif"))
aa[aa == 8] <- 99
aa[aa != 99] <- 0
names(aa) <- "ecoregion_nass"
writeRaster(aa, file.path(outputs, "ecoregion_nass.tif"), overwrite=TRUE)





# productivity - complete

gdd <- rast(file.path("inputs", "gdd_sk.tif"))
names(gdd)<- "gdd"
gdd_v <- as.polygons(gdd)
gddr_cover <- terra::rasterize(gdd_v , srast, "gdd", fun = "mean", na.rm = TRUE)
names(gddr_cover)<- "gdd"
gddr_cover <- mask(gddr_cover ,srast)

writeRaster(gddr_cover, file.path(outputs, "gdd_mean.tif"), overwrite=TRUE)

# threshold above 800
gd <- rast(file.path(outputs, "gdd_mean.tif"))
gd[gd < 800] <- 0
gd[gd  >= 800] <- 1
names(gd) <- "gdd_800"
gd <- mask(gd ,srast)
writeRaster(gd, file.path(outputs, "gdd_mean_800.tif"), overwrite=TRUE)


# threahold above 1000
gd <- rast(file.path(outputs, "gdd_mean.tif"))
gd[gd < 1000] <- 0
gd[gd  >= 1000] <- 1
names(gd) <- "gdd_1000"
gd <- mask(gd ,srast)
writeRaster(gd, file.path(outputs, "gdd_mean_1000.tif"), overwrite=TRUE)



# ndvi - complete
ndvi <- rast(file.path("outputs", "ndvi_2019_2023_mean_101c.tif"))
names(ndvi)<- "ndvi"
ndviv <- as.polygons(ndvi, digits= 4)
ndvir <- terra::rasterize(ndviv  , srast, "ndvi", fun = "mean", na.rm = TRUE)
names(ndvir)<- "ndvi"
ndvir <- mask(ndvir ,srast)
writeRaster(ndvir, file.path(outputs, "ndvi_mean.tif"), overwrite=TRUE)
#range(values(ndvir), na.rm = TRUE)

# Themes = 2 layers. 
#1. 0.58 to 0.68. 
#2. >0.68

nd <- rast(file.path(outputs, "ndvi_mean.tif"))
nd[nd < 0.58] <- 0
nd[nd  >= 0.68] <- 0
nd[nd  >= 0.58] <- 1
names(nd) <- "ndvi_0.58_0.68"
nd <- mask(nd, srast)
writeRaster(nd, file.path(outputs, "ndvi_mean_0.58_0.68.tif"), overwrite=TRUE)


# threshold above 0.68
gd <- rast(file.path(outputs, "ndvi_mean.tif"))
gd[gd < 0.68] <- 0
gd[gd  >=0.68] <- 1
names(gd) <- "ndvi_0.68"
gd <- mask(gd ,srast)
writeRaster(gd, file.path(outputs, "ndvi_mean_0.68.tif"), overwrite=TRUE)


#npp - complete
npp <- rast(file.path("inputs", "npp_skeena_2019_2023.tif"))
names(npp)<- "npp"
npp <- project(npp,srast, method = "average")
nppv_cover<- mask(npp , srast)
writeRaster(nppv_cover, file.path(outputs, "npp.tif"), overwrite = TRUE)
npp <- rast(file.path(outputs, "npp.tif"))
names(npp)<- "npp_productivity"
writeRaster(npp, file.path(outputs, "npp.tif"), overwrite = TRUE)





# resistence / connectivity - complete
res <- rast(file.path("inputs", "pither_resistance.tif"))
names(res) = "resistance"
res <- as.polygons(res, digits = 4)
res <- terra::rasterize(res , srast, "resistance", fun = "mean", na.rm = TRUE)
res <- mask(res,srast)
writeRaster(res, file.path(outputs, "res_mean.tif"), overwrite=TRUE)
range(values(res), na.rm = TRUE)

# export the 40th percentile and the 90th percentile (based on calcualtions in 08_connectivity script)

res <- rast(file.path(outputs, "res_mean.tif"))
hist(nres)
aa <- sort(values(res, na.rm = T))
quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE)

median(aa)
mean(aa)

aq <- as.data.frame(quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE))
names(aq) = "value"

# set threshold of above 90% 
# above 90% - 2.977584e+00

m <- c(-10, 2.977584e+00, 0,
       2.977584e+00, 999999, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
common <- classify(res, rclmat, include.lowest=TRUE)

unique(values(common))
rc1 <- mask(common, srast )
terra::writeRaster(rc1,file.path(outputs, "sk_pither_resistence_90threshold.tif"), overwrite = TRUE)

aa <- rast(file.path(outputs, "sk_pither_resistence_90threshold.tif"))
aa
names(aa) <- "resistence_90th"
writeRaster(aa, file.path(outputs, "sk_pither_resistence_90threshold1.tif"), overwrite=TRUE)



# or 40% 
res <- rast(file.path(outputs, "res_mean.tif"))
m <- c(-10, 1.037420e+00, 0,
       1.037420e+00, 999999, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
common <- classify(res, rclmat, include.lowest=TRUE)

unique(values(common))
rc1 <- mask(common, srast )
terra::writeRaster(rc1,file.path(outputs, "sk_pither_resistence_40threshold.tif"), overwrite = TRUE)

aa <- rast(file.path(outputs, "sk_pither_resistence_40threshold.tif"))
names(aa) <- "resistence_40th"
writeRaster(aa, file.path(outputs, "sk_pither_resistence_40threshold1.tif"), overwrite=TRUE)























# microrefugia - complete in 08_connectivity file

# microrefugia - probability so converting to binary and selecting threshold 
#mic <- rast(file.path("inputs", "microrefugia.tif"))
#names(mic) = "microrefugia"


# # conver to binary (based on 0.5)
# m <- c(0, .7, NA,
#        .7, 999999, 1)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# common <- classify(mic, rclmat, include.lowest=TRUE)
# convert to poly and back to raster at 1km grid - using cover
# 
# common <- as.polygons(common, digits = 2)
# mic <- terra::rasterize(common, srast, "microrefugia", touches = TRUE, cover = TRUE)
# 
# # threshold at 0.5 and convert to binary
# names(mic)<- "microrefugia"
# mic <- mask(mic ,srast)
# writeRaster(mic, file.path(outputs, "microrefugia_0.7_cover.tif"), overwrite = TRUE)
# mic[mic >= 0.5] <- 1
# mic[mic < 0.5] <- NA
# mic <- mask(mic ,srast)
# writeRaster(mic, file.path(outputs, "microrefugia_0.7.tif"), overwrite = TRUE)
# 

# macrorefugia - complete
# 
# # macrorefugia - probability so converting to binary and selecting threshold 
# mic <- rast(file.path("inputs", "2080s_macrorefugia.tif"))
# names(mic)<- "macrorefugia"
# 
# # convert to binary (based on 0.5)
# m <- c(0, .7, NA,
#        .7, 999999, 1)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# common <- classify(mic, rclmat, include.lowest=TRUE)
# 
# # convert to poly and back to raster at 1km grid - using cover
# common <- as.polygons(common, digits = 2)
# mic <- terra::rasterize(common, srast, "macrorefugia", touches = TRUE, cover = TRUE)
# 
# # threshold at 0.5 and convert to binary
# names(mic)<- "macrorefugia"
# mic <- mask(mic ,srast)
# writeRaster(mic, file.path(outputs, "macrorefugia_0.7_cover.tif"), overwrite = TRUE)
# mic[mic >= 0.5] <- 1
# mic[mic < 0.5] <- NA
# mic <- mask(mic ,srast)
# writeRaster(mic, file.path(outputs, "macrorefugia_0.7.tif"), overwrite = TRUE)


# #protected _lands = completed

pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pro <- rasterize(pro, srast,touches = TRUE, cover = TRUE)
names(pro) = "protected_lands"
pro[is.na(pro)] <- 0
pro <- mask(pro,srast)
writeRaster(pro, file.path(outputs, "protected_lands_cover.tif"), overwrite = TRUE)

pro <- rast(file.path(outputs, "protected_lands_cover.tif"))
# generate binary based 1km template usign 0.5 as threshold
pro[pro >= 0.2] <- 1
pro[pro < 0.2] <- 0
names(pro) <- "protected"
pro <- mask(pro,srast)
writeRaster(pro , file.path(outputs, "protected_lands_0.2.tif"), overwrite=TRUE)


# cancelled lands 

canc <- st_read(file.path("inputs", "cancelled_lands_final.gpkg"))
cancc <- canc %>% dplyr::select(cancelled_status) |> 
  filter(cancelled_status == "cancelled") 
pro <- rasterize(cancc , srast,touches = TRUE, cover = TRUE)
names(pro) = "cancelled_lands"
pro[is.na(pro)] <- 0
pro <- mask(pro,srast)
writeRaster(pro, file.path(outputs, "cancelled_lands_cover.tif"), overwrite = TRUE)
pro[pro >= 0.2] <- 1
pro[pro < 0.2] <- 0
pro <- mask(pro,srast)
names(pro) <- "cancelled_lands_historic"
writeRaster(pro  , file.path(outputs, "cancelled_lands_0.2.tif"), overwrite=TRUE)


cancnc <- canc %>% dplyr::select(cancelled_status) |> 
  filter(cancelled_status == "not_cancelled") 

pro <- rasterize(cancnc, srast,touches = TRUE, cover = TRUE)
names(pro) = "not_cancelled_lands"
pro[is.na(pro)] <- 0
pro <- mask(pro,srast)
writeRaster(pro, file.path(outputs, "not_cancelled_lands_cover.tif"), overwrite = TRUE)
pro[pro >= 0.2] <- 1
pro[pro < 0.2] <- 0
names(pro) <- "cancelled_lands_current"
writeRaster(pro  , file.path(outputs, "not_cancelled_lands_0.2.tif"), overwrite=TRUE)



## Species - these have been grouped and updateded within the 10_species_analysis.R script. 

# fed listed species
rb <- st_read(file.path("inputs", "fed_listed_sp_raw.gpkg"))

sp <- unique(rb$SCIENTIFIC_NAME)

purrr::map(sp, function(x){
  print(x)
 # x <- sp[1]
  rbb <- rb |> filter(SCIENTIFIC_NAME == x)
  comm_name <- gsub(" ", "_", unique(rbb$COMMON_NAME_ENGLISH))
  sciname <- gsub(" ", "_", unique(rbb$SCIENTIFIC_NAME))
  rbb <- rasterize(rbb, srast, cover = TRUE, touches = TRUE)
  names(rbb) = paste0("fed_listed_", comm_name )
  #rbb[rbb >= 0.5] <- 1
  #rbb[rbb < 0.5] <- NA
  rbb <- mask(rbb,srast)
  writeRaster(rbb, file.path(outputs, paste0("fed_listed_", sciname, "_cover.tif")), overwrite = TRUE)
})




# KBAs & IBAs - complete
#kbam <- st_read(file.path("outputs", "final", "KBA_marine.gpkg"))
#pro <- rasterize(kbam, srast)
#names(pro) = "kba_marine"
#writeRaster(pro, file.path(outputs, "kba_marine.tif"))

kbat <- st_read(file.path("outputs", "final", "KBA_terrestrial.gpkg"))
kbat <- st_transform(kbat, crs=st_crs(srast))
pro <- rasterize(vect(kbat), srast, touches = TRUE, cover = TRUE)
names(pro) = "ter_kba"
writeRaster(pro, file.path(outputs, "kba_terrestrial.tif"), overwrite = TRUE)
# not included as so small and the highest value is 0.33


# complete
j <- st_read(file.path("outputs", "final", "Jokulhaups_sk.gpkg"))
j <- st_transform(j, crs=st_crs(srast))
pro <- rasterize(j, srast, touches = TRUE, cover = TRUE)
names(pro) = "jokulhaups"
writeRaster(pro, file.path(outputs, "jokulhaups.tif"), overwrite = TRUE)

#IBA 
ib <- st_read(file.path("outputs", "final", "sk_important_bird_areas.gpkg"))
ib <- st_transform(ib, crs=st_crs(srast))
ib <- rasterize(ib, srast,touches = TRUE, cover = TRUE)
names(ib) = "iba"
ib <- mask(ib ,srast)
writeRaster(ib, file.path(outputs, "iba_cover.tif"), overwrite = TRUE)


ib[ib >= 0.5] <- 1
ib[ib < 0.5] <- NA
ib <- mask(ib ,srast)
writeRaster(ib, file.path(outputs, "iba.tif"), overwrite = TRUE)


# # TAP Old growth - see the download as need to go back to the raw data



## Landscape terrestrial barcodes 

#sk_barcode? 


# rarity
# test with the raw barcode version (classed into 1-5 classes). This is pre neigthbourhood analysis
# might want to use this later but not using currently 
#raw_rare <- rast(file.path("outputs", "sk_rarity_class_rcd.tif"), overwrite = TRUE)



# continuous measure of rarity this is post neighbourhood analysis 
con_rarec <- rast(file.path("outputs","sk_lf_rdc_rarity_101c.tif"))
names(con_rarec)= "ter_rarity"
sort(unique(values(con_rarec)))
hist(con_rarec) # values are continous betwee 1-5) 
con_rarec <- as.polygons(con_rarec, digits = 2)
con_rarecm <- terra::rasterize(con_rarec, srast, "ter_rarity", fun = "mean", na.rm = TRUE)
names(con_rarecm) <- "ter_rarity"
con_rarecm <- mask(con_rarecm, srast)
writeRaster(con_rarecm, file.path(outputs, "ter_rarity_continuous.tif"), overwrite=TRUE)


# old version 
#con_rarecm <- terra::rasterize(con_rarec, srast, "ter_rarity", fun = "mean", na.rm = TRUE)
#names(con_rarecm)= "ter_rarity_mean"
#con_rarecm <- mask(con_rarecm, srast)
#writeRaster(con_rarecm, file.path(outputs, "ter_rarity_c_mean.tif"), overwrite = T)
###option 2
#con_rarech <- terra::rasterize(con_rarec, srast, "ter_rarity", fun = "max", na.rm = TRUE)
#names(con_rarech)= "ter_rarity_max"
#con_rarech <- mask(con_rarech, srast)
#writeRaster(con_rarech, file.path(outputs, "ter_rarity_c_max.tif"), overwrite = T)





# updated to new 1km raster 

# classed version
con_rareclass <- rast(file.path("outputs", "sk_rarity_conc.tif"))
con_rareclass <- as.polygons(con_rareclass, digits = 2)
con_rareclass <- st_as_sf(con_rareclass)

#class5 
c5 <- con_rareclass |> 
  filter(rarity == 5) 

c4 <- con_rareclass |> 
  filter(rarity == 4) 
  
cc5 <- rasterize(c5 , srast,touches = TRUE, cover = TRUE)
cc4 <- rasterize(c4 , srast,touches = TRUE, cover = TRUE)

names(cc5) <- "rarity_5"
cc5 <- mask(cc5, srast)
writeRaster(cc5, file.path(outputs, "ter_rarity_5_cover.tif"), overwrite=TRUE)

cc5[cc5>= 0.5] <- 1
cc5[cc5< 0.5] <- NA
names(cc5) <- "rarity_5"
cc5 <- mask(cc5, srast)
writeRaster(cc5, file.path(outputs, "ter_rarity_5.tif"), overwrite=TRUE)

names(cc4) <- "rarity_4"
cc4 <- mask(cc4, srast)
writeRaster(cc4, file.path(outputs, "ter_rarity_4_cover.tif"), overwrite=TRUE)


cc4[cc4>= 0.5] <- 1
cc4[cc4< 0.5] <- NA
names(cc4) <- "rarity_4"
cc4 <- mask(cc4, srast)
writeRaster(cc4, file.path(outputs, "ter_rarity_4.tif"), overwrite=TRUE)


## old version 
#con_rareclass <-aggregate(con_rareclass , fact=10, fun="max")
#names(con_rareclass)= "ter_highveryhigh_rarity"
#con_rareclass[is.na(con_rareclass)] <- 0 
#
## select only class 4 and 5 and convert to binary layer 
#m <- c(0, 3, 0, # lowest diversity 
#       3, 5, 1) # highest diversity 
#rclmat <- matrix(m, ncol=3, byrow=TRUE)

#rar_vh <- classify(con_rareclass, rclmat, include.lowest=TRUE)
#names(rar_vh) = "ter_rare_hig_vhigh"
#writeRaster(rar_vh, file.path(outputs, "ter_rarity45_class.tif"), overwrite = T)


# diversity - terrestrial 
div= rast(file.path("outputs", "sk_lf_rdc_diversity_101c.tif"))
names(div)= "ter_diversity"
div <- as.polygons(div, digits = 2)
# option 1
divc<- terra::rasterize(div, srast, "ter_diversity", fun = "mean", na.rm = TRUE)
writeRaster(divc, file.path(outputs, "ter_diversity_c.tif"), overwrite = T)






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

#Split out the class 4 and 5  -still need to update code )()
divc <- rast(file.path("outputs", "sk_diversity_conc.tif"))
names(divc)= "ter_diversity_class"
divc <- as.polygons(divc, digits = 2)
divc <- st_as_sf(divc)

#class5 
c5 <- divc |> 
  filter(ter_diversity_class  == 5) 

c4 <- divc |> 
  filter(ter_diversity_class  == 4) 

cc5 <- rasterize(c5 , srast,touches = TRUE, cover = TRUE)
cc4 <- rasterize(c4 , srast,touches = TRUE, cover = TRUE)

names(cc5) <- "diversity_5"
cc5 <- mask(cc5, srast)
writeRaster(cc5, file.path(outputs, "ter_diversity_5_cover.tif"), overwrite=TRUE)

cc5[cc5>= 0.5] <- 1
cc5[cc5< 0.5] <- NA
names(cc5) <- "diversity_5"
cc5 <- mask(cc5, srast)
writeRaster(cc5, file.path(outputs, "ter_diversity_5.tif"), overwrite=TRUE)


names(cc4) <- "diversity_4"
cc4 <- mask(cc4, srast)
writeRaster(cc4, file.path(outputs, "ter_diversity_4_cover.tif"), overwrite=TRUE)

cc4[cc4>= 0.5] <- 1
cc4[cc4< 0.5] <- NA
names(cc4) <- "diversity_4"
cc4 <- mask(cc4, srast)
writeRaster(cc4, file.path(outputs, "ter_diversity_4.tif"), overwrite=TRUE)








## Aquatic terrestrial barcodes - lakes 
# 
# # rarity - continuous lakes 
# rar <- rast(file.path("outputs", "sk_lakes_meanbyarea_rarity_101c.tif"))
# rar<- aggregate(rar , fact=10, fun="mean")
# names(rar )= "aq_lakes_rarity_c"
# writeRaster(rar, file.path(outputs, "aq_lakes_rarity_c.tif"), overwrite = T)
# 
# # rarity - classed lakes 
# 
# rc <- rast(file.path("outputs", "sk_lakes_raritybyarea_conc.tif"))
# rc<- aggregate(rc , fact=10, fun="mean")
# names(rc )= "aq_lakes_rarity_class"
# writeRaster(rc, file.path(outputs, "aq_lakes_rarity_class.tif"), overwrite = T)
# 
# 
# # diversity - continuous lakes
# div <- rast(file.path("outputs", "sk_lakes_meanbyarea_div_101c.tif"))
# div<- aggregate(div , fact=10, fun="mean")
# names(div)= "aq_lakes_diversity_c"
# writeRaster(div, file.path(outputs, "aq_lakes_diversity_c.tif"), overwrite = T)
# 
# # diversity - classed lakes
# div <- rast(file.path("outputs", "sk_lakes_divarea_conc.tif"))
# div <- aggregate(div , fact=10, fun="mean")
# names(div)= "aq_lakes_diversity_class"
# writeRaster(div, file.path(outputs, "aq_lakes_diversity_class.tif"), overwrite = T)

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
wet <-  rast(file.path("inputs", "sk_wetland_density.tif"))
names(wet)<- "wetden"
wet <- as.polygons(wet, digits = 2)

wet <- terra::rasterize(wet, srast, "wetden", fun = "mean", na.rm = TRUE)
names(wet) = "aq_wetland_density"
wet <- mask(wet, srast)
writeRaster(wet, file.path(outputs, "aq_wetland_density_mean.tif"), overwrite=TRUE)

wet <- rast(file.path(outputs, "aq_wetland_density_mean.tif"))
wet [is.na(wet )] <- 0
wet <- mask(wet, srast)
writeRaster(wet, file.path(outputs, "aq_wetland_density_mean.tif"), overwrite = T)




# lake density

lak <- rast(file.path("inputs", "sk_lake_density.tif"))
names(lak)<- "lakeden"
lak <- as.polygons(lak, digits = 2)

lak <- terra::rasterize(lak, srast, "lakeden", fun = "mean", na.rm = TRUE)
names(lak) = "aq_lake_density"
lak <- mask(lak, srast)
writeRaster(lak, file.path(outputs, "aq_lake_density_mean.tif"), overwrite = T)

lak <- rast(file.path(outputs, "aq_lake_density_mean.tif"))
lak [is.na(lak )] <- 0
lak <- mask(lak, srast)
writeRaster(lak, file.path(outputs, "aq_lake_density_mean.tif"), overwrite = T)




# 
# ## check the number of cells 
# # ecoregion 
# 
# ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg")) |> 
#   mutate(ecocode = seq_along(ECOREGION_NAME))
# # 
# ecv <- vect(ec)
# 
# ecr <- rasterize(ecv, srast, field = "ecocode")
# 
# # count number of cells per ecoregion
# 
# ecr <- ecr |> 
#   as.data.frame() |> 
#   group_by(ecocode) |> 
#   summarise(n = n())
# 
# ecdf <- ec %>% st_drop_geometry(ec)
# 
# cellno <- left_join(ecr, ecdf)
# 

## 
#
#Shortcut to fix the names of the layers for final outputs


outdir <- file.path("outputs","final","sites_202505","Regional","Includes")
outdir <- file.path("outputs","final","sites_202505","Regional","weights")
ffs <- list.files(outdir) 

aa <- rast(file.path(outdir,ffs [1])) 
aa <- rast(file.path(outdir,ffs [5])) 
names(aa) <- "protected"
writeRaster(aa, file.path(outdir, "protected_lands_0.21.tif"), overwrite=TRUE)


aa <- rast(file.path(outdir,ffs [6])) 
names(aa) <- "macrorefugia_c"
writeRaster(aa, file.path(outdir, "macrorefugia_w.tif"), overwrite=TRUE)
aa <- rast(file.path(outdir,ffs [9]))
names(aa) <- "ndvi_w"
writeRaster(aa, file.path(outdir, "ndvi_w.tif"), overwrite=TRUE)
aa <- rast(file.path(outdir,ffs [10]))
names(aa) <- "npp_w"
writeRaster(aa, file.path(outdir, "npp_w.tif"), overwrite=TRUE)

aa <- rast(file.path(outdir,ffs [11]))
names(aa) <- "resistance_w"
writeRaster(aa, file.path(outdir, "resistance_w.tif"), overwrite=TRUE)
aa <- rast(file.path(outdir,ffs [12]))



