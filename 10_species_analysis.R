# consolidate the species data
# review presence loactions with the barcodes (terresttial, lakes and rivers)
# generate 100m x 100m grid for analysis 

## Set up 
library(terra)
library(sf)
library(readr)
library(tidyr)
library(dplyr)

# for 100x 100 measure
#srast <- rast(file.path("inputs", "sk_rast_template.tif"))

# for 1000m x 1000m measure
outputs <- file.path("outputs", "final", "sites_202505", "raw_tiffs")
srast <- rast(file.path(outputs, "template_1km.tif"))

in_aoi <- st_read(file.path("inputs", "sk_poly_template.gpkg"))

# convert terra landbarcode to points 
ter <- rast(file.path("inputs", "sk_lf_barcode.tif"))
#terpt <- as.points(ter)



# 1) SAR species - see the 21_sites_raster_prep script


# 2) BC species - cdc dataset

wcdc <- st_read(file.path("inputs", "bc_cbc_sp_raw.gpkg")) |>  
  dplyr::select(ENG_NAME, SCI_NAME, EL_TYPE,BC_LIST) |> 
  rename("SPECIES_ENGLISH_NAME" = ENG_NAME,
         "SCIENTIFIC_NAME" = SCI_NAME,
         "NAME_TYPE_SUB" = EL_TYPE,
         "BCLIST" = BC_LIST)


# Groups - vertebrate Animals 

#sort(unique(wcdc$SPECIES_ENGLISH_NAME))


# 1) blue listed birds 

bb <- c("American Bittern", 
        "Great Blue Heron, fannini subspecies",
        "Short-eared Owl" ,
        "Surf Scoter",
        "Tufted Puffin") 


bb <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% bb) |> 
  dplyr::mutate(area = as.numeric(st_area(geom))) |> 
  dplyr::filter(area < 1667611)
#st_write(bb1, file.path("outputs", "bc_blue_bird.gpkg"), append = FALSE)

bb1 <- rasterize(bb, srast, cover = TRUE, touches = TRUE)
#bb2 <- rasterize(bb, srast, touches = TRUE)
names(bb1) = "Blue-listed birds"
#names(bb2) = "bluelisted_birds"
bb1[is.na(bb1)] <- 0

bb1<- mask(bb1,srast)
#bb2<- mask(bb2,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_bluelisted_birds_cover.tif"), overwrite = TRUE)
#writeRaster(bb2, file.path(outputs, "bc_cdc_bluelisted_birds.tif"), overwrite = TRUE)



# 2) blues listed mammals (cdc). these are mostly small polygons buffered so dropped to one pt
# conterved to a singel polygons, then centroid, then buffer to 1km (500m)

bm <- c("Collared Pika",
        "Meadow Jumping Mouse, alascensis subspecies",
        "Steller Sea Lion" )

bm <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% bm) |> 
  sf::st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom))) |>
  st_centroid() |> 
  st_buffer(dist = 500) 

#st_write(bm, file.path("outputs", "bc_blue_mammals.gpkg"), append = FALSE)

bb1 <- rasterize(bm, srast, cover = TRUE, touches = TRUE)
bb2 <- rasterize(bb, srast, touches = TRUE)
names(bb1) = names(bb2) = "Blue-listed mammals"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
bb2<- mask(bb2,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_bluelisted_mammals_cover.tif"), overwrite = TRUE)
writeRaster(bb2, file.path(outputs, "bc_cdc_bluelisted_mammals.tif"), overwrite = TRUE)




# 3) blue listed fish (cdc)  - DID NOT REDUCE TO CENTROID AS WATERBUDY 
# filtered by 0.5% cover threshold 

bf <- "Least Cisco" 

bf <- wcdc %>% filter(SPECIES_ENGLISH_NAME == bf)
#st_write(bf, file.path("outputs", "bc_blue_fish.gpkg"), append = FALSE)

bb1 <- rasterize(bf, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Blue-listed fish"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_bluelisted_fish_cover.tif"), overwrite = TRUE)


bb1[bb1 >= 0.5] <- 1
bb1[bb1 < 0.5] <- NA
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_bluelisted_fish.tif"), overwrite = TRUE)



# 4) red listed fisfh (cdc) - DID NOT REDUCE TO CENTROID AS WATERBUDY
# filtered by 0.5% cover threshold 


rf <- c("Broad Whitefish",
        "Lake Chub - Atlin Warm Springs Populations" )

rf <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% rf) 
#st_write(rf, file.path("outputs", "bc_red_fish.gpkg"), append = FALSE)
bb1 <- rasterize(rf, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Red-listed fish"
bb1[is.na(bb1)] <- 0
#bb1[bb1 >= 0.5] <- 1
#bb1[bb1 < 0.5] <- NA
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_redlisted_fish_cover.tif"), overwrite = TRUE)



# 5) red listed birds (cdc) -keep cover amount 
  
rb <- c("Cassin's Auklet",
       # "Hudsonian Godwit",
        "Parasitic Jaeger",
        "Short-billed Dowitcher")
  
rb <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% rb) 

rbs <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Hudsonian Godwit") |>
  st_centroid() |> 
  st_buffer(dist = 500) 

rb <- rbind(rb, rbs)
#st_write(rb, file.path("outputs", "bc_cdc_red_birds1.gpkg"), append = FALSE)

bb1 <- rasterize(rb, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Red-listed birds"
bb1[is.na(bb1)] <- 0
#bb1[bb1 >= 0.5] <- 1
#bb1[bb1 < 0.5] <- NA

bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_redlisted_birds.tif"), overwrite = TRUE)


# 6) red listed mammals (cdc)
rm <- "Tundra Shrew" 

rm <- wcdc %>% filter(SPECIES_ENGLISH_NAME == rm) |> 
  sf::st_cast("POLYGON") |> 
  st_centroid() |> 
  st_buffer(dist = 500)

st_write(rm, file.path("outputs", "bc_red_mammals.gpkg"), append = FALSE)
bb1 <- rasterize(rm, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Red-listed mammals"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_redlisted_mammals.tif"), overwrite = TRUE)


# 7) blue vegetation (cdc)

bvv <- c("abbreviated bluegrass",
        "Alaska holly fern",
        "Alaska knotweed",
        "Alaskan sagebrush",
        "American sweet-flag",
        "Arctic sandwort",
        "beach groundsel",
        "Bostock's montia",
        "diapensia",
        "dwarf bog bunchberry",
        "elegant cinquefoil",
        "eminent bluegrass" ,
        "Frankton's draba",
        "goosegrass sedge",
        "Gorman's douglasia",
        "Gorman's penstemon" ,
        "Jordal's locoweed" ,
        "large-petalled saxifrage" ,
        "Mackenzie's sedge",
        "marsh fleabane",
        "mountain moonwort",
        "northern groundsel",
        "northern paintbrush",
        "pale greenish paintbrush" ,
        "pendantgrass",
        "polar bluegrass" ,
        "purple-haired groundsel" ,
        "Ross' avens" ,
        "Scamman's locoweed",
        "Setchell's willow",
        "three-forked mugwort" ,
        "two-coloured sedge" ,
        "two-flowered cinquefoil",
        "wedge-leaf primrose",
        "Wind River draba" ,
        "Wright's golden-saxifrage" ,
        "Yukon groundsel",
        "Yukon sawwort" )

bv <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% bvv) |> 
  sf::st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom))) 

# shrink the large uncertainty polys and add back to list
bv_centroids <- bv |> 
  filter(area > 30000000)|>
  st_centroid() |> 
 st_buffer(dist = 500)
  
bv_poly <- bv |> 
  filter(area < 30000000) 

bv<- rbind(bv_poly, bv_centroids) 
#st_write(bv, file.path("outputs", "bc_blue_vegetation.gpkg"), append = FALSE)

bb1 <- rasterize(bv, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Blue-listed vascular plants"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_bluelisted_vegetation_cover.tif"), overwrite = TRUE)

#bb1[bb1 >= 0.5] <- 1
#bb1[bb1 < 0.5] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_bluelisted_vegetation.tif"), overwrite = TRUE)


# 8) red listed vegetation (cdc)

rvv <- c("smooth draba",
        "Ogotoruk Creek butterweed",
        "northern swamp willowherb",
        "northern sawwort" ,
        "northern parrya",
        "northern daisy" ,
        "arctic daisy")
        
rv <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% rvv) |>
  sf::st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom)))

st_write(rv, file.path("outputs", "bc_red_vegetation.gpkg"), append = FALSE)

# shrink the large uncertainty polys and add back to list
rv_centroids <- rv |> 
  filter(area > 12000000)|>
  st_centroid() |> 
  st_buffer(dist = 500)

rv_poly <- rv |> 
  filter(area < 12000000) 

length(rv_poly$SPECIES_ENGLISH_NAME)

rv<- rbind(rv_poly, rv_centroids) 
#st_write(bv, file.path("outputs", "bc_blue_vegetation.gpkg"), append = FALSE)

bb1 <- rasterize(rv, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Red-listed vascular plants"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
#plot(bb1)
#bb1[bb1 >= 0.5] <- 1
#bb1[bb1 < 0.5] <- NA
#bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_redlisted_vegetation.tif"), overwrite = TRUE)






## map the uniques groupings 

# Ecosystem / community types 


# 9) Cottonwood floodplain forests (BC CDC - group black cottonwood	-red alder salmonberry & black cottonwood - hybrid spruce -redosier)

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "black cottonwood - hybrid white spruce / red-osier dogwood" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "black cottonwood - red alder / salmonberry"  )

el <- bind_rows(el, el2) |> 
  st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom))) 

#st_write(el, file.path("outputs", "cottonwood_poly_raw.gpkg"), append = FALSE)
bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Cottonwood floodplain forests: Black cottonwood-red alder-salmonberry, and Black cottonwood-hybrid spruce-salmonberry"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_cottonwood_floodplain_cover.tif"), overwrite = TRUE)

#bb1[bb1 >= 0.5] <- 1
#bb1[bb1 < 0.5] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_cottonwood_floodplain.tif"), overwrite = TRUE)


#10 Pacific willow 

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Pacific willow / red-osier dogwood / horsetails" ) #|> 
  #st_cast("POLYGON") |> 
  #dplyr::mutate(area = as.numeric(st_area(geom)))
st_write(el, file.path("outputs", "pacific_willow_poly_raw1.gpkg"), append = FALSE)
bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Low bench floodplain (Pacific willow/red-osier dogwood / horsetails)"
bb1[is.na(bb1)] <- 0
#bb1[bb1 >= 0.001] <- 1
#bb1[bb1 < 0.001] <- NA
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_pacific_willow.tif"), overwrite = TRUE)




#Sitka spruce / salmonberry high bench floodplain

sss <- c( "Sitka spruce / salmonberry Very Wet Maritime" ,              
         "Sitka spruce / salmonberry Wet Submaritime 1"  ,             
         "Sitka spruce / salmonberry Wet Submaritime 2" )

ss <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% sss) |>
  sf::st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom)))

st_write(ss, file.path("outputs", "Sitkaspruce_salmonberry_high_bench_floodplain.gpkg"), append = FALSE)
bb1 <- rasterize(ss, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Sitka spruce / salmonberry high bench floodplain"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_Sitkaspruce_salmonberry_high_bench_floodplain_cover.tif"), overwrite = TRUE)
#bb1[bb1 >= 0.05] <- 1
#bb1[bb1 < 0.05] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_Sitkaspruce_salmonberry_high_bench_floodplain0.05.tif"), overwrite = TRUE)



#10 "western hemlock - Sitka spruce / lanky moss"

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "western hemlock - Sitka spruce / lanky moss" ) #|> 
#st_write(el, file.path("outputs", "westhemlock_sitkasp_lankymoss.gpkg"), append = FALSE)

bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Western hemlock - Sitka spruce / lanky moss"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
plot(bb1)
writeRaster(bb1, file.path(outputs, "bc_cdc_westhemlock_sitkasp_lankymoss_cover.tif"), overwrite = TRUE)
#bb1[bb1 >= 0.5] <- 1
#bb1[bb1 < 0.5] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_westhemlock_sitkasp_lankymoss.tif"), overwrite = TRUE)


#11 western redcedar - Sitka spruce / sword fern - threshold 0.01 as very small isolated areas, 0.5 woudl exclude all pixals. 

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "western redcedar - Sitka spruce / sword fern" ) #|>
#st_write(el, file.path("outputs", "westredcedar_sitkasp_swordfern.gpkg"), append = FALSE)

bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Western redcedar - Sitka spruce / sword fern"
bb1[is.na(bb1)] <- 0

bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_westcedar_sitkasp_swordfern_cover.tif"), overwrite = TRUE)
#bb1[bb1 >= 0.01] <- 1
#bb1[bb1 < 0.01] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_westcedar_sitkasp_swordfern.tif"), overwrite = TRUE)


#12 Endemic grasslands:  Saskatoon - slender wheatgrass and Sandbergs bluegrass - slender wheatgrass
# threshold = 0.5

#grasslands bulkley (BC CDC - group  Saskatoon/slender wheatgrass and Sandbergs bluegrass - slender wheatgrass

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "saskatoon / slender wheatgrass" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Sandberg's bluegrass - slender wheatgrass" )
el <- bind_rows(el, el2)%>%
  st_cast("POLYGON") 

st_write(el, file.path("outputs", "grasslands_poly_raw.gpkg"), append = FALSE)

bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Endemic grasslands:  Saskatoon - slender wheatgrass, and Sandberg bluegrass - slender wheatgrass"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_endemic_grasslands_cover.tif"), overwrite = TRUE)
#bb1[bb1 >= 0.05] <- 1
#bb1[bb1 < 0.05] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_endemic_grasslands.tif"), overwrite = TRUE)


#13 Name is Sitka spruce / false lily of the valley,  Sitka spruce / tall trisetum floodplain forest


el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Sitka spruce / false lily-of-the-valley Wet Hypermaritime 1" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Sitka spruce / tall trisetum" )
el <- bind_rows(el, el2)%>%
  st_cast("POLYGON") 
#st_write(el, file.path("outputs", "Sitka_spruce_lily_trisetum_raw.gpkg"), append = FALSE)

bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
bb1[is.na(bb1)] <- 0
names(bb1) = "Sitka spruce / false lily of the valley,  Sitka spruce / tall trisetum floodplain forest"
bb1<- mask(bb1,srast)
plot(bb1)
writeRaster(bb1, file.path(outputs, "bc_cdc_Sitka_spruce_lily_trisetum_cover.tif"), overwrite = TRUE)

#bb1[bb1 >= 0.1] <- 1
#bb1[bb1 < 0.1] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_Sitka_spruce_lily_trisetum.tif"), overwrite = TRUE)




#15: epiphytic lichens (BC CDC - group cryptic paw, smoker’s lung combined)

sss <- c( "cryptic paw" ,              
          "smoker's lung"  ,             
          "oldgrowth specklebelly" )

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% sss) |>
  sf::st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom)))

#st_write(el, file.path("outputs", "epiphyticlichen_poly_raw.gpkg"), append = FALSE)

# shrink the large uncertainty polys and add back to list
rv_centroids <- el |> 
  filter(area > 3121400)|>
  st_centroid() |> 
  st_buffer(dist = 500)

rv_poly <- el |> 
  filter(area < 3121400)

#length(rv_poly$SPECIES_ENGLISH_NAME)

el <- rbind(rv_poly, rv_centroids) 
#st_write(bv, file.path("outputs", "bc_blue_vegetation.gpkg"), append = FALSE)
bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Blue-listed lichens (smoker's lung, cryptic paw, oldgrowth specklebelly)"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_epiphytic_lichens_cover.tif"), overwrite = TRUE)

#bb1 <- rast(file.path(outputs, "bc_cdc_epiphytic_lichens_cover.tif"))
#names(bb1) = "epiphytic_lichens"
#writeRaster(bb1, file.path(outputs, "bc_cdc_epiphytic_lichens_cover1.tif"), overwrite = TRUE)



# 16: Group purple - BC red-listed  lichens (mountain crab-eye and northwest waterfan)
# threshold = 0.1

sss <- c( "mountain crab-eye" , "northwest waterfan" )

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% sss) |>
  sf::st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom)))

st_write(el, file.path("outputs", "redlisted_lichen_poly_raw.gpkg"), append = FALSE)

# shrink the large uncertainty polys and add back to list
rv_centroids <- el |> 
  filter(area > 3121400)|>
  st_centroid() |> 
  st_buffer(dist = 500)

rv_poly <- el |> 
  filter(area < 3121400)

#length(rv_poly$SPECIES_ENGLISH_NAME)

el <- rbind(rv_poly, rv_centroids) 
#st_write(bv, file.path("outputs", "bc_blue_vegetation.gpkg"), append = FALSE)
bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
bb1[is.na(bb1)] <- 0
names(bb1) = "Red-listed lichens (mountain crab-eye and northwest waterfan)"
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_redlisted_lichen_cover.tif"), overwrite = TRUE)

#bb1[bb1 >= 0.01] <- 1
#bb1[bb1 < 0.01] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_redlisted_lichen.tif"), overwrite = TRUE)



#17 Group red. Name is BC listed bumblebees (McKay's, yellow-banded and gypsy cuckoo)

sss <- c( "McKay's Bumble Bee" , "Yellow-banded Bumble Bee", "Gypsy Cuckoo Bumble Bee" )

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME %in% sss) |>
  sf::st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom)))

st_write(el, file.path("outputs", "bc_cdc_listed_bumblebees_poly_raw.gpkg"), append = FALSE)

# shrink the large uncertainty polys and add back to list
rv_centroids <- el |> 
  filter(area > 78036000)|>
  st_centroid() |> 
  st_buffer(dist = 500)

rv_poly <- el |> 
  filter(area < 78036000)

#length(rv_poly$SPECIES_ENGLISH_NAME)

el <- rbind(rv_poly, rv_centroids) 
#st_write(bv, file.path("outputs", "bc_blue_vegetation.gpkg"), append = FALSE)
bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
bb1[is.na(bb1)] <- 0
names(bb1) = "Listed bumblebees (McKay's, yellow-banded and gypsy cuckoo)"
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_bclisted_bumblebees_cover.tif"), overwrite = TRUE)
#bb1[bb1 >= 0.01] <- 1
#bb1[bb1 < 0.01] <- NA
#bb1<- mask(bb1,srast)
#writeRaster(bb1, file.path(outputs, "bc_cdc_bclisted_bumblebees.tif"), overwrite = TRUE)


#18 BC blue-listed dragonfly

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Plains Forktail" ) |> 
  st_cast("POLYGON") |> 
  dplyr::mutate(area = as.numeric(st_area(geom))) |> 
  st_centroid() |> 
  st_buffer(dist = 500)

st_write(el, file.path("outputs", "bc_blue_listeddragonfly_poly_raw1.gpkg"), append = FALSE)
bb1 <- rasterize(el, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Blue-listed dragonfly (Plains forktail)"
bb1[is.na(bb1)] <- 0
#bb1[bb1 >= 0.001] <- 1
#bb1[bb1 < 0.001] <- NA
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_cdc_bluelisted_dragonfly.tif"), overwrite = TRUE)




#############################################################################
## Focal species of interest 
################################################################################
# 
# 2) wildlife obs
wa <- st_read(file.path("inputs", "wildlife_obs_all.gpkg")) %>%
  dplyr::select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATETIME,
         OBSERVATION_YEAR,LATITUDE, LONGITUDE,
         NAME_TYPE,  NAME_TYPE_SUB,TAXONOMIC_LEVEL,
         PHYLUM_NAME, CLASS_NAME, CLASS_ENGLISH, ORDER_NAME)

# 3) wildlife inc
wi <- st_read(file.path("inputs", "wildlife_incident_obs.gpkg")) %>%
  dplyr::select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATE,
         OBSERVATION_YEAR,LATITUDE, LONGITUDE,
         NAME_TYPE,  NAME_TYPE_SUB,TAXONOMIC_LEVEL,
         PHYLUM_NAME, CLASS_NAME, CLASS_ENGLISH, ORDER_NAME)

ww <- bind_rows(wa, wi)

# 4) wildlife telem
 wwt <- st_read(file.path("inputs", "wildlife_telemetry_pts.gpkg")) %>% 
   dplyr::select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATE)


# 5) cdc dataset
wcdc <- st_read(file.path("inputs", "bc_cbc_sp_raw.gpkg")) %>% 
  dplyr::select(ENG_NAME, SCI_NAME, EL_TYPE)%>%
  dplyr::rename("SPECIES_ENGLISH_NAME" = ENG_NAME,
         "SCIENTIFIC_NAME" = SCI_NAME,
         "NAME_TYPE_SUB" = EL_TYPE)



##################
# Amphibians 

wwa <- ww %>% filter(CLASS_NAME == "Amphibia")

#-western toad #6670 all records #5219 unique

wt <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Western Toad")%>%
  distinct()
st_write(wt, file.path("outputs", "western_toad_pt.gpkg"), append = FALSE)

bb1 <- rasterize(wt, srast, cover = TRUE, touches = TRUE)
bb1[is.na(bb1)] <- 0
names(bb1) = "Western Toad"
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_westerntoad_cover.tif"), overwrite = TRUE)



#-northwest salamander #7087 #3938
nwsal <- wwa %>% filter(SPECIES_ENGLISH_NAME == "Northwestern Salamander")%>%
  distinct()
st_write(nwsal, file.path("outputs", "northwestsal_pt.gpkg"), append = FALSE)

bb1 <- rasterize(nwsal, srast, cover = TRUE, touches = TRUE)
bb1[is.na(bb1)] <- 0
names(bb1) = "Northwestern Salamander"
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_northwestsal_cover.tif"), overwrite = TRUE)




#-rough-skinned newt #1535  #1111
srn <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Roughskin Newt")%>%
  distinct()
#st_write(srn, file.path("outputs", "roughskinnewt_pt.gpkg"), append = FALSE)

bb1 <- rasterize(srn, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Rough-skinned Newt"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_roughskinnewt_cover.tif"), overwrite = TRUE)



#-wood frog #188 # 166
wf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Wood Frog")%>%
  distinct()
st_write(wf, file.path("outputs", "woodfrog_pt.gpkg"), append = FALSE)

bb1 <- rasterize(wf, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Wood Frog"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_woodfrog_cover.tif"), overwrite = TRUE)



# coastal tailed frog (and bc cdc too) #973  #897
ctf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Coastal Tailed Frog")%>%
  distinct()

# all the wcdc locations are already captured with the pt data 

##ctf2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Coastal Tailed Frog") 
#  mutate(area_m = st_area(.))%>%
#  mutate(area = as.numeric(area_m))%>%
#  filter(area < 100000)

# ctf22 <- st_centroid(ctf2)%>% select(-area_m, -area)
# 
#ctf <- bind_rows(ctf, ctf2) 

st_write(ctf, file.path("outputs", "coastaltail_pt.gpkg"), append = FALSE)

bb1 <- rasterize(ctf, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Coastal Tailed Frog"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_coastaltail_cover.tif"), overwrite = TRUE)
aa <- 


#-Columbia spotted frog #2346 
csf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Columbia Spotted Frog")%>%
  distinct()
st_write(csf, file.path("outputs", "columbiaspotfrog_pt.gpkg"), append = FALSE)
bb1 <- rasterize(csf, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Columbia Spotted Frog"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_columbiaspotfrog_cover.tif"), overwrite = TRUE)



# Fish species

# 1) fish observations
fi <- st_read(file.path("inputs", "sk_known_fish_pts.gpkg"))

sort(unique(fi$SPECIES_NAME))

soi <- c("Eulachon", "Bull Trout", 
         "Chum Salmon", "Sockeye Salmon" ,"Pink Salmon","Coho Salmon",
         "Chinook Salmon", "All Salmon" ,
         "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)")

fii <- fi %>% filter(SPECIES_NAME %in% soi)


# Eulachon #63
ee <- fii %>% filter(SPECIES_NAME =="Eulachon") 
st_write(ee, file.path("outputs", "eulachon_pt.gpkg"), append = FALSE)

bb1 <- rasterize(ee, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Eulachon"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_eulachon_cover.tif"), overwrite = TRUE)


# bulltrout # 992
ee <- fii %>% filter(SPECIES_NAME =="Bull Trout")#%>%
#distinct()
st_write(ee, file.path("outputs", "bulltrout_pt.gpkg"), append = FALSE)

bb1 <- rasterize(ee, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Bull trout"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_bulltrout_cover.tif"), overwrite = TRUE)
#bb1 <- rast(file.path(outputs, "bc_bulltrout_cover.tif"))
#names(bb1) = "Bull trout"




# salmon #13452
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Chum Salmon", "Sockeye Salmon" ,"Pink Salmon","Coho Salmon",
  "Chinook Salmon", "All Salmon"))
st_write(ee, file.path("outputs", "salmon_pt.gpkg"), append = FALSE)
bb1 <- rasterize(ee, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Salmon (Chum, Sockeye, Pink, Coho, Chinook)"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_salmon_cover.tif"), overwrite = TRUE)




# steel head  #2940
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)"))

st_write(ee, file.path("outputs", "steelhead_pt.gpkg"),append = FALSE)
bb1 <- rasterize(ee, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Steelhead"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_steelhead_cover.tif"), overwrite = TRUE)


## Osprey #413 # 304

os <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Osprey")%>%distinct()
st_write(os, file.path("outputs", "osprey_pt.gpkg"), append = FALSE)
bb1 <- rasterize(os, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Osprey"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_osprey_cover.tif"), overwrite = TRUE)


# dragon flies #3340 # 1521

dra <- ww %>% filter(ORDER_NAME == "Odonata")%>% distinct()
st_write(dra, file.path("outputs", "odonata_pt.gpkg"), append = FALSE)
bb1 <- rasterize(dra, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Odonata (dragonflies)"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_odonata_cover.tif"), overwrite = TRUE)


### terrestrial 

# whitebark pine (also in bc cdc) #1299 
#wbp <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Whitebark Pine")

#wbp2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "whitebark pine")
#wbp2 <- st_centroid(wbp2)
#wbp <- bind_rows(wbp, wbp2) %>%
#  distinct()
#
#st_write(wbp, file.path("outputs", "whitebarkpine_pt.gpkg"), append = FALSE)


# Pacific marten #136 # 72
ee <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Pacific Marten")%>%
  distinct()
st_write(ee, file.path("outputs", "pacificmarten_pt.gpkg"), append = FALSE)
bb1 <- rasterize(ee, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Pacific Marten"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_pacificmarten_cover.tif"), overwrite = TRUE)




# northern flying squirrel #135 #81 
ee <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Northern Flying Squirrel")%>%
  distinct()
st_write(ee, file.path("outputs", "nthfylingsq_pt.gpkg"), append = FALSE)
bb1 <- rasterize(ee, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Northern Flying Squirrel"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_nthfylingsq_cover.tif"), overwrite = TRUE)


# bats #34704 # 505
#-bats (all species and “bats” (unidentified bats group in data set)

bb <- ww %>% filter(ORDER_NAME =="Chiroptera")  
bb <- bb %>% distinct()                 
st_write(bb, file.path("outputs", "bat_pt.gpkg"), append = FALSE)
bb<- st_read(file.path("outputs", "bat_pt.gpkg"))
bb1 <- rasterize(bb, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Chiroptera"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_bat_cover.tif"), overwrite = TRUE)
bb1 <- rast(file.path(outputs, "bc_bat_cover.tif"))
names(bb1) = "Chiroptera (bats)"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_bat_cover1.tif"), overwrite = TRUE)


# -red-backed vole # 290
ee <- ww %>% filter(SPECIES_ENGLISH_NAME == "Southern Red-backed Vole") %>% distinct()  
st_write(ee , file.path("outputs", "sthredbackedvole_pt.gpkg"), append = FALSE)
bb1 <- rasterize(ee, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Southern Red-backed Vole"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_sthredbackedvole_cover.tif"), overwrite = TRUE)


#black bear #518
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "American Black Bear")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "American Black Bear") 
nn <- bind_rows(nn, nn2)%>% distinct()  
st_write(nn , file.path("outputs", "blackbear_pt.gpkg"), append = FALSE)
bb1 <- rasterize(nn, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Black Bear"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_blackbear_cover.tif"), overwrite = TRUE)







# grizz # 6825
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "Grizzly Bear")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "Grizzly Bear") 
nn <- bind_rows(nn, nn2)%>% distinct()  
st_write(nn , file.path("outputs", "grizbear_pt.gpkg"), append = FALSE)

bb1 <- rasterize(nn, srast, cover = TRUE, touches = TRUE)
names(bb1) = "Grizzly Bear"
bb1[is.na(bb1)] <- 0
bb1<- mask(bb1,srast)
writeRaster(bb1, file.path(outputs, "bc_grizbear_cover.tif"), overwrite = TRUE)





















































# Ecosystem / community types 

# rare epiphytic lichens (BC CDC - group cryptic paw, smoker’s lung combined)


#grasslands bulkley (BC CDC - group  Saskatoon/slender wheatgrass and Sandbergs bluegrass - slender wheatgrass

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "saskatoon / slender wheatgrass" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Sandberg's bluegrass - slender wheatgrass" )
el <- bind_rows(el, el2)%>%
  st_cast("POLYGON") 

st_write(el, file.path("outputs", "grasslands_poly_raw.gpkg"), append = FALSE)


el <- bind_rows(el, el2)%>%
  st_cast("POLYGON") 
st_write(el, file.path("outputs", "cottonwood_poly_raw.gpkg"), append = FALSE)
#st_write(el, file.path("outputs", "grasslands_poly_raw.gpkg"), append = FALSE)


# el <- el %>%
#   distinct()%>%
#   mutate(area_m = st_area(.))%>%
#   mutate(area = as.numeric(area_m))%>%
#   filter(area < 100000)
# 
# el <- st_centroid(el)%>% select(-area_m, -area)
# st_write(el, file.path("outputs", "cottonwood_pt.gpkg"), append = FALSE)
# 

# read in species dataset 
# 
# # 1) fish observations
# fi <- st_read(file.path("inputs", "sk_known_fish_pts.gpkg"))
# 
# # 2) wildlife obs
# wa <- st_read(file.path("inputs", "wildlife_obs_all.gpkg")) %>%
#   select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATETIME,
#          OBSERVATION_YEAR,LATITUDE, LONGITUDE,
#          NAME_TYPE,  NAME_TYPE_SUB,TAXONOMIC_LEVEL,
#          PHYLUM_NAME, CLASS_NAME, CLASS_ENGLISH, ORDER_NAME)
# 
# # 3) wildlife inc
# wi <- st_read(file.path("inputs", "wildlife_incident_obs.gpkg")) %>% 
#   select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATE,
#          OBSERVATION_YEAR,LATITUDE, LONGITUDE,
#          NAME_TYPE,  NAME_TYPE_SUB,TAXONOMIC_LEVEL,
#          PHYLUM_NAME, CLASS_NAME, CLASS_ENGLISH, ORDER_NAME)
# 
# ww <- bind_rows(wa, wi)
# 
# 
# # 4) wildlife telem
# wwt <- st_read(file.path("inputs", "wildlife_telemetry_pts.gpkg")) %>% 
#   select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATE)

#[1] "Northern Goshawk"    "Grizzly Bear"        "American Black Bear"
#[4] "Grey Wolf" 

# 5) cdc dataset
wcdc <- st_read(file.path("inputs", "bc_cbc_sp_raw.gpkg")) %>% 
  select(ENG_NAME, SCI_NAME, EL_TYPE)%>%
  rename("SPECIES_ENGLISH_NAME" = ENG_NAME,
           "SCIENTIFIC_NAME" = SCI_NAME,
          "NAME_TYPE_SUB" = EL_TYPE)


# 6) surveys not useful 
#ws <- st_read(file.path("inputs", "wildlife_surveys.gpkg"))


## VERTEBRATES 
#wwv <- ww %>% 
#  filter(NAME_TYPE == "Vertebrate Animal")

## amphibians 
wwa <- ww %>% filter(CLASS_NAME == "Amphibia")

#-western toad #6670 all records #5219 unique

wt <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Western Toad")%>%
  distinct()
st_write(wt, file.path("outputs", "western_toad_pt.gpkg"), append = FALSE)


#-northwest salamander #7087 #3938
nwsal <- wwa %>% filter(SPECIES_ENGLISH_NAME == "Northwestern Salamander")%>%
  distinct()
st_write(nwsal, file.path("outputs", "northwestsal_pt.gpkg"), append = FALSE)


#-rough-skinned newt #1535  #1111
srn <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Roughskin Newt")%>%
  distinct()
st_write(srn, file.path("outputs", "roughskinnewt_pt.gpkg"), append = FALSE)

#-wood frog #188 # 166
wf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Wood Frog")%>%
  distinct()
st_write(wf, file.path("outputs", "woodfrog_pt.gpkg"), append = FALSE)


# coastal tailed frog (and bc cdc too) #973  #897
ctf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Coastal Tailed Frog")%>%
  distinct()

# all the wcdc locations are already captured with the pt data 

##ctf2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Coastal Tailed Frog") 
#  mutate(area_m = st_area(.))%>%
#  mutate(area = as.numeric(area_m))%>%
#  filter(area < 100000)
         
# ctf22 <- st_centroid(ctf2)%>% select(-area_m, -area)
# 
#ctf <- bind_rows(ctf, ctf2) 

st_write(ctf, file.path("outputs", "coastaltail_pt.gpkg"), append = FALSE)


#-Columbia spotted frog #2346 
csf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Columbia Spotted Frog")%>%
  distinct()
st_write(csf, file.path("outputs", "columbiaspotfrog_pt.gpkg"), append = FALSE)


# Fish species

sort(unique(fi$SPECIES_NAME))

soi <- c("Eulachon", "Bull Trout", 
         "Chum Salmon", "Sockeye Salmon" ,"Pink Salmon","Coho Salmon",
         "Chinook Salmon", "All Salmon" ,
         "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)")

fii <- fi %>% filter(SPECIES_NAME %in% soi)


# Eulachon #63
ee <- fii %>% filter(SPECIES_NAME =="Eulachon") 

st_write(ee, file.path("outputs", "eulachon_pt.gpkg"), append = FALSE)


# bulltrout # 992
ee <- fii %>% filter(SPECIES_NAME =="Bull Trout")#%>%
  #distinct()
st_write(ee, file.path("outputs", "bulltrout_pt.gpkg"), append = FALSE)


# salmon #13452
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Chum Salmon", "Sockeye Salmon" ,"Pink Salmon","Coho Salmon",
  "Chinook Salmon", "All Salmon"))
st_write(ee, file.path("outputs", "salmon_pt.gpkg"), append = FALSE)

# steel head  #2940
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)"))

st_write(ee, file.path("outputs", "steelhead_pt.gpkg"),append = FALSE)


## Osprey #413 # 304

os <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Osprey")%>%distinct()
st_write(os, file.path("outputs", "osprey_pt.gpkg"), append = FALSE)


# dragon flies #3340 # 1521

dra <- ww %>% filter(ORDER_NAME == "Odonata")%>% distinct()
st_write(dra, file.path("outputs", "odonata_pt.gpkg"), append = FALSE)


### terrestrial 

# whitebark pine (also in bc cdc) #1299 
wbp <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Whitebark Pine")

wbp2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "whitebark pine")
wbp2 <- st_centroid(wbp2)
wbp <- bind_rows(wbp, wbp2) %>%
  distinct()

st_write(wbp, file.path("outputs", "whitebarkpine_pt.gpkg"), append = FALSE)


# Pacific marten #136 # 72
ee <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Pacific Marten")%>%
  distinct()
st_write(ee, file.path("outputs", "pacificmarten_pt.gpkg"), append = FALSE)


# northern flying squirrel #135 #81 
ee <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Northern Flying Squirrel")%>%
  distinct()
st_write(ee, file.path("outputs", "nthfylingsq_pt.gpkg"), append = FALSE)


# bats #34704 # 505
#-bats (all species and “bats” (unidentified bats group in data set)
         
bb <- ww %>% filter(ORDER_NAME =="Chiroptera")  
bb <- bb %>% distinct()                 
st_write(bb, file.path("outputs", "bat_pt.gpkg"), append = FALSE)


#northern goshawk 1520
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "Northern Goshawk")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "Northern Goshawk") 
nn <- bind_rows(nn, nn2) %>% distinct()  
st_write(nn, file.path("outputs", "northerngoshawk_pt.gpkg"), append = FALSE)

        
# -red-backed vole # 290
ee <- ww %>% filter(SPECIES_ENGLISH_NAME == "Southern Red-backed Vole") %>% distinct()  
st_write(ee , file.path("outputs", "sthredbackedvole_pt.gpkg"), append = FALSE)

#black bear #518
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "American Black Bear")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "American Black Bear") 
nn <- bind_rows(nn, nn2)%>% distinct()  
st_write(nn , file.path("outputs", "blackbear_pt.gpkg"), append = FALSE)

# grizz # 6825
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "Grizzly Bear")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "Grizzly Bear") 
nn <- bind_rows(nn, nn2)%>% distinct()  
st_write(nn , file.path("outputs", "grizbear_pt.gpkg"), append = FALSE)





# Ecosystem / community types 

# rare epiphytic lichens (BC CDC - group cryptic paw, smoker’s lung combined)

wcdc <- st_read(file.path("inputs", "bc_cbc_sp_raw.gpkg")) %>% 
  #select(ENG_NAME, SCI_NAME, EL_TYPE)%>%
  rename("SPECIES_ENGLISH_NAME" = ENG_NAME,
         "SCIENTIFIC_NAME" = SCI_NAME,
         "NAME_TYPE_SUB" = EL_TYPE)

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "cryptic paw" ) #56
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "smoker's lung" ) #49

el <- bind_rows(el, el2) %>%
st_cast("POLYGON") 

st_write(el, file.path("outputs", "epiphyticlichen_poly_raw.gpkg"), append = FALSE)


# in QGIS identify the pts and polygons and export 

# the polygons will be intersected with the raster area and
# points selected for uncertain and then taken centroids

elpol <- st_read(file.path("outputs", "epiphyticlichen_poly_only_raw.gpkg"))
elpt <- st_read(file.path("outputs", "epiphyticlichen_pts_raw.gpkg"))

elptt <- elpt %>%
  st_centroid(.)

st_write(elptt, file.path("outputs", "epiphyticlichen_pt.gpkg"), append = FALSE)



# using polygons, no large areas where uncertainty 

#grasslands bulkley (BC CDC - group  Saskatoon/slender wheatgrass and Sandbergs bluegrass - slender wheatgrass

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "saskatoon / slender wheatgrass" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Sandberg's bluegrass - slender wheatgrass" )
el <- bind_rows(el, el2)%>%
  st_cast("POLYGON") 

st_write(el, file.path("outputs", "grasslands_poly_raw.gpkg"), append = FALSE)

  

# waiting for Paula to check mapping 

# -cottonwood floodplain forests (BC CDC - group black cottonwood	-red alder salmonberry & black cottonwood - hybrid spruce -redosier)
el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "black cottonwood - hybrid white spruce / red-osier dogwood" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "black cottonwood - red alder / salmonberry"  )

el <- bind_rows(el, el2)%>%
  st_cast("POLYGON") 
st_write(el, file.path("outputs", "cottonwood_poly_raw.gpkg"), append = FALSE)
#st_write(el, file.path("outputs", "grasslands_poly_raw.gpkg"), append = FALSE)


# el <- el %>%
#   distinct()%>%
#   mutate(area_m = st_area(.))%>%
#   mutate(area = as.numeric(area_m))%>%
#   filter(area < 100000)
# 
# el <- st_centroid(el)%>% select(-area_m, -area)
# st_write(el, file.path("outputs", "cottonwood_pt.gpkg"), append = FALSE)
# 







                          
############################################################

library(tidyverse)

# intersect with terrestrial barcodes
ter <- rast(file.path("inputs", "sk_lf_barcode.tif"))

#te <- st_read(file.path("outputs", "final", "sk_lf_barcode_poly.gpkg"))

# land barcode 
te_csv <- read_csv(file.path("outputs", "lf_barcode_summary.csv"))%>%
  select(layer1, count)%>% 
  rename("land_barcode" = layer1)



# lakes barcode 

la <- st_read(file.path("outputs", "final", "sk_lakes_barcode_poly.gpkg"))%>%
  select(lake_code)

la_csv <- la %>%
  st_drop_geometry() %>%
  select(lake_code)%>%
  group_by(lake_code)%>%
  count()


# river barcode 

ri <- st_read(file.path("outputs", "final", "sk_river_barcode_poly.gpkg"))%>%
  select(river_code)

ri_csv <- ri %>%
  st_drop_geometry() %>%
  select(river_code)%>%
  group_by(river_code) %>%
  count()


## Read in the land intersect species pts 

sp <- list.files(file.path("outputs"), pattern = "*_pt.gpkg")

DF <-  st_read(file.path("outputs", sp[1])) %>% mutate(lf_group = sp[1])
for (f in sp[-1]) DF <- bind_rows(DF, st_read(file.path("outputs", f))%>% mutate(lf_group = f))   

DF <- DF %>% select(SPECIES_ENGLISH_NAME,  SCIENTIFIC_NAME, OBSERVATION_DATETIME,lf_group)%>%
  distinct(.)

st_write(DF, file.path("outputs", "allsp.gpkg"), append = F)

#DF <- st_read(file.path("outputs", "allsp.gpkg"))
  
#Intersect with polygons 
elpol <- st_read(file.path("outputs", "epiphyticlichen_poly_only_raw.gpkg"))%>%
  mutate(lf_group = "epiphyticlichen_poly")  %>%
  select(SPECIES_ENGLISH_NAME,  SCIENTIFIC_NAME,lf_group)

# grassland
grpol <- st_read(file.path("outputs", "grasslands_poly_raw.gpkg"))%>%
  mutate(lf_group = "grasslands_poly") %>%
  select(SPECIES_ENGLISH_NAME,  SCIENTIFIC_NAME, lf_group)

cotton <- st_read(file.path("outputs", "cottonwood_poly_raw.gpkg"))%>%
  mutate(lf_group = "cottonwood_poly") %>%
  select(SPECIES_ENGLISH_NAME,  SCIENTIFIC_NAME, lf_group)


pols <- rbind(elpol, grpol, cotton)
st_write(pols, file.path("outputs", "allsp_pols.gpkg"), append = F)


# intersect with landscape barcode with the points 

dfv <- vect(DF)
wt_land <- terra::extract( ter, dfv)

dfcsv <- st_drop_geometry(DF)
dfcsv <- bind_cols(dfcsv, wt_land) %>%
  rename("land_barcode" = lyr.1)

wte <- dfcsv %>% 
  select(lf_group, land_barcode)%>% 
  mutate(lf_group = gsub("*_pt.gpkg", "", lf_group)) %>%
  #st_drop_geometry()%>% 
  group_by(lf_group)%>% 
  count(land_barcode)

wte <- wte %>% 
  pivot_wider( names_from = lf_group , values_from = n)

wter <- left_join(te_csv, wte)
wter <- wter %>%
  mutate_all(~replace(., is.na(.), 0))

write.csv(wter, file.path("outputs", "all_sp_landbarcodes_pt.csv"))


# intersect with polygons: 


dfv <- vect(pols)
wt_land <- terra::extract( ter, dfv)

dfcsv <- st_drop_geometry(pols)%>% 
  dplyr::mutate( ID = row_number())

dfcsv <- left_join(dfcsv, wt_land, by = "ID") %>%
  rename("land_barcode" = lyr.1)

wte <- dfcsv %>% 
  select(lf_group, land_barcode)%>% 
  mutate(lf_group = gsub("*_poly.gpkg", "", lf_group)) %>%
  #st_drop_geometry()%>% 
  group_by(lf_group)%>% 
  count(land_barcode)

wte <- wte %>% 
  pivot_wider(names_from = lf_group , values_from = n)

wter <- left_join(te_csv, wte)
wter <- wter %>%
  mutate_all(~replace(., is.na(.), 0))

write.csv(wter, file.path("outputs", "all_sp_landbarcodes_poly.csv"))







# intersect with river

wt_ri <- st_intersection(DF, ri) 
wri <- wt_ri %>% 
  select(lf_group, river_code)%>% 
  mutate(lf_group = gsub("*_pt.gpkg", "", lf_group)) %>%
  st_drop_geometry()%>% 
  group_by(lf_group)%>% 
  count(river_code)

wri <- wri %>% 
  pivot_wider( names_from = lf_group , values_from = n)

ri_wri <- left_join(ri_csv, wri)
ri_wri <- ri_wri%>%
    mutate_all(~replace(., is.na(.), 0))

write.csv(ri_wri, file.path("outputs", "all_sp_riverbarcodes.csv"))




# intersect with polygons: 


wt_ri <- st_intersection(pols, ri) 
wri <- wt_ri %>% 
  select(lf_group, river_code)%>% 
  mutate(lf_group = gsub("*_poly.gpkg", "", lf_group)) %>%
  st_drop_geometry()%>% 
  group_by(lf_group)%>% 
  count(river_code)

wri <- wri %>% 
  pivot_wider( names_from = lf_group , values_from = n)

ri_wri <- left_join(ri_csv, wri)
ri_wri <- ri_wri%>%
  mutate_all(~replace(., is.na(.), 0))

write.csv(ri_wri, file.path("outputs", "all_sp_riverbarcodes_poly.csv"))



# intersect with lakes
wt_la <- st_intersection(DF, la) 
wla <- wt_la %>% 
  select(lf_group, lake_code)%>% 
  mutate(lf_group = gsub("*_pt.gpkg", "", lf_group)) %>%
  st_drop_geometry()%>% 
  group_by(lf_group)%>% 
  count(lake_code)

wla <- wla %>% 
  pivot_wider( names_from = lf_group , values_from = n)

law <- left_join(la_csv, wla)
law <- law %>%
  mutate_all(~replace(., is.na(.), 0))

write.csv(law, file.path("outputs", "all_sp_lakebarcodes.csv"))



# intersect with lakes with polygons

wt_la <- st_intersection(pols, la) 
wla <- wt_la %>% 
  select(lf_group, lake_code)%>% 
  mutate(lf_group = gsub("*_pt.gpkg", "", lf_group)) %>%
  st_drop_geometry() %>% 
  group_by(lf_group) %>% 
  count(lake_code)

wla <- wla %>% 
  pivot_wider( names_from = lf_group , values_from = n)

law <- left_join(la_csv, wla)
law <- law %>%
  mutate_all(~replace(., is.na(.), 0))

write.csv(law, file.path("outputs", "all_sp_lakebarcodes_poly.csv"))




##########################################################################

## Convert the species layers to a raster presence/absence 

library(dplyr)
library(terra)
library(sf)
library(readr)
library(tidyr)

srast <- rast(file.path("inputs", "sk_rast_template.tif"))
in_aoi <- st_read(file.path("inputs", "sk_poly_template.gpkg"))


# species list # not yet working.....

df <- st_read(file.path("outputs", "allsp.gpkg"))

splist <- unique(df$lf_group)
splist <- splist[-c(1:15)] # "epiphyticlichen_pt.gpkg" 


for(i in splist){
  
  #i = splist[2]
  outname <- gsub("_pt.gpkg", "", i)
  
  isp <- df %>% filter(lf_group == i) %>% 
    select(geom)%>% 
    mutate(pres = 1)
  
  # convert to raster 
  isr <- rasterize(isp, srast, field = "pres", background = 0)
  rr <- mask(isr, srast)
  
  names(rr) = "pa"
  
  writeRaster(rr, file.path("outputs", paste0(outname, "_presabs.tif")), overwrite = TRUE)
  
}

dfpol <- st_read(pols, file.path("outputs", "allsp_pols.gpkg"))
                 
                 
                 
## convert cotton wood and grassland to polygons

cw <-  st_read(file.path("outputs", "cottonwood_poly_raw.gpkg"))
isp<- cw %>% 
  select(geom)%>% 
  mutate(pres = 1)    

isr <- rasterize(isp, srast, field = "pres", background = 0)
rr <- mask(isr, srast)
names(rr) = "pa"

writeRaster(rr, file.path("outputs", "cottonwood_presabs.tif"), overwrite = TRUE)


# repeat with grassland 
cw <-  st_read(file.path("outputs", "grasslands_poly_raw.gpkg"))
isp<- cw %>% 
  select(geom)%>% 
  mutate(pres = 1)    

isr <- rasterize(isp, srast, field = "pres", background = 0)
rr <- mask(isr, srast)
names(rr) = "pa"

writeRaster(rr, file.path("outputs", "grassland_presabs.tif"), overwrite = TRUE)


 



# CDC species # have not yet added all these species. 

# create a new layer for each species and output as matching raster 
wcdc <- st_read(file.path("inputs", "bc_cbc_sp_raw.gpkg"))



