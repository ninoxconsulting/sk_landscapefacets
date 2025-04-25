# 04. Mapping 

library(bcdata)
library(dplyr)
library(terra)
library(sf)

## note templates are generated in scripr : 01_prep..

srast <- rast(file.path("inputs", "sk_rast_template.tif"))

srast1km <- aggregate(srast, fact = 10)

in_aoi <- st_read(file.path("inputs", "sk_poly_template.gpkg"))

#in_aoi <- st_as_sf(in_aoi)

##########################################################################
# Get data from bcdata catcalogue


# 1. lakes
#https://catalogue.data.gov.bc.ca/dataset/b3f58ed8-376f-4962-9657-36297a5f41cf
# 
# lakes <- bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6") |>
#   # filter(INTERSECTS(aoi_sf)) |> 
#   select( WATERBODY_TYPE, AREA_HA) |>
#   collect()
# 
# lakes1 <- sf::st_intersection(lakes, in_aoi) |> 
#   select( WATERBODY_TYPE, AREA_HA) %>% 
#   filter(AREA_HA > 1) 
# 
# 
# st_write(lakes1, file.path("inputs", "lakes.gpkg"), append = FALSE)
# 

# rivers 

#ri <- bcdc_query_geodata("f7dac054-efbf-402f-ab62-6fc4b32a619e") |>
#  # filter(INTERSECTS(aoi_sf)) |> 
#  select( WATERBODY_TYPE, AREA_HA) |>
#  collect()

#ri <- sf::st_intersection(ri, in_aoi) |> 
#    select( WATERBODY_TYPE, AREA_HA) 

#st_write(ri, file.path("inputs", "rivers.gpkg"), append = FALSE)


# 
# # wetlands 
# #https://catalogue.data.gov.bc.ca/dataset/93b413d8-1840-4770-9629-641d74bd1cc6
wetlands <- bcdc_query_geodata("93b413d8-1840-4770-9629-641d74bd1cc6") |>
  # filter(INTERSECTS(aoi_sf)) |>
  select( WATERBODY_TYPE, AREA_HA) |>
  collect()

wetlands <- sf::st_intersection(wetlands, in_aoi) |>
      select(WATERBODY_TYPE, AREA_HA)

st_write(wetlands, file.path("inputs", "wetlands.gpkg"), append = FALSE)


# determine percent density per pixel (100m x 100m) 
wl <- st_read(file.path("inputs", "wetlands.gpkg"))

wll <- vect(file.path("inputs", "wetlands.gpkg"))

tt <- rasterize(wll, srast, cover = TRUE)
tt[is.na(tt)] <- 0 
tt <- mask(tt, srast)
writeRaster(tt, file.path("inputs", "sk_wetland_density.tif"), overwrite = TRUE)


# determine percent density per pixel (1000m x 1000m) 
wl <- st_read(file.path("inputs", "wetlands.gpkg"))

wll <- vect(file.path("inputs", "wetlands.gpkg"))

tt <- rasterize(wll, srast1km, cover = TRUE)
tt[is.na(tt)] <- 0 
tt <- mask(tt, srast1km)
writeRaster(tt, file.path("inputs", "sk_wetland_density_1km.tif"), overwrite = TRUE)







## glaciers 
#https://catalogue.data.gov.bc.ca/dataset/8f2aee65-9f4c-4f72-b54c-0937dbf3e6f7

glac <- bcdc_query_geodata("8f2aee65-9f4c-4f72-b54c-0937dbf3e6f7") |>
  # filter(INTERSECTS(aoi_sf)) |> 
  select(WATERBODY_TYPE, AREA_HA) |>
  collect()

glac <- sf::st_intersection(glac, in_aoi)

st_write(glac, file.path("inputs", "glaciers.gpkg"), append = FALSE)



# extract protected areas 
#https://catalogue.data.gov.bc.ca/dataset/1130248f-f1a3-4956-8b2e-38d29d3e4af7

pro <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7") |>
  # filter(INTERSECTS(aoi_sf)) |> 
  select(PROTECTED_LANDS_NAME, PROTECTED_LANDS_CODE, PROTECTED_LANDS_DESIGNATION) |>
  collect()

pro <- sf::st_intersection(pro, in_aoi)

st_write(pro, file.path("inputs", "protected_lands.gpkg"), append = FALSE)



# conservation lands: 
#https://catalogue.data.gov.bc.ca/dataset/68327529-c0d5-4fcb-b84e-f8d98a7f8612

cons <- bcdc_query_geodata("68327529-c0d5-4fcb-b84e-f8d98a7f8612") |>
  # filter(INTERSECTS(aoi_sf)) |> 
  #  select(SITE_NAME, CONSERVATION_LAND_TYPE, TENURE_TYPE,TENURE_DESCRIPTION) |>
  collect()

cons <- sf::st_intersection(cons, in_aoi)|>   
  select(SITE_NAME, CONSERVATION_LAND_TYPE, TENURE_TYPE,TENURE_DESCRIPTION) 

st_write(cons, file.path("inputs", "cons_lands.gpkg"), append = FALSE)



# bc parks conservancy 
#https://catalogue.data.gov.bc.ca/dataset/550b3133-2004-468f-ba1f-b95d0e281e78

pcon <- bcdc_query_geodata("550b3133-2004-468f-ba1f-b95d0e281e78") |>
  collect()

pcon<- sf::st_intersection(pcon , in_aoi)|>   
  select(ADMIN_AREA_SID, CONSERVANCY_AREA_NAME)%>%
  rename("PROTECTED_LANDS_NAME" = CONSERVANCY_AREA_NAME)%>%
  mutate(PROTECTED_LANDS_DESIGNATION = "BC CONSERVANCY")


st_write(pcon, file.path("inputs", "bc_conservancy.gpkg"), append = FALSE)





# # quarternary sedimte? # do not use - not reliable 
# #https://catalogue.data.gov.bc.ca/dataset/b3f58ed8-376f-4962-9657-36297a5f41cf
# # binary 
# sed <- bcdc_query_geodata("b3f58ed8-376f-4962-9657-36297a5f41cf") |>
#   collect()
# sed <- sf::st_intersection(sed,  in_aoi) %>% 
#   select(QUATERNARY_ID)
# 
# st_write(sed , file.path("inputs", "quat_sed.gpkg"), append = FALSE)




# ecoregions 
#https://catalogue.data.gov.bc.ca/dataset/d00389e0-66da-4895-bd56-39a0dd64aa78

ec <- bcdc_query_geodata("d00389e0-66da-4895-bd56-39a0dd64aa78") |>
  collect()

#st_write(ec , file.path("inputs", "bc_ecoreg.gpkg"), append = FALSE)
ec <- st_read(file.path("inputs", "bc_ecoreg.gpkg"))

ec <- sf::st_intersection(ec, in_aoi)%>% 
  select(ECOREGION_CODE,  ECOREGION_NAME)%>%
  st_intersection(in_aoi)%>% 
  select(-MLF_Kehm_2012)%>% 
  mutate(ECOREGION_NAME = case_when(
    ECOREGION_NAME == "HECATE CONTINENTAL SHELF" ~ "COASTAL GAP",
    ECOREGION_NAME == "INNER PACIFIC SHELF" ~ "COASTAL GAP",
    .default = as.character(ECOREGION_NAME)
  )) %>% 
  select(-ECOREGION_CODE) %>%
  st_union(by_feature = T)
           
ecc <- ec %>% group_by(ECOREGION_NAME)%>%
summarise(.)


ec <- ec %>% 
  mutate(total_area = st_area(.))%>% 
  arrange(total_area)


st_write(ecc , file.path("inputs", "sk_ecoreg_reduced.gpkg"), append = FALSE)







# TAP Old growth 
## intact watersheds - From TAP old growth 
#https://catalogue.data.gov.bc.ca/dataset/b684bdbf-8824-4bc6-8d22-329d9b97c043



# TAP Old growth 
## intact watersheds - From TAP old growth 
#https://catalogue.data.gov.bc.ca/dataset/b684bdbf-8824-4bc6-8d22-329d9b97c043

list.files(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/'))

iw <- rast(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/',"Map6_IntactWS_2021_08_08.tif"))
template <-rast(file.path("inputs", "sk_rast_template.tif"))
iw <- resample(iw, template)
iw_sk <- mask(iw, template)

# MIGHT WANT TO REORDER TO SELECT WHERE vALUE >7 ETC 
writeRaster(iw_sk, file.path("inputs", "TAP_intact_watershed.tif"), overwrite = TRUE)


# new updates
iw <- rast(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/',"Map6_IntactWS_2021_08_08.tif"))
names(iw)<- "ws"

y8 <- ifel(iw == 8, 1, NA)
y9 <- ifel(iw == 9, 1, NA )
y10 <- ifel(iw ==10, 1, NA)
y0 <- ifel(iw == 0, 1, NA)

plot(y0)
plot(y8)
plot(y9)
plot(y10)

# 0, 8, 9, 10

outputs <- file.path("outputs", "final", "sites", "raw_tiffs")
srast <- rast(file.path(outputs, "template_1km.tif"))

common <- y0

# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "ws", touches = TRUE, cover = TRUE)
names(iww)<- "ws"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "ws0"
writeRaster(iww, file.path(outputs, "intactwatershed_0.tif"), overwrite = TRUE)


common <- y8
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "ws", touches = TRUE, cover = TRUE)
names(iww)<- "ws"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "ws8"
writeRaster(iww, file.path(outputs, "intactwatershed_8.tif"), overwrite = TRUE)

common <- y9
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "ws", touches = TRUE, cover = TRUE)
names(iww)<- "ws"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "ws9"
writeRaster(iww, file.path(outputs, "intactwatershed_9.tif"), overwrite = TRUE)


common <- y10
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "ws", touches = TRUE, cover = TRUE)
names(iww)<- "ws"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "ws10"
writeRaster(iww, file.path(outputs, "intactwatershed_10.tif"), overwrite = TRUE)




# BIG TREES ###############################################
outputs <- file.path("outputs", "final", "sites", "raw_tiffs")
srast <- rast(file.path(outputs, "template_1km.tif"))

# note data is in two files (Map2 and Map 3), these are combined to give the catergories of 1-4 in the report. 
# I separated these out into single layers, although they largely overlap so we might not need both sets?


bt<- rast(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/',"Map2_BigTreeOG_2021_10_24.tif"))
bt1 <- rast(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/',"Map3_PriorityBigTreeOG_2021_10_24.tif"))
##bt <- project(bt, template)
##template <-rast(file.path("inputs", "sk_rast_template.tif"))
#bt <- resample(bt, template)
#bt_sk <- mask(bt, template)

#writeRaster(bt_sk , file.path("inputs", "TAP_bigtrees_raw.tif"), overwrite = TRUE)

names(bt)<- "bt"
unique(values(bt))

y3 <- ifel(bt == 3, 1, NA)
y4 <- ifel(bt == 4, 1, NA )
y0 <- ifel(bt == 0, 1, NA)
#y2<- ifel(bt == 2,1 , NA)

plot(y0)
plot(y3)
plot(y4)

# 
# common <- y0
# # convert to poly and back to raster at 1km grid - using cover
# common <- as.polygons(common, digits = 2)
# iww <- terra::rasterize(common, srast, "bt", touches = TRUE, cover = TRUE)
# names(iww)<- "bt"
# iww <- mask(iww ,srast)
# plot(iww)
# iww[iww >= 0.5] <- 1
# iww[iww < 0.5] <- NA
# iww <- mask(iww ,srast)
# names(iww)<- "bt0"
# writeRaster(iww, file.path(outputs, "bigtree_0.tif"), overwrite = TRUE)

common <- y3
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "bt", touches = TRUE, cover = TRUE)
names(iww)<- "bt"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "bt1"
writeRaster(iww, file.path(outputs, "bigtree_1.tif"), overwrite = TRUE)

common <- y4
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "bt", touches = TRUE, cover = TRUE)
names(iww)<- "bt"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "bt2"
writeRaster(iww, file.path(outputs, "bigtree_2.tif"), overwrite = TRUE)


# Priority old growth 

bt1 <- rast(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/',"Map3_PriorityBigTreeOG_2021_10_24.tif"))
names(bt1)<- "bt"
unique(values(bt1))

y3 <- ifel(bt1 == 3, 1, NA)
y4 <- ifel(bt1 == 4, 1, NA )
y0 <- ifel(bt1 == 0, 1, NA)
#y2<- ifel(bt == 2,1 , NA)

plot(y0)
plot(y3)
plot(y4)

common <- y3
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "bt", touches = TRUE, cover = TRUE)
names(iww)<- "bt"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "bt3"
writeRaster(iww, file.path(outputs, "bigtree_3.tif"), overwrite = TRUE)

common <- y4
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "bt", touches = TRUE, cover = TRUE)
names(iww)<- "bt"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "bt4"
writeRaster(iww, file.path(outputs, "bigtree_4.tif"), overwrite = TRUE)




















# ancient forests
bt <- rast(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/',"Map4_Ancient_forest_2021_07_22.tif"))
#bt <- project(bt, template)
#template <-rast(file.path("inputs", "sk_rast_template.tif"))
#bt <- resample(bt, template)
#bt_sk <- mask(bt, template)
#writeRaster(bt_sk , file.path("inputs", "TAP_ancient_forest_raw.tif"), overwrite = TRUE)

names(bt)<- "af"
unique(values(bt))

y2 <- ifel(bt == 2, 1, NA)
y1 <- ifel(bt == 1, 1, NA )

plot(y2)
plot(y1)

common <- y2
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "af", touches = TRUE, cover = TRUE)
names(iww)<- "af"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "af2"
writeRaster(iww, file.path(outputs, "ancientforest_2.tif"), overwrite = TRUE)

common <- y1
# convert to poly and back to raster at 1km grid - using cover
common <- as.polygons(common, digits = 2)
iww <- terra::rasterize(common, srast, "af", touches = TRUE, cover = TRUE)
names(iww)<- "af"
iww <- mask(iww ,srast)
plot(iww)
iww[iww >= 0.5] <- 1
iww[iww < 0.5] <- NA
iww <- mask(iww ,srast)
names(iww)<- "af1"
writeRaster(iww, file.path(outputs, "ancientforest_1.tif"), overwrite = TRUE)







# cumulative effects 
#https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021






# refugia processing: 

ref_path <- file.path("inputs", "Stolar_et_al_2024_CiCP_Zenodo_upload_Version_1.1")
ref <- list.files(ref_path , pattern = "*.tif")

#ref1 <- rast(file.path(ref_path, "2080s_macro_micro_connectivity.tif"))
ref3 <- rast(file.path(ref_path, "2080s_macrorefugia.tif"))
#ref4 <- rast(file.path(ref_path, "Conservation_priorities_2080s.tif"))
ref5 <- rast(file.path(ref_path, "microrefugia.tif"))
#ref6 <- rast(file.path(ref_path, "Restoration_priorities_2080s.tif"))

reff <- ref3

cc <- resample(reff, srast)
mcc <- mask(cc, srast)
writeRaster(mcc, file.path("inputs", "2080s_macrorefugia.tif"), overwrite = TRUE)
plot(mcc)
#rast(c(mcc, base_raster))





#####################
## EAUBC datasets

# EAUBC_rivers = https://catalogue.data.gov.bc.ca/dataset/eaubc-rivers
#https://catalogue.data.gov.bc.ca/dataset/96707e83-bd9a-4230-b5a3-836731fe46aa

rriv <- bcdc_query_geodata("96707e83-bd9a-4230-b5a3-836731fe46aa") |>
  #select( #####) |>
  collect()

rrriv <- sf::st_intersection(rriv,  in_aoi) #%>% 
  select(QUATERNARY_ID)

st_write(rrriv , file.path("inputs", "eaubc_rivers.gpkg"), append = FALSE)

# lakes 
##https://catalogue.data.gov.bc.ca/dataset/eaubc-lakes

rlak <- bcdc_query_geodata("be394666-5850-4951-8c68-724ae7f72017") |>
  #select( #####) |>
    collect()
    
rrrlak <- sf::st_intersection(rlak,  in_aoi) 
st_write(rrrlak , file.path("inputs", "eaubc_lakes.gpkg"), append = FALSE)
    


# create lake density 
# determine percent density per pixel (100m x 100m) 
wl <- st_read(file.path("inputs", "eaubc_lakes.gpkg"))%>% 
  select(WSA_TYPE)

wll <- vect(wl)
tt <- rasterize(wll, srast, cover = TRUE)
tt[is.na(tt)] <- 0 
tt <- mask(tt, srast)
writeRaster(tt, file.path("inputs", "sk_lake_density.tif"), overwrite = TRUE)



## create a lake density by 1km grid 

tt <- rasterize(wll, srast1km, cover = TRUE)
tt[is.na(tt)] <- 0 
tt <- mask(tt,srast1km)
writeRaster(tt, file.path("inputs", "sk_lake_density_1km.tif"), overwrite = TRUE)









# EAUBC freshwater ecoregions 
# https://catalogue.data.gov.bc.ca/dataset/eaubc-freshwater-ecoregion
#https://catalogue.data.gov.bc.ca/dataset/f8c3dc01-0fdc-41ce-a156-cc9cc0a80092

rreg <- bcdc_query_geodata("f8c3dc01-0fdc-41ce-a156-cc9cc0a80092") |>
  collect()

rrrreg <- sf::st_intersection(rreg,  in_aoi) 

st_write(rrrreg, file.path("inputs", "eaubc_reg.gpkg"), append = FALSE)



## wetland % density 

# EAUBC_rivers = https://catalogue.data.gov.bc.ca/dataset/eaubc-rivers
#https://catalogue.data.gov.bc.ca/dataset/96707e83-bd9a-4230-b5a3-836731fe46aa
# 
# ri <- st_read(file.path("inputs", "eaubc_rivers.gpkg"))
# 
# Weaver used the density values of % cover of 1km grid cell 
# 
# very high > 50 
# high 15 - 50%
# moderate = <15% 
# 
# 
# 
# PCT_WETLAND NUMBER_OF_WETLANDS
# 
# FEATURE_AREA_SQM



## generate other features for the landscape resistence
dem <-rast(file.path("inputs", "sk_dem_aoi.tif"))
slope <- terrain(dem, v="slope", neighbors=8, unit="degrees")  
writeRaster(slope, file.path("inputs", "sk_slope_degree.tif"), overwrite = TRUE)



# extract cutblocks and compare to 2021 CE layer 

## 3) Get harvest history and FTEN --------------------------------
  ## URL for warehouse download
  url = "https://catalogue.data.gov.bc.ca/dataset/b1b647a6-f271-42e0-9cd0-89ec24bce9f7"
  
  ## Name relevant columns to extract
  rel_cols = c("HARVEST_YEAR")
  
  ## Name output geopackage
  out_name = "cutblocks.gpkg"
  
  # Uses date filter which filters cutblock ages less than 20 years, or 7305 days
  
  # Uses date filter which filters cutblock ages less than 20 years, or 7305 days
  bcdata::bcdc_query_geodata(record = url, crs = sf::st_crs(in_aoi)$epsg) %>% ## Query dataset
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    bcdata::select(HARVEST_YEAR)%>%
    bcdata::collect() %>% ## Download specified dataset
    dplyr::select(all_of(rel_cols))# %>% ## Select relevant cols if defined
    #{if(nrow(.) > 0) st_intersection(., in_aoi) else .} %>% ## Crop to AOI extent
    #st_write(file.path(out_path, out_name), append = FALSE) ## Write to output file path
  
  # # 4) ften  - Download latest harvest layer
  # 
  # ## URL for warehouse download
  # url = "https://catalogue.data.gov.bc.ca/dataset/cff7b8f7-6897-444f-8c53-4bb93c7e9f8b"
  # 
  # ## Name relevant columns to extract
  # rel_cols = c("HARVEST_AUTH_STATUS_CODE",
  #              "ISSUE_DATE",
  #              "CURRENT_EXPIRY_DATE_CALC",
  #              "LIFE_CYCLE_STATUS_CODE",
  #              "FILE_STATUS_CODE",
  #              "FEATURE_AREA")
  # 
  # ## Name output geopackage
  # out_name = "ften.gpkg"
  # 
  # bcdata::bcdc_query_geodata(record = url, crs = sf::st_crs(in_aoi)$epsg) %>% ## Query dataset
  #   bcdata::filter(INTERSECTS(in_aoi) & ISSUE_DATE > as.Date("2000-01-01")) %>% ## Pull all polygons which intersect with the provided AOI + special filter
  #   bcdata::collect() %>% ## Download specified dataset
  #   ifelse(length(rel_cols) > 0, dplyr::select(., all_of(rel_cols)), .) %>% ## Select relevant cols
  #   {if(nrow(.) > 0) st_intersection(., in_aoi) else .} %>% ## Crop to AOI extent
  #   st_write(file.path(out_path, out_name), append = FALSE) ## Write to output file path
  # 
  # 


  
  # private lands 
# https://catalogue.data.gov.bc.ca/dataset/29a6171a-fab6-4644-b0f7-5d4e466f6837  
  
private <- st_read(file.path('/home/user/Documents/00_data/base_vector/bc/TANTALIS/pmbc_parcel_fabric_poly.gpkg'))
  
priv <- st_intersection(private, in_aoi)
priv <- priv %>% 
  select(PARCEL_CLASS, OWNER_TYPE)


st_write(priv, file.path("inputs", "sk_privateland_raw.gpkg"))



## fine scale intact 

# critcal habitat - federal 
#https://catalogue.data.gov.bc.ca/dataset/critical-habitat-for-federally-listed-species-at-risk-posted-

cri <- bcdc_query_geodata("076b8c98-a3f1-429b-9dae-03faed0c6aef") |>
  select(SCIENTIFIC_NAME, COMMON_NAME_ENGLISH, CRITICAL_HABITAT_STATUS, LAND_TENURE)
  collect()
  
cri <- cri %>%  
  select(SCIENTIFIC_NAME, COMMON_NAME_ENGLISH, CRITICAL_HABITAT_STATUS, LAND_TENURE)%>%
  st_intersection(in_aoi)

st_write(cri, file.path("inputs", "fed_listed_sp_raw.gpkg"))



# wetland density layer 

wl <- st_read("inputs")











#  Species and Ecosystems at Risk - Publicly Available
#https://catalogue.data.gov.bc.ca/dataset/species-and-ecosystems-at-risk-publicly-available-occurrences-cdc

eco <- bcdc_query_geodata("0e035e55-f257-458f-9a96-80c01c69d389") |>
  select(SCI_NAME, ENG_NAME, EL_TYPE, PROV_RANK, BC_LIST, RANK, RANK_DESC) %>%
  collect()

eco <- eco %>%st_intersection(in_aoi)

st_write(eco, file.path("inputs", "bc_cbc_sp_raw.gpkg"))


eco <- eco %>%
  select(SCI_NAME, ENG_NAME, EL_TYPE, PROV_RANK, BC_LIST, RANK, RANK_DESC) %>%
  st_intersection(in_aoi)%>% 
  filter(BC_LIST %in% c("Red", "Blue"))
    
st_write(eco, file.path("inputs", "bc_red_blue_sp_raw.gpkg"))


## might want to further filter the reliability of the area via rqnk description/code/ Need to check with Paula.

sort(unique(eco$ENG_NAME))



# wildlife species inventory publically available 


#Wildlife observations ####################


# 1) Wildlife Species Inventory - Telemetry Points - Publicly Available 
#https://catalogue.data.gov.bc.ca/dataset/6d48657f-ab33-43c5-ad40-09bd56140845
# downloaded directly from website 

ff = "D:\\r_repo\\2024_landscape_facets\\sk_landscapefacets\\inputs\\wildlife_sightings\\BCGW_7113060B_1724310111241_12932\\SPI_TELEMETRY_OBS_NONSENS_SP.gpkg"
sp <- st_read(file.path(ff)) %>%
  select(SCIENTIFIC_NAME, SPECIES_ENGLISH_NAME, OBSERVATION_DATE, VISUAL_FLAG) %>%
  st_intersection(in_aoi)
              
# sp <- bcdc_query_geodata("6d48657f-ab33-43c5-ad40-09bd56140845") %>%
#   select(SCIENTIFIC_NAME, SPECIES_ENGLISH_NAME, OBSERVATION_DATE, VISUAL_FLAG) %>%
#   collect() %>%
#   st_intersection(in_aoi)

unique(sp$SPECIES_ENGLISH_NAME)
st_write(sp, file.path("inputs", "wildlife_telemetry_pts.gpkg"))


# 2) Wildlife survey
#Wildlife Species Inventory Study Areas - All
#https://catalogue.data.gov.bc.ca/dataset/94695736-6a35-4688-bb5f-dca4fc5a23de

ff = "D:\\r_repo\\2024_landscape_facets\\sk_landscapefacets\\inputs\\wildlife_sightings\\BCGW_7113060B_1724310602855_19100\\SPI_STUDY_AREAS_ALL_SP.gpkg"
su <- st_read(file.path(ff)) %>%
#su <- bcdc_query_geodata("94695736-6a35-4688-bb5f-dca4fc5a23de") %>%
#  select(SCIENTIFIC_NAME, SPECIES_ENGLISH_NAME, OBSERVATION_DATE, VISUAL_FLAG) %>%
  st_intersection(in_aoi)

st_write(su, file.path("inputs", "wildlife_surveys.gpkg"))


# 3) Wildlife Species Inventory - Survey Observations - Publicly Available 
#https://catalogue.data.gov.bc.ca/dataset/wildlife-species-inventory-survey-observations-publicly-available

# manual download from internet in pieces and added back together for one dataset 

wilddir <- "inputs\\wildlife_sightings\\wildlife"


nonzip <- grep(list.files(wilddir), pattern=".zip", invert=TRUE, value=TRUE)

# survey incidental 

wl1 <- st_read(file.path(wilddir, nonzip[1],"SPI_INCIDENT_OBS_NONSENS_SP.gpkg"))
wl1 <- wl1 %>% 
  st_intersection(in_aoi)

st_write(wl1, file.path("inputs", "wildlife_incident_obs.gpkg"))




# survey non-sensittive
wl2 <- st_read(file.path(wilddir, nonzip[2],"SPI_SURVEY_OBS_NONSENS_SP.gpkg"))
wl3 <- st_read(file.path(wilddir, nonzip[3],"SPI_SURVEY_OBS_NONSENS_SP.gpkg"))
wl4 <- st_read(file.path(wilddir, nonzip[4],"SPI_SURVEY_OBS_NONSENS_SP.gpkg"))
wl5 <- st_read(file.path(wilddir, nonzip[5],"SPI_SURVEY_OBS_NONSENS_SP.gpkg"))
wl6 <- st_read(file.path(wilddir, nonzip[6],"SPI_SURVEY_OBS_NONSENS_SP.gpkg"))
wl7 <- st_read(file.path(wilddir, nonzip[7],"SPI_SURVEY_OBS_NONSENS_SP.gpkg"))


wl <- rbind(wl2, wl3, wl4, wl5, wl5, wl6, wl7)
wl <- unique(wl)
wl <- wl %>% 
  st_intersection(in_aoi)
 
st_write(wl, file.path("inputs", "wildlife_obs_all.gpkg"))


# length(wl2$SURVEY_OBSERVATION_ID)
# length(wl3$SURVEY_OBSERVATION_ID)
# length(wl4$SURVEY_OBSERVATION_ID)




# bc Known fish locations 
#https://catalogue.data.gov.bc.ca/dataset/aca81811-4b08-4382-9af7-204e0b9d2448
# downloaded directly from website 

fi <- st_read(file.path('/home/user/Documents/00_data/base_vector/bc/BC_fish_obs/FISS_FISH_OBSRVTN_PNT_SP.gpkg'))

#fi <- bcdc_query_geodata("aca81811-4b08-4382-9af7-204e0b9d2448") %>%
  #select(SCIENTIFIC_NAME, SPECIES_ENGLISH_NAME, OBSERVATION_DATE, VISUAL_FLAG) %>%
  #collect() %>%
  
fi <- fi %>% 
    st_intersection(in_aoi)

# subset species?
#fi <- fi |> 
#  select(fid, SPECIES_CODE, SPECIES_NAME, OBSERVATION_DATE, )

#st_write(fi, file.path("inputs", "sk_known_fish_pts.gpkg"))


fi <- st_read(file.path("inputs", "sk_known_fish_pts.gpkg"))

unique(fi$SPECIES_NAME)




## Protected esstuariues 

est <- st_read(file.path("inputs", "Pacific estuary ranking shape files-20240516T174559Z-001",
                         "Pacific estuary ranking shape files","PECP_estuary_polys_ranked_2019_PUBLIC.shp"))

est <- st_intersection(est, in_aoi)

st_write(est, file.path("inputs", "sk_estuary.gpkg"))







## Jokulhaups
gg <- read.csv(file.path("inputs", "glofdatabase_nthAm.csv"))

gg <- gg[,1:58]
gg <- gg[c(-1,-2),]
gg$Lat = as.numeric(gg$Latitude)
gg$Long = as.numeric(gg$Longitude)

gg <- gg %>%
  filter(!is.na(Lat))

ggsf <- st_as_sf(gg, crs = 4326, coords = c("Long", "Lat"))
ggsf <- st_transform(ggsf, 3005)

ggsf <- st_intersection(ggsf, in_aoi)

ggsf
st_write(ggsf, file.path("outputs", "Jokulhaups_sk.gpkg"))









### Pitehr data sets 

con <- rast(file.path("inputs", "Pither_etal", "Current_Density_1_Conductance.tif"))
res <- rast(file.path("inputs", "Pither_etal", "Current_Density_2_Resistance.tif"))
cost <- rast(file.path("inputs", "Pither_etal", "Movement_Cost_Layer.tif"))

template <- rast(file.path("inputs", "sk_rast_template.tif"))

# connectivity 
conn <- project(con, template)
conn <- crop(conn, template)
conn <- mask(conn, template)
  
writeRaster(conn , file.path("inputs", "pither_conductance.tif"), overwrite = TRUE)

# resistance
ress <- project(res, template)
ress<- crop(ress, template)
ress <- mask(ress, template)

writeRaster(ress , file.path("inputs", "pither_resistance.tif"), overwrite = TRUE)


# cost
costt <- project(cost, template)
costt <- crop(costt, template)
costt <- mask(costt, template)

writeRaster(costt , file.path("inputs", "pither_move_cost.tif"), overwrite = TRUE)




#########################################################################

# wildlife inventory 
#https://catalogue.data.gov.bc.ca/dataset/wildlife-species-inventory-telemetry-observations-publicly-available

tele <- bcdc_query_geodata("6d48657f-ab33-43c5-ad40-09bd56140845") |>
  collect()






