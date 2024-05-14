# 04. Mapping 

library(bcdata)
library(dplyr)
library(terra)
library(sf)

## note templates are generated in scripr : 01_prep..

srast <- rast(file.path("inputs", "sk_rast_template.tif"))

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
# wetlands <- bcdc_query_geodata("93b413d8-1840-4770-9629-641d74bd1cc6") |>
#   # filter(INTERSECTS(aoi_sf)) |> 
#   select( WATERBODY_TYPE, AREA_HA) |>
#   collect()
# 
# wetlands <- sf::st_intersection(wetlands, in_aoi) #|> 
#       #select(WATERBODY_TYPE, AREA_HA)
# 
# # select area > xxxx 
# 
# st_write(wetlands, file.path("inputs", "wetlands.gpkg"), append = FALSE)



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
  st_intersection(in_aoi)

st_write(ec , file.path("inputs", "sk_ecoreg.gpkg"), append = FALSE)



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


# BIG TREES 
bt<- rast(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/',"Map2_BigTreeOG_2021_10_24.tif"))
#bt <- project(bt, template)
#template <-rast(file.path("inputs", "sk_rast_template.tif"))
bt <- resample(bt, template)
bt_sk <- mask(bt, template)

writeRaster(bt_sk , file.path("inputs", "TAP_bigtrees_raw.tif"), overwrite = TRUE)

# ancient forests
bt <- rast(file.path('/home/user/Documents/00_data/base_vector/bc/BC_TAP_Forestry_data/BCVW_tap_watewrshed_data/',"Map4_Ancient_forest_2021_07_22.tif"))
#bt <- project(bt, template)
#template <-rast(file.path("inputs", "sk_rast_template.tif"))
bt <- resample(bt, template)
bt_sk <- mask(bt, template)

writeRaster(bt_sk , file.path("inputs", "TAP_ancient_forest_raw.tif"), overwrite = TRUE)







# cumulative effects 
#https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021






# refugia processing: 

ref_path <- file.path("inputs", "Stolar_et_al_2024_CiCP_Zenodo_upload_Version_1.1")
ref <- list.files(ref_path , pattern = "*.tif")

ref1 <- rast(file.path(ref_path, "2080s_macro_micro_connectivity.tif"))
ref3 <- rast(file.path(ref_path, "2080s_macrorefugia.tif"))
ref4 <- rast(file.path(ref_path, "Conservation_priorities_2080s.tif"))
ref5 <- rast(file.path(ref_path, "microrefugia.tif"))
ref6 <- rast(file.path(ref_path, "Restoration_priorities_2080s.tif"))

reff <- ref5

cc <- resample(reff, srast)
mcc <- mask(cc, srast)
writeRaster(mcc, file.path("inputs", "microrefugia.tif"), overwrite = TRUE)
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
    


# EAUBC freshwater ecoregions 
# https://catalogue.data.gov.bc.ca/dataset/eaubc-freshwater-ecoregion
#https://catalogue.data.gov.bc.ca/dataset/f8c3dc01-0fdc-41ce-a156-cc9cc0a80092

rreg <- bcdc_query_geodata("f8c3dc01-0fdc-41ce-a156-cc9cc0a80092") |>
  collect()

rrrreg <- sf::st_intersection(rreg,  in_aoi) 

st_write(rrrreg, file.path("inputs", "eaubc_reg.gpkg"), append = FALSE)





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






#  Species and Ecosystems at Risk - Publicly Available
#https://catalogue.data.gov.bc.ca/dataset/species-and-ecosystems-at-risk-publicly-available-occurrences-cdc

eco <- bcdc_query_geodata("0e035e55-f257-458f-9a96-80c01c69d389") |>
  select(SCI_NAME, ENG_NAME, EL_TYPE, PROV_RANK, BC_LIST, RANK, RANK_DESC) %>%
  collect()

eco <- eco %>%
  select(SCI_NAME, ENG_NAME, EL_TYPE, PROV_RANK, BC_LIST, RANK, RANK_DESC) %>%
  st_intersection(in_aoi)%>% 
  filter(BC_LIST %in% c("Red", "Blue"))
    
st_write(eco, file.path("inputs", "bc_red_blue_sp_raw.gpkg"))


## might want to further filter the reliability of the area via rqnk description/code/ Need to check with Paula.
