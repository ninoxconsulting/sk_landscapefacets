#1. Gap analysis 

# review the proteced areas: 


library(bcdata)

basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"

in_aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
in_aoi <- st_as_sf(in_aoi)

# extract protected areas 
#https://catalogue.data.gov.bc.ca/dataset/1130248f-f1a3-4956-8b2e-38d29d3e4af7

pro <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7") |>
# filter(INTERSECTS(aoi_sf)) |> 
#  select(PROTECTED_LANDS_NAME, PROTECTED_LANDS_CODE, PROTECTED_LANDS_DESIGNATION) |>
  collect()

pro <- sf::st_intersection(pro, aoi_sf)|> 
    select(PROTECTED_LANDS_NAME, PROTECTED_LANDS_CODE, PROTECTED_LANDS_DESIGNATION)

st_write(pro, file.path("inputs", "protected_lands.gpkg"))



# this might be a duplicate? 
# conservation lands: 
#https://catalogue.data.gov.bc.ca/dataset/68327529-c0d5-4fcb-b84e-f8d98a7f8612


cons <- bcdc_query_geodata("68327529-c0d5-4fcb-b84e-f8d98a7f8612") |>
# filter(INTERSECTS(aoi_sf)) |> 
#  select(SITE_NAME, CONSERVATION_LAND_TYPE, TENURE_TYPE,TENURE_DESCRIPTION) |>
  collect()

cons <- sf::st_intersection(cons, aoi_sf)|>   
  select(SITE_NAME, CONSERVATION_LAND_TYPE, TENURE_TYPE,TENURE_DESCRIPTION) 

st_write(cons, file.path("inputs", "cons_lands.gpkg"))



# cumulative effects 
https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021



## landscape Units
#https://catalogue.data.gov.bc.ca/dataset/11277e35-d8be-47e4-bb1f-c38e393179c6
#lu <- bcdc_query_geodata("11277e35-d8be-47e4-bb1f-c38e393179c6") |>
  #filter(INTERSECTS(aoi_sf)) |> 
 # select(PROTECTED_LANDS_NAME, PROTECTED_LANDS_CODE, PROTECTED_LANDS_DESIGNATION) |>
#  collect()




# quarternary sedimte?
https://catalogue.data.gov.bc.ca/dataset/b3f58ed8-376f-4962-9657-36297a5f41cf
# binary 


# ecoregions 
#https://catalogue.data.gov.bc.ca/dataset/d00389e0-66da-4895-bd56-39a0dd64aa78

ec <- bcdc_query_geodata("d00389e0-66da-4895-bd56-39a0dd64aa78") |>
  collect()

ec <- sf::st_intersection(ec, aoi_sf)

