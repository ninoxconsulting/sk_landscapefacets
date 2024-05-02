# 04. Mapping 

library(bcdata)
library(dplyr)
library(terra)
library(sf)

#basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
basedata = "inputs"


#in_aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
in_aoi <- vect(file.path(basedata, "SkeenaRegionBndry.gpkg"))
template_poly <- st_read(file.path(basedata, "template_poly.gpkg"))

in_aoi <- st_as_sf(in_aoi)


## 6) Water (Lakes, Rivers, Wetlands) --------------------------------------
get_water <- function(in_aoi) {
  
  message("\rDownloading lake, river, and wetland layers")
  water_records <- c("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6")#, # lakes
                    # "f7dac054-efbf-402f-ab62-6fc4b32a619e") # rivers
  
  for (i in 1:length(water_records)) {
    
    i = 1
    waterbodies <- bcdata::bcdc_query_geodata(water_records[i]) %>%
      #bcdata::filter(INTERSECTS(in_aoi)) %>%
      bcdata::collect()
    
    if(nrow(waterbodies) > 0) {
      waterbodies <- sf::st_intersection(waterbodies, in_aoi)
      
      waterbodies_sf <- waterbodies %>%
        dplyr::select(id, WATERBODY_TYPE, AREA_HA)
      
      if (i == 1) {
        all_water <- waterbodies_sf } else {
          all_water <- rbind(all_water, waterbodies_sf)
        }
    }
  }
  return(all_water)
  #st_write(all_water, file.path(out_path, "water.gpkg"), append = FALSE)
}

w <- get_water(in_aoi)

#6ff1809a-f7cd-4641-abc5-9740f60a6d52


# lakes
lakes <- bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6") |>
  # filter(INTERSECTS(aoi_sf)) |> 
  select( WATERBODY_TYPE, AREA_HA) |>
  collect()

lakes <- sf::st_intersection(lakes, in_aoi) |> 
  select( WATERBODY_TYPE, AREA_HA) 

st_write(lakes, file.path("inputs", "lakes.gpkg"), append = FALSE)



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
  #  select(PROTECTED_LANDS_NAME, PROTECTED_LANDS_CODE, PROTECTED_LANDS_DESIGNATION) |>
  collect()

glac <- sf::st_intersection(glac, in_aoi)
  select(WATERBODY_TYPE, AREA_HA)

st_write(glac, file.path("inputs", "glaciers.gpkg"), append = FALSE)









# lakes
#https://catalogue.data.gov.bc.ca/dataset/b3f58ed8-376f-4962-9657-36297a5f41cf
# binary 
lakes <- bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6") |>
  select(WATERBODY_TYPE, AREA_HA) %>% 
  collect()

lakes <- sf::st_intersection(lakes ,template_poly) %>% 
  select(WATERBODY_TYPE, AREA_HA) %>% 
  filter(AREA_HA > 1) 


st_write(lakes , file.path("inputs", "lakes.gpkg"), append = FALSE)











# extract protected areas 
#https://catalogue.data.gov.bc.ca/dataset/1130248f-f1a3-4956-8b2e-38d29d3e4af7

pro <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7") |>
  # filter(INTERSECTS(aoi_sf)) |> 
  #  select(PROTECTED_LANDS_NAME, PROTECTED_LANDS_CODE, PROTECTED_LANDS_DESIGNATION) |>
  collect()

pro <- sf::st_intersection(pro, in_aoi)|> 
  select(PROTECTED_LANDS_NAME, PROTECTED_LANDS_CODE, PROTECTED_LANDS_DESIGNATION)

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







# quarternary sedimte?
#https://catalogue.data.gov.bc.ca/dataset/b3f58ed8-376f-4962-9657-36297a5f41cf
# binary 
sed <- bcdc_query_geodata("b3f58ed8-376f-4962-9657-36297a5f41cf") |>
  collect()
sed <- sf::st_intersection(sed,  in_aoi) %>% 
  select(QUATERNARY_ID)

st_write(sed , file.path("inputs", "quat_sed.gpkg"), append = FALSE)




# ecoregions 
#https://catalogue.data.gov.bc.ca/dataset/d00389e0-66da-4895-bd56-39a0dd64aa78

ec <- bcdc_query_geodata("d00389e0-66da-4895-bd56-39a0dd64aa78") |>
  collect()

st_write(ec , file.path("inputs", "bc_ecoreg.gpkg"), append = FALSE)

ec <- sf::st_intersection(ec, in_aoi)%>% 
  select(ECOREGION_CODE,  ECOREGION_NAME)

st_write(ec , file.path("inputs", "sk_ecoreg.gpkg"), append = FALSE)






# cumulative effects 
#https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021





# refugia processing: 

base_raster <- rast(file.path("outputs", "sk_lf_3005.tif"))

ref_path <- file.path("inputs", "Stolar_et_al_2024_CiCP_Zenodo_upload_Version_1.1")
ref <- list.files(ref_path , pattern = "*.tif")

ref1 <- rast(file.path(ref_path, "2080s_macro_micro_connectivity.tif"))
ref3 <- rast(file.path(ref_path, "2080s_macrorefugia.tif"))
ref4 <- rast(file.path(ref_path, "Conservation_priorities_2080s.tif"))
ref5 <- rast(file.path(ref_path, "microrefugia.tif"))
ref6 <- rast(file.path(ref_path, "Restoration_priorities_2080s.tif"))

reff <- ref6

cc <- resample(reff, base_raster)
mcc <- mask(cc, base_raster)
writeRaster(mcc, file.path("inputs", "Restoration_priorities_2080s.tif"))

plot(mcc)
#rast(c(mcc, base_raster))




## DEM 

dem <- rast(file.path("inputs", "dem_sk_raw.tif"))
template <-rast(file.path("outputs", "sk_lf_rdc_rarity_101c_clip.tif"))

# resample 
demf <- resample(dem, template)

writeRaster(demf, file.path("inputs", "raw_sk_dem.tif"), overwrite = TRUE)

hist(demf)


# classify the values into groups based on the values 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(-1, 200, 1, # lowest diversity 
       200, 500, 2,
       500, 750, 3,
       750, 1000 , 4,
       1000, 1250, 5,
       1250, 1500, 6, 
       1500, 2000, 7,
       2000, 2500, 8,
       2500, 3000 , 9,
       3000, 5000, 10) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)
dem_class<- classify(demf, rclmat, include.lowest=TRUE)

writeRaster(dem_class, file.path("inputs", "sk_dem_class.tif"), overwrite = TRUE)










