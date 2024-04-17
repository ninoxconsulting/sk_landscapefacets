# 04. Mapping 

library(bcdata)
library(dplyr)
library(terra)
library(sf)

basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
basedata = "inputs"

in_aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
in_aoi <- st_as_sf(in_aoi)


## 6) Water (Lakes, Rivers, Wetlands) --------------------------------------
get_water <- function(in_aoi) {
  
  message("\rDownloading lake, river, and wetland layers")
  water_records <- c("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", # lakes
                     "f7dac054-efbf-402f-ab62-6fc4b32a619e") # rivers
  
  for (i in 1:length(water_records)) {
    
    waterbodies <- bcdata::bcdc_query_geodata(water_records[i]) %>%
      bcdata::filter(INTERSECTS(in_aoi)) %>%
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
https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021





# refugia processing: 

base_raster <- rast(file.path("outputs", "sk_lf_3005.tif"))

ref_path <- file.path("inputs", "Stolar_et_al_2024_CiCP_Zenodo_upload_Version_1.1")
ref <- list.files(ref_path , pattern = "*.tif")


ref1 <- rast(file.path(ref_path, "microrefugia.tif"))
ref2 <- rast(file.path(ref_path, "2080s_macrorefugia.tif"))



a <- list(ref1)
b <- lapply(a, \(i) {
  x <- extend(rast(base_raster), i)
  resample(i, crop(x, i, "out"))
})




bb <- b[[1]]

#plot(bb)
#b <- sprc(c(b, base_raster))
#m <- merge(b)

#mbb <- mask(bb, base_raster)

e <- extend(bb, base_raster)
mbb <- mask(e, base_raster)


cc <- resample(ref1, base_raster)

rast(c(cc, base_raster))



