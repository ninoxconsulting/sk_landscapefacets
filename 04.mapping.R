# 04. Mapping 

library(bcdata)

basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"

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