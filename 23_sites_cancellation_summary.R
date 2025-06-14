### Post sites analysis 

# intersect

library(dplyr)
library(terra)
library(sf)
library(readr)


# read in the datasets

outputs <- file.path("outputs", "final", "sites_202505", "raw_tiffs")
sc_dir <- file.path("outputs", "final", "sites_202505", "scenarios")
sc_dir_out <- file.path("outputs", "final", "sites_202505", "scenario_outputs")

srast <- srast <- rast(file.path(outputs, "template_1km.tif")) # new 1km raster with coastline updata


# Load the sites cancellation data: 
# Conservation Lands - current (not_cancelled_lands_0.2.tif)
# Conservation Lands - historic (Cancelled_lands_0.2.tif)
# Crown lands (crown_lands_cover.tif ) 

conscur <- file.path("outputs", "final", "sites_202505", "raw_tiffs", "not_cancelled_lands_0.2.tif")
conshist <- file.path("outputs", "final", "sites_202505", "raw_tiffs", "cancelled_lands_0.2.tif")
crlds <- file.path("outputs", "final", "sites_202505", "raw_tiffs", "crown_lands_cover.tif")



# load the scenarios: 

# Combine all the 8 scenarios (as binary) - Paula 
scs <- rast(list.files(sc_dir, full.names = TRUE))


out <- scs[[1]] + scs[[2]] + scs[[3]] + scs[[4]] +
  scs[[5]] + scs[[6]] + scs[[7]] + scs[[8]] + 
  scs[[9]] + scs[[10]]

names(out) = "scenarios_combined"

writeRaster(out, 
            file.path(sc_dir_out, "scenarios_combined.tif"), 
            overwrite = TRUE)

# convert to polygons

outv <- as.polygons(out)
outsf <- st_as_sf(outv) 
outsf <- st_transform(outsf, 3005)


# Save the scenarios as polygons
st_write(outsf, 
         file.path(sc_dir_out, "scenarios_combined.gpkg"), 
         delete_dsn = TRUE)




# Overlay the output scenarios with the (colour coded 0-8 with scenarios), 
# Output 
# (table) Name , Max Values 
# Spatial (raster) with number values. (Not necessary) 


canc <- st_read(file.path("inputs", "cancelled_lands_final.gpkg")) |>
  mutate("id" = row_number())
  
 # dplyr::select(cancelled_status,  SITE_NAME, SITE_NUMBE, TANTALIS_L)

can_sc <- st_intersection(canc, outsf) 
#can_sc <-st_make_valid(can_sc) # make sure the geometries are valid

st_write(can_sc, 
         file.path(sc_dir_out, "cancelled_lands_scenarios.gpkg"), 
         append = FALSE)  
  
  
# max sum

sums <- can_sc |> 
  group_by(id) |>  
  summarise(max_scenario = max(scenarios_combined, na.rm = TRUE)) %>% 
  st_drop_geometry() |> 
  ungroup()

canc_max <- left_join(canc, sums, by = "id") |> 
  st_drop_geometry()

write_csv(canc_max, 
         file.path(sc_dir_out, "cancelled_lands_scenarios_max_value.csv"))



## Repeat with Crown lands


cls <- st_read(file.path("inputs", "crown_lands_filtered.gpkg")) |> 
  mutate("id" = row_number())

#278 number of rows

cls_sc <- st_intersection(cls, outsf) 
# intersect with scenarios

st_write(cls_sc , 
         file.path(sc_dir_out, "crown_lands_scenarios.gpkg"), 
         append = FALSE)  


# max sum

sums_cls <- cls_sc |> 
  group_by(id) |>  
  summarise(max_scenario = max(scenarios_combined, na.rm = TRUE)) %>% 
  st_drop_geometry() |> 
  ungroup()

# 226 number of rows some are outside the study area 


cls_max <- left_join(cls, sums_cls, by = "id")# |> 
 # st_drop_geometry()

st_write(cls_max, 
          file.path(sc_dir_out, "crown_lands_scenarios_max_value.gpkg"), 
          append = FALSE)

length(cls_max$id) #278 


cls_max_df <- cls_max |> 
  st_drop_geometry() 

write_csv(cls_max_df , 
          file.path(sc_dir_out, "crown_lands_scenarios_max_value.csv"))




# Old code to convert cancelled lands to raster
  
# cancc <- canc %>% dplyr::select(cancelled_status) |> 
#   filter(cancelled_status == "cancelled") 
# pro <- rasterize(cancc , srast,touches = TRUE, cover = TRUE)
# names(pro) = "cancelled_lands"
# pro[is.na(pro)] <- 0
# pro <- mask(pro,srast)
# writeRaster(pro, file.path(outputs, "cancelled_lands_cover.tif"), overwrite = TRUE)
# 
# pro <- rast(file.path(outputs, "cancelled_lands_cover.tif"))
# pro[pro >= 0.2] <- 1
# pro[pro < 0.2] <- 0
# pro[is.na(pro)] <- 0
# pro <- mask(pro,srast)
# names(pro) <- "Historic conservation lands"
# writeRaster(pro  , file.path(outputs, "cancelled_lands_0.2.tif"), overwrite=TRUE)

# 
# cancnc <- canc %>% dplyr::select(cancelled_status) |> 
#   filter(cancelled_status == "not_cancelled") 
# 
# pro <- rasterize(cancnc, srast,touches = TRUE, cover = TRUE)
# names(pro) = "not_cancelled_lands"
# pro[is.na(pro)] <- 0
# pro <- mask(pro,srast)
# writeRaster(pro, file.path(outputs, "not_cancelled_lands_cover.tif"), overwrite = TRUE)
# pro <- rast(file.path(outputs, "not_cancelled_lands_cover.tif"))
# pro[pro >= 0.2] <- 1
# pro[pro < 0.2] <- 0
# pro[is.na(pro)] <- 0
# names(pro) <- "Current conservation lands"
# pro <- mask(pro,srast)
# writeRaster(pro  , file.path(outputs, "not_cancelled_lands_0.2.tif"), overwrite=TRUE)










