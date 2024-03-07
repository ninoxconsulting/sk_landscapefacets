
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(bcdata)

skeena = rast("skeena_lfacet_3005.tif")

#skeena = rast("skeena_raw_lfacet.tif")

basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
basedata_soil  = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\bc"
aoi <-vect(file.path(basedata, "SkeenaRegionBndry.shp"))
aoi_sf <- st_as_sf(aoi)
#aoil <- st_transform(aoi, 9820)

#PROJCS["NA_Lambert_Azimuthal_Equal_Area",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433],METADATA["World",-180.0,-90.0,180.0,90.0,0.0,0.0174532925199433,0.0,1262]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-100.0],PARAMETER["Latitude_Of_Origin",45.0],UNIT["Meter",1.0]]

# mask the raster by aoi 

srast <- mask(skeena, aoi)

terra::writeRaster(srast, "sk_lf_3005.tif")
unique(srast)


# number of codes within Aoi 
uval = length(unique(values(srast)))


# Summarise values 
routdf <- as.data.frame(srast)

# polt histogram
ggplot2::ggplot(routdf, aes(skeena_lfacet_3005)) +
  ggplot2::geom_histogram(bins = uval) 
  #geom_freqpoly()

# summary of values 

ids = routdf %>% 
  group_by(skeena_lfacet_3005)%>%
  summarise(count = n())%>%
  mutate(total = sum(count))%>%
  rowwise() %>%
  mutate(prop = (count/total)*100)


# download soils data type 
bcdc_search("rock")


rocks <- bcdc_query_geodata("ef8476ed-b02d-4f5c-b778-0d44c9126144") |>
  filter(INTERSECTS(aoi_sf)) |> 
  select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS) |> 
  collect()

# or read in downloaded datset 


rocks  <-st_read(file.path(basedata_soil, "BC_digital_geology_gpkg", "BC_digital_geology.gpkg"))
rocks  <- rocks %>%
select(unit_desc,rock_type,rock_code,rock_class)  
aoi_sf <- st_as_sf(aoi)


https://catalogue.data.gov.bc.ca/dataset/2dc8a4f1-c8f8-4603-813e-855af99b7ba5/resource/51f0620a-42cf-458f-821c-bdd895f24fdc



         