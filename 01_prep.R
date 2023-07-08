
library(terra)
library(sf)
library(ggplot2)
library(dplyr)

skeena = rast("skeena_lfacet_3005.tif")

#skeena = rast("skeena_raw_lfacet.tif")

basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"

aoi <-vect(file.path(basedata, "SkeenaRegionBndry.shp"))
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


         