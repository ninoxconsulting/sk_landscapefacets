
## ndvi 

##align with template for NDVI 


library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)

template <- rast(file.path("inputs", "sk_rast_template.tif"))
srast = rast(file.path("inputs", "ndvi_2019_2023.tif"))
# 
# # reproj data
# srast <- project(srast, template)
# ndvi <- crop(srast, template)
# 
# writeRaster(ndvi, file.path("inputs", "ndvi_2019_2023_aligned.tif"))


# create a moving window in qgis and then 
## mean 
ndvi <- rast(file.path("outputs", "ndvi_2019_2023_mean_101c.tif"))
ndvi <- mask(ndvi, template)

writeRaster(ndvi, file.path("outputs", "ndvi_2019_2023_mean_101c_clip.tif"))


# read in the average neighbourhood option 


m <- c(-1, 0.58, 0,
       0.58, 0.68, 1,
       0.68, 0.9, 2)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(ndvi , rclmat, include.lowest=TRUE)

writeRaster(rc, file.path("outputs", "ndvi_high_vhigh.tif"))

## max






# threshold at 8.3 and generate polygon 

unique(values(srast))

m <- c(-1, 0.83, 1,
       0.83, 10, 2)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(srast , rclmat, include.lowest=TRUE)

writeRaster(rc, file.path("outputs", "ndvi_201923_8_3_thresh_raw.tif"), overwrite = TRUE)


rc_poly <- as.polygons(rc)

rc_sf <- st_as_sf(rc_poly)
st_write(rc_sf ,file.path( "outputs", "ndvi_201923_8_3_thresh_raw.gpkg"), append = FALSE)
