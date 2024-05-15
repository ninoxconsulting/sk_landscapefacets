
## ndvi 

##align with template for NDVI 


library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)

template <- rast(file.path("inputs", "sk_rast_template.tif"))

srast = rast(file.path("inputs", "ndvi_skeena_composite.tif"))

# reproj data
srast <- project(srast, template)
ndvi <- crop(srast, template)

writeRaster(ndvi, file.path("inputs", "ndvi_skeena_comp_aligned.tif"))


# create a moving window in qgis and then 

ndvi <- rast(file.path("inputs", "ndvi_sk_mean_101c.tif"))
ndvi <- mask(ndvi, template)

writeRaster(ndvi, file.path("outputs", "ndvi_final.tif"))
