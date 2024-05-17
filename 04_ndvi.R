
## ndvi 

##align with template for NDVI 


library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(ggspatial)
library(tidyterra)
library(cowplot)


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


# Josh starts here
# read in raster
ndvi_2019 <- rast(file.path("..", "inputs", "ndvi_skeena_2019.tif"))
ndvi_2020 <- rast(file.path("..", "inputs", "ndvi_skeena_2020.tif"))
ndvi_2021 <- rast(file.path("..", "inputs", "ndvi_skeena_2021.tif"))
ndvi_2022 <- rast(file.path("..", "inputs", "ndvi_skeena_2022.tif"))
ndvi_2023 <- rast(file.path("..", "inputs", "ndvi_skeena_2023.tif"))

npp <- rast(file.path("..", "inputs", "npp_skeena_2019_2023.tif"))
template <- rast(file.path("..", "inputs", "sk_rast_template.tif"))

# resample ndvi to the template
sprc <- sprc(ndvi_2019, ndvi_2020, ndvi_2021, ndvi_2022, ndvi_2023, npp)

sprc <- impose(sprc, template)

npp <-  sprc[[6]]
sprc <- sprc[[1:5]]

# take the median of the ndvi 
median <- app(sprc, fun="median")


rescale <- function(x) {
  
  (x - global(x, "min", na.rm = T) %>% pull()) / 
  (global(x, "max", na.rm = T) %>% pull() - global(x, "min", na.rm = T) %>% pull)
  
}

ndvi_scale <- rescale(median)

# write out the median and scaled products 

writeRaster(median, file.path("..", "primary_productivity", "ndvi_2019_2023.tif"))

writeRaster(ndvi_scale, file.path("..", "primary_productivity", "ndvi_2019_2023_scaled.tif"))

npp_scale <- rescale(npp)

# I am going to stretch the npp layer 
npp_stretch <- stretch(npp_scale, minq = 0.97, maxq = 1)

npp_stretch <- rescale(npp_stretch)

npp_stretch <- subst(npp_stretch, NA, 0)

added <- rescale(npp_stretch + ndvi_scale)


ndvi <- ggplot() +
  geom_spatraster(data = ndvi_scale) +
  # Use the palette you like, in this case:
  scale_fill_whitebox_c( palette = "viridi") +
  theme_void() +
  labs(title = "NDVI")

npp <- ggplot() +
  geom_spatraster(data = npp_scale) +
  # Use the palette you like, in this case:
  scale_fill_whitebox_c( palette = "viridi") +
  theme_void() +
  labs(title = "NPP")


npp_stretch <- ggplot() +
  geom_spatraster(data = npp_stretch) +
  # Use the palette you like, in this case:
  scale_fill_whitebox_c( palette = "viridi") +
  theme_void() +
  labs(title = "NPP Stretched (0.97%)")


add <- ggplot() +
  geom_spatraster(data = added) +
  # Use the palette you like, in this case:
  scale_fill_whitebox_c( palette = "viridi") +
  theme_void() +
  labs(title = "NDVI + NPP")

cowplot::plot_grid(ndvi, npp, npp_stretch, add, align = "hv")

# writing out the added product
writeRaster(added, file.path("..", "primary_productivity", "ndvi_npp_scaled_2019_2023.tif"), overwrite = T)

rstudio.prefs::use_rstudio_prefs(editor_theme = 'Idle Fingers', indend_guides = 'rainbowlines')

