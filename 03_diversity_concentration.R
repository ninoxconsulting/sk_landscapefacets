#1. Gap analysis 

# review the proteced areas: 

library(bcdata)
library(dplyr)
library(terra)
library(sf)

# read in study area 

#basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
#aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
#aoi_sf <- st_as_sf(aoi)

temp <- rast(file.path("inputs", "sk_rast_template.tif"))
#aoi <- vect(file.path("inputs", "template_poly.gpkg"))


## clip the neighbourhood analysis output 
# srast = terra::rast(file.path("outputs","sk_lf_rdc_diversity_101c.tif"))
# names(srast)= "diversity"
# 
# # mask the raster by aoi and clip and then export
# srast <- mask(srast, temp)
# writeRaster(srast, file.path("outputs","sk_lf_rdc_diversity_101c_clip.tif"))

srast = rast(file.path("outputs","sk_lf_rdc_diversity_101c_clip.tif"))


hist(srast$diversity)

# reclasify rasters 
#range(srast$diversity)
#head(srast )
vals <- values(srast$diversity, mat = F)
svals <- vals[vals>0]

hist(svals)

# # estimate quantiles 
 quantile(svals, probs = seq(0, 1, 0.05), na.rm = TRUE)
# 
# 0%   5%  10%  15%  20%  25%  30%  35%  40%  45%  50%  55%  60%  65%  70%  75%  80%  85%  90%  95% 100% 
#  8   23   28   32   36   39   42   45   48   50   53   57   60   64   68   73   78   84   93  104  192 



### reclass the raster 

## from-to-becomes
# classify the values into groups 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(0, 20, 1, # lowest diversity 
       20, 40, 2,
       40, 80, 3,
       80, 150 , 4,
       150, 300, 5) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)
div_con <- classify(srast, rclmat, include.lowest=TRUE)


writeRaster(div_con, file.path("outputs", "sk_diveristy_conc.tif"), overwrite = TRUE)





