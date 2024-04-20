#1. Gap analysis 

# review the proteced areas: 

library(bcdata)
library(dplyr)
library(terra)
library(sf)

# read in study area 

basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"

aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
aoi_sf <- st_as_sf(aoi)



## read in diversity layer ? or rarity layer ? 

srast = terra::rast(file.path("outputs","facet_rcd_diversity_101c.tif"))
names(srast)= "diversity"

# mask the raster by aoi 
srast <- mask(srast, aoi)

hist(srast$diversity)


# reclasify rasters 
#range(srast$diversity)
#head(srast )
vals <- values(srast$diversity, mat = F)
svals <- vals[vals>0]

hist(svals)

# # estimate quantiles 
# quantile(svals, probs = seq(0, 1, 0.05), na.rm = FALSE)
# 
# 0%   5%  10%  15%  20%  25%  30%  35%  40%  45%  50%  55%  60%  65% 
#   1    7    8    9   10   10   11   12   12   13   13   14   15   15 
# 70%  75%  80%  85%  90%  95% 100% 
# 16   17   18   20   22   25   68 



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





