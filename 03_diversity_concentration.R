#1. Gap analysis 

# review the proteced areas: 

library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)

srast = rast(file.path("outputs", "sk_lf_rdc_diversity_101c.tif"))
names(srast)= "diversity"
hist(srast$diversity)

# read in study area 

temp <- rast(file.path("inputs", "sk_rast_template.tif"))
aoi <- st_read(file.path("inputs", "sk_poly_template.gpkg"))


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
m <- c(0, 50, 1, # lowest diversity 
       50, 100, 2,
       100, 150, 3,
       150, 250 , 4,
       250, 700, 5) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)
div_con <- classify(srast, rclmat, include.lowest=TRUE)


writeRaster(div_con, file.path("outputs", "sk_diversity_conc.tif"), overwrite = TRUE)





