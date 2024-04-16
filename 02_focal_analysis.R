#02. Focal_mean

#install.packages("landscapemetrics")
#library(landscapemetrics)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)

srast = terra::rast(file.path("inputs","sk_lf_3005.tif"))

bbox(srast)

#r <- rast(ncols=10, nrows=10, ext(0, 10, 0, 10))
#values(r) <- 1:50
#plot(r)


# generate focal statistics which show the number of unique facet types. 
#f <- focal(r, w=3, fun=function(x, ...) n_distinct(x, ...), na.rm=TRUE) 

#f3 = f 
library(tictoc)

tic()
f3 <- focal(srast, w = 3, fun = function(x, ...) n_distinct(x, ...), na.rm=TRUE) 
toc()
