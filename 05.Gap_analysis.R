#1. Gap analysis 

# review the proteced areas: 

library(bcdata)
library(dplyr)
library(terra)
library(sf)


## read in diversity layer ? or rarity layer ? 

srast = terra::rast(file.path("outputs","facet_rcd_diversity_101.tif"))


# reclasify rasters 

range(srast$min)

vals <- values(srast$min, mat = F)
svals <- vals[vals>0]

hist(svals)


quantile(svals)
0%  25%  50%  75% 100% 
1   34   51   74  211 


# rock class (n = 19) 
#divr <- srast  %>% 
#  dplyr::mutate(div_class = case_when(
#    count < 12 ~ 1,
#    count > 12 & count < 62 ~ 2,
#    count > 62 & count < 132 ~ 3,
#    count > 132 & count < 268 ~ 4,
#    .default = as.numeric(99)
#  ))



## read in protected layers 

pro <- st_read(file.path("inputs", "protected_lands.gpkg"))
con <- st_read(file.path("inputs", "cons_lands.gpkg"))

## read in ecoregions 

bc_ec <- st_read(file.path("inputs", "bc_ecoreg.gpkg"))





# calculate % protected for all of Skeena


# calculate % protected by type? per ecoregion? 





