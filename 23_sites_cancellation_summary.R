### Post sites analysis 

# intersect

library(dplyr)
library(terra)
library(sf)


# read in the datasets

outputs <- file.path("outputs", "final", "sites_202505", "raw_tiffs")

srast <- srast <- rast(file.path(outputs, "template_1km.tif")) # new 1km raster with coastline updata


# Load the sites cancellation data: 
# Conservation Lands - current (not_cancelled_lands_0.2.tif)
# Conservation Lands - historic (Cancelled_lands_0.2.tif)
# Crown lands (crown_lands_cover.tif ) 

conscur <- file.path("outputs", "final", "sites_202505", "raw_tiffs", "not_cancelled_lands_0.2.tif")
conshist <- file.path("outputs", "final", "sites_202505", "raw_tiffs", "cancelled_lands_0.2.tif")
crlds <- file.path("outputs", "final", "sites_202505", "raw_tiffs", "crown_lands_cover.tif")



# load the scenarios: 

# Combine all the 8 scenarios (as binary) - Paula 

# Overlay the output scenarios with the (colour coded 0-8 with scenarios), 
# Output 
# (table) Name , Max Values 
# Spatial (raster) with number values. (Not necessary) 
