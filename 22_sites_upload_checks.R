## Checks for sites upload 

## load packages
library(sf)
library(terra)
library(dplyr)

#outputs <- file.path("outputs", "final", "sites", "raw_tiffs")
outputs <- file.path("outputs", "final", "sites_202505", "raw_tiffs")

srast <- rast(file.path(outputs, "template_1km.tif"))


# regional_focal_species

rout <- file.path("outputs", "final", "sites_202505","Regional","Themes", "regional_focal_species")
lists <- list.files(rout, full.names = TRUE)

purrr::map(lists , function(x){
  print(x)
  # x <- lists[1]
  tt <- rast(x)
  names(tt)
  unique(values(tt))

})






#terrestrial 

