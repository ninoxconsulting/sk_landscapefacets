## Checks for sites upload 

## load packages
library(sf)
library(terra)
library(dplyr)

#outputs <- file.path("outputs", "final", "sites", "raw_tiffs")
outputs <- file.path("outputs", "final", "sites_202505", "raw_tiffs")

srast <- rast(file.path(outputs, "template_1km.tif"))


# fix iba_cover 
# convert lower to 0 


list.files(outputs, full.names = TRUE)








# regional_focal_species

rout <- file.path("outputs", "final", "sites_202505","Regional","Themes", "regional_focal_species")
v
purrr::map(lists , function(x){
  print(x)
   x <- lists[4]
  tt <- rast(x)
  tt[is.na(tt)]<- 0
  tt <- mask(tt, srast)
  writeRaster(tt, filename = file.path(outputs, "iba_cover.tif"), overwrite = TRUE)
  
  names(tt)
  unique(values(tt))

})



#terrestrial 

rout <- file.path("outputs", "final", "sites_202505","Regional","Weights")
lists <- list.files(rout, full.names = TRUE)

aa <- purrr::map(lists , function(x){
  print(x)
  # x <- lists[1]
  tt <- rast(x)
  names(tt)
  unique(values(tt))
  
})

# weights
rout <- file.path("outputs", "final", "sites_202505","Regional","Weights")
