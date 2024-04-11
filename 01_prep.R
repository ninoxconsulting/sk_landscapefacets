#ibrary(googlesheets4)
#library(googledrive)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(bcdata)
library(readr)
library(janitor)
library(readxl)

# 
# # location of shared files: 
# #
# # https://drive.google.com/drive/u/0/folders/1jlWXc7PibTohHOl-s-rUIK1UuEH7jpVY
# #
# # To authorize the googlesheets download, set your google auth email with:
# # options(
# #   gargle_oauth_email = "email.which.gives.you.access.to.these.files@gmail.com"
# # )
# # If this is different from your normal google auth email you can add this to a
# # project-specific .Rprofile file to cache
# 
# 
# # 1. check if file is on google drive 
# 
# # get list of id files in shared google drive: 
# bfiles <- drive_ls(as_id("1jlWXc7PibTohHOl-s-rUIK1UuEH7jpVY"))
# 
# bname <- "SkeenaRegionBndry.gpkg"
# 
# # check if thefile is downloaded
# 
# #drive_download(as_id(bboi$id[1]), path = file.path("fitted_models", bboi$name[1]))
# 
# 
# # check if local copy exists
# dlfiles <- list.files(path = "inputs")
# 
# 
# 
# if(bname %in% dlfiles){
#   print("file already downloaded")
# } else {
#   print("downloading file - hang tight")
#   
#   # select the first file and download a local copy
#   
#   bboi <- bfiles %>%
#     filter(name %in% bname)
#   
#   drive_download(as_id(bboi$id[1]), path = file.path("inputs", bboi$name[1]))
#   
# }
# 
# aa <- sf::st_read("inputs", bname)
# 




# Read in the skeena facets 

skeena = rast(file.path("inputs", "skeena_lfacet_3005.tif"))

#skeena = rast("skeena_raw_lfacet.tif")

# or use the base data on google drive 

basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
basedata_soil  = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\bc"

# keep as vect to use terra
aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
aoi_sf <- st_as_sf(aoi)


#PROJCS["NA_Lambert_Azimuthal_Equal_Area",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433],METADATA["World",-180.0,-90.0,180.0,90.0,0.0,0.0174532925199433,0.0,1262]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-100.0],PARAMETER["Latitude_Of_Origin",45.0],UNIT["Meter",1.0]]

# mask the raster by aoi 

srast <- mask(skeena, aoi)





# update the bedrock layer based on geology file 

# # download soils data type 
# bcdc_search("rock")
# 
# rocks <- bcdc_query_geodata("ef8476ed-b02d-4f5c-b778-0d44c9126144") |>
#   filter(INTERSECTS(aoi_sf)) |> 
#   select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS) |> 
#   collect()

# or read in downloaded datset 


rocks  <- st_read(file.path(basedata_soil, "BC_digital_geology_gpkg", "BC_digital_geology.gpkg"), layer = "Bedrock_ll83_poly")

#st_layers(file.path(basedata_soil, "BC_digital_geology_gpkg", "BC_digital_geology.gpkg"))

rocks  <- rocks %>%
  select(unit_desc, rock_type, rock_code, rock_class) %>%
  st_transform(3005)

skrocks <- st_intersection(rocks, aoi_sf)
#st_write(skrocks, file.path("inputs", "sk_bedrock_raw.gpkg"))
skrocks <- st_read(file.path("inputs", "sk_bedrock_raw.gpkg"))

#https://catalogue.data.gov.bc.ca/dataset/2dc8a4f1-c8f8-4603-813e-855af99b7ba5/resource/51f0620a-42cf-458f-821c-bdd895f24fdc

lsf_key <- read_csv(file.path("inputs", "facets_full_key"))

bedrock_key <- read_excel(file.path("inputs","Skeena_Geology for land facets April 2_2024.xlsx"), 
                          sheet = "rock types", skip =5, .name_repair = "universal") %>% 
  rename("rock_type" = Rock_type_description.,
         "rock_class" = Rock.class.,
         "rock_class_det" = Groups.for.land.facet.analysis)%>%
  select(rock_type, rock_class, rock_class_det)

# check that hte classes match

uids <- skrocks %>% 
  st_drop_geometry() %>% 
  group_by(rock_type)%>%
  summarise(count = n())#%>%
  #mutate(total = sum(count))#%>%
  #rowwise() %>%
  #mutate(prop = (count/total)*100)

uids <- left_join(uids, bedrock_key)

uuids <- uids %>%
  filter(!is.na(rock_class_det)) 

bed_rock_det <- uuids %>% 
  group_by(rock_class_det)%>%
  summarise(count = n())%>%
  mutate(rock_class_det_no = seq(1,length(unique(rock_class_det)),1)) %>%
  select(-count)

uuidss <- left_join(uuids, bed_rock_det , by = "rock_class_det" ) %>%
  select(-count, -rock_class)

#uuids <- uids %>%
#  filter(!is.na(rock_class_det))

#head(skrocks$rock_type)


# create two more rasters to match the skrast values 

# conver to rast 

skrocks <- skrocks %>% 
  select( rock_type, rock_code, rock_class)


uids_key <- skrocks %>% 
  st_drop_geometry() %>% 
  group_by(rock_class) %>%
  summarise(count = n()) %>%
  mutate(rock_class_no = seq(1,length(unique(rock_class)),1))

skrocks_out <- left_join(skrocks, uids_key)

skrocks_out <- left_join(skrocks_out, uuidss)


# convert to raster 

rast(skrocks_out )

 
srockvec = as(skrocks_out,"SpatVector")
rock_no = rasterize(srockvec, skeena, field="rock_class_no")
rockdet_no = rasterize(srockvec, skeena, field="rock_class_det_no")


writeRaster(rock_no, file.path("inputs", "sk_bedrock_class.tif"))
writeRaster(rockdet_no, file.path("inputs", "sk_bedrock_class_det.tif"))



# add new columns 

unique(skrocks$rock_type) # n = 260

unique(skrocks$rock_class) #n = 9 












terra::writeRaster(srast, file.path("sk_lf_3005.tif")
unique(srast)


# number of codes within Aoi 
uval = length(unique(values(srast)))


# Summarise values 
routdf <- as.data.frame(srast)

# polt histogram
ggplot2::ggplot(routdf, aes(skeena_lfacet_3005)) +
  ggplot2::geom_histogram(bins = uval) 
  #geom_freqpoly()

# summary of values 

ids = routdf %>% 
  group_by(skeena_lfacet_3005)%>%
  summarise(count = n())%>%
  mutate(total = sum(count))%>%
  rowwise() %>%
  mutate(prop = (count/total)*100)


         