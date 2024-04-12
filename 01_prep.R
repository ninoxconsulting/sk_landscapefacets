#ibrary(googlesheets4)
#library(googledrive)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(bcdata)
library(readr)
#library(janitor)
library(readxl)

## note all data in google drive in "inputs folder"



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
#bcdc_search("rock")

rocks <- bcdc_query_geodata("ef8476ed-b02d-4f5c-b778-0d44c9126144") |>
  filter(INTERSECTS(aoi_sf)) |>
  select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS) |>
  collect()

skrocks <- rename_with(rocks, tolower)
  

st_write(skrocks, file.path("inputs", "skeena_soils.gpkg"))

# read in Paulas key and make a unique id values
bedrock_key <- read_excel(file.path("inputs","Skeena_Geology for land facets April 2_2024.xlsx"), 
                          sheet = "rock types", skip =5, .name_repair = "universal") %>% 
  rename("rock_type" = Rock_type_description.,
         "rock_class" = Rock.class.,
         "rock_class_det" = Groups.for.land.facet.analysis)%>%
  select(rock_type, rock_class, rock_class_det)


rock_desc_class <- unique(bedrock_key$rock_type) #50
rock_class_class <- unique(bedrock_key$rock_class) #5
rock_class_det_class <- unique(bedrock_key$rock_class_det) #19 


rock_class_id <- bedrock_key %>% 
  select(rock_class) %>% 
  distinct() %>%
  mutate(rock_class_no = seq(1,length(unique(rock_class)),1))

rock_class_det_id <-  bedrock_key %>% 
  select(rock_class_det) %>%
  distinct() %>%
  mutate(rock_class_det_no = seq(1,length(unique(rock_class_det)),1))


bedrock_key <- left_join(bedrock_key, rock_class_id ) %>% 
  left_join(rock_class_det_id) %>%
   rename("rock_type_description" = rock_type )


# see what data is in the skeena soil data and if everything matches
# 
# rocks_class_id <- skrocks %>% 
#   st_drop_geometry() %>% 
#   group_by(rock_class)%>%
#   summarise(count = n())
# 
# rock_type_description <- skrocks %>% 
#   st_drop_geometry() %>% 
#   group_by(rock_type_description)%>%
#   summarise(count = n())%>% 
#   mutate(rock_type = rock_type_description)
# 
# rocks_class_id <- left_join(rock_type_description, bedrock_key, by = "rock_type")
# 

# combine the raster and key 
skrocks <- skrocks %>% 
  select(-rock_class)

skrocks_out <- left_join(skrocks, bedrock_key, by = "rock_type_description") %>% 
  select(-id, -bedrock_unit_id, -stratigraphic_age_code, -objectid, -rock_type_code )

# convert to raster 
#rast(skrocks_out)

srockvec = as(skrocks_out,"SpatVector")
rock_no = rasterize(srockvec, skeena, field="rock_class_no")
rockdet_no = rasterize(srockvec, skeena, field="rock_class_det_no")


rock_no <- mask(rock_no, aoi)
rockdet_no<- mask(rockdet_no, aoi)

writeRaster(rock_no, file.path("inputs", "sk_bedrock_class.tif"), overwrite = TRUE)
writeRaster(rockdet_no, file.path("inputs", "sk_bedrock_class_det.tif"), overwrite = TRUE)




### Add the new codes to the landscape faceet key and update the rasters to create the new objects. 


srast = rast(file.path("inputs", "sk_lf_3005.tif"))

# number of codes within Aoi 
uval = length(unique(values(srast)))


# remove the last two digits
w <- app(srast, fun=function(x){ round(as.numeric(x) / 100, 0) } )

# create a blank for space for new code
w <- app(w, fun=function(x){ as.numeric(x) * 100 } )
w 


# create new barcode for the new bedrock layers 

sk_rockclass_cat <- w + rock_no
sk_rockclassdet_cat <- w + rockdet_no

writeRaster(sk_rockclass_cat, file.path("outputs", "sk_lf_rockclass.tif"), overwrite = TRUE)
writeRaster(sk_rockclassdet_cat, file.path("outputs", "sk_lf_rockclassdet.tif"), overwrite = TRUE)







# Read in the skeena facets 

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
# 
# ids = routdf %>% 
#   group_by(skeena_lfacet_3005)%>%
#   summarise(count = n())%>%
#   mutate(total = sum(count))%>%
#   rowwise() %>%
#   mutate(prop = (count/total)*100)



sumraster = function(inrast){
  
  #inrast <- sk_rockclassdet_cat
  routdf <- as.data.frame(inrast)
  
  colnames(routdf) = "layer1"
  
  ids = routdf %>% 
    group_by(layer1)%>%
    summarise(count = n())%>%
    mutate(total = sum(count))%>%
    rowwise() %>%
    mutate(prop = (count/total)*100)%>%
    arrange(count)
  

    
  #mySum = t(apply(ids$prop, 1, cumsum))
  ids <-within(ids, acc_sum <- cumsum(prop)*100)
  ids <- ids %>% 
    mutate(rare_under_10 = ifelse(acc_sum <10, "y", "n"))

  return(ids)
  
}


# generate Summary tables 

ls_facet_sum_rcd <- sumraster(sk_rockclassdet_cat)

ls_facet_sum_rc <- sumraster(sk_rockclass_cat)

write_csv(ls_facet_sum_rcd, file.path("outputs", "landscape_facet_summary_rcd.csv"))
write_csv(ls_facet_sum_rc, file.path("outputs", "landscape_facet_summary_rc.csv"))

         