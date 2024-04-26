
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
#skeena = rast(file.path("inputs", "sk_lf_3005.tif")) # use this one as clipped to skeeena region


#basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
#basedata_soil  = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\bc"
#aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
#aoi_sf <- st_as_sf(aoi)

#basedata = "inputs"
#aoi <- vect(file.path(basedata, "SkeenaRegionBndry.gpkg"))
#aoi_sf <- st_as_sf(aoi)


#PROJCS["NA_Lambert_Azimuthal_Equal_Area",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433],METADATA["World",-180.0,-90.0,180.0,90.0,0.0,0.0174532925199433,0.0,1262]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-100.0],PARAMETER["Latitude_Of_Origin",45.0],UNIT["Meter",1.0]]

# mask the raster by aoi 
#srast <- mask(skeena, aoi)
#srast <- crop(skeena, aoi)
#writeRaster(srast, file.path("inputs", "sk_rast_template.tif"))

srast <- rast(file.path("inputs", "sk_rast_template.tif"))

# convert to vector 

m <- c(1, 10000000, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(srast , rclmat, include.lowest=TRUE)

svec <- as.polygons(rc1)

sfvec <- st_as_sf(svec)
st_write(sfvec, file.path("inputs", "template_poly.gpkg"))

# update the bedrock layer based on geology file 

# # download soils data type 
#bcdc_search("rock")

rocks <- bcdc_query_geodata("ef8476ed-b02d-4f5c-b778-0d44c9126144") |>
  #filter(INTERSECTS(aoi_sf)) |>
  #select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS, ORIGINAL_DESCRIPTION) |>
  collect()

rocks <- rocks %>% 
  select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS, ORIGINAL_DESCRIPTION) 


skrocks <- rename_with(rocks, tolower) %>% 
  st_intersection(aoi_sf)%>% 
  select(rock_type_description, original_description)

st_write(skrocks, file.path("inputs", "skeena_clip_soils.gpkg"), append = FALSE)

skrocks <- st_read(file.path("inputs", "skeena_clip_soils.gpkg"))


# merge the soild with sediment layer 

# download quaternary soils sediment, union to single polygon / multipolygonb and use in QGIS 

sed <- st_read(file.path("inputs", "quat_sed.gpkg")) %>%
  mutate(rock_type_description = "quaternary sediment") %>% 
  select(rock_type_description)

st_write(sed, file.path("inputs", "quad_sed_simple.gpkg"))



# in QGIS use symmetrical difference to remove sediment areas within the bedrock layer & UNION   WITH BEDROCK 

br <- st_read(file.path("inputs", "sk_bedrock_sediment_raw.gpkg")) 
br <- br %>% 
  mutate(rock_type_description  = case_when(
    is.na(rock_type_description) ~ "quaternary sediment", 
    TRUE ~ as.character(rock_type_description))) %>% 
  select(c(-rock_type_description_2, -layer))


# read in Paulas key and make a unique id values
bedrock_key <- read_excel(file.path("inputs","Skeena_Geology for land facets April 2_2024.xlsx"), 
                          sheet = "rock types", skip =5, .name_repair = "universal") %>% 
  rename("rock_type" = Rock_type_description.,
         "rock_class" = Rock.class.,
         "rock_class_det" = Groups.for.land.facet.analysis)%>%
  select(rock_type, rock_class, rock_class_det) %>% 
  mutate(rock_class_det = tolower(rock_class_det))

# merge the updated bedrock layer with the bedrock key 

#rock_desc_class <- unique(bedrock_key$rock_type) #50
#rock_class_class <- unique(bedrock_key$rock_class) #5
#rock_class_det_class <- unique(bedrock_key$rock_class_det) #19 

rock_class_det_id <-  bedrock_key %>% 
  select(rock_class_det) %>%
  mutate(rock_class_det = tolower(rock_class_det)) %>%
  distinct() %>%
  mutate(rock_class_det_no = seq(1,length(unique(rock_class_det)),1))


# create a key that matched the details with a numeric number 
bk_key <- left_join(bedrock_key,rock_class_det_id) %>% 
  rename("rock_type_description" = rock_type) %>% 
  mutate(rock_type_description  = tolower(rock_type_description),
         rock_class_det = tolower(rock_class_det))%>%
  mutate(rock_type_description = case_when(
    rock_type_description == "metamorphic rocks, undivided (metavolcanics and metasediments)" ~ "metamorphic rocks, undivided",
    TRUE ~ as.character(rock_type_description)))

bk_key_out <- bk_key %>% 
  select(rock_class_det, rock_class_det_no)%>% 
  distinct()

#write.csv(bk_key_out, file.path("inputs", "bedrock_key.csv"))

#ubr <- unique(br$rock_type_description)
#ubk <- unique(bk_key$rock_type_description)
#setdiff(ubr, ubk)


# update the undivided rocks into a detailed classifiaction 

# read in the raw sheet and update the undivided categories first 
undiv_key <- read_excel(file.path("inputs","Skeena_Geology for land facets April 2_2024.xlsx"), 
                        sheet = "raw", .name_repair = "universal") %>% 
  select(ROCK_TYPE_DESCRIPTION, ORIGINAL_DESCRIPTION, New.Rock_type.description) %>% 
  filter(ROCK_TYPE_DESCRIPTION == "undivided sedimentary rocks") %>%
  distinct()

# match the undivided detai;led catergory and update the br bedrock data set

aa <- br %>% 
  #select(original_description, rock_type_description) %>% 
  #filter(rock_type_description == "undivided sedimentary rocks") %>% 
  #st_drop_geometry() %>% 
  left_join(undiv_key, join_by(original_description == ORIGINAL_DESCRIPTION), relationship = "many-to-many") %>% 
  select(-ROCK_TYPE_DESCRIPTION)

ab <- aa %>% 
  mutate(rock_type_description2 = case_when(
    !is.na(New.Rock_type.description) ~ New.Rock_type.description,
    TRUE ~ as.character(rock_type_description))) %>%
  select(rock_type_description2)%>%
  mutate(rock_type_description2 = tolower(rock_type_description2)) %>% 
  rename("rock_type_description" = rock_type_description2)

br <- ab

# check against key 
ubk <- unique(bk_key$rock_type_description)
ubr <- unique(br$rock_type_description)

setdiff(ubr, ubk)

unique(bk_key$rock_class_det)

bb <- bk_key %>% 
  add_row(rock_type_description  = "coarse clastic sedimentary" ,rock_class = NA, 
          rock_class_det =  "coarse clastic sedimentary", rock_class_det_no = 10) %>% 
  add_row(rock_type_description  = "marine sedimentary and volcanic" ,rock_class = NA, 
        rock_class_det =  "marine sedimentary and volcanic", rock_class_det_no = 12 ) %>% 
  add_row(rock_type_description  = "fine clastic sedimentary" ,rock_class = NA, 
          rock_class_det ="fine clastic sedimentary", rock_class_det_no = 8) %>% 
  add_row(rock_type_description  = "metavolcanics and metasediments" ,rock_class = NA, 
          rock_class_det ="metavolcanics and metasediments", rock_class_det_no = 6) %>% 
  add_row(rock_type_description  = "siliceous metamorphic"  ,rock_class = NA, 
          rock_class_det ="siliceous metamorphic" , rock_class_det_no = 9)







skrocks_out <- left_join(br, bb , by = "rock_type_description") %>% 
  select(-rock_class )#, -rock_class_no, -original_description)
# select(-id, -bedrock_unit_id, -stratigraphic_age_code, -objectid, -rock_type_code )




# convert to raster 
#rast(skrocks_out)

srockvec = as(skrocks_out,"SpatVector")
#rock_no = rasterize(srockvec, skeena, field="rock_class_no")
rockdet_no = rasterize(srockvec, srast, field="rock_class_det_no")

#rock_no <- mask(rock_no, aoi)
rockdet_no<- mask(rockdet_no, srast)

#writeRaster(rock_no, file.path("inputs", "sk_bedrock_class.tif"), overwrite = TRUE)
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
#sk_rockclass_cat <- w + rock_no
sk_rockclassdet_cat <- w + rockdet_no

#writeRaster(sk_rockclass_cat, file.path("outputs", "sk_lf_rockclass.tif"), overwrite = TRUE)
writeRaster(sk_rockclassdet_cat, file.path("outputs", "sk_lf_rockclassdet.tif"), overwrite = TRUE)


#srast <- rast(file.path("outputs", "sk_lf_rockclassdet.tif"))





## generate the rare table.....



srast <- sk_rockclassdet_cat


# Read in the skeena facets 

# number of codes within Aoi 
uval = length(unique(values(srast)))

# Summarise values 
routdf <- as.data.frame(srast)

# polt histogram
ggplot2::ggplot(routdf, aes(lyr.1)) +
  ggplot2::geom_histogram(bins = uval) 
  #geom_freqpoly()


colnames(routdf) = "layer1"
  
ids = routdf %>% 
    group_by(layer1)%>%
    summarise(count = n())%>%
    mutate(total = sum(count))%>%
    rowwise() %>%
    mutate(pc = (count/total)*100)%>%
    arrange(count)

    
#mySum = t(apply(ids$prop, 1, cumsum))
ids <-within(ids, acc_sum <- cumsum(pc))
  
ids <- ids %>% 
    mutate(rarity_class = case_when(
      acc_sum <= 1 ~ 5, 
      acc_sum > 1 & acc_sum <=2 ~ 4,
      acc_sum > 2 & acc_sum <=4 ~ 3,
      acc_sum > 4 & acc_sum <=8 ~ 2,
      acc_sum > 8 & acc_sum <=16 ~ 1,
      .default = as.numeric(1)
    ))
             
             

# generate Summary tables 

write_csv(ids , file.path("outputs", "landscape_facet_summary_rcd.csv"))
#write_csv(ls_facet_sum_rc, file.path("outputs", "landscape_facet_summary_rc.csv"))

         