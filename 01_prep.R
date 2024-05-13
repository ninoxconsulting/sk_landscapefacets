
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(bcdata)
library(readr)
library(readxl)
library(purrr)

## note all data in google drive in "inputs folder"

# read in and clean up and generate template
skeena = rast(file.path("inputs", "MLF_Kehm_2012.tif"))
sk3005 = project(skeena, "epsg:3005")
writeRaster(sk3005, file.path("inputs", "MLF_Kehm_3005.tif"), overwrite = TRUE)


# classify the values into three groups 
m <- c(1, 54, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(sk3005, rclmat, include.lowest=TRUE)

# write out a raster template 
writeRaster(rc1, file.path("inputs", "sk_rast_template.tif"), overwrite = TRUE)

# convert to a vector and output 
rcpoly <- as.polygons(rc1)
rcpolsf <- st_as_sf(rcpoly)
st_write(rcpolsf, file.path("inputs", "sk_poly_template.gpkg"), append = FALSE) 



## Generate Barcode of data 
## this will be macrolandforms + soils + elevation
## 

## 1) macrolandforms
mlf <- rast(file.path("inputs", "MLF_Kehm_3005.tif"))
# read in csv key 

mlfkey <- read_csv(file.path("inputs", "Macrolandforms_Kehm_key.csv"))%>% 
  select(VALUE, TYPE)



# 2) elevation measures 
dem_file <- rast(file.path('/home/user/Documents/00_data/base_vector/bc/bc_dem_25m.tif'))
template <-rast(file.path("inputs", "sk_rast_template.tif"))

dem_sk <- crop(dem_file, template)
dem_sk100  <- aggregate(dem_sk , 4)
demf <- mask(dem_sk100, template)
dem <- resample(demf, template)

writeRaster(dem, file.path("inputs", "sk_dem_aoi.tif"), overwrite = TRUE)

hist(demf)


# classify the values into groups based on the values 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(-1, 200, 1, # lowest diversity 
       200, 500, 2,
       500, 750, 3,
       750, 1000 , 4,
       1000, 1250, 5,
       1250, 1500, 6, 
       1500, 2000, 7,
       2000, 2500, 8,
       2500, 3000 , 9,
       3000, 5000, 10) # highest diversity 

rclmat <- matrix(m, ncol=3, byrow=TRUE)
dem_class<- classify(demf, rclmat, include.lowest=TRUE)

writeRaster(dem_class, file.path("inputs", "sk_dem_class.tif"), overwrite = TRUE)

#hist(dem_class)


# create a key 
dem_key <- as.data.frame(rclmat)
names(dem_key) <- c("lower", "upper", "code")
dem_key <- dem_key %>% 
  mutate(elevation_group = paste0(lower, " - ", upper))



## bedrock and soils

# 1) read in sediment layer (this is the layer derived from Greg Kehm. )

# read in csv key & get the values you want to keep  
sokey <- read_csv(file.path("inputs", "parentmaterial_Kehm_key.csv")) %>% 
  select(VALUE, Description)
# select the values you want to keep (ie. fluvial (6), glaciofluvial (7), glaciolacustine (1))


#  Parent soils 
soils <- rast(file.path("inputs", "Parent_Material_Skeena_2024.tif"))
soils = project(soils, "epsg:3005")
soils = crop(soils, mlf)
soils_pol <- st_as_sf(as.polygons(soils))

soils_pol <- soils_pol %>% 
  dplyr::filter(Parent_Material_Skeena_2024 %in% c(6,7,10)) %>% 
  dplyr::mutate(rock_type_description = "quaternary sediment")

#plot(soils_pol)
st_write(soils_pol, file.path("inputs", "soil_parent_sediment.gpkg"), append = FALSE) 


# 2)read in the bedrock layer that Paula generated 

##################################################################
# update the bedrock layer based on geology file 

# # download soils data type 
#bcdc_search("rock")
# 
# rocks <- bcdc_query_geodata("ef8476ed-b02d-4f5c-b778-0d44c9126144") |>
#   #filter(INTERSECTS(aoi_sf)) |>
#   #select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS, ORIGINAL_DESCRIPTION) |>
#   collect()
# 
# rocks <- rocks %>% 
#   select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS, ORIGINAL_DESCRIPTION) 
# 
# 
# skrocks <- rename_with(rocks, tolower) %>% 
#   st_intersection(aoi_sf)%>% 
#   select(rock_type_description, original_description)
# 
# st_write(skrocks, file.path("inputs", "skeena_clip_soils.gpkg"), append = FALSE)

skrocks <- st_read(file.path("inputs", "skeena_clip_soils.gpkg"))
poly_template <-  st_read(file.path("inputs", "sk_poly_template.gpkg"))  

# in QGIS use symmetrical difference to remove sediment areas within the bedrock layer & UNION   WITH BEDROCK 
#  br <- st_read(file.path("inputs", "sk_bedrock_sediment_merge.gpkg")) %>%
#    st_intersection(., poly_template) %>% 
#   select(-Parent_Material_Skeena_2024, -MLF_Kehm_2012) 
#  
# st_write(br, file.path("inputs", "sk_bedrock_sediment_rawc.gpkg"), append = FALSE)

br <- st_read(file.path("inputs", "sk_bedrock_sediment_rawc.gpkg"), layer = 'br') %>% 
  select(-Parent_Material_Skeena_2024) %>% 
  filter(is.na(rock_type_description_2)) %>% 
  select(-rock_type_description_2, -layer)
  
br <- br %>% 
  mutate(rock_type_description  = case_when(
    is.na(rock_type_description) ~ "quaternary sediment", 
    TRUE ~ as.character(rock_type_description))) %>% 
  select(c(-rock_type_description_2, -layer))



# update the values in the bedrock layer 

# read in Paulas key and make a unique id values
bedrock_key <- read_excel(file.path("inputs","Skeena_Geology for land facets_May1_2024.xlsx"), 
                          sheet = "rock types", skip =5, .name_repair = "universal") %>% 
  rename("rock_type" = Rock_type_description.,
         "rock_class" = Rock.class.,
         #"rock_class_det" = Groups.for.land.facet.analysis)%>%
         "rock_class_det" = new_groups)%>%
  select(rock_type, rock_class, rock_class_det) %>% 
  mutate(rock_class_det = tolower(rock_class_det))

# merge the updated bedrock layer with the bedrock key 
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

br <- aa %>% 
  mutate(rock_type_description2 = case_when(
    !is.na(New.Rock_type.description) ~ New.Rock_type.description,
    TRUE ~ as.character(rock_type_description))) %>%
  select(rock_type_description2)%>%
  mutate(rock_type_description2 = tolower(rock_type_description2)) %>% 
  rename("rock_type_description" = rock_type_description2)


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

skrocks_key <- skrocks_out %>%
  select(rock_class_det, rock_class_det_no) %>%
  st_drop_geometry() %>% 
  distinct()

# convert to vector and raster 
srockvec = as(skrocks_out,"SpatVector")
rockdet_no = rasterize(srockvec, template, field="rock_class_det_no")

#rock_no <- mask(rock_no, aoi)
#rockdet_no<- mask(rockdet_no, srast)

writeRaster(rockdet_no, file.path("inputs", "sk_bedrock_class_det.tif"), overwrite = TRUE)







###########################################################################

## combine the base tifs data 
## now we have the components of the barcode 


## 1) macrolandforms
mlf <- rast(file.path("inputs", "MLF_Kehm_3005.tif"))
mlfkey <- read_csv(file.path("inputs", "Macrolandforms_Kehm_key.csv"))%>% 
  select(VALUE, TYPE)
# n = 10

# 2) elevation measures 
demf <- rast(file.path("inputs", "sk_dem_class.tif"))
dem_key
# n = 10

# 3) bedrock layers 
br <- rast(file.path("inputs", "sk_bedrock_class_det.tif"))
skrocks_key 
# n = 16




# mlf 

#mlf * 100
# create a blank for space for new code
mlff <- app(mlf, fun=function(x){ as.numeric(x) * 100 } )
mlff

# br 
#br * 10000
brf <- app(br , fun=function(x){ as.numeric(x) * 10000 } )
brf 

# elev
# leave as the same 

barcode <- brf + mlff
barcode <- barcode + demf

writeRaster(barcode , file.path("outputs", "sk_lf_barcode.tif"), overwrite = TRUE)


#####################################
# create a key for these

mlfkey <- mlfkey %>% 
  mutate(mlf_code = VALUE *100) %>% 
  rename("macrolandform" = TYPE) %>%
  select(-VALUE) 

dem_key <- dem_key %>% 
  select(elevation_group, code) %>% 
  rename("elev_code" = code)

skrocks_key <- skrocks_key %>% 
  mutate(br_code = rock_class_det_no * 10000) %>% 
  select(-rock_class_det_no)
  
  
bind_cols_fill <- function(df_list){
  max_rows <- map_int(df_list, nrow) %>% max()
    
  map(df_list, function(df){
    if(nrow(df) == max_rows) return(df)
    first<- names(df)[1] %>% sym ()
    df %>% add_row(!!first :=rep(NA, max_rows -nrow(df)))
  }) %>% bind_cols()
  }
  
all_key <- bind_cols_fill(list(dem_key,mlfkey, skrocks_key))  
write_csv(all_key, file.path("outputs", "sk_lf_barcode_key.csv"))
  

















# ##############################################################################
# 
# ####### OLD VERION ####################################################
# 
# 
# 
# 
# basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
# 
# #skeena = rast(file.path("inputs", "sk_rast_template.tif"))
# 
# #basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
# 
# #basedata_soil  = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\bc"
# 
# 
# # keep as vect to use terra
# #aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
# #aoi_sf <- st_as_sf(aoi)
# 
# #basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
# basedata = "inputs"
# 
# aoi <- vect(file.path(basedata, "SkeenaRegionBndry.gpkg"))
# aoi_sf <- st_as_sf(aoi)
# 
# 
# ###################################################################
# ## Create base rasters - only need to be run once
# 
# # # convert to vector 
# # 
# # m <- c(1, 10000000, 1)
# # rclmat <- matrix(m, ncol=3, byrow=TRUE)
# # rc1 <- classify(srast , rclmat, include.lowest=TRUE)
# # 
# # svec <- as.polygons(rc1)
# # 
# # sfvec <- st_as_sf(svec)
# # st_write(sfvec, file.path("inputs", "template_poly.gpkg"))
# 
# 
# ##################################################################
# # update the bedrock layer based on geology file 
# 
# # # download soils data type 
# #bcdc_search("rock")
# # 
# # rocks <- bcdc_query_geodata("ef8476ed-b02d-4f5c-b778-0d44c9126144") |>
# #   #filter(INTERSECTS(aoi_sf)) |>
# #   #select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS, ORIGINAL_DESCRIPTION) |>
# #   collect()
# # 
# # rocks <- rocks %>% 
# #   select(ROCK_TYPE_DESCRIPTION, ROCK_TYPE_CODE, ROCK_CLASS, ORIGINAL_DESCRIPTION) 
# # 
# # 
# # skrocks <- rename_with(rocks, tolower) %>% 
# #   st_intersection(aoi_sf)%>% 
# #   select(rock_type_description, original_description)
# # 
# # st_write(skrocks, file.path("inputs", "skeena_clip_soils.gpkg"), append = FALSE)
# 
# skrocks <- st_read(file.path("inputs", "skeena_clip_soils.gpkg"))
# 
# 
# 
# # merge the soils with sediment layer, gloacier and water layer
# 
# # download quaternary soils sediment, union to single polygon / multipolygonb and use in QGIS 
# # 
# # sed <- st_read(file.path("inputs", "quat_sed.gpkg")) %>%
# #   mutate(rock_type_description = "quaternary sediment") %>% 
# #   select(rock_type_description)
# # 
# # st_write(sed, file.path("inputs", "quad_sed_simple.gpkg"))
# 
# 
# 
# 
# # 
# # 
# # # keep only lakes (not include rivers)
# # 
# # lakes <- st_read(file.path("inputs", "lakes.gpkg")) %>%
# #   mutate(rock_type_description = "waterbodies") %>%
# #   select(rock_type_description)
# # 
# # st_write(lakes , file.path("inputs", "lakes_simple.gpkg"), append = FALSE)
# # 
# 
# # 
# # rivers <- st_read(file.pa("inputs", "rivers.gpkg")) %>%
# #       mutate(rock_type_description = "waterbodies") %>%
# #       select(rock_type_description))
# 
# # gla <- st_read(file.path("inputs", "glaciers.gpkg")) %>%
# #       st_intersection(., template_poly) %>% 
# #       mutate(rock_type_description = "glaciers") %>%
# #       select(rock_type_description)  
# # 
# # st_write(gla, file.path("inputs", "glacier_simple.gpkg"), append = FALSE)
# # 
# # 
# 
# 
# # in QGIS use symmetrical difference to remove sediment areas within the bedrock layer & UNION   WITH BEDROCK 
# # br <- st_read(file.path("inputs", "sk_bedrock_sediment_raw.gpkg")) %>%
# #   st_intersection(., template_poly)
# # 
# # st_write(br, file.path("inputs", "sk_bedrock_sediment_rawc.gpkg"))
# 
# 
# 
# # repeat the process with glaciers and lakes > 1ha
# 
# 
# 
# 
# br <- st_read(file.path("inputs", "sk_bedrock_sediment_raw.gpkg")) 
# br <- br %>% 
#   mutate(rock_type_description  = case_when(
#     is.na(rock_type_description) ~ "quaternary sediment", 
#     TRUE ~ as.character(rock_type_description))) %>% 
#   select(c(-rock_type_description_2, -layer))
# 
# 
# # read in Paulas key and make a unique id values
# bedrock_key <- read_excel(file.path("inputs","Skeena_Geology for land facets_May1_2024.xlsx"), 
#                           sheet = "rock types", skip =5, .name_repair = "universal") %>% 
#   rename("rock_type" = Rock_type_description.,
#          "rock_class" = Rock.class.,
#          #"rock_class_det" = Groups.for.land.facet.analysis)%>%
#           "rock_class_det" = new_groups)%>%
#   select(rock_type, rock_class, rock_class_det) %>% 
#   mutate(rock_class_det = tolower(rock_class_det))
# 
# # merge the updated bedrock layer with the bedrock key 
# 
# #rock_desc_class <- unique(bedrock_key$rock_type) #50
# #rock_class_class <- unique(bedrock_key$rock_class) #5
# #rock_class_det_class <- unique(bedrock_key$rock_class_det) #19 
# 
# rock_class_det_id <-  bedrock_key %>% 
#   select(rock_class_det) %>%
#   mutate(rock_class_det = tolower(rock_class_det)) %>%
#   distinct() %>%
#   mutate(rock_class_det_no = seq(1,length(unique(rock_class_det)),1))
# 
# 
# # create a key that matched the details with a numeric number 
# bk_key <- left_join(bedrock_key,rock_class_det_id) %>% 
#   rename("rock_type_description" = rock_type) %>% 
#   mutate(rock_type_description  = tolower(rock_type_description),
#          rock_class_det = tolower(rock_class_det))%>%
#   mutate(rock_type_description = case_when(
#     rock_type_description == "metamorphic rocks, undivided (metavolcanics and metasediments)" ~ "metamorphic rocks, undivided",
#     TRUE ~ as.character(rock_type_description)))
# 
# bk_key_out <- bk_key %>% 
#   select(rock_class_det, rock_class_det_no)%>% 
#   distinct()
# 
# #write.csv(bk_key_out, file.path("inputs", "bedrock_key.csv"))
# 
# #ubr <- unique(br$rock_type_description)
# #ubk <- unique(bk_key$rock_type_description)
# #setdiff(ubr, ubk)
# 
# 
# # update the undivided rocks into a detailed classifiaction 
# 
# # read in the raw sheet and update the undivided categories first 
# undiv_key <- read_excel(file.path("inputs","Skeena_Geology for land facets April 2_2024.xlsx"), 
#                         sheet = "raw", .name_repair = "universal") %>% 
#   select(ROCK_TYPE_DESCRIPTION, ORIGINAL_DESCRIPTION, New.Rock_type.description) %>% 
#   filter(ROCK_TYPE_DESCRIPTION == "undivided sedimentary rocks") %>%
#   distinct()
# 
# # match the undivided detai;led catergory and update the br bedrock data set
# 
# aa <- br %>% 
#   #select(original_description, rock_type_description) %>% 
#   #filter(rock_type_description == "undivided sedimentary rocks") %>% 
#   #st_drop_geometry() %>% 
#   left_join(undiv_key, join_by(original_description == ORIGINAL_DESCRIPTION), relationship = "many-to-many") %>% 
#   select(-ROCK_TYPE_DESCRIPTION)
# 
# ab <- aa %>% 
#   mutate(rock_type_description2 = case_when(
#     !is.na(New.Rock_type.description) ~ New.Rock_type.description,
#     TRUE ~ as.character(rock_type_description))) %>%
#   select(rock_type_description2)%>%
#   mutate(rock_type_description2 = tolower(rock_type_description2)) %>% 
#   rename("rock_type_description" = rock_type_description2)
# 
# br <- ab
# 
# # check against key 
# ubk <- unique(bk_key$rock_type_description)
# ubr <- unique(br$rock_type_description)
# 
# setdiff(ubr, ubk)
# 
# unique(bk_key$rock_class_det)
# 
# bb <- bk_key %>% 
#   add_row(rock_type_description  = "coarse clastic sedimentary" ,rock_class = NA, 
#           rock_class_det =  "coarse clastic sedimentary", rock_class_det_no = 10) %>% 
#   add_row(rock_type_description  = "marine sedimentary and volcanic" ,rock_class = NA, 
#         rock_class_det =  "marine sedimentary and volcanic", rock_class_det_no = 12 ) %>% 
#   add_row(rock_type_description  = "fine clastic sedimentary" ,rock_class = NA, 
#           rock_class_det ="fine clastic sedimentary", rock_class_det_no = 8) %>% 
#   add_row(rock_type_description  = "metavolcanics and metasediments" ,rock_class = NA, 
#           rock_class_det ="metavolcanics and metasediments", rock_class_det_no = 6) %>% 
#   add_row(rock_type_description  = "siliceous metamorphic"  ,rock_class = NA, 
#           rock_class_det ="siliceous metamorphic" , rock_class_det_no = 9)
# 
# 
# 
# 
# skrocks_out <- left_join(br, bb , by = "rock_type_description") %>% 
#   select(-rock_class )#, -rock_class_no, -original_description)
# # select(-id, -bedrock_unit_id, -stratigraphic_age_code, -objectid, -rock_type_code )
# 
# 
# 
# 
# # convert to raster 
# #rast(skrocks_out)
# 
# srockvec = as(skrocks_out,"SpatVector")
# #rock_no = rasterize(srockvec, skeena, field="rock_class_no")
# rockdet_no = rasterize(srockvec, srast, field="rock_class_det_no")
# 
# #rock_no <- mask(rock_no, aoi)
# rockdet_no<- mask(rockdet_no, srast)
# 
# #writeRaster(rock_no, file.path("inputs", "sk_bedrock_class.tif"), overwrite = TRUE)
# writeRaster(rockdet_no, file.path("inputs", "sk_bedrock_class_det.tif"), overwrite = TRUE)
# 
# 
# 
# 
# ### Add the new codes to the landscape faceet key and update the rasters to create the new objects. 
# 
# 
# dem <- rast(file.path("inputs", "sk_dem_class.tif"))
# 
# srast = rast(file.path("inputs", "sk_lf_3005.tif"))
# 
# # number of codes within Aoi 
# uval = length(unique(values(srast)))
# uval = length(unique(values(dem)))
# 
# 
# 
# 
# 
# 
# 
# 
# # remove the last two digits
# w <- app(srast, fun=function(x){ round(as.numeric(x) / 100, 0) } )
# 
# sort(unique(values(w)))
# 
# 
# # create a blank for space for new code
# w <- app(w, fun=function(x){ as.numeric(x) * 100 } )
# w 
# 
# 
# # create new barcode for the new bedrock layers 
# #sk_rockclass_cat <- w + rock_no
# sk_rockclassdet_cat <- w + rockdet_no
# 
# #writeRaster(sk_rockclass_cat, file.path("outputs", "sk_lf_rockclass.tif"), overwrite = TRUE)
# writeRaster(sk_rockclassdet_cat, file.path("outputs", "sk_lf_rockclassdet.tif"), overwrite = TRUE)
# 
# 
# #srast <- rast(file.path("outputs", "sk_lf_rockclassdet.tif"))
# 
# 
# 
# 
