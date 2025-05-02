## lakes Barcode assembly

library(dplyr)
library(sf)
library(tidyr)
library(readr)
library(terra)

srast <- rast(file.path("inputs", "sk_rast_template.tif"))

in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))

## LAKES  - barcode 

##https://catalogue.data.gov.bc.ca/dataset/eaubc-lakes

lac_sf <- read_sf(file.path("inputs", "eaubc_lakes.gpkg")) %>% 
  select("WSA_LAKE_ID", "WSA_SURFACE_AREA_HECTARE", "WSA_SHORELINE_COMPLEXITY",  "WSA_ELEVATION_IN_METER", 
         "LAKE_ECOSYSTEM_CLASS", "BEDROCK_GEOLOGY_CLASSES")
ldf <- lac_sf |> 
  st_drop_geometry() 

#1) WSA_SURFACE_AREA_HECTARE
ldf <- ldf %>%
  mutate(surface_area = case_when(
    WSA_SURFACE_AREA_HECTARE <= 1 ~ 1, 
    WSA_SURFACE_AREA_HECTARE > 1 &  WSA_SURFACE_AREA_HECTARE <=10 ~ 2, 
    WSA_SURFACE_AREA_HECTARE > 10 &  WSA_SURFACE_AREA_HECTARE <=100 ~ 3,
    WSA_SURFACE_AREA_HECTARE > 100 &  WSA_SURFACE_AREA_HECTARE <=1000 ~ 4,
    WSA_SURFACE_AREA_HECTARE > 1000 &  WSA_SURFACE_AREA_HECTARE <=10000 ~ 5,
    WSA_SURFACE_AREA_HECTARE > 10000 ~ 6
  ))

#comp <- lac |> st_drop_geometry() |> group_by(surface_area) |> count()


#2) Lake Ecosystem class

ldf <- ldf %>%
  mutate(ecosystem_class = case_when(
    LAKE_ECOSYSTEM_CLASS == "U" ~ 1, 
    LAKE_ECOSYSTEM_CLASS == "D" ~ 2, 
    LAKE_ECOSYSTEM_CLASS == "I" ~ 3, 
    LAKE_ECOSYSTEM_CLASS == "- 999" ~ 4
  ))



#3) "WSA_SHORELINE_COMPLEXITY"

ldf <- ldf %>%
  mutate(shoreline_complex = case_when(
    WSA_SHORELINE_COMPLEXITY <= 1.02 ~ 1, 
    WSA_SHORELINE_COMPLEXITY > 1.02 &  WSA_SHORELINE_COMPLEXITY <=2.03 ~ 2, 
    WSA_SHORELINE_COMPLEXITY > 2.03 &  WSA_SHORELINE_COMPLEXITY <=4 ~ 3, 
    WSA_SHORELINE_COMPLEXITY > 4 ~ 4
  ))


#3)  "WSA_ELEVATION_IN_METER", 
ldf <- ldf %>%
  mutate(wsa_elevation = case_when(
    WSA_ELEVATION_IN_METER <= 100 ~ 1, 
    WSA_ELEVATION_IN_METER> 100 &  WSA_ELEVATION_IN_METER <=600 ~ 2, 
    WSA_ELEVATION_IN_METER > 600 &  WSA_ELEVATION_IN_METER <=1000 ~ 3, 
    WSA_ELEVATION_IN_METER > 1000 ~ 4
  ))


#3)  "WSA_ELEVATION_IN_METER", 
ldf <- ldf %>%
    mutate(bedrock_class = case_when(
           BEDROCK_GEOLOGY_CLASSES == - 999 ~ 0, 
           .default = BEDROCK_GEOLOGY_CLASSES
           ))


## combine datasets 
# Filter out the -999 for ecosystem class. 

ldf_sum <- ldf |> 
  select(WSA_LAKE_ID, "surface_area","ecosystem_class",
         "wsa_elevation", "shoreline_complex" ,
        "bedrock_class") %>% 
  mutate(lake_code = bedrock_class + (10 * shoreline_complex)+ (100*wsa_elevation)+
           (1000* ecosystem_class) + (10000 * surface_area)) 

lac_sf <- lac_sf %>% 
  left_join(ldf_sum)%>% 
  select( WSA_LAKE_ID,"surface_area", "ecosystem_class" , "wsa_elevation" , "shoreline_complex", "bedrock_class",  "lake_code")    

st_write(lac_sf, file.path("inputs", "sk_lakes_barcode_poly.gpkg"), append = FALSE)

ldf_ss <- ldf_sum |> 
  select(-WSA_LAKE_ID)

summ <- ldf_ss %>%
  dplyr::group_by(lake_code) %>%
  dplyr::summarise(count= n())

write_csv(summ, file.path("inputs", "fw_lakes_summary.csv"))

# go straight to reading in...
summ <- read_csv(file.path("inputs", "fw_lakes_summary.csv"))









#hist(summ$count)

## Assign rarity code based on percent 
ids = summ %>% 
  mutate(total = sum(count))%>%
  rowwise() %>%
  mutate(pc = (count/total)*100)%>%
  arrange(count)

ids <-within(ids, acc_sum <- cumsum(pc))

rare <- ids %>% 
  mutate(rare_id = case_when(
    acc_sum <= 1 ~ 6, 
    acc_sum > 1 & acc_sum <=2 ~ 5,
    acc_sum > 2 & acc_sum <=4 ~ 4,
    acc_sum > 4 & acc_sum <=8 ~ 3,
    acc_sum > 8 & acc_sum <=16 ~ 2,
    .default = as.numeric(1)
  ))

# re join the rarity code back with the lake id and then group with the rivers polygon. 

rlac_sf <- lac_sf %>%
  left_join(rare) %>% 
  select("WSA_LAKE_ID", "lake_code", "rare_id") 

ri <- st_read(file.path("inputs", "eaubc_rivers.gpkg")) %>% 
  select(RIVER_ID)

# intersect the watershed
lac_river_poly <- st_intersection(rlac_sf, ri)


# summarise into a single polygon by taking the rarest value persent in the polygon

sum_df <- lac_river_poly %>%
  st_drop_geometry()%>%
  select(-WSA_LAKE_ID)


# # OPTION 1: MEAN RARITY 
# #select the mean rarity code per polygon and convert to a sparial output 
# 
# mrare_lake <- lac_river_poly %>% 
#   dplyr::group_by(RIVER_ID )%>%
#   dplyr::select(-WSA_LAKE_ID, -lake_code)%>%
#   dplyr::mutate(rarity_lake_code = mean(rare_id)) %>%
#   dplyr::select(-rare_id)%>%
#   dplyr::distinct()%>% 
#   st_drop_geometry()
# 
# rare_lake_poly <- left_join(ri, mrare_lake )
# 
# st_write(rare_lake_poly , file.path("inputs", "lake_rare_poly_mean_raw.gpkg"), append = FALSE)
# 
# 
# ## convert to a tif then run through the neighbourhood analysis 
# rare_lake_poly <-vect(file.path("inputs", "lake_rare_poly_mean_raw.gpkg"))
# 
# rare_laker <- rasterize(rare_lake_poly , srast, field= "rarity_lake_code")
# 
# plot(rare_laker)
# 
# writeRaster(rare_laker, file.path("outputs","lake_rare_mean.tif"))
# 
# 
# # ran rarity neighbourhood analysis in QGIS with 101c circular neighbourhood and average value. 
# 
# rar <- rast(file.path("outputs", "sk_lakes_mean_rarity_101c.tif"))
# 
# hist(rar)
# 
# names(rar)= "rarity"
# #reclass the valyers to a conccentration 
# 
# unique(values(rar))
# 
# m <- c(0, 1.1, 1,
#        1.1, 1.2, 2,
#        1.2, 1.4, 3,
#        1.4, 1.8, 4,
#        1.8, 10, 5)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# rc <- classify(rar , rclmat, include.lowest=TRUE)
# 
# writeRaster(rc, file.path("outputs", "sk_lakes_rarity_conc.tif"), overwrite = TRUE)
# 
# 



## Option 2 : DIVIDE THE MOST RARE by watershed area  (codes 4, 5 and 6)

#1 produce tabel with the rarity class and number per watershed , then select the units of interest

rare_count_lake <- lac_river_poly %>% 
  st_drop_geometry() %>% 
  dplyr::group_by(RIVER_ID,rare_id)%>%
  dplyr::select(-WSA_LAKE_ID, -lake_code)%>%
  dplyr::mutate(no_rare_lakestypes = n()) 

# add the river ppolygon area
ridf <- ri %>% 
  mutate(river_area_m2 = st_area(geom))%>% 
  st_drop_geometry()

# divide values by watershed area (ha) - all codes 
ri_lake <- left_join(rare_count_lake, ridf)%>% 
  ungroup() %>%
  distinct() %>% 
  rowwise() %>% 
  mutate(lakerarity_countbywatershed_ha = no_rare_lakestypes/(river_area_m2/10000))
  dplyr::distinct()%>% 
  st_drop_geometry()

# select just codes 4 and 5 
  # divide values by watershed area (ha) - all codes 
ri_lake456 <- rare_count_lake %>% 
  filter(rare_id %in% c(4,5,6))%>% 
  group_by(RIVER_ID) %>% 
  distinct() %>%
  select(-rare_id) %>% 
  mutate(count_lakes_rare456 = sum(no_rare_lakestypes)) %>% 
  select(-no_rare_lakestypes)%>%
  left_join(ridf) %>%
  rowwise() %>% 
  mutate(lakerarity_countbywatershed_ha = count_lakes_rare456/(river_area_m2/10000))%>%
  st_drop_geometry()
  
lakes_rare456 <-  left_join(ri, ri_lake456) 


st_write(lakes_rare456 , file.path("inputs", "lake_rare456_div_watershed_raw.gpkg"), append = FALSE)

# 
# # select just codes 5 and 6
# # divide values by watershed area (ha) - all codes 
# ri_lake56 <- rare_count_lake %>% 
#   filter(rare_id %in% c(6,5))%>% 
#   group_by(RIVER_ID) %>% 
#   distinct() %>%
#   select(-rare_id) %>% 
#   mutate(count_lakes_rare56 = sum(no_rare_lakestypes)) %>% 
#   select(-no_rare_lakestypes)%>%
#   left_join(ridf) %>%
#   rowwise() %>% 
#   mutate(lakerarity_countbywatershed_ha = count_lakes_rare56/(river_area_m2/10000))%>%
#   st_drop_geometry()
# 
# lakes_rare56 <-  left_join(ri, ri_lake56) 
# 
# 
# st_write(lakes_rare56 , file.path("inputs", "lake_rare56_div_watershed_raw.gpkg"), append = FALSE)
# 
# 
#   


## convert to a tif then run through the neighbourhood analysis 
rare_lake_poly <-vect(file.path("inputs", "lake_rare456_div_watershed_raw.gpkg"))

rare_laker <- rasterize(rare_lake_poly , srast, field= "lakerarity_countbywatershed_ha")

plot(rare_laker)

writeRaster(rare_laker, file.path("outputs","lake_rare_byarea.tif"))



# ran rarity neighbourhood analysis in QGIS with 101c circular neighbourhood and average value. 

rar <- rast(file.path("outputs", "sk_lakes_meanbyarea_rarity_101c.tif"))

hist(rar)
#values(rar)
t = quantile(values(rar), probs = seq(0, 1, 0.1), na.rm = TRUE)

t = data.frame(t)

names(rar)= "rarity"
#reclass the valyers to a conccentration 


## still to decide on these values 

sort(unique(values(rar)))

m <- c(0, 0.0001, 1,
       0.0001, 0.0002, 2,
       0.0002, 0.0003, 3,
       0.0003, 0.0005, 4,
       0.0005, 10, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(rar , rclmat, include.lowest=TRUE)
plot(rc)
 
writeRaster(rc, file.path("outputs", "sk_lakes_raritybyarea_conc.tif"), overwrite = TRUE)







################################################################################
## MOST COMMON bARCODES
##############################################################################

## Calculate the most common bar code types for 1) all of skeena region and 2) per ecoregion 

# 1 ) skeena 

# read in the rare csv file 
rare <- read_csv(file.path("inputs", "fw_lakes_summary.csv"))
lac_sf<- st_read(file.path("inputs", "sk_lakes_barcode_poly.gpkg"))

## Assign rarity/common code based on percent of number of lakes 
ids = rare %>% 
  mutate(total = sum(count))%>%
  rowwise() %>%
  mutate(pc = (count/total)*100)%>%
  arrange(count)

ids <-within(ids, acc_sum <- cumsum(pc))

common <- ids %>% 
  mutate(rare_id = case_when(
    acc_sum >= 50 ~ 1,
    .default = as.numeric(0)
  ))

write.csv(common, file.path("outputs", "sk_lakes_common_barcodes.csv"))

# re join the rarity code back with the lake id and then keep the common polygons
rlac_sf <- lac_sf %>%
  left_join(common) %>% 
  dplyr::select("lake_code", "rare_id") %>% 
  dplyr::filter(rare_id == 1) %>%
  dplyr::select(lake_code) %>% 
  mutate(lake_area_m2 = st_area(geom))

nrow(rlac_sf)

# calculate the common lakes that are under protection 

pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
# simplify protected area 
pross_u <- pro %>% select(protected)
pro_common_sk <- st_intersection( rlac_sf, pross_u) 

nrow(pro_common_ecoreg)

pro_common_ecoreg  <- pro_common_sk  |> 
  mutate(pro_lake_area = st_area(geom))%>% 
  select(-protected)

sk_lakes <- rlac_sf %>% 
  st_drop_geometry() %>% 
  dplyr::group_by(lake_code) %>% 
  dplyr::mutate(sum_lake_area_m2  = sum(lake_area_m2))%>%
  dplyr::select(-lake_area_m2)%>%
  distinct()

sk_pro_lakes <- pro_common_ecoreg %>% 
  st_drop_geometry() %>% 
  dplyr::group_by(lake_code) %>% 
  dplyr::mutate(sum_pro_lake_area_m2  = sum(pro_lake_area))%>%
  dplyr::select(-pro_lake_area, -lake_area_m2)%>%
  distinct()

protected_lakes <- left_join(sk_lakes, sk_pro_lakes)%>%
  rowwise() %>% 
  mutate(pc_protected = (sum_pro_lake_area_m2/ sum_lake_area_m2)*100)






# read in study area 

ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))

rr = terra::rast(file.path("outputs", "sk_lf_barcode.tif"))
rr_poly <-  as.polygons(rr, na.rm=FALSE)
rr_sf <- st_as_sf(rr_poly)

bc_ec <- st_intersection(rr_sf, ec) 

bc_ec <- bc_ec %>% 
  mutate(area_type = st_area(.))%>% 
  filter(!is.na(lyr.1))

st_write(bc_ec, file.path("outputs", "sk_lf_barcode_ecoreg_poly.gpkg"), append = FALSE)


ec <- ec %>% 
  mutate(ec_area_type = st_area(.))


# calculate the barcodes most common for each of the ecoregions 

comm <- bc_ec %>% 
  st_drop_geometry() %>% 
  left_join(ec)%>% 
  select(-geom) %>% 
  rowwise() %>% 
  mutate(pc = (area_type /ec_area_type)*100) %>% 
  select(-area_type, -ec_area_type)%>%
  ungroup()%>%
  arrange(pc)

library(plyr)
ccc <- ddply(comm,.(ECOREGION_NAME),transform,csum=cumsum(pc))
ccc <- as_tibble(ccc)%>% 
  mutate(pc = as.numeric(pc), 
         csum = as.numeric(csum))

common <- ccc %>% 
  mutate(rare_id = case_when(
    csum >= 50 ~ 1,
    .default = as.numeric(0)
  ))

#most_common <- common %>% filter(rare_id == 1)
#write.csv(most_common , file.path("outputs", "common_terrestrial_barcodes_per_ecoregion.csv"))


comm_sf <- left_join(bc_ec, common )%>% 
  filter(-area_type)


st_write(comm_sf, file.path("outputs", "sk_lf_barcode_ecoreg_detail_poly.gpkg"), append = FALSE)


# need to calculate number of barcodes per ecoregion 
no_barcodes_per_ecoregion <- common %>%
  filter(rare_id == 1)%>% 
  select(ECOREGION_NAME)%>%
  group_by(ECOREGION_NAME) %>% 
  count()

# generate the spatial layer 

common_by_ecoregion <- comm_sf %>%
  filter(rare_id == 1) %>% 
  select(ECOREGION_NAME)

# generate the amount of area that the common class takes up per ecoregion 
common_area_by_ecoregion <- common_by_ecoregion %>% 
  dplyr::mutate(common_area = st_area(geometry))

common_area_sum <- aggregate(common_area_by_ecoregion$common_area, by=list(ECOREGION_NAME=common_area_by_ecoregion$ECOREGION_NAME), FUN=sum)
names(common_area_sum) <- c("ECOREGION_NAME", "common_ecoreg_m2")


# overlay the protected areas with the most common to see how much is protected. 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))

# 1: calculate % protected for most common for all Skeena
## read in protected layers 

# calculate the area of each region 
ecsum <- ec %>%
  group_by(ECOREGION_NAME) |>
  mutate(area_m2 = st_area(geom)) 

ecsum_df <- ecsum %>% 
  st_drop_geometry()

all_sk <- sum(ecsum_df$area_m2)

ecsum_df <- ecsum_df %>% 
  mutate(total_sk_area = all_sk) %>% 
  rowwise() %>% 
  mutate(pc_of_sk = (area_m2/total_sk_area)*100)

write_csv(ecsum_df, file.path("outputs", "ecoregion_area_totals.csv"))


## what proportion of the common region is currently protected? 
# how much area is protected within each eco_region: ()

# simplify protected area 
pross_u <- pro %>% select(protected)
pro_common_ecoreg <- st_intersection( common_by_ecoregion, pross_u) 

pro_common_ecoreg  <- pro_common_ecoreg  |> 
  mutate(pro_area = st_area(geometry))

#st_write(pro_common_ecoreg , file.path("outputs", "sk_lf_common_ecoreg_protected.gpkg"), append = FALSE)

pro_sum <- pro_common_ecoreg %>%
  st_drop_geometry() |> 
  select(-protected) 

pro_sum <- aggregate(pro_sum$pro_area, by=list(ECOREGION_NAME=pro_sum$ECOREGION_NAME), FUN=sum)
names(pro_sum) <- c("ECOREGION_NAME", "common_pro_m2")


# join the protected area of common with the total area of common per ecoregion and calculate the percentage 

pro_sum <- left_join(pro_sum, common_area_sum) %>% 
  #select(-total_sk_area, -pc_of_sk) %>% 
  rowwise() %>% 
  dplyr::mutate(common_pro_pc = (common_pro_m2/common_ecoreg_m2)*100)

# export 

write_csv(pro_sum, file.path( "outputs", "common_protected_by_ecoregion.csv"))





















# 
# 
# 
# # Read in the skeena facets and update the most common barcodes >50% by land area 
# 
# common <- rare %>% 
#   mutate(rare_id = case_when(
#     acc_sum >= 50 ~ 1,
#     .default = as.numeric(0)
#   ))
# 
# # assign rarity class
# class1 <-common %>% filter(rare_id == 1) %>% pull(layer1)
# 
# write.csv(class1, file.path("outputs", "common_terrestrial_barcodes_allsk.csv"))
# length(class1)
# 
# uvr <- as.vector(unique(values(rr)))
# 
# # check if needs class 1: 
# 
# if(any(unique(uvr %in% class1)) == TRUE){
#   print("reclass values")
#   
#   for(i in class1){
#     #  i = class1[1]
#     print(i)
#     rr <- subst(rr, i, 1)
#     
#   }
#   
# }else {
#   print("no class1 reclass needed")
# }
# 
# reclass <- as.vector(unique(values(rr)))
# sort(reclass)
# 
# # classify the values into groups 
# 
# m <- c(1, 1, 1,
#        2, 999999, 0)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# common <- classify(rr, rclmat, include.lowest=TRUE)
# 
# unique(values(common))
# rc1 <- mask(common, aoi )
# terra::writeRaster(rc1,file.path("outputs", "sk_entire_common.tif"), overwrite = TRUE)
# 
# 
# ### 
# 
# # read in study area 
# 
# ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))
# 
# rr = terra::rast(file.path("outputs", "sk_lf_barcode.tif"))
# rr_poly <-  as.polygons(rr, na.rm=FALSE)
# rr_sf <- st_as_sf(rr_poly)
# 
# bc_ec <- st_intersection(rr_sf, ec) 
# 
# bc_ec <- bc_ec %>% 
#   mutate(area_type = st_area(.))%>% 
#   filter(!is.na(lyr.1))
# 
# st_write(bc_ec, file.path("outputs", "sk_lf_barcode_ecoreg_poly.gpkg"), append = FALSE)
# 
# 
# ec <- ec %>% 
#   mutate(ec_area_type = st_area(.))
# 
# 
# # calculate the barcodes most common for each of the ecoregions 
# 
# comm <- bc_ec %>% 
#   st_drop_geometry() %>% 
#   left_join(ec)%>% 
#   select(-geom) %>% 
#   rowwise() %>% 
#   mutate(pc = (area_type /ec_area_type)*100) %>% 
#   select(-area_type, -ec_area_type)%>%
#   ungroup()%>%
#   arrange(pc)
# 
# library(plyr)
# ccc <- ddply(comm,.(ECOREGION_NAME),transform,csum=cumsum(pc))
# ccc <- as_tibble(ccc)%>% 
#   mutate(pc = as.numeric(pc), 
#          csum = as.numeric(csum))
# 
# common <- ccc %>% 
#   mutate(rare_id = case_when(
#     csum >= 50 ~ 1,
#     .default = as.numeric(0)
#   ))
# 
# #most_common <- common %>% filter(rare_id == 1)
# #write.csv(most_common , file.path("outputs", "common_terrestrial_barcodes_per_ecoregion.csv"))
# 
# 
# comm_sf <- left_join(bc_ec, common )%>% 
#   filter(-area_type)
# 
# 
# st_write(comm_sf, file.path("outputs", "sk_lf_barcode_ecoreg_detail_poly.gpkg"), append = FALSE)
# 
# 
# # need to calculate number of barcodes per ecoregion 
# no_barcodes_per_ecoregion <- common %>%
#   filter(rare_id == 1)%>% 
#   select(ECOREGION_NAME)%>%
#   group_by(ECOREGION_NAME) %>% 
#   count()
# 
# # generate the spatial layer 
# 
# common_by_ecoregion <- comm_sf %>%
#   filter(rare_id == 1) %>% 
#   select(ECOREGION_NAME)
# 
# # generate the amount of area that the common class takes up per ecoregion 
# common_area_by_ecoregion <- common_by_ecoregion %>% 
#   dplyr::mutate(common_area = st_area(geometry))
# 
# common_area_sum <- aggregate(common_area_by_ecoregion$common_area, by=list(ECOREGION_NAME=common_area_by_ecoregion$ECOREGION_NAME), FUN=sum)
# names(common_area_sum) <- c("ECOREGION_NAME", "common_ecoreg_m2")
# 
# 
# # overlay the protected areas with the most common to see how much is protected. 
# pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
# ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))
# 
# # 1: calculate % protected for most common for all Skeena
# ## read in protected layers 
# 
# # calculate the area of each region 
# ecsum <- ec %>%
#   group_by(ECOREGION_NAME) |>
#   mutate(area_m2 = st_area(geom)) 
# 
# ecsum_df <- ecsum %>% 
#   st_drop_geometry()
# 
# all_sk <- sum(ecsum_df$area_m2)
# 
# ecsum_df <- ecsum_df %>% 
#   mutate(total_sk_area = all_sk) %>% 
#   rowwise() %>% 
#   mutate(pc_of_sk = (area_m2/total_sk_area)*100)
# 
# write_csv(ecsum_df, file.path("outputs", "ecoregion_area_totals.csv"))
# 
# 
# ## what proportion of the common region is currently protected? 
# # how much area is protected within each eco_region: ()
# 
# # simplify protected area 
# pross_u <- pro %>% select(protected)
# pro_common_ecoreg <- st_intersection( common_by_ecoregion, pross_u) 
# 
# pro_common_ecoreg  <- pro_common_ecoreg  |> 
#   mutate(pro_area = st_area(geometry))
# 
# #st_write(pro_common_ecoreg , file.path("outputs", "sk_lf_common_ecoreg_protected.gpkg"), append = FALSE)
# 
# pro_sum <- pro_common_ecoreg %>%
#   st_drop_geometry() |> 
#   select(-protected) 
# 
# pro_sum <- aggregate(pro_sum$pro_area, by=list(ECOREGION_NAME=pro_sum$ECOREGION_NAME), FUN=sum)
# names(pro_sum) <- c("ECOREGION_NAME", "common_pro_m2")
# 
# 
# # join the protected area of common with the total area of common per ecoregion and calculate the percentage 
# 
# pro_sum <- left_join(pro_sum, common_area_sum) %>% 
#   #select(-total_sk_area, -pc_of_sk) %>% 
#   rowwise() %>% 
#   dplyr::mutate(common_pro_pc = (common_pro_m2/common_ecoreg_m2)*100)
# 
# # export 
# 
# write_csv(pro_sum, file.path( "outputs", "common_protected_by_ecoregion.csv"))











############################################

## Lake diversity base calculation 
library(dplyr)

############################################

# diverity based on lake area 

lac_sf <- st_read(file.path("inputs", "sk_lakes_barcode_poly.gpkg"))

ri <- st_read(file.path("inputs", "eaubc_rivers.gpkg")) %>% 
                    select(RIVER_ID)%>% 
  mutate(river_poly_area_m2 = st_area(geom))

# intersect the watershed
lac_river_poly <- st_intersection(lac_sf, ri)%>% 
  select(WSA_LAKE_ID, lake_code, RIVER_ID, river_poly_area_m2) %>% 
  mutate(lake_area_m2 = st_area(geom))

lac_count_per_poly <- lac_river_poly %>% 
  dplyr::group_by(RIVER_ID)%>% 
  dplyr::mutate(lake_no_per_watershed = n())%>% 
  dplyr::mutate(lake_area_per_watershed = sum(lake_area_m2)) %>% 
  select(RIVER_ID, river_poly_area_m2, lake_no_per_watershed,lake_area_per_watershed)%>% 
  st_drop_geometry() %>% 
  distinct()%>%
  rowwise() %>% 
  dplyr::mutate(pc_cover_lakes = as.numeric((lake_area_per_watershed/river_poly_area_m2)*100)) %>% 
  ungroup()

# summary of no of lakes and area per polygon *not broken down by barcode)
write.csv <- st_write(lac_count_per_poly, file.path("outputs", "lac_count_per_watershed_no_area.csv"),append=FALSE)
  
pc_cover <- left_join(ri, lac_count_per_poly)

st_write(pc_cover , file.path("inputs", "lake_div_pccover.gpkg"), append = FALSE)




# count number of barcodes per river polygon
lakecode_area_per_poly <- lac_river_poly %>% 
  dplyr::group_by(RIVER_ID)%>% 
  dplyr::mutate(lake_area_per_watershed = sum(lake_area_m2)) %>%
  ungroup() %>%
  group_by(RIVER_ID, lake_code) %>% 
  dplyr::mutate(lakecode_no_per_watershed = n())%>% 
  dplyr::mutate(lakecode_area_per_watershed = sum(lake_area_m2)) %>% 
  select(RIVER_ID, lake_code,lakecode_no_per_watershed,lakecode_area_per_watershed, river_poly_area_m2 )%>% 
  
  
# output as table 
lakecode_area_per_poly_df <-lakecode_area_per_poly %>% 
  select(- river_poly_area_m2 )%>%
  st_drop_geometry() %>% 
  distinct()

# summary of no of lake codes and areas per polygon 
write.csv <- st_write(lakecode_area_per_poly, file.path("outputs", "lakecode_count_per_watershed_no_area.csv"))



## OPTION 2: diversity based on watershed area 
# output as spatial and calculate raw diversity based on number of lake barcodes per watershed by area of watershed (hectares)

lac_sf <- st_read(file.path("inputs", "sk_lakes_barcode_poly.gpkg"))

ri <- st_read(file.path("inputs", "eaubc_rivers.gpkg")) %>% 
  select(RIVER_ID)%>% 
  mutate(river_poly_area_m2 = st_area(geom))

# intersect the watershed
lac_river_poly <- st_intersection(lac_sf, ri)%>% 
  select(lake_code, RIVER_ID, river_poly_area_m2) 

lac_count_per_poly <- lac_river_poly %>% 
  dplyr::group_by(RIVER_ID)%>% 
  dplyr::mutate(lake_no_per_watershed = n()) %>% 
  select(RIVER_ID, river_poly_area_m2, lake_no_per_watershed)%>% 
  st_drop_geometry() %>% 
  distinct()%>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(lake_div_raw_m2 = lake_no_per_watershed/river_poly_area_m2)%>%
  mutate(lake_div_raw_ha = lake_no_per_watershed/(river_poly_area_m2/10000))

la_div_sf_raw <- left_join(ri, lac_count_per_poly) 


 st_write(la_div_sf_raw , file.path("inputs", "lake_div_raw.gpkg"), append = FALSE)

 
 ## convert to a tif then run through the neighbourhood analysis 

 div_lake_poly <-vect(file.path("inputs", "lake_div_raw.gpkg"))
 
 div_laker <- rasterize(div_lake_poly  , srast, field= "lake_div_raw_ha")
 
 plot(div_laker)
 
 writeRaster(div_laker, file.path("outputs","lake_div_byarea.tif"))

# ran diversity neighbourhood analysis in QGIS with 101c circular neighbourhood and average value. 

div <- rast(file.path("outputs", "sk_lakes_meanbyarea_div_101c.tif"))

hist(div)

names(div)= "diversity"

#reclass the layers to a conccentration 

#t = quantile(values(div), probs = seq(0, 1, 0.1), na.rm = TRUE)
#
#t = data.frame(t)



## still to decide on these values 

unique(values(div))

m <- c(0, 0.002, 1,
       0.002, 0.003, 2,
       0.003, 0.004, 3,
       0.004 , 0.006, 4,
       0.006, 1, 5)
 rclmat <- matrix(m, ncol=3, byrow=TRUE)
 rc <- classify(div , rclmat, include.lowest=TRUE)
 plot(rc)
 
 

 
 writeRaster(rc, file.path("outputs", "sk_lakes_divarea_conc.tif"), overwrite = TRUE)


 
 




# 
# 
# ############################################################################
# 
# ## Lakes density values 
# 
# srast <- rast(file.path("inputs", "sk_rast_template.tif"))
# 
# #srast1km <- aggregate(srast, fact = 10)
# 
# den1km <- rast(file.path("inputs", "sk_lake_density_1km.tif"))
# # read in 1km density lakes and clasify to Weaver 
# 
# # Weaver used the density values of % cover of 1km grid cell 
# # 
# # very high > 50 ~ 1
# # high 15 - 50%  ~ 2
# # moderate = <15% ~ 3 
# #
# m <- c(0, 0.15, 3,
#        0.15, 0.5, 2,
#        0.5, 1, 1)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# rc1 <- classify(den1km, rclmat, include.lowest=TRUE)
# 
# writeRaster(rc1, file.path("inputs", "sk_lake_density_class_1km.tif"), overwrite = TRUE)
# 
# 
# 
# ############################################################################
# 
# ##WETLAND density values 
# 
# srast <- rast(file.path("inputs", "sk_rast_template.tif"))
# 
# wet1km <- rast(file.path("inputs", "sk_wetland_density_1km.tif"))
# # read in 1km density lakes and clasify to Weaver 
# 
# # Weaver used the density values of % cover of 1km grid cell 
# # 
# # very high > 50 ~ 1
# # high 15 - 50%  ~ 2
# # moderate = <15% ~ 3 
# #
# m <- c(0, 0.15, 3,
#        0.15, 0.5, 2,
#        0.5, 1, 1)
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# rc1 <- classify(wet1km, rclmat, include.lowest=TRUE)
# 
# writeRaster(rc1, file.path("inputs", "sk_wetland_density_class_1km.tif"), overwrite = TRUE)
# 
# 















##############################################################################
library(dplyr)
# EAUBC freshwater rivers
ri <- st_read(file.path("inputs", "eaubc_rivers.gpkg"))

# filtyer slivers
ri <- ri %>% 
  filter(FEATURE_AREA_SQM > 100000)


## FIX BEDROCK CODE - COMBINE INTO 1 column (predomient) 

ribr <- ri %>%
  st_drop_geometry( ) %>% 
  select("RIVER_ID", "PCT_WS_UNT_BEDROCK_ALLUVIUM", "PCT_WS_UNT_BEDROCK_CARB_SEDS",
         "PCT_WS_UNT_BEDROCK_CHEM_SEDS",  "PCT_WS_UNT_BEDROCK_HARD_SEDS",   
         "PCT_WS_UNT_BEDROCK_INT_META", "PCT_WS_UNT_BEDROCK_SOFT_SEDS", 
         "PCT_WS_UNT_BEDROCK_VOLCANIC") %>% 
  pivot_longer(!"RIVER_ID", names_to = "bedrock_type", values_to = "percent_type") %>% 
  mutate(bedrock_code = case_when(
    bedrock_type == "PCT_WS_UNT_BEDROCK_ALLUVIUM" ~ 3,
    bedrock_type == "PCT_WS_UNT_BEDROCK_CARB_SEDS" ~ 4,
    bedrock_type == "PCT_WS_UNT_BEDROCK_CHEM_SEDS" ~ 5,
    bedrock_type == "PCT_WS_UNT_BEDROCK_HARD_SEDS" ~ 6,
    bedrock_type == "PCT_WS_UNT_BEDROCK_INT_META" ~ 2,
    bedrock_type == "PCT_WS_UNT_BEDROCK_SOFT_SEDS" ~7 ,
    bedrock_type == "PCT_WS_UNT_BEDROCK_VOLCANIC" ~1
  ))

# select the maximum percent bedrock type
ribr_max <- ribr %>%
  group_by(RIVER_ID) %>% 
  slice_max(percent_type)


# select the river ids where more than one max value (i.e. equal percent or -9999 no data )
multi_id <- ribr_max %>%
  dplyr::group_by(RIVER_ID) %>% 
  #filter(percent_type != -999) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1)

# get list of all imulti idas
multi_id_list = unique(multi_id$RIVER_ID)
all_id_list = unique(ribr$RIVER_ID)

single_max_ri <- setdiff(all_id_list, multi_id_list)


# get subset of multilist where no data (-999 and convert to 0)
ribr_nodata <- ribr_max %>% 
  filter(percent_type == -999) %>% 
  select(RIVER_ID) %>% 
  distinct()%>% 
  mutate(bedrock_code = 0) 
           
# get unique ids
nodat_list = unique(ribr_nodata$RIVER_ID)


# get list of units were equal between two values
ribr_multi <- multi_id %>% 
  filter(percent_type != -999) %>% 
  slice_head(n = 1)%>% 
  mutate(bedrock_code = 8) %>%
  select(RIVER_ID, bedrock_code)


# reassemble values 
single_max <- ribr_max %>% 
  filter(RIVER_ID %in% single_max_ri) %>% 
  select(RIVER_ID, bedrock_code)


out <- bind_rows(ribr_nodata, ribr_multi )
bedrockout <- bind_rows(single_max, out)
# join back to spatial data
ri <- left_join(ri, bedrockout)
           
           

ri <- ri %>%
  select("RIVER_ID", #"WSA_WATERSHED_CODE",
         RIVER_ECOSYSTEM_CLASS,
         MEAN_WATERSHED_ELEVATION,
         MAX_STREAM_ORDER,
         MELTONS_RATIO,
         STREAM_GRADIENT_MODEL, bedrock_code)


rdf <- ri |> 
  st_drop_geometry() 

#1) RIVER_ECOSYSTEM_CLASS
rdf<- rdf %>%
  mutate(ecosystem_class = case_when(
    RIVER_ECOSYSTEM_CLASS == "C" ~ 1, 
    RIVER_ECOSYSTEM_CLASS == "H" ~ 2,
    RIVER_ECOSYSTEM_CLASS == "M" ~ 3,
    RIVER_ECOSYSTEM_CLASS == "T" ~ 4,
    RIVER_ECOSYSTEM_CLASS == "- 999" ~ 9
  ))

#2) Stream Order 
rdf <- rdf %>%
  mutate(streamorder_max = case_when(
    MAX_STREAM_ORDER == 1 ~ 1,
    MAX_STREAM_ORDER %in% c(2,3) ~ 2,
    MAX_STREAM_ORDER %in% c(4,5,6) ~ 3,
    MAX_STREAM_ORDER %in% c(7,8) ~ 4,
    MAX_STREAM_ORDER == - 999 ~ 9
  ))

#3)  " MEAN ELEVATION_IN_METER", 
rdf <- rdf %>%
  mutate(elevation = case_when(
    MEAN_WATERSHED_ELEVATION <= 100 ~ 1, 
    MEAN_WATERSHED_ELEVATION> 100 &  MEAN_WATERSHED_ELEVATION <=600 ~ 2, 
    MEAN_WATERSHED_ELEVATION > 600 &  MEAN_WATERSHED_ELEVATION <=1000 ~ 3, 
    MEAN_WATERSHED_ELEVATION> 1000 ~ 4
  ))

#3) "RUGGEDNESS
rdf <- rdf %>%
  mutate(meltons = case_when(
    MELTONS_RATIO <= 0.1 ~ 1, 
    MELTONS_RATIO > 0.1 &  MELTONS_RATIO<=0.2 ~ 2, 
    MELTONS_RATIO > 0.2 &  MELTONS_RATIO <=0.5 ~ 3, 
    MELTONS_RATIO > 0.5 ~ 4
  ))



## combine datasets 

rdf_sum <- rdf |> 
  select(RIVER_ID, "ecosystem_class" , "streamorder_max", "elevation" , 
         "meltons",  "bedrock_code") %>% 
  mutate(river_code = bedrock_code + (10 * meltons)+ (100* elevation)+
           (1000* streamorder_max) + (10000 * ecosystem_class)) 


ri_sf <- ri %>% 
  left_join(rdf_sum)

st_write(ri_sf, file.path("inputs", "sk_river_barcode_poly.gpkg"), append = FALSE)

rdf_ss <-rdf_sum  |> 
  select(-RIVER_ID)

summ <- rdf_ss %>%
  dplyr::group_by(river_code) %>%
  dplyr::summarise(count= n())


write_csv(summ, file.path("inputs", "fw_river_summary.csv"))


#quantile(summ$count, seq(0,1, 0.1))

## determine the diversity values 
# convert to raster and run neighbourhood analysis

rri <- terra::rasterize(ri_sf, srast, field = "river_code")

writeRaster(rri, file.path("inputs", "sk_river_barcodes_raw.tif"), overwrite = TRUE)



# up to here - currently runinng on qgis



# read in the neighbourhood version 

div <- rast(file.path("outputs","sk_rivers_diversity_101.tif"))
div <- mask(div, srast)
#hist(div)
#aa <- values(div, na.rm = T)
#quantile(aa, seq(0,1, 0.1))
writeRaster(div, file.path("outputs", "sk_rivers_diversity_101c.tif"), overwrite = TRUE)

# new version - 1km
div <- rast(file.path("outputs","sk_rivers_diversity_101.tif"))
divp <- as.polygons(div, digits = 2)
divp <- terra::rasterize(divp, srast, "sk_rivers_diversity_101", fun = "mean", na.rm = TRUE)
names(divp) <- "rivers_diversity_101"
divp <- mask(divp, srast)
writeRaster(divp, file.path(outputs, "rivers_diversity_101_c.tif"), overwrite=TRUE)




# set thresholds 
names(div)= "diversity"
#reclass the valyers to a conccentration 

unique(values(div))

m <- c(1, 2.5, 1,
       2.5, 3, 2,
       3, 4.5, 3, 
       4.5, 6, 4,
       6, 20, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(div , rclmat, include.lowest=TRUE)

writeRaster(rc, file.path("outputs", "sk_rivers_diversity_conc.tif"), overwrite = TRUE)



##############################################################################


############################################################

# Set rivers rarity based on barcode 

# number of codes within Aoi 
uval = length(unique(values(rri)))
summ <- read_csv(file.path("inputs", "fw_river_summary.csv"))

ids = summ  %>% 
  mutate(total = sum(count))%>%
  rowwise() %>%
  mutate(pc = (count/total)*100)%>%
  arrange(count)

ids <-within(ids, acc_sum <- cumsum(pc))

rare <- ids %>% 
  mutate(rare_id = case_when(
    acc_sum <= 1 ~ 6, 
    acc_sum > 1 & acc_sum <=2 ~ 5,
    acc_sum > 2 & acc_sum <=4 ~ 4,
    acc_sum > 4 & acc_sum <=8 ~ 3,
    acc_sum > 8 & acc_sum <=16 ~ 2,
    .default = as.numeric(1)
  ))


# generate Summary tables 
write_csv(rare , file.path("outputs", "sk_river_barcode_summary.csv"))



# if above is already run....

# read in the rare csv file 
rare <- read.csv(file.path("outputs", "sk_river_barcode_summary.csv")) 
rr = terra::rast(file.path("inputs", "sk_river_barcodes_raw.tif"))


#cutoff is count of 46127

hist(rare$count)
hist(rare$rare_id)


# assign rarity class
class1 <-rare  %>% filter(rare_id == 1) %>% pull(river_code)
class2 <- rare %>% filter(rare_id == 2)%>% pull(river_code)
class3 <-rare  %>% filter(rare_id == 3) %>% pull(river_code)
class4 <-rare  %>% filter(rare_id == 4) %>% pull(river_code)
class5 <-rare  %>% filter(rare_id == 5) %>% pull(river_code)
class6 <-rare  %>% filter(rare_id == 6) %>% pull(river_code)

uvr <- as.vector(unique(values(rr)))

# check if needs class 1: 

if(any(unique(uvr %in% class1)) == TRUE){
  print("reclass values")
  
  for(i in class1){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 1)
    
  }
  
}else {
  print("no class1 reclass needed")
}

# check if needs class 6 

if(any(unique(uvr %in% class6)) == TRUE){
  print("reclass values")
  
  for(i in class6){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 6)
    
  }
  
}else {
  print("no class 6 reclass needed")
}


# check if needs class 2: 

if(any(unique(uvr %in% class2)) == TRUE){
  print("reclass values")
  
  for(i in class2){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 2)
    
  }
  
}else {
  print("no class2 reclass needed")
}


# check if needs class 3: 

if(any(unique(uvr %in% class3)) == TRUE){
  print("reclass values")
  
  for(i in class3){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 3)
    
  }
  
}else {
  print("no class3 reclass needed")
}



# check if needs class 4

if(any(unique(uvr %in% class4)) == TRUE){
  print("reclass values")
  
  for(i in class4){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 4)
  }
}else {
  print("no class4 reclass needed")
}


# check if needs class 5
#class5 <- aa
#aa <- class5 
#class5 <- aa[21:50]

#setdiff(reclass, class5)


if(any(unique(uvr %in% class5)) == TRUE){
  print("reclass values")
  
  for(i in class5){
    # i = class1[1]
    print(i)
    rr <- subst(rr, i, 5)
  }
}else {
  print("no class5 reclass needed")
}


reclass <- as.vector(unique(values(rr)))

sort(reclass)

## from-to-becomes
# classify the values into groups 

m <- c(1, 1, 1,
       6, 6, 6,
       2, 2, 2,
       3, 3, 3,
       4, 4, 4,
       5, 5, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(rr, rclmat, include.lowest=TRUE)

unique(values(rc1))

terra::writeRaster(rc1,file.path("outputs", "sk_rivers_rarity_class.tif"), overwrite = TRUE)



# run the neighbourhood analysis on mean 


# run in QGIS


# read in the neighbourhood version 

avrare <- rast(file.path("outputs","sk_rivers_rarity_mean_101c.tif"))
avrare <- mask(avrare, srast)

hist(avrare)

#writeRaster(div, file.path("outputs", "sk_rivers_diversity_101c.tif"), overwrite = TRUE)

unique(values(avrare))

m <- c(1, 1.1, 1,
       1.1, 1.2, 2,
       1.2, 1.8, 3,
       1.8, 2.8, 4,
       2.8, 20, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(avrare , rclmat, include.lowest=TRUE)

writeRaster(rc, file.path("outputs", "sk_rivers_rarity_mean_conc.tif"), overwrite = TRUE)


# new version - 1km
avrare <- rast(file.path("outputs","sk_rivers_rarity_mean_101c.tif"))
avrarep <- as.polygons(avrare, digits = 2)
avrarep
avrarep <- terra::rasterize(avrarep, srast, "sk_rivers_rarity_mean_101c", fun = "mean", na.rm = TRUE)
names(avrarep) <- "rivers_rarity_mean_101c"
avrarep<- mask(avrarep, srast)
writeRaster(avrarep, file.path(outputs, "rivers_rarity_mean_101c.tif"), overwrite=TRUE)



