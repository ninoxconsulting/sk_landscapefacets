#02. Gneerate rarity codes for barcode 

library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# read in study area 
aoi <- rast(file.path("inputs", "sk_rast_template.tif"))
temp <- st_read(file.path("inputs", "sk_poly_template.gpkg"))

srast = terra::rast(file.path("outputs", "sk_lf_barcode.tif"))

# Read in the skeena facets 

# number of codes within Aoi 
uval = length(unique(values(srast)))

# Summarise values 
routdf <- as.data.frame(srast)

# polt histogram
ggplot2::ggplot(routdf, aes(lyr.1)) +
  ggplot2::geom_histogram(bins = uval) 

colnames(routdf) = "layer1"

ids = routdf %>% 
  group_by(layer1)%>%
  summarise(count = n())%>%
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
write_csv(rare , file.path("outputs", "lf_barcode_summary.csv"))



# if above is already run....

# read in the rare csv file 
rare <- read.csv(file.path("outputs", "lf_barcode_summary.csv")) 
rr = terra::rast(file.path("outputs", "sk_lf_barcode.tif"))

#cutoff is count of 46127

hist(rare$count)
hist(rare$rare_id)

 # assign rarity class
class1 <-rare  %>% filter(rare_id == 1) %>% pull(layer1)
class2 <- rare %>% filter(rare_id == 2)%>% pull(layer1)
class3 <-rare  %>% filter(rare_id == 3) %>% pull(layer1)
class4 <-rare  %>% filter(rare_id == 4) %>% pull(layer1)
class5 <-rare  %>% filter(rare_id == 5) %>% pull(layer1)
class6 <-rare  %>% filter(rare_id == 6) %>% pull(layer1)

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

terra::writeRaster(rc1,file.path("outputs", "sk_rarity_class_rcd.tif"), overwrite = TRUE)



## in QGIS run neighbourhood analysis 



## Calculoate concentration 

# clip to boundary 

# con_rare <- rast(file.path("outputs", "sk_lf_rdc_rarity_101c.tif"))
# names(con_rare)= "rarity"
# 
# temp <- rast(file.path("inputs", "sk_rast_template.tif"))
# 
# # mask the raster by aoi and clip and then export
# con_rarec <- mask(con_rare, temp)
# writeRaster(con_rarec, file.path("outputs","sk_lf_rdc_rarity_101c_clip.tif"))


# read in clipped dataset
con_rarec <- rast(file.path("outputs","sk_lf_rdc_rarity_101c.tif"))
names(con_rarec)= "rarity"
#reclass the valyers to a conccentration 

# hist(con_rarec$rarity, breaks = 40)
# 
# aa <- sort(values(con_rarec))
# 
# quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE)
# ## WAITING ON INPUT FROM PAULA 

unique(values(con_rarec))

## from-to-becomes
# classify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.

# might need to update these....
m <- c(0, 1.1, 1,
       1.1, 1.3, 2,
       1.3, 1.5, 3,
       1.5, 2.5, 4,
       2.5, 6, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(con_rarec , rclmat, include.lowest=TRUE)

writeRaster(rc, file.path("outputs", "sk_rarity_conc.tif"), overwrite = TRUE)







#################################################################################################


## MOST COMMON bARCODES

## Calculate the most common bar code types for 1) all of skeena region and 2) per ecoregion 

# 1 ) skeena 

# read in the rare csv file 
rare <- read.csv(file.path("outputs", "lf_barcode_summary.csv")) 
rr = terra::rast(file.path("outputs", "sk_lf_barcode.tif"))

# Read in the skeena facets and update the most common barcodes >50% by land area 

common <- rare %>% 
  mutate(rare_id = case_when(
    acc_sum >= 50 ~ 1,
    .default = as.numeric(0)
  ))

# assign rarity class
class1 <-common %>% filter(rare_id == 1) %>% pull(layer1)

write.csv(class1, file.path("outputs", "common_terrestrial_barcodes_allsk.csv"))
length(class1)

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

reclass <- as.vector(unique(values(rr)))
sort(reclass)

# classify the values into groups 

m <- c(1, 1, 1,
       2, 999999, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
common <- classify(rr, rclmat, include.lowest=TRUE)

unique(values(common))
rc1 <- mask(common, aoi )
terra::writeRaster(rc1,file.path("outputs", "sk_entire_common.tif"), overwrite = TRUE)


### 

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



####################  

#Repeat this one with all the codes intact


## MOST COMMON bARCODES (individually)

## Calculate the most common bar code types per ecoregion

# read in the rare csv file 
rare <- read.csv(file.path("outputs", "lf_barcode_summary.csv")) 
rr = terra::rast(file.path("outputs", "sk_lf_barcode.tif"))
rr_poly <-  as.polygons(rr, na.rm=FALSE)
rr_sf <- st_as_sf(rr_poly)

# read in study area 
ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))

bc_ec <- st_intersection(rr_sf, ec) 

bc_ec <- bc_ec %>% 
  mutate(area_type = st_area(.))%>% 
  filter(!is.na(lyr.1))

st_write(bc_ec, file.path("outputs", "sk_lf_barcode_common_ecoreg_poly.gpkg"), append = FALSE)

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




