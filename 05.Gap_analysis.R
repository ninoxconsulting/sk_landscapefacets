#1. Gap analysis 

#library(bcdata)
library(dplyr)
library(terra)
library(sf)
library(readr)
library(janitor)
library(tidyr)

# read in study area 
aoi <- rast(file.path("inputs", "sk_rast_template.tif"))
temp <- st_read(file.path("inputs", "sk_poly_template.gpkg"))


# # 1. Standardize the protection layer
# ## read in protected layers
# 
# pro <- st_read(file.path("inputs", "protected_lands.gpkg"))
# con <- st_read(file.path("inputs", "cons_lands.gpkg"))%>%
#   filter(CONSERVATION_LAND_TYPE %in% c("Administered Lands","Wildlife Management Areas")) %>%
#   rename("PROTECTED_LANDS_NAME" = SITE_NAME,
#          "PROTECTED_LANDS_DESIGNATION" = CONSERVATION_LAND_TYPE) %>%
#   select(-TENURE_DESCRIPTION, -TENURE_TYPE)
# 
# pros <- bind_rows(pro, con)
# 
# 
# # add the bc conservancy lands 
# pcon <- st_read(file.path("inputs", "bc_conservancy.gpkg"))
# 
# pros <- bind_rows(pros, pcon)
# 
# pros <- pros %>%
#   mutate(protected = "protected")
# 
# 
# # clip to the regions
# pross <- st_intersection(pros, temp) %>%
#   select(-MLF_Kehm_2012, -MLF_Kehm_2012.1 )
# 
# st_write(pross, file.path("outputs", "sk_protected_lands.gpkg"), append = FALSE)
# 



# or read in final protected areas and final ecoregion 

pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))

ecsum_df <- read_csv(file.path("outputs", "ecoregion_area_totals.csv"))%>% select(-X)


# 1: calculate % protected for all of Skeena
## read in protected layers 

## what proportion of the skeena region is currently protected? 
# how much area is protected within each eco_region: ()

# simplify protected area 
pross_u <- pro %>% select(protected)
pro_ecoreg <- st_intersection( ec, pross_u) 

pro_ecoreg <- pro_ecoreg |> 
  mutate(pro_area = st_area(pro_ecoreg))

pro_sum <- pro_ecoreg %>%
  st_drop_geometry() |>
  select(-protected)
 
 pro_sum <- aggregate(pro_sum$pro_area, by=list(ECOREGION_NAME=pro_sum$ECOREGION_NAME), FUN=sum)
 names(pro_sum) <- c("ECOREGION_NAME", "eco_pro_m2")

 
# # join the protected area with the total area  per ecoregion and calculate the percentage 

 pro_sum <- left_join(pro_sum, ecsum_df ) %>% 
   #select(-total_sk_area, -pc_of_sk) %>% 
   rowwise() %>% 
   dplyr::mutate(eco_pro_pc = (eco_pro_m2/area_m2)*100)

# export 
 write_csv(pro_sum, file.path("outputs", "protection_per_ecoregion.csv"))
 
# pro_sum <- pro_ecoreg %>%
#   st_drop_geometry() |> 
#   group_by(ECOREGION_NAME) |> 
#   mutate(prosum = sum(pro_area)) %>%
#   select(-protected, -pro_area) |> 
#   distinct()
# 
# ecsum <- ec %>%
#   st_drop_geometry() |> 
#   group_by(ECOREGION_NAME) |> 
#   mutate(totsum = sum(area_m2))
# 
# ecpro <- left_join(pro_sum, ecsum)%>%
#   group_by(ECOREGION_NAME) |> 
#   mutate(pc_pro = (prosum/totsum)*100)
# 

# # summary for all of skeena 
 
sum(pro_sum$eco_pro_m2)/sum(pro_sum$area_m2) * 100

sk_pro_sum = sum(pro_sum$eco_pro_m2)
sk_area_sum = sum(pro_sum$area_m2)



#################################################################################
## Break down of each type of catergory by ecoregion 

# 1) diverity 
# 2) rarity
# 3) high diversity and rartity 


# read in concentration layers: diverity and rarity
div <- rast(file.path("outputs", "sk_diversity_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)


# calcualte for all of skeena combined
sk_combined <- div_sf %>% 
  filter(!is.na(diversity)) %>%
  mutate(div_area_sk = st_area(.))%>% 
  st_drop_geometry()%>% 
  mutate(all_area_sk = sum(div_area_sk))%>% 
  rowwise()%>%
  mutate(pc_type = div_area_sk/all_area_sk * 100)


# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , ec) 

ec_divv <- ec_div   |> 
  dplyr::mutate(ec_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(diversity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, diversity)%>%
  dplyr::mutate(div_class_sum = sum(ec_area_m2)) %>% 
  dplyr::select( -ec_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_divvv, names_from = diversity, values_from = div_class_sum)

aa <- left_join(aa, ecsum_df )%>%
  select(-total_sk_area, -pc_of_sk)

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "diversity_class_per_ecoregion.csv"))





################################################################################

## 2. Repeat for Rarity Class - per ecoregion 
library(tidyr)
# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_rarity_conc.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

sk_combined_rare <- div_sf %>% 
  filter(!is.na(rarity)) %>%
  mutate(rare_area_sk = st_area(.))%>% 
  st_drop_geometry()%>% 
  mutate(all_area_sk = sum(rare_area_sk))%>% 
  rowwise()%>%
  mutate(pc_type = rare_area_sk/all_area_sk * 100)


# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , ec) 

ec_divv <- ec_div   |> 
  dplyr::mutate(ec_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, rarity)%>%
  dplyr::mutate(rare_class_sum = sum(ec_area_m2)) %>% 
  dplyr::select( -ec_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_divvv, names_from = rarity, values_from = rare_class_sum)

aa <- left_join(aa, ecsum_df )%>%
  select(-total_sk_area, -pc_of_sk)

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "rarity_class_per_ecoregion.csv"))





################################################################################

# 3. repeat for the rare and diversit catergories 

## Repeat for Rarity Class - per ecoregion 
divrare <- rast(file.path("outputs", "high_div_rare.tif"))

# unique va;ues 
# 1         0  - not rare or diversity (common)
# 2         4  - Rare (r)
# 3         5  - Very Rare (vr)
# 4        40  - High Variety (hv)y
# 5        44  - high Variety and rare (hv_r)
# 6        45  - high variety and very rare (hv_vr)
# 7        50  - Very High Variety  (vhv)
# 8        54  - Very High Variety and rare (vhv_r)
# 9        55  - Very High Variety and very rare (vhv_vr)

#"r", "vr", "hv", "hv_r", "hv_vr", "vhv". "vhv_r", "vhv_vr"

divpoly <- as.polygons(divrare, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

sk_combined_rarediv <- div_sf %>% 
  filter(!is.na(diversity )) %>%
  mutate(rare_area_sk = st_area(.))%>% 
  st_drop_geometry()%>% 
  mutate(all_area_sk = sum(rare_area_sk))%>% 
  rowwise()%>%
  mutate(pc_type = rare_area_sk/all_area_sk * 100)




# 1 # get diversity class per ecoregion 
ec_div <- st_intersection(div_sf , ec) 

ec_divv <- ec_div   |> 
   dplyr::mutate(ec_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(diversity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, diversity)%>%
  dplyr::mutate(divrare_class_sum = sum(ec_area_m2)) %>%
  dplyr::select( -ec_area_m2) %>%
  st_drop_geometry()

aa <- pivot_wider(ec_divvv, names_from = diversity, values_from = divrare_class_sum)
 
aa <- left_join(aa, ecsum_df )%>%
    select(-total_sk_area, -pc_of_sk)


# unique va;ues 
# 1         0  - not rare or diversity (common)
# 2         4  - Rare (r)
# 3         5  - Very Rare (vr)
# 4        40  - High Variety (hv)
# 5        44  - high Variety and rare (hv_r)
# 6        45  - high variety and very rare (hv_vr)
# 7        50  - Very High Variety  (vhv)
# 8        54  - Very High Variety and rare (vhv_r)
# 9        55  - Very High Variety and very rare (vhv_vr)

#"r", "vr", "hv", "hv_r", "hv_vr", "vhv". "vhv_r", "vhv_vr"

aa <- aa %>%
  dplyr::rename("common" = `0`,
                "r" = `4`,
                "vr" = `5`,
                "hv" = `40`,
                "hv_r" = `44`,
                "hv_vr" = `45`,
                "vhv" = `50`,
                "vhv_r" = `54`,
                "vhv_vr_p" = `55`)  



names(aa)<- c("ECOREGION_NAME", "common", "r", "vr", "hv", "hv_r", "hv_vr", "vhv", "vhv_r", "vhv_vr", "area_m2")

type_by_ecoregion <- aa # keep this for the calculation of protected areas below 

ab <-aa %>% 
      dplyr::ungroup() %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
      dplyr::select(-area_m2)%>% 
      dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "divrarity_class_per_ecoregion.csv"))





##################################################################################


# 2 Determine level of protection per class

## Repeat for Rarity Class - per ecoregion 
divrare <- rast(file.path("outputs", "high_div_rare.tif"))

# unique va;ues 
# 1         0  - not rare or diversity (common)
# 2         4  - Rare (r)
# 3         5  - Very Rare (vr)
# 4        40  - High Variety (hv)
# 5        44  - high Variety and rare (hv_r)
# 6        45  - high variety and very rare (hv_vr)
# 7        50  - Very High Variety  (vhv)
# 8        54  - Very High Variety and rare (vhv_r)
# 9        55  - Very High Variety and very rare (vhv_vr)

#"r", "vr", "hv", "hv_r", "hv_vr", "vhv". "vhv_r", "vhv_vr"

# calculate the summary of ecoregions within skeeena region

divpoly <- as.polygons(divrare, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

## read in protected layers 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pross_u <- pro %>% select(protected)

# 1 # get diversity class per ecoregion 
ec_div <- st_intersection(div_sf , ec) 

# intersect protected areas
ec_div_pro <- st_intersection(ec_div, pross_u) %>%
  dplyr::mutate(pro_ec_div_area = st_area(.))%>% 
  dplyr::filter(!is.na(diversity)) %>%
  dplyr::group_by(ECOREGION_NAME, diversity) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_div_area)) %>% 
  dplyr::mutate(pro_area = as.numeric(pro_area_sum))%>%
  dplyr::select(-protected, -pro_area_sum, - pro_ec_div_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()

#st_write(ec_div_pro, file.path("outputs", "sk_pro_high_div_test.gpkg"))

aa <- pivot_wider(ec_div_pro, names_from = diversity, values_from = pro_area)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>%
  dplyr::rename("common_p" = `0`,
                "r_p" = `4`,
                       "vr_p" = `5`,
                       "hv_p" = `40`,
                       "hv_r_p" = `44`,
                       "hv_vr_p" = `45`,
                       "vhv_p" = `50`,
                       "vhv_r_p" = `54`,
                       "vhv_vr_p" = `55`)  
                       
# unique va;ues 
# 1         0  - not rare or diversity (common)
# 2         4  - Rare (r)
# 3         5  - Very Rare (vr)
# 4        40  - High Variety (hv)
# 5        44  - high Variety and rare (hv_r)
# 6        45  - high variety and very rare (hv_vr)
# 7        50  - Very High Variety  (vhv)
# 8        54  - Very High Variety and rare (vhv_r)
# 9        55  - Very High Variety and very rare (vhv_vr)

# area of protection per ecoregion per catergory (ha)
aa 

# # areas of ecoregion per catergory 
# type_by_ecoregion <- type_by_ecoregion %>%
#   arrange(`ECOREGION_NAME`)
#   # ha area 
# 
# # totoal protection and total area 
# ecpro <- ecpro %>%
#   #rename(`ECOREGION_NAME ` = ECOREGION_NAME)%>%
#   arrange(`ECOREGION_NAME `) %>%
#   select(-totsum)


eco_pro_rardiv_output <- left_join(type_by_ecoregion, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "common", "common_p",  "r" ,"r_p" ,
         "vr", "vr_p"   ,  "hv"  ,  "hv_p" , "hv_r",  "hv_r_p" , 
         "hv_vr" ,  "hv_vr_p" ,           "vhv"  , "vhv_p" ,
         "vhv_r" , "vhv_r_p",  "vhv_vr"  ,  "vhv_vr_p",  "area_m2"  )            
                                   

write_csv(eco_pro_rardiv_output, file.path("outputs", "divrarity_class_per_ecoregion_protection.csv"))










#################

# gap analysis for primary productivity  by ecoregion 

# 1) ndvi - very high and high
# 2) gdd - per catergories


# read in concentration layers: diverity and rarity
gdd <- rast(file.path("outputs",  "gdd_sk_classed.tif"))
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))

#m <- c(-1, 800, 0,
#       800, 900, 1,
#       900, 1100, 2,
#       1100, 1400, 3, 
#       1400, 1600, 4)

gddpoly <- as.polygons(gdd , na.rm=FALSE)
gdd_sf <- st_as_sf(gddpoly)%>% 
  filter(!is.na(DD5))

# all of skeena 

sk_combined_ggd <- gdd_sf  %>% 
  dplyr::mutate(gdd_area_sk = st_area(.))%>% 
  st_drop_geometry()%>% 
  dplyr::mutate(all_area_sk = sum(gdd_area_sk))%>% 
  rowwise()%>%
  mutate(pc_type = gdd_area_sk/all_area_sk * 100)


# 1 By ecoregion 
ec_gdd <- st_intersection(gdd_sf , ec) 

ec_gdd  <- ec_gdd   |> 
  mutate(ggd_area_m2 = st_area(ec_gdd ))%>%
  filter(!is.na(DD5))

ec_gddd <- ec_gdd %>%
  dplyr::group_by(ECOREGION_NAME, DD5 )%>%
  dplyr::mutate(gdd_class_sum = sum(ggd_area_m2)) %>% 
  select(-ggd_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_gddd, names_from = DD5, values_from = gdd_class_sum)

aa <- left_join(aa, ecsum_df )%>%
  select(-total_sk_area, -pc_of_sk)

ggd_area_by_ecoregion_m2 <- aa # keeping this for the protection analysis 

# convert to ha 
ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  dplyr::select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))


names(ab)<- c("ECOREGION_NAME", "lt_800", "800_900", "900_1100", "1100_1400", "gt_1400")

#m <- c(-1, 800, 0,
#       800, 900, 1,
#       900, 1100, 2,
#       1100, 1400, 3, 
#       1400, 1600, 4)

write_csv(ab, file.path("outputs", "gdd_class_per_ecoregion.csv"))


########################################################################

# repeat for NDVI? 

# read in concentration layers: diverity and rarity
ndvi<- rast(file.path("outputs",  "ndvi_high_vhigh.tif"))
ndvi <- mask(ndvi, aoi)
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))

#m <- c(-1, 800, 0,
#       800, 900, 1,
#       900, 1100, 2,
#       1100, 1400, 3, 
#       1400, 1600, 4)


ndvipoly <- as.polygons(ndvi , na.rm=FALSE)
ndvi_sf <- st_as_sf(ndvipoly) %>% 
  filter(!is.na(min))%>%
  filter(min !=0)

# all of skeena 

sk_combined_ndvi <- ndvi_sf   %>% 
  dplyr::mutate(ndvi_area_sk = st_area(.))%>% 
  st_drop_geometry()%>%
  dplyr::mutate(all_area_sk = 243366640125) %>% 
  rowwise()%>%
  dplyr::mutate(pc_type = ndvi_area_sk/all_area_sk * 100)


# 1 By ecoregion 
ec_ndvi <- st_intersection(ndvi_sf , ec) 

ec_ndvii  <- ec_ndvi   |> 
  mutate(ndvi_area_m2 = st_area(geometry))%>%
  filter(!is.na(min))

ec_ndvii <- ec_ndvii %>%
  dplyr::group_by(ECOREGION_NAME, min)%>%
  dplyr::mutate(ndvi_class_sum = sum(ndvi_area_m2)) %>% 
  select(-ndvi_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_ndvii, names_from = min, values_from = ndvi_class_sum)

aa <- left_join(aa, ecsum_df )%>%
  select(-total_sk_area, -pc_of_sk)

ndvi_area_by_ecoregion_m2 <- aa # keeping this for the protection analysis 

# convert to ha 
ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  dplyr::select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))


names(ab)<- c("ECOREGION_NAME", "high", "very high")

#m <- c(-1, 800, 0,
#       800, 900, 1,
#       900, 1100, 2,
#       1100, 1400, 3, 
#       1400, 1600, 4)

write_csv(ab, file.path("outputs", "ndvi_class_per_ecoregion.csv"))




###########################################################################

### How much of NDVI and GDD is protected> 

# read in concentration layers: diverity and rarity
ndvi<- rast(file.path("outputs",  "ndvi_high_vhigh.tif"))
ndvi <- mask(ndvi, aoi)
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))

ndvipoly <- as.polygons(ndvi , na.rm=FALSE)
ndvi_sf <- st_as_sf(ndvipoly) %>% 
  filter(!is.na(min))%>%
  filter(min !=0)

## read in protected layers 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pross_u <- pro %>% select(protected)

# 1 # get diversity class per ecoregion 
ec_div <- st_intersection(ndvi_sf , ec) 

# intersect protected areas
ec_div_pro <- st_intersection(ec_div, pross_u) %>%
  dplyr::mutate(pro_ec_ndvi_area = st_area(.))%>% 
  dplyr::filter(!is.na(min)) %>%
  dplyr::group_by(ECOREGION_NAME, min) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_ndvi_area)) %>% 
  #dplyr::mutate(pro_area = as.numeric(pro_area_sum/1000))%>%
  dplyr::select(-protected, -pro_ec_ndvi_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()

aa <- pivot_wider(ec_div_pro, names_from = min, values_from = pro_area_sum)%>%
  arrange(ECOREGION_NAME)

names(aa)<- c("ECOREGION_NAME", "high_p", "very_high_p")

# area of protection per ecoregion per catergory (ha)
aa 
names(ndvi_area_by_ecoregion_m2) <-c("ECOREGION_NAME", "high", "very_high", "area_m2")

eco_pro_ndvi_output <- left_join(ndvi_area_by_ecoregion_m2, aa) 

eco_pro_ndvi_output <-eco_pro_ndvi_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "high", "high_p",  "very_high" ,"very_high_p" , "area_m2"  )            


write_csv(eco_pro_ndvi_output, file.path("outputs", "ndvi_class_per_ecoregion_protection.csv"))




#### GDD Protectes 

# read in concentration layers:
gdd <- rast(file.path("outputs",  "gdd_sk_classed.tif"))
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
gdd <- mask(gdd, aoi)

#m <- c(-1, 800, 0,
#       800, 900, 1,
#       900, 1100, 2,
#       1100, 1400, 3, 
#       1400, 1600, 4)

gddpoly <- as.polygons(gdd , na.rm=FALSE)
gdd_sf <- st_as_sf(gddpoly)%>% 
  filter(!is.na(DD5))

## read in protected layers 
pross_u <- pro %>% select(protected)

# 1 # get diversity class per ecoregion 
ec_div <- st_intersection(gdd_sf , ec) 

# intersect protected areas
ec_div_pro <- st_intersection(ec_div, pross_u) %>%
  dplyr::mutate(pro_ec_gdd_area = st_area(.))%>% 
  dplyr::filter(!is.na(DD5)) %>%
  dplyr::group_by(ECOREGION_NAME, DD5) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_gdd_area)) %>% 
  #dplyr::mutate(pro_area = as.numeric(pro_area_sum/1000))%>%
  dplyr::select(-protected, -pro_ec_gdd_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()

aa <- pivot_wider(ec_div_pro, names_from = DD5, values_from = pro_area_sum)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>% select(ECOREGION_NAME, `0`, `1`, `2`, `3`, `4`)

names(aa)<- c("ECOREGION_NAME", "lt_800_p", "800_900_p", "900_1100_p", "1100_1400_p", "gt_1400_p")

# area of protection per ecoregion per catergory (ha)
aa 
names(ggd_area_by_ecoregion_m2) <- c("ECOREGION_NAME", "lt_800", "800_900", "900_1100", "1100_1400", "gt_1400", "area_m2")


eco_pro_ggd_output <- left_join(ggd_area_by_ecoregion_m2, aa) 

eco_pro_ggd_output <-eco_pro_ggd_output %>%
  dplyr::mutate(across(where(is.numeric), round, 0)) %>%
  select("ECOREGION_NAME", "lt_800", "lt_800_p", "800_900","800_900_p", "900_1100","900_1100_p", 
         "1100_1400", "1100_1400_p","gt_1400","gt_1400_p", "area_m2"  )            


write_csv(eco_pro_ggd_output, file.path("outputs", "gdd_class_per_ecoregion_protection.csv"))





############################################################################

## Potection for wilderness areas - seee wilderness script





































# 
# 
# 
# ex <- expanse(divrare, unit="m", byValue=TRUE, zones=ecr, wide=TRUE)[,-1]
# 
# write_csv(ex, file.path("outputs", "div_per_ecoregion.csv"))
# 
# 
# ex <- readr::read_csv(file.path("outputs", "div_per_ecoregion.csv")) %>%
#   select(-...1)
# 
# # calculate the % cover of each group by class 
# pc_div_sum <- ex %>%
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits = 1) |> 
#   bind_cols(tot_area = ecsum$totsum)
# 
# write.csv(pc_div_sum,file.path("outputs", "div_pcper_ecoregion.csv"))
# 
# 
# 
# ecsum <- ec %>%
#   st_drop_geometry() |> 
#   group_by(ECOREGION_NAME) |> 
#   mutate(totsum = sum(area_m2))
# 
# ecv <- vect(ec)
# 
# 
# # # calculate the number of pixels per group, per ecoregion 
# # e <- extract(div, ecv, un="table", na.rm=TRUE, exact=FALSE)
# # edf <- data.frame(NAME_2=ecv$ECOREGION_NAME[e[,1]], e[,-1])
# # 
# # edff <- edf %>% 
# #   group_by(NAME_2, e....1.) |> 
# #   count()
# 
# # rasterize the zones
# ecr <- rasterize(ecv, aoi, field="ECOREGION_NAME")
# ex <- expanse(divrare, unit="m", byValue=TRUE, zones=ecr, wide=TRUE)[,-1]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # rasterize the zones
# pror <- rasterize(pross, temp, field="protected")
# 
# 
# 
# #diversity
# div_ex <- expanse(div, unit="m", byValue=TRUE, zones=pror, wide=TRUE)[,-1]
# write.csv(div_ex, file.path("outputs", "div_per_protection.csv"))
# 
# # rarity
# rare_ex <- expanse(rar, unit="m", byValue=TRUE, zones=pror, wide=TRUE)[,-1]
# 
# 
# 
# ## what proporion of diversity classes are protected? 
# 
# 
# write.csv(rare_ex, file.path("outputs", "rare_per_ecoregion.csv"))
# 
# 
# ex <- readr::read_csv(file.path("outputs", "rare_per_ecoregion.csv"))%>%
#   select(-...1)
# 
# # calculate the % cover of each group by class 
# library(janitor)
# pc_rare_sum <- ex %>%
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits = 1) |> 
#   bind_cols(tot_area = ecsum$totsum)
# 
# write.csv(pc_rare_sum,file.path("outputs", "rare_pcper_ecoregion.csv"))
# 
# 
# 
# 
# ## what proporion of rarity classes are protected? 
# 
# 
# # 
# # 
# # # read in concentration layers: diverity and rarity
# # divrare <- rast(file.path("outputs", "high_div_rare.tif"))
# # 
# # # unique va;ues 
# # # 1         0  - not rare or diversity (common)
# # # 2         4  - Rare (r)
# # # 3         5  - Very Rare (vr)
# # # 4        40  - High Variety (hv)
# # # 5        44  - high Variety and rare (hv_r)
# # # 6        45  - high variety and very rare (hv_vr)
# # # 7        50  - Very High Variety  (vhv)
# # # 8        54  - Very High Variety and rare (vhv_r)
# # # 9        55  - Very High Variety and very rare (vhv_vr)
# # 
# # #"r", "vr", "hv", "hv_r", "hv_vr", "vhv". "vhv_r", "vhv_vr"
# # 
# # 
# # # TYPE 1 SUMMARY BY ECOREGION 
# # 
# # ec <- ec %>%
# #   select(ECOREGION_NAME)%>% 
# #   mutate(area_m2 = st_area(.))
# # 
# # 
# # # calculate the summary of ecoregions within skeeena region
# # 
# # ecsum <- ec %>%
# #   st_drop_geometry() |> 
# #   group_by(ECOREGION_NAME) |> 
# #   mutate(totsum = sum(area_m2))
# # 
# # ecv <- vect(ec)
# # 
# # 
# # # # calculate the number of pixels per group, per ecoregion 
# # # e <- extract(div, ecv, un="table", na.rm=TRUE, exact=FALSE)
# # # edf <- data.frame(NAME_2=ecv$ECOREGION_NAME[e[,1]], e[,-1])
# # # 
# # # edff <- edf %>% 
# # #   group_by(NAME_2, e....1.) |> 
# # #   count()
# # 
# # # rasterize the zones
# # ecr <- rasterize(ecv, aoi, field="ECOREGION_NAME")
# # ex <- expanse(divrare, unit="m", byValue=TRUE, zones=ecr, wide=TRUE)[,-1]
# # 
# # names(ex)<- c("zone", "common", "r", "vr", "hv", "hv_r", "hv_vr", "vhv", "vhv_r", "vhv_vr")
# # 
# # write_csv(ex, file.path("outputs", "div_per_ecoregion.csv"))
# # 
# # 
# # ex <- readr::read_csv(file.path("outputs", "div_per_ecoregion.csv")) 
# # 
# # # calculate the % cover of each group by class 
# # pc_div_sum <- ex %>%
# #   adorn_percentages("row") %>%
# #   adorn_pct_formatting(digits = 1) |> 
# #   bind_cols(tot_area = ecsum$totsum) 
# #   
# #   names(pc_div_sum)<- c("zone", "common", "r", "vr", "hv", "hv_r", "hv_vr", "vhv", "vhv_r", "vhv_vr", "tot_area")
# #   
# # write.csv(pc_div_sum,file.path("outputs", "div_pcper_ecoregion.csv"))
# #   
# # 
# # 
# 
