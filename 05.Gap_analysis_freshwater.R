# 05_gap analysis for freshwater 



library(terra)
library(sf)
library(readr)
library(janitor)
library(tidyr)
library(dplyr)

# read in study area 
aoi <- rast(file.path("inputs", "sk_rast_template.tif"))
temp <- st_read(file.path("inputs", "sk_poly_template.gpkg"))


# or read in final protected areas and final ecoregion 

pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))

ecsum_df <- read_csv(file.path("outputs", "ecoregion_area_totals.csv"))%>% select(-X)


##############################################################3

# 1: calculate % protected for all of Skeena
## read in protected layers 

## what proportion of the skeena region is currently protected? 
# how much area is protected within each eco_region: ()


# export 
#write_csv(pro_sum, file.path("outputs", "protection_per_ecoregion.csv"))

#################################################################################
## Break down of each type of catergory by ecoregion 


###############################
# Wetlands
################################

wet <- rast(file.path("inputs", "sk_wetland_density_class_1km.tif"))

wetpoly <- as.polygons(wet, na.rm=FALSE)
wet_sf <- st_as_sf(wetpoly)


# calcualte for all of skeena combined
sk_combined <- wet_sf  %>% 
  filter(!is.na(layer)) %>%
  mutate(wet_area_sk = st_area(.))%>% 
  st_drop_geometry()%>% 
  mutate(all_area_sk = sum(wet_area_sk))%>% 
  rowwise()%>%
  mutate(pc_type = wet_area_sk/all_area_sk * 100)


# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(wet_sf , ec) 

ec_divv <- ec_div   |> 
  dplyr::mutate(ec_area_m2 = st_area(geometry ))%>%
  filter(!is.na(layer))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, layer)%>%
  dplyr::mutate(wet_class_sum = sum(ec_area_m2)) %>% 
  dplyr::select( -ec_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_divvv, names_from = layer, values_from = wet_class_sum)

aa <- left_join(aa, ecsum_df )%>%
  select(-total_sk_area, -pc_of_sk)

type_by_wetland_class = aa

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "wetland_1km_weaver_class_per_ecoregion.csv"))



## How much is protected 
# 2 Determine level of protection per class

## read in protected layers 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pross_u <- pro %>% select(protected)

# 1 # get diversity class per ecoregion 
ec_div <- st_intersection(wet_sf , ec) 

# intersect protected areas
ec_div_pro <- st_intersection(ec_div, pross_u) %>%
  dplyr::mutate(pro_ec_wet_area = st_area(.))%>% 
  dplyr::filter(!is.na(layer)) %>%
  dplyr::group_by(ECOREGION_NAME, layer) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_wet_area)) %>% 
  dplyr::mutate(pro_area = as.numeric(pro_area_sum)) %>%
  dplyr::select(-protected, -pro_area_sum, - pro_ec_wet_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()

#st_write(ec_div_pro, file.path("outputs", "sk_pro_high_div_test.gpkg"))

aa <- pivot_wider(ec_div_pro, names_from = layer, values_from = pro_area)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>%
  dplyr::rename("1_p" = `1`,
                "2_p" = `2`,
                "3_p" = `3`)  

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


eco_pro_rardiv_output <- left_join(type_by_wetland_class, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "1", "1_p",  "2" ,"2_p" ,
         "3", "3_p", "area_m2"  )            


write_csv(eco_pro_rardiv_output, file.path("outputs", "wetland_density_class_per_ecoregion_protection.csv"))




##############################################################################
# Lakes 
############################################################################


# or read in final protected areas and final ecoregion 

pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))

ecsum_df <- read_csv(file.path("outputs", "ecoregion_area_totals.csv"))%>% select(-X)



##### Lakes Rarity - by ecosystem and by protection #################################3

library(tidyr)
# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_lakes_raritybyarea_conc.tif"))
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

lakes_rare_area_totals = aa

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "rarity_lakes_class_per_ecoregion.csv"))


## How much of the rare areas are protected. 

# 2 Determine level of protection per class

## read in protected layers 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pross_u <- pro %>% select(protected)

# 1 # get diversity class per ecoregion 
ec_div <- st_intersection(div_sf , ec) 

# intersect protected areas
ec_div_pro <- st_intersection(ec_div, pross_u) %>%
  dplyr::mutate(pro_ec_div_area = st_area(.))%>% 
  dplyr::filter(!is.na(rarity)) %>%
  dplyr::group_by(ECOREGION_NAME, rarity) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_div_area)) %>% 
  dplyr::mutate(pro_area = as.numeric(pro_area_sum))%>%
  dplyr::select(-protected, -pro_area_sum, - pro_ec_div_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()

#st_write(ec_div_pro, file.path("outputs", "sk_pro_high_div_test.gpkg"))

aa <- pivot_wider(ec_div_pro, names_from = rarity, values_from = pro_area)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>%
  dplyr::rename("1_p" = `1`,
                "2_p" = `2`,
                "3_p" = `3`,
                "4_p" = `4`,
                "5_p" = `5`)  


# area of protection per ecoregion per catergory (ha)
aa 



eco_pro_rardiv_output <- left_join(lakes_rare_area_totals, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
 select("ECOREGION_NAME", "1", "1_p",  "2" ,"2_p" ,
                            "3", "3_p", "4", "4_p", "5","5_p","area_m2" )           


write_csv(eco_pro_rardiv_output, file.path("outputs", "lakes_rarity_class_per_ecoregion_protection.csv"))




################################################################################3

# Lakes  # 1) diverity 

# read in concentration layers: diverity 
div <- rast(file.path("outputs", "sk_lakes_divarea_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# calcualte for all of skeena combined
sk_combined <- div_sf %>% 
  dplyr::filter(!is.na(diversity)) %>%
  dplyr::mutate(div_area_sk = st_area(.))%>% 
  st_drop_geometry()%>% 
  dplyr::mutate(all_area_sk = sum(div_area_sk))%>% 
  dplyr::rowwise()%>%
  dplyr::mutate(pc_type = div_area_sk/all_area_sk * 100)%>% 
  filter(diversity>0)


# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , ec) %>% 
  filter(diversity>0)

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

lakes_diversity_area_totals = aa

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "lakes_diversity_class_per_ecoregion.csv"))


# 2 Determine level of protection per class
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

aa <- pivot_wider(ec_div_pro, names_from = diversity, values_from = pro_area)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>%
  dplyr::rename("1_p" = `1`,
                "2_p" = `2`,
                "3_p" = `3`,
                "4_p" = `4`,
                "5_p" = `5`)  


# area of protection per ecoregion per catergory (ha)
aa 

eco_pro_rardiv_output <- left_join(lakes_diversity_area_totals, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "1", "1_p",  "2" ,"2_p" ,
         "3", "3_p", "4", "4_p", "5","5_p","area_m2" )           


write_csv(eco_pro_rardiv_output, file.path("outputs", "lakes_diversity_class_per_ecoregion_protection.csv"))



#################################










################################################################################3

# Rivers 

################################################################################

# 1) diverity 


# read in concentration layers: diverity and rarity
div <- rast(file.path("outputs", "sk_rivers_diversity_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)#%>% 
 # dplyr::filter(diversity >=4)


# calcualte for all of skeena combined
sk_combined <- div_sf %>% 
  dplyr::filter(!is.na(diversity)) %>%
  dplyr::mutate(div_area_sk = st_area(.))%>% 
  st_drop_geometry()%>% 
  dplyr::mutate(all_area_sk = sum(div_area_sk))%>% 
  dplyr::rowwise()%>%
  dplyr::mutate(pc_type = div_area_sk/all_area_sk * 100)%>% 
  filter(diversity>0)


# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , ec) %>% 
  filter(diversity>0)

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

rivers_diversity_area_totals = aa

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "rivers_diversity_class_per_ecoregion.csv"))


# 2 Determine level of protection per class
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

aa <- pivot_wider(ec_div_pro, names_from = diversity, values_from = pro_area)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>%
  dplyr::rename("1_p" = `1`,
                "2_p" = `2`,
                "3_p" = `3`,
                "4_p" = `4`,
                "5_p" = `5`)  


# area of protection per ecoregion per catergory (ha)
aa 

eco_pro_rardiv_output <- left_join(rivers_diversity_area_totals, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "1", "1_p",  "2" ,"2_p" ,
         "3", "3_p", "4", "4_p", "5","5_p","area_m2" )           


write_csv(eco_pro_rardiv_output, file.path("outputs", "rivers_diversity_class_per_ecoregion_protection.csv"))


#############################################################################3

## rivers Rarity 

library(tidyr)
# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_rivers_rarity_mean_conc.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = sk_rivers_rarity_mean_101c)
# 
# sk_combined_rare <- div_sf %>% 
#   filter(!is.na(rarity)) %>%
#   mutate(rare_area_sk = st_area(.))%>% 
#   st_drop_geometry()%>% 
#   mutate(all_area_sk = sum(rare_area_sk))%>% 
#   rowwise()%>%
#   mutate(pc_type = rare_area_sk/all_area_sk * 100)


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

rivers_rare_area_totals = aa

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "rarity_rivers_class_per_ecoregion.csv"))


## How much of the rare areas are protected. 

# 2 Determine level of protection per class

## read in protected layers 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
pross_u <- pro %>% select(protected)

# 1 # get diversity class per ecoregion 
ec_div <- st_intersection(div_sf , ec) 

# intersect protected areas
ec_div_pro <- st_intersection(ec_div, pross_u) %>%
  dplyr::mutate(pro_ec_div_area = st_area(.))%>% 
  dplyr::filter(!is.na(rarity)) %>%
  dplyr::group_by(ECOREGION_NAME, rarity) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_div_area)) %>% 
  dplyr::mutate(pro_area = as.numeric(pro_area_sum))%>%
  dplyr::select(-protected, -pro_area_sum, - pro_ec_div_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()

#st_write(ec_div_pro, file.path("outputs", "sk_pro_high_div_test.gpkg"))

aa <- pivot_wider(ec_div_pro, names_from = rarity, values_from = pro_area)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>%
  dplyr::rename("1_p" = `1`,
                "2_p" = `2`,
                "3_p" = `3`,
                "4_p" = `4`,
                "5_p" = `5`)  


# area of protection per ecoregion per catergory (ha)
aa 

eco_pro_rardiv_output <- left_join(lakes_rare_area_totals, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "1", "1_p",  "2" ,"2_p" ,
         "3", "3_p", "4", "4_p", "5","5_p","area_m2" )           


write_csv(eco_pro_rardiv_output, file.path("outputs", "rivers_rarity_class_per_ecoregion_protection.csv"))



