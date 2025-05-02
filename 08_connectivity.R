# 08 Pther data set

library(terra)
library(tidyr)

# download the data from pither

template <- rast(file.path("inputs", "sk_rast_template.tif"))
ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))
#ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))

ecsum_df <- read_csv(file.path("outputs", "ecoregion_area_totals.csv"))%>% select(-X)

# connectivity 
#conn <- rast(file.path("inputs", "pither_conductance.tif"))

#hist(conn)
#aa <- sort(values(conn, na.rm = T))
#quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE)

#aq <- as.data.frame(quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE))
#names(aq) = "value"

# 
# # cost
# costt <- rast(file.path("inputs", "pither_move_cost.tif"))
# hist(costt)
# aa <- sort(values(costt, na.rm = T))
# quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE)
# 
# aq <- as.data.frame(quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE))
# names(aq) = "value"



# resistance

res <- rast(file.path("inputs", "pither_resistance.tif"))
names(res) = "resistance"

hist(res)
aa <- sort(values(res, na.rm = T))
quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE)

median(aa)
mean(aa)


aq <- as.data.frame(quantile(aa, probs = seq(0, 1, 0.05), na.rm = TRUE))
names(aq) = "value"


# set threshold of above 90% 
# above 90% - 2.977584e+00


m <- c(-10, 2.977584e+00, 0,
       2.977584e+00, 999999, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
common <- classify(res, rclmat, include.lowest=TRUE)

unique(values(common))
rc1 <- mask(common, template )
terra::writeRaster(rc1,file.path("outputs", "sk_pither_resistence_90threshold.tif"), overwrite = TRUE)



# or 40% 

m <- c(-10, 1.037420e+00, 0,
       1.037420e+00, 999999, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
common <- classify(res, rclmat, include.lowest=TRUE)

unique(values(common))
rc1 <- mask(common, template )
terra::writeRaster(rc1,file.path("outputs", "sk_pither_resistence_40threshold.tif"), overwrite = TRUE)





###################################################################################

## Compare the areas per ecoregion and protected areas 

##############################################################################

library(tidyr)
# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_pither_resistence_90threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = resistance)

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

resistence_area_totals = aa

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "resistence_pither_per_ecoregion.csv"))


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
  dplyr::rename("0_p" = `0`,
                "1_p" = `1`)  

# area of protection per ecoregion per catergory (ha)
aa 

eco_pro_rardiv_output <- left_join(resistence_area_totals, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "1", "1_p",  "0" ,"0_p" ,
        "area_m2" )           


write_csv(eco_pro_rardiv_output, file.path("outputs", "resistence_per_ecoregion_protection.csv"))


#########################################################################





### Jessica macro refugia and macrorefugia 2080

#template <- rast(file.path("inputs", "sk_rast_template.tif"))

# microrefugia 
mic <- rast(file.path("inputs", "microrefugia.tif"))
rc1 <- mask(mic, srast )
terra::writeRaster(rc1,file.path(outputs, "microrefugia_c.tif"), overwrite = TRUE)

# threshold values of 0.7
m <- c(0, 0.7, 0,
       0.7, 999999, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
common <- classify(mic, rclmat, include.lowest=TRUE)

#unique(values(common))
rc1 <- mask(common, srast )
terra::writeRaster(rc1,file.path(outputs, "microrefugia_70threshold.tif"), overwrite = TRUE)



# macrorefugia
mic <- rast(file.path("inputs", "2080s_macrorefugia.tif"))
rc1 <- mask(mic, srast )
terra::writeRaster(rc1,file.path(outputs, "macrorefugia_c.tif"), overwrite = TRUE)

m <- c(0, 0.7, 0,
       0.7, 999999, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
common <- classify(mic, rclmat, include.lowest=TRUE)

#unique(values(common))
rc1 <- mask(common, srast )
terra::writeRaster(rc1,file.path(outputs, "macrorefugia_70threshold.tif"), overwrite = TRUE)



## Gap analysis 

###################################################################################

## Compare the areas per ecoregion and protected areas 

##############################################################################

## microrefugia 


library(tidyr)
# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_microrefugia_70threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , ec) 

ec_divv <- ec_div   |> 
  dplyr::mutate(ec_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(microrefugia))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, microrefugia)%>%
  dplyr::mutate(rare_class_sum = sum(ec_area_m2)) %>% 
  dplyr::select( -ec_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_divvv, names_from = microrefugia, values_from = rare_class_sum)

aa <- left_join(aa, ecsum_df )%>%
  select(-total_sk_area, -pc_of_sk)

microrefugia_area_totals = aa

ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "microrefugia_per_ecoregion.csv"))


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
  dplyr::filter(!is.na(microrefugia)) %>%
  dplyr::group_by(ECOREGION_NAME, microrefugia) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_div_area)) %>% 
  dplyr::mutate(pro_area = as.numeric(pro_area_sum))%>%
  dplyr::select(-protected, -pro_area_sum, - pro_ec_div_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()

#st_write(ec_div_pro, file.path("outputs", "sk_pro_high_div_test.gpkg"))

aa <- pivot_wider(ec_div_pro, names_from = microrefugia, values_from = pro_area)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>%
  dplyr::rename("0_p" = `0`,
                "1_p" = `1`)  

# area of protection per ecoregion per catergory (ha)
aa 

eco_pro_rardiv_output <- left_join(microrefugia_area_totals, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "1", "1_p",  "0" ,"0_p" ,
         "area_m2" )           


write_csv(eco_pro_rardiv_output, file.path("outputs", "microrefugia_per_ecoregion_protection.csv"))


#########################################################################

## macrorefugia


library(tidyr)
# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_2080macrorefugia_70threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
dplyr::rename("microrefugia" = `2080s_macrorefugia`)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , ec) 

ec_divv <- ec_div   |> 
  dplyr::mutate(ec_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(microrefugia))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, microrefugia)%>%
  dplyr::mutate(rare_class_sum = sum(ec_area_m2)) %>% 
  dplyr::select( -ec_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_divvv, names_from = microrefugia, values_from = rare_class_sum)

aa <- left_join(aa, ecsum_df )%>%
  select(-total_sk_area, -pc_of_sk)

macrorefugia_area_totals = aa

ab <- aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~.x/area_m2 *100))%>% 
  select(-area_m2) %>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "macrorefugia_per_ecoregion.csv"))


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
  dplyr::filter(!is.na(microrefugia)) %>%
  dplyr::group_by(ECOREGION_NAME, microrefugia) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_div_area)) %>% 
  dplyr::mutate(pro_area = as.numeric(pro_area_sum))%>%
  dplyr::select(-protected, -pro_area_sum, - pro_ec_div_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()

#st_write(ec_div_pro, file.path("outputs", "sk_pro_high_div_test.gpkg"))

ec_div_pro <- ec_div_pro %>%
  mutate(macrorefugia = microrefugia)%>%
  select(-microrefugia)
  

aa <- pivot_wider(ec_div_pro, names_from = macrorefugia, values_from = pro_area)%>%
  arrange(ECOREGION_NAME)

aa <- aa %>%
  dplyr::rename("0_p" = `0`,
                "1_p" = `1`)  

# area of protection per ecoregion per catergory (ha)
aa 

eco_pro_rardiv_output <- left_join(macrorefugia_area_totals, aa) 

eco_pro_rardiv_output <-eco_pro_rardiv_output %>%
  mutate(across(where(is.numeric), round, 0)) %>% 
  select("ECOREGION_NAME", "1", "1_p",  "0" ,"0_p" ,
         "area_m2" )           

# rename to macrorefugia 


write_csv(eco_pro_rardiv_output, file.path("outputs", "macrorefugia_per_ecoregion_protection.csv"))

