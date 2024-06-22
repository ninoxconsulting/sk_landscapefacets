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
ec <- st_read(file.path("outputs", "sk_ecoregions.gpkg"))





# 1: calculate % protected for all of Skeena
## read in protected layers 
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))

## what proportion of the skeena region is currently protected? 
# how much area is protected within each eco_region: ()

# simplify protected area 
pross_u <- pro %>% select(protected)
pro_ecoreg <- st_intersection( ec, pross_u) 
pro_ecoreg <- pro_ecoreg |> 
  mutate(pro_area = st_area(pro_ecoreg))

pro_sum <- pro_ecoreg %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(prosum = sum(pro_area)) %>%
  select(-protected, -pro_area, -area_m2) |> 
  distinct()

ecsum <- ec %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(totsum = sum(area_m2))

ecpro <- left_join(pro_sum, ecsum)%>%
  group_by(ECOREGION_NAME) |> 
  mutate(pc_pro = (prosum/totsum)*100)

#library(readr)
write_csv(ecpro, file.path("outputs", "protection_per_ecoregion.csv"))

sum(ecpro$prosum)
sum(ecpro$totsum)

sum(ecpro$prosum)/sum(ecpro$totsum)*100





## UP TO HERE NEED TO REVIEW THIS ..
## HOw much rare and diverse areas are already protected

divrarepoly <- as.polygons(divrare, na.rm=FALSE)
divrare_sf <- st_as_sf(divrarepoly)

# 1 
pro_rare <- st_intersection(divrare_sf , pross_u) 

pro_rare  <- pro_rare  |> 
  mutate(pro_area_m2 = st_area(pro_rare))

pro_rare <- st_intersection(pro_rare, ec) %>% 
  filter(!is.na(diversity))

pro_rare <- pro_rare |> 
  mutate(pro_area_m2 = st_area(pro_rare))

#st_write(pro_rare, file.path("inputs", "test_pro_outputs.gpkg"))

pro_sum <- pro_rare  %>%
  st_drop_geometry() |> 
  group_by(diversity, ECOREGION_NAME ) |> 
  mutate(prosum = sum(pro_area_m2)) %>%
  select(-protected, -pro_area_m2) |> 
  distinct()

ecpro_sum <- ecpro %>% 
  select(ECOREGION_NAME, prosum) %>% 
  rename("eco_protect_total_m2" = prosum)

pro_sum <- left_join(pro_sum, ecpro_sum)

# now we have diverity type, by ecoregion, by protection 

ecsum <- ec %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(totsum = sum(area_m2))


ecpro <- left_join(pro_sum, ecsum)%>%
  group_by(ECOREGION_NAME) |> 
  mutate(pc_pro = (prosum/totsum)*100)

#library(readr)
write_csv(ecpro, file.path("outputs", "protection_per_ecoregion.csv"))















## Break down of each type of catergory by ecoregion 

# 1) diverity 
# 2) rarity
# 3) high diversity and rartity 


# read in concentration layers: diverity and rarity
div <- rast(file.path("outputs", "sk_diversity_conc.tif"))

# TYPE 1 SUMMARY BY ECOREGION 
ec <- ec %>%
  select(ECOREGION_NAME)%>% 
  mutate(area_m2 = st_area(.))

# calculate the summary of ecoregions within skeeena region

ecsum <- ec %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(totsum = sum(area_m2))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 
ec_div <- st_intersection(div_sf , ec) 

ec_div   <- ec_div   |> 
  mutate(ec_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(diversity))

ec_div <- ec_div %>%
 group_by(ECOREGION_NAME, diversity)%>%
  mutate(div_class_sum = sum(ec_area_m2)) %>% 
  select(-area_m2, -ec_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_div, names_from = diversity, values_from = div_class_sum)

aa <- left_join(aa, ecsum)
# convert to ha 
aa <- aa %>% mutate(across(where(is.numeric), ~.x/1000))%>%
  select(-area_m2)

ab <-aa %>% 
  ungroup() %>% 
  rowwise() %>%
  mutate(across(where(is.numeric), ~.x/totsum *100))%>% 
  select(-totsum)%>% 
  mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "diversity_class_per_ecoregion.csv"))





## 2. Repeat for Rarity Class - per ecoregion 

# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_rarity_conc.tif"))

# TYPE 1 SUMMARY BY ECOREGION 
ec <- ec %>%
  select(ECOREGION_NAME)%>% 
  mutate(area_m2 = st_area(.))

# calculate the summary of ecoregions within skeeena region

ecsum <- ec %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(totsum = sum(area_m2))

library(tidyr)
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 
ec_div <- st_intersection(div_sf , ec) 

ec_div   <- ec_div   |> 
  mutate(ec_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_div <- ec_div %>%
  group_by(ECOREGION_NAME, rarity)%>%
  mutate(rare_class_sum = sum(ec_area_m2)) %>% 
  select(-area_m2, -ec_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_div, names_from = rarity, values_from = rare_class_sum)

aa <- left_join(aa, ecsum)
# convert to ha 
aa <- aa %>% mutate(across(where(is.numeric), ~.x/1000))%>%
  select(-area_m2)

ab <-aa %>% 
  ungroup() %>% 
  rowwise() %>%
  mutate(across(where(is.numeric), ~.x/totsum *100))%>% 
  select(-totsum)%>% 
  mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "rarity_class_per_ecoregion.csv"))



# 3. repeat for the rare and diversit catergories 

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


# TYPE 1 SUMMARY BY ECOREGION 
ec <- ec %>%
  select(ECOREGION_NAME)%>% 
  mutate(area_m2 = st_area(.))

# calculate the summary of ecoregions within skeeena region

ecsum <- ec %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(totsum = sum(area_m2))

divpoly <- as.polygons(divrare, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 
ec_div <- st_intersection(div_sf , ec) 

ec_div   <- ec_div   |> 
  mutate(ec_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(diversity))

ec_div <- ec_div %>%
  group_by(ECOREGION_NAME, diversity)%>%
  mutate(divrare_class_sum = sum(ec_area_m2)) %>% 
  select(-area_m2, -ec_area_m2) %>% 
  st_drop_geometry()

aa <- pivot_wider(ec_div, names_from = diversity, values_from = divrare_class_sum)

aa <- left_join(aa, ecsum)

names(aa)<- c("ECOREGION_NAME ", "common", "r", "vr", "hv", "hv_r", "hv_vr", "vhv", "vhv_r", "vhv_vr", "area", "tot_area")

# convert to ha 
aa <- aa %>% mutate(across(where(is.numeric), ~.x/1000))%>%
  select(-area)

ab <-aa %>% 
  ungroup() %>% 
  rowwise() %>%
  mutate(across(where(is.numeric), ~.x/tot_area *100))%>% 
  select(-tot_area)%>% 
  mutate(across(where(is.numeric), round, 1))

write_csv(ab, file.path("outputs", "divrarity_class_per_ecoregion.csv"))




















































ex <- expanse(divrare, unit="m", byValue=TRUE, zones=ecr, wide=TRUE)[,-1]

write_csv(ex, file.path("outputs", "div_per_ecoregion.csv"))


ex <- readr::read_csv(file.path("outputs", "div_per_ecoregion.csv")) %>%
  select(-...1)

# calculate the % cover of each group by class 
pc_div_sum <- ex %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) |> 
  bind_cols(tot_area = ecsum$totsum)

write.csv(pc_div_sum,file.path("outputs", "div_pcper_ecoregion.csv"))



ecsum <- ec %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(totsum = sum(area_m2))

ecv <- vect(ec)


# # calculate the number of pixels per group, per ecoregion 
# e <- extract(div, ecv, un="table", na.rm=TRUE, exact=FALSE)
# edf <- data.frame(NAME_2=ecv$ECOREGION_NAME[e[,1]], e[,-1])
# 
# edff <- edf %>% 
#   group_by(NAME_2, e....1.) |> 
#   count()

# rasterize the zones
ecr <- rasterize(ecv, aoi, field="ECOREGION_NAME")
ex <- expanse(divrare, unit="m", byValue=TRUE, zones=ecr, wide=TRUE)[,-1]














# rasterize the zones
pror <- rasterize(pross, temp, field="protected")



#diversity
div_ex <- expanse(div, unit="m", byValue=TRUE, zones=pror, wide=TRUE)[,-1]
write.csv(div_ex, file.path("outputs", "div_per_protection.csv"))

# rarity
rare_ex <- expanse(rar, unit="m", byValue=TRUE, zones=pror, wide=TRUE)[,-1]



## what proporion of diversity classes are protected? 


write.csv(rare_ex, file.path("outputs", "rare_per_ecoregion.csv"))


ex <- readr::read_csv(file.path("outputs", "rare_per_ecoregion.csv"))%>%
  select(-...1)

# calculate the % cover of each group by class 
library(janitor)
pc_rare_sum <- ex %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) |> 
  bind_cols(tot_area = ecsum$totsum)

write.csv(pc_rare_sum,file.path("outputs", "rare_pcper_ecoregion.csv"))




## what proporion of rarity classes are protected? 


# 
# 
# # read in concentration layers: diverity and rarity
# divrare <- rast(file.path("outputs", "high_div_rare.tif"))
# 
# # unique va;ues 
# # 1         0  - not rare or diversity (common)
# # 2         4  - Rare (r)
# # 3         5  - Very Rare (vr)
# # 4        40  - High Variety (hv)
# # 5        44  - high Variety and rare (hv_r)
# # 6        45  - high variety and very rare (hv_vr)
# # 7        50  - Very High Variety  (vhv)
# # 8        54  - Very High Variety and rare (vhv_r)
# # 9        55  - Very High Variety and very rare (vhv_vr)
# 
# #"r", "vr", "hv", "hv_r", "hv_vr", "vhv". "vhv_r", "vhv_vr"
# 
# 
# # TYPE 1 SUMMARY BY ECOREGION 
# 
# ec <- ec %>%
#   select(ECOREGION_NAME)%>% 
#   mutate(area_m2 = st_area(.))
# 
# 
# # calculate the summary of ecoregions within skeeena region
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
# names(ex)<- c("zone", "common", "r", "vr", "hv", "hv_r", "hv_vr", "vhv", "vhv_r", "vhv_vr")
# 
# write_csv(ex, file.path("outputs", "div_per_ecoregion.csv"))
# 
# 
# ex <- readr::read_csv(file.path("outputs", "div_per_ecoregion.csv")) 
# 
# # calculate the % cover of each group by class 
# pc_div_sum <- ex %>%
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits = 1) |> 
#   bind_cols(tot_area = ecsum$totsum) 
#   
#   names(pc_div_sum)<- c("zone", "common", "r", "vr", "hv", "hv_r", "hv_vr", "vhv", "vhv_r", "vhv_vr", "tot_area")
#   
# write.csv(pc_div_sum,file.path("outputs", "div_pcper_ecoregion.csv"))
#   
# 
# 

