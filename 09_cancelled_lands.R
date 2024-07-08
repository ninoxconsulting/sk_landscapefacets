## Cancelled lands preparation

library(dplyr)
library(terra)
library(sf)

# read in templates 
srast <- rast(file.path("inputs", "sk_rast_template.tif"))
in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))


# read in the datasets

# cristinas lands file 

cancelled <- st_read(file.path("inputs", "cancelled_lands", "cancelled_lands_removed.gpkg"))%>%
  st_cast("POLYGON")%>%
  st_transform(3005)%>%
  select("CONSERVATI","SITE_NAME","SITE_NUMBE" ,"TANTALIS_L")


# paulas points 
pts <- st_read(file.path("inputs", "cancelled_lands", "placemarks_paula.gpkg")) %>% 
  select(Name)%>% 
  st_transform(3005)

cancelled_merge <- st_join(cancelled, pts)

st_write(cancelled_merge, file.path("inputs", "cancelled_lands", "cancelled_lands_removed_labelled.gpkg"))

# add additional missing layers 

# list.files(file.path("inputs", "cancelled_lands"))
# 
# fp <- st_read(file.path("inputs", "cancelled_lands", "fishpan_lake_official.gpkg" )) 
# fl <- st_read(file.path("inputs", "cancelled_lands", "flintcreek_official.gpkg" )) 
# sn <- st_read(file.path("inputs", "cancelled_lands", "seymourlake_official.gpkg" )) 
# ssth <- st_read(file.path("inputs", "cancelled_lands", "seymourlake_sth_official.gpkg" )) 
#            
#          
# aa <- st_union(fp, fl)
# 

cl <- st_read(file.path("inputs", "cancelled_lands","crownlands_official.gpkg"), layer = "merged")%>% 
  select("CROWN_LANDS_FILE", "TENURE_LOCATION", "DISPOSITION_TRANSACTION_SID")


# conservation lands 

cons <- st_read(file.path("inputs", "cons_lands.gpkg")) %>% 
  filter(TENURE_TYPE %in% c ("Map Reserve", "Notation of Interest","Designated Use Area")) %>% 
  #filter(CONSERVATION_LAND_TYPE!= "Administered Lands")%>%
  select(-TENURE_DESCRIPTION)

st_write(cons, file.path("inputs", "cancelled_lands", "cons_lands_map_reserved.gpkg"), append = FALSE)


## In QGIS - merge the three layers above and export as QGIS 
# merge the layers and add cancel and not cancellea


## 
canc <- st_read(file.path("inputs", "cancelled_lands_merge.gpkg"), layer = "final")

canc <- canc %>% 
  mutate(cancelled_status = ifelse(is.na(CONSERVATI), "not_cancelled", "cancelled")) %>%
  mutate(cancelled_status = case_when(
    SITE_NAME == "Round Lake Northeast (NOI)" ~ "cancelled",
    .default = cancelled_status))


canc <- st_write(canc, file.path("inputs", "cancelled_lands_final.gpkg"))



################################################################################
# Gap analysis for the layers using the conservation lands as the protected "layer" grouped into cancelled and not cancelled. 
library(readr)
library(dplyr)

canc <- st_read(file.path("inputs", "cancelled_lands_final.gpkg"))
canc <- canc %>% select(cancelled_status)


# read in the ecoregion 
ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))
ecsum_df <- read_csv(file.path("outputs", "ecoregion_area_totals.csv"))%>% select(-X)


## what proportion of the skeena region is currently in cancellend and non-cancelled lands 

can_ecoreg <- st_intersection( ec, canc) 

can_ecoreg <- can_ecoreg |> 
  mutate(can_area = st_area(can_ecoreg))

pro_sum <- can_ecoreg %>%
  st_drop_geometry() |>
  dplyr::group_by(ECOREGION_NAME, cancelled_status) %>% 
  dplyr::mutate(sum_tot = sum(can_area))%>% 
  select(-can_area)%>% 
  distinct()
  
  
pro_sum <- left_join(pro_sum, ecsum_df ) %>% 
  select(-total_sk_area, -pc_of_sk) %>% 
  rowwise() %>% 
  dplyr::mutate(eco_can_pc = (sum_tot/area_m2)*100)

# export 
write_csv(pro_sum, file.path("outputs", "cancelled_lands_per_ecoregion.csv"))


#################################################################################
## Break down of each type of catergory by ecoregion 

# 1) diverity 
# 2) rarity
# 3) high diversity and rartity 

# read in concentration layers: diverity and rarity
div <- rast(file.path("outputs", "sk_diversity_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(diversity))
  
ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, diversity, cancelled_status)%>%
  dplyr::mutate(div_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "cancelled_lands_diversity_class_per_ecoregion.csv"))




################################################################################

## 2. Repeat for Rarity Class - per ecoregion 
# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_rarity_conc.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  dplyr::mutate(rare_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "cancelled_lands_rarity_class_per_ecoregion.csv"))


################################################################################

# 3. repeat for the rare and diversit catergories 

## Repeat for Rarity Class - per ecoregion 
divrare <- rast(file.path("outputs", "high_div_rare.tif"))

divpoly <- as.polygons(divrare, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(diversity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, diversity, cancelled_status)%>%
  dplyr::mutate(div_rare_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()


ec_divvv <- ec_divvv %>% 
  mutate(divrare_code = case_when(
    diversity == 0  ~ "common",
    diversity == 4  ~ "rare", 
    diversity == 5  ~ "vrare",
    diversity == 40   ~"high_div",
    diversity == 44  ~ "high_div_rare",
    diversity == 45  ~ "high_div_vrare",
    diversity == 50  ~ "vhigh_div",
    diversity == 54  ~ "vhigh_div_rare", 
    diversity == 55  ~ "vhigh_div_vrare"))
  


write_csv(ec_divvv, file.path("outputs", "cancelled_lands_divrare_class_per_ecoregion.csv"))


##################################################################################

# gap analysis for primary productivity  by ecoregion 

# 1) ndvi - very high and high
# 2) gdd - per catergories


# read in concentration layers: diverity and rarity
gdd <- rast(file.path("outputs",  "gdd_sk_classed.tif"))

gddpoly <- as.polygons(gdd , na.rm=FALSE)
gdd_sf <- st_as_sf(gddpoly)%>% 
  filter(!is.na(DD5))


# 1 By ecoregion 
ec_gdd <- st_intersection(gdd_sf , can_ecoreg) 

ec_gdd  <- ec_gdd   |> 
  mutate(ggd_area_m2 = st_area(ec_gdd ))%>%
  filter(!is.na(DD5))

ec_gddd <- ec_gdd %>%
  dplyr::group_by(ECOREGION_NAME, DD5,cancelled_status )%>%
  dplyr::mutate(gdd_class_sum = sum(ggd_area_m2)) %>% 
  select(-ggd_area_m2,-can_area) %>% 
  st_drop_geometry()%>% 
  distinct()

#m <- c(-1, 800, 0,
#       800, 900, 1,
#       900, 1100, 2,
#       1100, 1400, 3, 
#       1400, 1600, 4)


ec_divvv <- ec_gddd %>% 
  mutate(DD5_code = case_when(
    DD5 == 0  ~ "lt_800",
    DD5 == 1  ~ "800_900", 
    DD5 == 2  ~ "900_1100",
    DD5 == 3   ~"1100_1400",
    DD5 == 4  ~ "gt_1400"))


write_csv(ec_divvv, file.path("outputs", "cancelled_lands_gdd_class_per_ecoregion.csv"))



########################################################################

# repeat for NDVI? 

# read in concentration layers: diverity and rarity
ndvi<- rast(file.path("outputs",  "ndvi_high_vhigh.tif"))
ndvi <- mask(ndvi, in_aoi)

ndvipoly <- as.polygons(ndvi , na.rm=FALSE)
ndvi_sf <- st_as_sf(ndvipoly) %>% 
  filter(!is.na(min))%>%
  filter(min !=0)


# 1 By ecoregion 
ec_ndvi <- st_intersection(ndvi_sf , can_ecoreg)

ec_ndvii  <- ec_ndvi   |> 
  mutate(ndvi_area_m2 = st_area(geometry))%>%
  filter(!is.na(min))

ec_ndvii <- ec_ndvii %>%
  dplyr::group_by(ECOREGION_NAME, min,cancelled_status )%>%
  dplyr::mutate(ndvi_class_sum = sum(ndvi_area_m2)) %>% 
  select(-ndvi_area_m2,-can_area) %>% 
  st_drop_geometry()%>% 
  distinct()

ec_ndvii <- ec_ndvii %>% 
  mutate(ndvi_code = case_when(
    min == 1  ~ "high",
    min == 2  ~ "vhigh"))


write_csv(ec_ndvii, file.path("outputs", "cancelled_lands_ndvi_class_per_ecoregion.csv"))



############################################################################

## Protection for wilderness areas - seee wilderness script

wild_sf <- st_read(file.path("outputs", "sk_wilderness.gpkg"))%>%
  select(-MLF_Kehm_2012)

# 1 By ecoregion 
ec_wild <- st_intersection(wild_sf , can_ecoreg)

ec_wild  <- ec_wild   |> 
  mutate(wild_area_m2 = st_area(ec_wild))

ec_wild <- ec_wild %>%
  dplyr::group_by(ECOREGION_NAME, type,cancelled_status )%>%
  dplyr::mutate(wild_class_sum = sum(wild_area_m2)) %>% 
  select(-wild_area_m2,-can_area) %>% 
  st_drop_geometry()%>% 
  distinct()

ec_wild <- ec_wild %>% 
  mutate(wild_code = case_when(
    type == 0  ~ "wilderness"))

write_csv(ec_wild, file.path("outputs", "cancelled_lands_wilderness_class_per_ecoregion.csv"))



#################################################################################
## wetland density 

div <- rast(file.path("inputs", "sk_wetland_density_class_1km.tif"))
divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(layer))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, layer, cancelled_status)%>%
  dplyr::mutate(wetdiv_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

ec_divvv <- ec_divvv %>% 
  mutate(layer_code = case_when(
    layer == 1  ~ "very high > 50",
    layer == 2  ~ "high 15 - 50%",
    layer == 3  ~ "moderate"))

write_csv(ec_divvv, file.path("outputs", "cancelled_lands_wetlanddensity_class_per_ecoregion.csv"))





## River diversity and rarity

rar <- rast(file.path("outputs", "sk_rivers_rarity_mean_conc.tif"))

divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = sk_rivers_rarity_mean_101c)


# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  dplyr::mutate(rare_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "cancelled_rivers_rarity_class_per_ecoregion.csv"))




# read in concentration layers: diverity 
div <- rast(file.path("outputs", "sk_rivers_diversity_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(diversity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, diversity, cancelled_status)%>%
  dplyr::mutate(div_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "cancelled_rivers_diversity_class_per_ecoregion.csv"))





## refugia

#microrefugia 

rar <- rast(file.path("outputs", "sk_microrefugia_70threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = microrefugia)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  dplyr::mutate(resist_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

ec_divvv <- ec_divvv %>% 
  mutate(microrefugia_code = case_when(
    rarity == 0  ~ "lt_>70theshold",
    rarity == 1  ~ "gt_70threshold"))

write_csv(ec_divvv, file.path("outputs", "cancelled_lands_microrefugia_class_per_ecoregion.csv"))







## Macrorefugia

rar <- rast(file.path("outputs", "sk_2080macrorefugia_70threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = `2080s_macrorefugia`)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  dplyr::mutate(resist_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

ec_divvv <- ec_divvv %>% 
  mutate(macrorefugia_code = case_when(
    rarity == 0  ~ "lt_>70theshold",
    rarity == 1  ~ "gt_70threshold"))

write_csv(ec_divvv, file.path("outputs", "cancelled_lands_macrorefugia_class_per_ecoregion.csv"))













#Lake rarity class

rar <- rar <- rast(file.path("outputs", "sk_lakes_rarity_conc.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  dplyr::mutate(rare_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "cancelled_lake_rarity_class_per_ecoregion.csv"))





## connectivity

rar <- rast(file.path("outputs", "sk_pither_resistence_90threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = resistance)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  dplyr::mutate(resist_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

ec_divvv <- ec_divvv %>% 
  mutate(layer_code = case_when(
    rarity == 0  ~ "lt_>90theshold",
    rarity == 1  ~ "gt_90threshold"))

write_csv(ec_divvv, file.path("outputs", "cancelled_lands_conductivity_class_per_ecoregion.csv"))



#################################################################################
## lake rarity code

div <- rast(file.path("outputs", "sk_diversity_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , can_ecoreg ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(diversity))

ec_divvv <- ec_divv %>%
  dplyr::group_by(ECOREGION_NAME, diversity, cancelled_status)%>%
  dplyr::mutate(div_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -can_area) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "cancelled_lands_diversity_class_per_ecoregion.csv"))


