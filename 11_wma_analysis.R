
################################################################################
# Gap analysis for wma three locations as additional task from R.Holmes
library(readr)
library(dplyr)
library(sf)
library(terra)
library(tidyr)

canc <- st_read(file.path("inputs", "cancelled_lands_final.gpkg"))

wma <- canc %>% 
  filter(SITE_NAME %in% c("Morley Lake (MR)","Swift River (MR)","Kumealon Lagoon (MR)" ))%>%
  select(SITE_NAME) %>% 
  mutate(area_m = as.numeric(st_area(.)))

wmadf <- wma %>% st_drop_geometry()
write_csv(wmadf, file.path("outputs", "wma_total_area.csv"))

#st_write(wma, file.path("inputs", "wma_test.gpkg"))


# overlaps with 

#red-blue sp intersected

# area and percent cover for each of our layers 
#red-blue species and possible federal if there are caribou or marbled murrelet critical habitat but I don't know how hard that is.


#################################################################################
## Break down of each type of catergory by ecoregion 

# 1) diverity 
# 2) rarity
# 3) high diversity and rartity 

# read in concentration layers: diverity and rarity
div <- rast(file.path("outputs", "sk_diversity_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # intersect with wma
ec_div <- st_intersection(div_sf, wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_area_m2 = as.numeric(st_area(ec_div)))%>%
  mutate_all(., ~replace_na(.,0))
         
ec_divvv <- ec_divv %>%
  dplyr::select( -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

aa <- pivot_wider(ec_divvv, names_from = diversity, values_from = div_area_m2)%>%
  mutate_all(., ~replace_na(.,0))


# diversity 
div = aa

div <- left_join(wmadf , aa )
names(div) = c('SITE_NAME', "area_m", "div_0", "div_1", "div_2", "div_3","div_4")

# calculate percentage
ab <- div %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m *100))%>% 
  #select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

#div
#ab

write_csv(div, file.path("outputs", "wma_diversity_class.csv"))



################################################################################

## 2. Repeat for Rarity Class - per ecoregion 
# read in concentration layers:  rarity
rar <- rast(file.path("outputs", "sk_rarity_conc.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf, wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(rar_area_m2 = as.numeric(st_area(ec_div)))%>%
  mutate_all(., ~replace_na(.,0))

ec_divvv <- ec_divv %>%
  dplyr::select( -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

aa <- pivot_wider(ec_divvv, names_from = rarity, values_from = rar_area_m2)%>%
  mutate_all(., ~replace_na(.,0))

# diversity 
rar = aa

rar <- left_join(wmadf , rar )
names(rar) = c('SITE_NAME', "area_m", "rare_0", "rare_1", "rare_2", "rare_3","rare_4")

# calculate percentage
ab <- rar %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(across(where(is.numeric), ~.x/area_m *100))%>% 
  #select(-area_m2)%>% 
  dplyr::mutate(across(where(is.numeric), round, 1))

#div
#ab

write_csv(rar, file.path("outputs", "wma_rarity_class.csv"))






################################################################################

# 3. repeat for the rare and diversit catergories 

## Repeat for Rarity Class - per ecoregion 
divrare <- rast(file.path("outputs", "high_div_rare.tif"))

divpoly <- as.polygons(divrare, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf, wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(highdivrar_area_m2 = as.numeric(st_area(ec_div)))%>%
  mutate_all(., ~replace_na(.,0))

ec_divvv <- ec_divv %>%
  dplyr::group_by( diversity, SITE_NAME)%>%
  dplyr::mutate(highdivrar_area_m2 = sum(highdivrar_area_m2)) %>% 
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



write_csv(ec_divvv, file.path("outputs", "wma_divrare_class_per_ecoregion.csv"))




##################################################################################

# gap analysis for primary productivity  by ecoregion 

# 1) ndvi - very high and high
# 2) gdd - per catergories


# read in concentration layers: diverity and rarity
gdd <- rast(file.path("outputs",  "gdd_sk_classed.tif"))

gddpoly <- as.polygons(gdd , na.rm=FALSE)
gdd_sf <- st_as_sf(gddpoly)#%>% 
 # filter(!is.na(DD5))

# 1 By ecoregion 
ec_gdd <- st_intersection(gdd_sf , wma) 

ec_gdd  <- ec_gdd   |> 
  mutate(ggd_area_m2 = as.numeric(st_area(ec_gdd) ))%>%
  mutate_all(., ~replace_na(.,0))

ec_gddd <- ec_gdd %>%
  #dplyr::group_by(ECOREGION_NAME, DD5,cancelled_status )%>%
  #dplyr::mutate(gdd_class_sum = sum(ggd_area_m2)) %>% 
 # select(-ggd_area_m2,-can_area) %>% 
  st_drop_geometry()%>% 
  distinct()

ec_divvv <- ec_gddd %>% 
  mutate(DD5_code = case_when(
    DD5 == 0  ~ "lt_800",
    DD5 == 1  ~ "800_900", 
    DD5 == 2  ~ "900_1100",
    DD5 == 3   ~"1100_1400",
    DD5 == 4  ~ "gt_1400"))

write_csv(ec_divvv, file.path("outputs", "wma_gdd_class.csv"))


########################################################################

# repeat for NDVI? 

# read in concentration layers: diverity and rarity
ndvi<- rast(file.path("outputs",  "ndvi_high_vhigh.tif"))

ndvipoly <- as.polygons(ndvi , na.rm=FALSE)
ndvi_sf <- st_as_sf(ndvipoly) %>% 
  filter(!is.na(min))%>%
  filter(min !=0)


# 1 By ecoregion 
ec_ndvi <- st_intersection(ndvi_sf , wma)

ec_ndvii  <- ec_ndvi   |> 
  mutate(ndvi_area_m2 = as.numeric(st_area(geometry)))%>%
  mutate_all(., ~replace_na(.,0)) %>%
  st_drop_geometry()%>% 
  distinct()

ec_ndvii <- ec_ndvii %>% 
  mutate(ndvi_code = case_when(
    min == 1  ~ "high",
    min == 2  ~ "vhigh"))


write_csv(ec_ndvii, file.path("outputs", "wma_ndvi.csv"))



############################################################################

## Protection for wilderness areas - seee wilderness script

wild_sf <- st_read(file.path("outputs", "sk_wilderness.gpkg"))%>%
  select(-MLF_Kehm_2012)

# 1 By ecoregion 
ec_wild <- st_intersection(wild_sf , wma)

ec_wild  <- ec_wild   |> 
  mutate(wild_area_m2 = st_area(ec_wild)) %>%
  st_drop_geometry() %>% 
  distinct()

ec_wild <- ec_wild %>% 
  mutate(wild_code = case_when(
    type == 0  ~ "wilderness"))

write_csv(ec_wild, file.path("outputs", "wma_wilderness_class.csv"))



#################################################################################
## wetland density 

div <- rast(file.path("inputs", "sk_wetland_density_class_1km.tif"))
divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calculate per ecoregion 
ec_div <- st_intersection(div_sf, wma ) 

ec_divv <- ec_div  |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  mutate_all(., ~replace_na(.,0)) 

ec_divvv <- ec_divv %>%
  #dplyr::select( -div_cancel_area_m2, area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

ec_divvv <- ec_divvv %>% 
  mutate(layer_code = case_when(
    layer == 1  ~ "very high > 50",
    layer == 2  ~ "high 15 - 50%",
    layer == 3  ~ "moderate"))

write_csv(ec_divvv, file.path("outputs", "wma_wetlanddensity.csv"))





## River diversity and rarity

rar <- rast(file.path("outputs", "sk_rivers_rarity_mean_conc.tif"))

divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = sk_rivers_rarity_mean_101c)


# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  mutate_all(., ~replace_na(.,0)) 

ec_divvv <- ec_divv %>%
#  dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
#  dplyr::mutate(rare_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select(-area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "wma_rivers_rarity.csv"))




# read in concentration layers: diverity 
div <- rast(file.path("outputs", "sk_rivers_diversity_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  mutate_all(., ~replace_na(.,0)) 

ec_divvv <- ec_divv %>%
  #dplyr::group_by(ECOREGION_NAME, diversity, cancelled_status)%>%
  #dplyr::mutate(div_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "wma_rivers_diversity_class.csv"))





## refugia

#microrefugia 

rar <- rast(file.path("outputs", "sk_microrefugia_70threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = microrefugia)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , wma) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  mutate_all(., ~replace_na(.,0)) 

ec_divvv <- ec_divv %>%
  #dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  #dplyr::mutate(resist_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

ec_divvv <- ec_divvv %>% 
  mutate(microrefugia_code = case_when(
    rarity == 0  ~ "lt_>70theshold",
    rarity == 1  ~ "gt_70threshold"))

write_csv(ec_divvv, file.path("outputs", "wma_microrefugia_class.csv"))







## Macrorefugia

rar <- rast(file.path("outputs", "sk_2080macrorefugia_70threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = `2080s_macrorefugia`)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  mutate_all(., ~replace_na(.,0)) 

ec_divvv <- ec_divv %>%
  #dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  #dplyr::mutate(resist_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

ec_divvv <- ec_divvv %>% 
  mutate(macrorefugia_code = case_when(
    rarity == 0  ~ "lt_>70theshold",
    rarity == 1  ~ "gt_70threshold"))

write_csv(ec_divvv, file.path("outputs", "wma_macrorefugia_class.csv"))













#Lake rarity class

rar <- rar <- rast(file.path("outputs", "sk_lakes_rarity_conc.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = as.numeric(st_area(ec_div )))%>%
  mutate_all(., ~replace_na(.,0)) 

ec_divvv <- ec_divv %>%
 #dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  #dplyr::mutate(rare_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "wma_lake_rarity_class_per_ecoregion.csv"))





## connectivity

rar <- rast(file.path("outputs", "sk_pither_resistence_90threshold.tif"))
divpoly <- as.polygons(rar, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)%>%
  rename("rarity" = resistance)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf ,wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = st_area(ec_div ))%>%
  filter(!is.na(rarity))

ec_divvv <- ec_divv %>%
  #dplyr::group_by(ECOREGION_NAME, rarity, cancelled_status)%>%
  #dplyr::mutate(resist_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

ec_divvv <- ec_divvv %>% 
  mutate(layer_code = case_when(
    rarity == 0  ~ "lt_>90theshold",
    rarity == 1  ~ "gt_90threshold"))

write_csv(ec_divvv, file.path("outputs", "wma_conductivity_class.csv"))



#################################################################################
## lake rarity code

div <- rast(file.path("outputs", "sk_diversity_conc.tif"))

divpoly <- as.polygons(div, na.rm=FALSE)
div_sf <- st_as_sf(divpoly)

# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(div_sf , wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(div_cancel_area_m2 = as.numeric(st_area(ec_div )))

ec_divvv <- ec_divv %>%
  #dplyr::group_by(ECOREGION_NAME, diversity, cancelled_status)%>%
  #dplyr::mutate(div_class_sum = sum(div_cancel_area_m2)) %>% 
  dplyr::select( -div_cancel_area_m2, -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "wma_diversity_class.csv"))



#########################################################################

# overlap of the red and blue sp and federal listed species. 

rb <- st_read(file.path("inputs", "bc_red_blue_sp_raw.gpkg"))


# 1 # calcaulte per ecoregion 
ec_div <- st_intersection(rb , wma ) 

ec_divv <- ec_div   |> 
  dplyr::mutate(sp_area_m2 = as.numeric(st_area(ec_div ))) %>%
  select(c(SITE_NAME, SCI_NAME,ENG_NAME,EL_TYPE,BC_LIST,RANK_DESC,sp_area_m2))

ec_divvv <- ec_divv %>%
  dplyr::group_by(SITE_NAME,ENG_NAME)%>%
  #dplyr::mutate(div_class_sum = sum(div_cancel_area_m2)) %>% 
  #dplyr::select( -div_cancel_area_m2, -area_m) %>% 
  st_drop_geometry() %>% 
  distinct()

write_csv(ec_divvv, file.path("outputs", "wma_red_blue_sp_area.csv"))




