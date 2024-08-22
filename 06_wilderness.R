# wilderness

library(dplyr)
library(terra)
library(sf)

# read in templates 
srast <- rast(file.path("inputs", "sk_rast_template.tif"))
in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))

dis <- st_read(file.path("inputs", "skeena_clip_ce_2023.gpkg" ))%>%
  filter(CEF_HUMAN_DISTURB_FLAG =="Human Disturb Current 20yr")%>%
  mutate(type = 1) %>% 
  select(type)

rds <- st_read(file.path("inputs", "skeena_clippoly_roads_2023_1000buf.gpkg" ))%>% 
  mutate(type = 1) %>% 
  select(type)


private <- st_read(file.path("inputs",  "sk_privateland_raw.gpkg"))%>%
  filter(OWNER_TYPE == "Private") %>%
  mutate(type = 1) %>% 
  select(type)


dist <- bind_rows(rds, dis)

dist <- rbind(dist, private)

dist <- vect(dist)
human <- terra::rasterize(dist, srast, field = "type")

plot(human)

h <- subst(human, NA, 0)
h <-  mask(h, srast)


terra::writeRaster(h,file.path("outputs", "sk_wilderness_2023.tif"), overwrite = TRUE)

# conbvert to a polygon and intersect with ecoregion
wild <- as.polygons(h)%>% 
  st_as_sf() %>%
  filter(type == 0)

st_write(wild, file.path("outputs", "sk_wilderness.gpkg"))

#################

# gap analysis for wildlerness by ecoregion 

ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))
ecsum_df <- read_csv(file.path("outputs", "ecoregion_area_totals.csv"))%>% select(-X)
pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))


# all of skeena 

sk_wild <- wild  %>% 
  dplyr::mutate(wild_area_sk = st_area(.))%>% 
  st_drop_geometry() %>% 
  mutate(pc_type = wild_area_sk/243366640125 * 100)

# 1 By ecoregion 
ec_wild <- st_intersection(wild , ec) 

ec_wildd  <- ec_wild   |> 
  dplyr::mutate(wild_area_m2 = st_area(geometry)) %>% 
  st_drop_geometry()%>%
  select(-type)

aa <- left_join(ec_wildd, ecsum_df ) %>%
  select(-total_sk_area, -pc_of_sk)

wild_area_by_ecoregion_m2 <- aa # keeping this for the protection analysis 

# convert to ha 
ab <-aa %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(pc =  (wild_area_m2/area_m2) *100)

write_csv(ab, file.path("outputs", "wild_class_per_ecoregion.csv"))





#######################################################################

# gap analysis for protection wilderness regions 

pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
wild <- st_read(file.path("outputs", "sk_wilderness.gpkg")) %>% 
  select(-MLF_Kehm_2012 )
ec <- st_read(file.path("outputs", "sk_ecoreg_reduced.gpkg"))
ecsum_df <- read_csv(file.path("outputs", "ecoregion_area_totals.csv"))%>% select(-X)


## read in protected layers 
pross_u <- pro %>% select(protected)

# 1 # get wild class per ecoregion 
ec_wild <- st_intersection(wild , ec) 
  
# intersect protected areas
ec_wild_pro <- st_intersection(ec_wild, pross_u) %>%
  dplyr::mutate(pro_ec_wild_area = st_area(.))%>% 
  dplyr::filter(!is.na(type)) %>%
  dplyr::group_by(ECOREGION_NAME, type) %>%
  dplyr::mutate(pro_area_sum = sum(pro_ec_wild_area)) %>% 
  #dplyr::mutate(pro_area = as.numeric(pro_area_sum/1000))%>%
  dplyr::select(-protected, -pro_ec_wild_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()


eco_pro_wild_output <- left_join(wild_area_by_ecoregion_m2, ec_wild_pro ) 

eco_pro_wild_output <-eco_pro_wild_output %>%
  dplyr::mutate(across(where(is.numeric), round, 0)) %>% 
  rowwise() %>% 
  mutate(pc_wilderness = (wild_area_m2/area_m2)*100) %>% 
  mutate(pc_wilderness_protected = (pro_area_sum/wild_area_m2)*100) %>% 
  dplyr::mutate(across(where(is.numeric), round, 0)) 


write_csv(eco_pro_wild_output, file.path("outputs", "wild_class_per_ecoregion_protection.csv"))


## Gap analysis for all of skeena 

pro <- st_read(file.path("outputs", "sk_protected_lands.gpkg"))
wild <- st_read(file.path("outputs", "sk_wilderness.gpkg")) %>% 
  select(-MLF_Kehm_2012 )%>%
  mutate(sk_wild_area = st_area(geom))

## read in protected layers 
pross_u <- pro %>% select(protected)

# 1 # get wild class per ecoregion 
sk_wild_pro <- st_intersection(wild , pross_u) %>% 
  dplyr::mutate(pro_sk_wild_area = st_area(.))%>% 
  dplyr::filter(!is.na(type)) %>%
  dplyr::group_by(type) %>%
  dplyr::mutate(pro_area_sum = sum(pro_sk_wild_area)) %>% 
  #dplyr::mutate(pro_area = as.numeric(pro_area_sum/1000))%>%
  dplyr::select(-protected, -pro_sk_wild_area)%>%
  ungroup()%>% 
  st_drop_geometry()%>%
  distinct()%>% 
  mutate(pc_wild_pro = (pro_area_sum/1.93693e+11) * 100)











