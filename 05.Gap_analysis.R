#1. Gap analysis 

#library(bcdata)
library(dplyr)
library(terra)
library(sf)

# read in study area 
# 
# basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"
# 
#aoi <- vect(file.path("inputs", "SkeenaRegionBndry.shp"))
# aoi_sf <- st_as_sf(aoi)
aoi <- vect(file.path("inputs", "template_poly.gpkg"))
temp <- rast(file.path("inputs", "sk_rast_template.tif"))


# read in concentration layers: diverity and rarity

div <- rast(file.path("outputs", "sk_diveristy_conc.tif"))
rar <- rast(file.path("outputs", "sk_rarity_conc.tif"))

div <- mask(div, aoi)
rar <- mask(rar, aoi)

# 
# # reclass only the highest values , convert to 1 and 2. 
# 
# m <- c(0, 3, 0, # lowest diversity 
#        4, 4, 1,
#        5, 5, 2) # highest diversity 
# 
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# 
# # reclass diversity 
# div_high <- classify(div, rclmat, include.lowest=TRUE)
# rar_high <- classify(rar, rclmat, include.lowest = TRUE)
# 
# sr <- c(div_high, rar_high)
# writeRaster(sr, file.path("outputs", "sk_high_conc_dr_stack.tif"), overwrite = TRUE)
# 
# 
# 




# summary by Ecoregion: 

ec <- st_read(file.path("inputs", "sk_ecoreg_clip.gpkg")) %>%
  select(ECOREGION_NAME)%>% 
  mutate(area_m2 = st_area(.))


# calculate the summary of ecoregions within skeeena region

ecsum <- ec %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(totsum = sum(area_m2))

ecv <- vect(ec)

# 
# # calculate the number of pixels per group, per ecoregion 
# e <- extract(div, ecv, un="table", na.rm=TRUE, exact=FALSE)
# edf <- data.frame(NAME_2=ecv$ECOREGION_NAME[e[,1]], e[,-1])
# 
# edff <- edf %>% 
#   group_by(NAME_2, e....1.) |> 
#   count()

# rasterize the zones
ecr <- rasterize(ecv,temp, field="ECOREGION_NAME")
ex <- expanse(div, unit="m", byValue=TRUE, zones=ecr, wide=TRUE)[,-1]

write.csv(ex, file.path("outputs", "div_per_ecoregion.csv"))


ex <- readr::read_csv(file.path("outputs", "div_per_ecoregion.csv"))%>%
  select(-...1)

# calculate the % cover of each group by class 
library(janitor)
pc_div_sum <- ex %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) |> 
  bind_cols(tot_area = ecsum$totsum)

write.csv(pc_div_sum,file.path("outputs", "div_pcper_ecoregion.csv"))
  



## Repeat for the rarity codes 


# rasterize the zones
ecr <- rasterize(ecv, temp, field="ECOREGION_NAME")
ex <- expanse(rar, unit="m", byValue=TRUE, zones=ecr, wide=TRUE)[,-1]

write.csv(ex, file.path("outputs", "rare_per_ecoregion.csv"))


ex <- readr::read_csv(file.path("outputs", "rare_per_ecoregion.csv"))%>%
  select(-...1)

# calculate the % cover of each group by class 
library(janitor)
pc_rare_sum <- ex %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) |> 
  bind_cols(tot_area = ecsum$totsum)

write.csv(pc_rare_sum,file.path("outputs", "rare_pcper_ecoregion.csv"))















# calculate % protected for all of Skeena

## read in protected layers 

pro <- st_read(file.path("inputs", "protected_lands.gpkg")) 
con <- st_read(file.path("inputs", "cons_lands.gpkg"))%>%
  filter(CONSERVATION_LAND_TYPE %in% c("Administered Lands","Wildlife Management Areas")) %>%
  rename("PROTECTED_LANDS_NAME" = SITE_NAME,
         "PROTECTED_LANDS_DESIGNATION" = CONSERVATION_LAND_TYPE) %>% 
  select(-TENURE_DESCRIPTION, -TENURE_TYPE)


pros <- bind_rows(pro, con)

pros <- pros %>% 
  mutate(protected = "protected")

 
# clip to the regions 
aoi <- st_as_sf(aoi)
pross <- st_intersection(pros, aoi) 

st_write(pross, file.path("outputs", "protected_lands_clipped.gpkg"))




## what proportion of the skeena region is currently protected? 

# how much area is protected within each eco_region: ()

# simplify protected area 
pross_u <- pross %>% select(protected)
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

library(readr)
write_csv(ecpro, file.path("outputs", "protection_per_ecoregion.csv"))












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



