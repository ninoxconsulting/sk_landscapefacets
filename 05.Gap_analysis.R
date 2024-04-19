#1. Gap analysis 

# review the proteced areas: 

library(bcdata)
library(dplyr)
library(terra)
library(sf)

# read in study area 

basedata = "C:\\Users\\genev\\OneDrive\\Documents\\02.Contracts\\00_data\\base_vector\\regions"

aoi <- vect(file.path(basedata, "SkeenaRegionBndry.shp"))
aoi_sf <- st_as_sf(aoi)



# read in concentration layers: diverity and rarity

div <- rast(file.path("outputs", "sk_diveristy_conc.tif"))
rar <- rast(file.path())







## read in protected layers 


pro <- st_read(file.path("inputs", "protected_lands.gpkg"))
con <- st_read(file.path("inputs", "cons_lands.gpkg"))%>%
  filter(CONSERVATION_LAND_TYPE %in% c("Administered Lands","Wildlife Management Areas")) %>%
  rename("PROTECTED_LANDS_NAME" = SITE_NAME,
          "PROTECTED_LANDS_DESIGNATION" = CONSERVATION_LAND_TYPE) %>% 
  select(-TENURE_DESCRIPTION, -TENURE_TYPE)


pros <- bind_rows(pro, con)

## read in ecoregions 

ec <- st_read(file.path("inputs", "sk_ecoreg.gpkg")) %>%
  select(ECOREGION_NAME)%>% 
  mutate(area_m2 = st_area(.))


# calculate the summary of ecoregions within skeeena region

ecsum <- ec %>%
  st_drop_geometry() |> 
  group_by(ECOREGION_NAME) |> 
  mutate(totsum = sum(area_m2))

ecv <- vect(ec)



# calculate the number of pixels per group, per ecoregion 
e <- extract(div_con, ecv, un="table", na.rm=TRUE, exact=FALSE)
edf <- data.frame(NAME_2=ecv$ECOREGION_NAME[e[,1]], e[,-1])

edff <- edf %>% 
  group_by(NAME_2, e....1.) |> 
  count()



# rasterize the zones
ecr <- rasterize(ecv, srast, field="ECOREGION_NAME")
ex <- expanse(div_con, unit="m", byValue=TRUE, zones=ecr, wide=TRUE)[,-1]

# using zonal 
#x <- cellSize(r, unit="km")
#zonal(x, c(r, zone), fun="sum", wide=TRUE)

# 
# library(terra)
# #terra 1.7.21
# v <- vect(system.file("ex/lux.shp", package="terra"))
# r <- rast(system.file("ex/elev.tif", package="terra"))
# r <- round((r-50)/100)
# levels(r) <- data.frame(id=1:5, name=c("forest", "water", "urban", "crops", "grass"))
# e <- extract(r, v, fun="table", na.rm=TRUE, exact=FALSE)
# 










# calculate % protected for all of Skeena


# calculate % protected by type? per ecoregion? 




# 

sk_rc <- rast(file.path("outputs", "sk_lf_rockclass.tif"))
sk_rcd <- rast(file.path("outputs", "sk_lf_rockclassdet.tif"))

skrcdf <- as.data.frame(sk_rc)
skrcdff <- skrcdf %>% 
  group_by(lyr.1) |> 
  count()|> 
  mutate(type = "rockclass") %>%
  ungroup()%>% 
  select(-lyr.1)

hist(skrcdff$n)

skrcddf <- as.data.frame(sk_rcd)
skrcddff <- skrcddf %>% 
  group_by(lyr.1) |> 
  count() |> 
  mutate(type = "detailed") %>%
  ungroup()%>%
  select(-lyr.1)

aa <- bind_rows(skrcdff,skrcddff )

xx <- skrcddf

hist(skrcddff$n)

library(ggplot2)



hist(as.numeric(skrcdf$lyr.1))

hist(as.numeric(skrcddf$lyr.1))
