# consolidate the species data
# review presence loactions with the barcodes (terresttial, lakes and rivers)
# generate 100m x 100m grid for analysis 

## Set up 

library(dplyr)
library(terra)
library(sf)
library(readr)

srast <- rast(file.path("inputs", "sk_rast_template.tif"))

in_aoi <- st_read(file.path("inputs", "sk_poly_template.gpkg"))


# read in species dataset 

# 1) fish observations
fi <- st_read(file.path("inputs", "sk_known_fish_pts.gpkg"))

# 2) wildlife obs
wa <- st_read(file.path("inputs", "wildlife_obs_all.gpkg")) %>%
  select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATETIME,
         OBSERVATION_YEAR,LATITUDE, LONGITUDE,
         NAME_TYPE,  NAME_TYPE_SUB,TAXONOMIC_LEVEL,
         PHYLUM_NAME, CLASS_NAME, CLASS_ENGLISH, ORDER_NAME)

# 3) wildlife inc
wi <- st_read(file.path("inputs", "wildlife_incident_obs.gpkg")) %>% 
  select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATE,
         OBSERVATION_YEAR,LATITUDE, LONGITUDE,
         NAME_TYPE,  NAME_TYPE_SUB,TAXONOMIC_LEVEL,
         PHYLUM_NAME, CLASS_NAME, CLASS_ENGLISH, ORDER_NAME)

ww <- bind_rows(wa, wi)


# 4) wildlife telem
wwt <- st_read(file.path("inputs", "wildlife_telemetry_pts.gpkg")) %>% 
  select(SPECIES_ENGLISH_NAME, SCIENTIFIC_NAME,OBSERVATION_DATE)

#[1] "Northern Goshawk"    "Grizzly Bear"        "American Black Bear"
#[4] "Grey Wolf" 

# 5) cdc dataset
wcdc <- st_read(file.path("inputs", "bc_cbc_sp_raw.gpkg")) %>% 
  select(ENG_NAME, SCI_NAME, EL_TYPE)%>%
  rename("SPECIES_ENGLISH_NAME" = ENG_NAME,
           "SCIENTIFIC_NAME" = SCI_NAME,
          "NAME_TYPE_SUB" = EL_TYPE)


# 6) surveys not useful 
#ws <- st_read(file.path("inputs", "wildlife_surveys.gpkg"))


## VERTEBRATES 
#wwv <- ww %>% 
#  filter(NAME_TYPE == "Vertebrate Animal")

## amphibians 
wwa <- ww %>% filter(CLASS_NAME == "Amphibia")

#-western toad

wt <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Western Toad")%>%
 distinct()
st_write(wt, file.path("outputs", "western_toad_pt.gpkg"), append = FALSE)
# 
# # intersect with 
# wt_la <- st_intersection(wt, la) %>% select(SPECIES_ENGLISH_NAME,lake_code)
# wt_ri <- st_intersection(wt, ri) %>% select(SPECIES_ENGLISH_NAME, river_code)
# wt_land <- st_intersection(wt, tesf) %>% select(SPECIES_ENGLISH_NAME, land_barcode)
# 
# wt_lake <- wt_la %>%
#   group_by(lake_code)%>% 
#   count()%>%
#   mutate(westtoad_lake_n = n)%>% 
#   select(-n)
# 
# wt_river <- wt_ri %>%
#   group_by(river_code)%>% 
#   count()%>%
#   mutate(westtoad_river_n = n)%>% 
#   select(-n)
# 
# wt_land <- wt_land %>%
#   group_by(land_barcode)%>% 
#   count()%>%
#   mutate(westtoad_land_n = n)%>% 
#   select(-n)
# 
# ter_csv <- left_join(ter_csv, wt_land)
# la_csv <- left_join(la_csv, wt_lake)
# ri_csv <- left_join(ri_csv, wt_river)
# 



#-northwest salamander
nwsal <- wwa %>% filter(SPECIES_ENGLISH_NAME == "Northwestern Salamander")%>%
  distinct()
st_write(nwsal, file.path("outputs", "northwestsal_pt.gpkg"), append = FALSE)


#-rough-skinned newt
srn <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Roughskin Newt")%>%
  distinct()
st_write(srn, file.path("outputs", "roughskinnewt_pt.gpkg"), append = FALSE)



#-wood frog
wf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Wood Frog")%>%
  distinct()

st_write(wf, file.path("outputs", "woodfrog_pt.gpkg"), append = FALSE)


# coastal tailed frog (and bc cdc too)
ctf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Coastal Tailed Frog")
ctf2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Coastal Tailed Frog")%>%
  mutate(area_m = st_area(.))%>%
  mutate(area = as.numeric(area_m))%>%
  filter(area < 100000)
         
ctf22 <- st_centroid(ctf2)%>% select(-area_m, -area)

ctf <- bind_rows(ctf, ctf22) %>% 
  distinct()

st_write(ctf, file.path("outputs", "coastaltail_pt.gpkg"), append = FALSE)


#-Columbia spotted frog
csf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Columbia Spotted Frog")%>%
  distinct()
st_write(csf, file.path("outputs", "columbiaspotfrog_pt.gpkg"), append = FALSE)


# Fish species

sort(unique(fi$SPECIES_NAME))

soi <- c("Eulachon", "Bull Trout", 
         "Chum Salmon", "Sockeye Salmon" ,"Pink Salmon","Coho Salmon",
         "Chinook Salmon", "All Salmon" ,
         "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)")

fii <- fi %>% filter(SPECIES_NAME %in% soi)


# Eulachon
ee <- fii %>% filter(SPECIES_NAME =="Eulachon") 

st_write(ee, file.path("outputs", "eulachon_pt.gpkg"), append = FALSE)


# bulltrout
ee <- fii %>% filter(SPECIES_NAME =="Bull Trout")#%>%
  #distinct()
st_write(ee, file.path("outputs", "bulltrout_pt.gpkg"), append = FALSE)


# salmon 
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Chum Salmon", "Sockeye Salmon" ,"Pink Salmon","Coho Salmon",
  "Chinook Salmon", "All Salmon"))
st_write(ee, file.path("outputs", "salmon_pt.gpkg"), append = FALSE)

# steel head 
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)"))

st_write(ee, file.path("outputs", "steelhead_pt.gpkg"),append = FALSE)

# steel head 
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)"))

st_write(ee, file.path("outputs", "steelhead_pt.gpkg"),append = FALSE)


## Osprey 

os <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Osprey")
st_write(os, file.path("outputs", "osprey_pt.gpkg"), append = FALSE)


# dragon flies 

dra <- ww %>% filter(ORDER_NAME == "Odonata")
st_write(dra, file.path("outputs", "odonata_pt.gpkg"), append = FALSE)



### terrestrial 

# whitebark pine (also in bc cdc)
wbp <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Whitebark Pine")
#wbp2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "whitebark pine")
# these are already included
##wbp <- bind_rows(wbp, wbp2)%>%
#  distinct()

st_write(wbp, file.path("outputs", "whitebarkpine_pt.gpkg"), append = FALSE)


# Pacific marten
ee <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Pacific Marten")%>%
  distinct()
st_write(ee, file.path("outputs", "pacificmarten_pt.gpkg"), append = FALSE)


# northern flying squirrel
ee <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Northern Flying Squirrel")%>%
  distinct()
st_write(ee, file.path("outputs", "nthfylingsq_pt.gpkg"), append = FALSE)


# rare epiphytic lichen
#rare epiphytic lichens (BC CDC - group cryptic paw, smoker’s lung combined)

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "cryptic paw" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "smoker's lung" )

el <- bind_rows(el, el2)%>%
  distinct()%>%
  mutate(area_m = st_area(.))%>%
  mutate(area = as.numeric(area_m))%>%
  filter(area < 100000)

el <- st_centroid(el)%>% select(-area_m, -area)

st_write(el, file.path("outputs", "epiphyticlichen_pt.gpkg"), append = FALSE)


#grasslands bulkley (BC CDC - group  Saskatoon/slender wheatgrass and Sandbergs bluegrass - slender wheatgrass

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "saskatoon / slender wheatgrass" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Sandberg's bluegrass - slender wheatgrass" )
el <- bind_rows(el, el2)%>%
  distinct()%>%
  mutate(area_m = st_area(.))%>%
  mutate(area = as.numeric(area_m))%>%
  filter(area < 100000)

el <- st_centroid(el)%>% select(-area_m, -area)
st_write(el, file.path("outputs", "grasslands_pt.gpkg"), append = FALSE)


# -cottonwood floodplain forests (BC CDC - group black cottonwood	-red alder salmonberry & black cottonwood - hybrid spruce -redosier)
el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "black cottonwood - hybrid white spruce / red-osier dogwood" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "black cottonwood - red alder / salmonberry"  )
el <- bind_rows(el, el2)%>%
  distinct()%>%
  mutate(area_m = st_area(.))%>%
  mutate(area = as.numeric(area_m))%>%
  filter(area < 100000)

el <- st_centroid(el)%>% select(-area_m, -area)
st_write(el, file.path("outputs", "cottonwood_pt.gpkg"), append = FALSE)


# bats 
#-bats (all species and “bats” (unidentified bats group in data set)
         
# ww        
sort(unique(ww$ORDER_NAME ))
   
bb <- ww %>% filter(ORDER_NAME =="Chiroptera")    %>% distinct()                 
st_write(bb, file.path("outputs", "bat_pt.gpkg"), append = FALSE)


#northern goshawk
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "Northern Goshawk")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "Northern Goshawk") 
nn <- bind_rows(nn, nn2)%>% distinct()  
st_write(nn, file.path("outputs", "northerngoshawk_pt.gpkg"), append = FALSE)

        
# -red-backed vole
ee <- ww %>% filter(SPECIES_ENGLISH_NAME == "Southern Red-backed Vole")  
st_write(ee , file.path("outputs", "sthredbackedvole_pt.gpkg"), append = FALSE)


#black bear
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "American Black Bear")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "American Black Bear") 
nn <- bind_rows(nn, nn2)%>% distinct()  
st_write(nn , file.path("outputs", "blackbear_pt.gpkg"), append = FALSE)

# grizz 
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "Grizzly Bear")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "Grizzly Bear") 
nn <- bind_rows(nn, nn2)%>% distinct()  
st_write(nn , file.path("outputs", "grizbear_pt.gpkg"), append = FALSE)

                            


############################################################
library(tidyverse)

# intersect with terrestrial barcodes

te <- st_read(file.path("outputs", "final", "sk_lf_barcode_poly.gpkg"))

# land barcode 
te_csv <- read_csv(file.path("outputs", "lf_barcode_summary.csv"))%>%
  select(layer1, count)%>% 
  rename("land_barcode" = layer1)


# lakes barcode 

la <- st_read(file.path("outputs", "final", "sk_lakes_barcode_poly.gpkg"))%>%
  select(lake_code)

la_csv <- la %>%
  st_drop_geometry() %>%
  select(lake_code)%>%
  group_by(lake_code)%>%
  count()


# river barcode 

ri <- st_read(file.path("outputs", "final", "sk_river_barcode_poly.gpkg"))%>%
  select(river_code)

ri_csv <- ri %>%
  st_drop_geometry() %>%
  select(river_code)%>%
  group_by(river_code) %>%
  count()


## Read in the land intersect species 

sp <- list.files(file.path("outputs"), pattern = "*_pt.gpkg")

DF <-  st_read(file.path("outputs", sp[1])) %>% mutate(lf_group = sp[1])
for (f in sp[-1]) DF <- bind_rows(DF, st_read(file.path("outputs", f))%>% mutate(lf_group = f))   

DF <- DF %>% select(SPECIES_ENGLISH_NAME,  SCIENTIFIC_NAME, OBSERVATION_DATETIME,lf_group)%>%
  distinct(.)

#st_write(DF, file.path("outputs", "allsp.gpkg"))
# n = 34862 

# intersect with land 
wt_land <- st_intersection(DF, te)

st_write(wt_land, file.path("outputs", "allsp_land.gpkg"))

%>% select(SPECIES_ENGLISH_NAME, lf_group, land_barcode)



# intersect with river
wt_ri <- st_intersection(DF, ri) 
wri <- wt_ri %>% 
  select(lf_group, river_code)%>% 
  mutate(lf_group = gsub("*_pt.gpkg", "", lf_group)) %>%
  st_drop_geometry()%>% 
  group_by(lf_group)%>% 
  count(river_code)

wri <- wri %>% 
  pivot_wider( names_from = lf_group , values_from = n)

ri_wri <- left_join(ri_csv, wri)
ri_wri <- ri_wri%>%
    mutate_all(~replace(., is.na(.), 0))

write.csv(ri_wri, file.path("outputs", "all_sp_riverbarcodes.csv"))



# intersect with lakes
wt_la <- st_intersection(DF, la) 
wla <- wt_la %>% 
  select(lf_group, lake_code)%>% 
  mutate(lf_group = gsub("*_pt.gpkg", "", lf_group)) %>%
  st_drop_geometry()%>% 
  group_by(lf_group)%>% 
  count(lake_code)

wla <- wla %>% 
  pivot_wider( names_from = lf_group , values_from = n)

law <- left_join(la_csv, wla)
law <- law %>%
  mutate_all(~replace(., is.na(.), 0))

write.csv(law, file.path("outputs", "all_sp_lakebarcodes.csv"))



