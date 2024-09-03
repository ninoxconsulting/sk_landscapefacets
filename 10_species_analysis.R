
# consolidate the species data
# review presence loactions with the barcodes (terresttial, lakes and rivers)
# generate 100m x 100m grid for analysis 


## Set up 

library(dplyr)
library(terra)
library(sf)

srast <- rast(file.path("inputs", "sk_rast_template.tif"))

in_aoi <- st_read(file.path("inputs", "sk_poly_template.gpkg"))
bc_cbc_sp_raw.gpkg


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


# 6) surveysnot useful 
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


#-northwest salamander
nwsal <- wwa %>% filter(SPECIES_ENGLISH_NAME == "Northwestern Salamander")%>%
  distinct()
st_write(nwsal, file.path("outputs", "northwestsal_pt.gpkg"), append = FALSE)


#-rough-skinned newt
srn <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Roughskin Newt")
st_write(srn, file.path("outputs", "roughskinnewt_pt.gpkg"))

#-wood frog
wf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Wood Frog")
st_write(wf, file.path("outputs", "woodfrog_pt.gpkg"))


#-coastal tailed frog (and bc cdc too)
ctf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Coastal Tailed Frog")
ctf2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Coastal Tailed Frog")

ctf <- bind_rows(ctf, ctf2)

st_write(ctf, file.path("outputs", "coastaltail_pt.gpkg"))

#-Columbia spotted frog
csf <-  wwa %>% filter(SPECIES_ENGLISH_NAME == "Columbia Spotted Frog")
st_write(csf, file.path("outputs", "columbiaspotfrog_pt.gpkg"))






# Fish species

sort(unique(fi$SPECIES_NAME))

soi <- c("Eulachon", "Bull Trout", 
         "Chum Salmon", "Sockeye Salmon" ,"Pink Salmon","Coho Salmon",
         "Chinook Salmon", "All Salmon" ,
         "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)")

fii <- fi %>% filter(SPECIES_NAME %in% soi)

st_write(fii, file.path("outputs", "fish_test.gpkg"))

# Eulachon
ee <- fii %>% filter(SPECIES_NAME =="Eulachon")
st_write(ee, file.path("outputs", "eulachon_pt.gpkg"))

# bulltrout
ee <- fii %>% filter(SPECIES_NAME =="Bull Trout")
st_write(ee, file.path("outputs", "bulltrout_pt.gpkg"))

# salmon 
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Chum Salmon", "Sockeye Salmon" ,"Pink Salmon","Coho Salmon",
  "Chinook Salmon", "All Salmon"))
  
st_write(ee, file.path("outputs", "salmon_pt.gpkg"))
  
# steel head 
ee <- fii %>% filter(SPECIES_NAME %in% c(
  "Steelhead",  "Steelhead (Summer-run)", "Steelhead (Winter-run)"))

st_write(ee, file.path("outputs", "steelhead_pt.gpkg"))




## Osprey 

os <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Osprey")
st_write(os, file.path("outputs", "osprey_pt.gpkg"), append = FALSE)


# dragon flies 

dra <- ww %>% filter(ORDER_NAME == "Odonata")
st_write(dra, file.path("outputs", "odonata_pt.gpkg"))







### terrestrial 

# whitebark pine (also in bc cdc)
wbp <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Whitebark Pine")
wbp2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "whitebark pine")

wbp <- bind_rows(wbp, wbp2)

st_write(wbp, file.path("outputs", "whitebarkpine_pt.gpkg"))


# Pacific marten
ee <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Pacific Marten")
st_write(ee, file.path("outputs", "pacificmarten_pt.gpkg"))


# northern flying squirrel
ee <-  ww %>% filter(SPECIES_ENGLISH_NAME == "Northern Flying Squirrel")
st_write(ee, file.path("outputs", "nthfylingsq_pt.gpkg"))


# rare epiphytic lichen
#rare epiphytic lichens (BC CDC - group cryptic paw, smoker’s lung combined)

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "cryptic paw" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "smoker's lung" )

el <- bind_rows(el, el2)

st_write(el, file.path("outputs", "epiphyticlichen_pt.gpkg"))


#grasslands bulkley (BC CDC - group  Saskatoon/slender wheatgrass and Sandbergs bluegrass - slender wheatgrass

el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "saskatoon / slender wheatgrass" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "Sandberg's bluegrass - slender wheatgrass" )
el <- bind_rows(el, el2)
st_write(el, file.path("outputs", "grasslands_pt.gpkg"))


# -cottonwood floodplain forests (BC CDC - group black cottonwood	-red alder salmonberry & black cottonwood - hybrid spruce -redosier)
el <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "black cottonwood - hybrid white spruce / red-osier dogwood" )
el2 <- wcdc %>% filter(SPECIES_ENGLISH_NAME == "black cottonwood - red alder / salmonberry"  )
el <- bind_rows(el, el2)
st_write(el, file.path("outputs", "cottonwood_pt.gpkg"))


# bats 
#-bats (all species and “bats” (unidentified bats group in data set)
         
# ww        
sort(unique(ww$ORDER_NAME ))
   
bb <- ww %>% filter(ORDER_NAME =="Chiroptera")                     
st_write(bb, file.path("outputs", "bat_pt.gpkg"))


#northern goshawk
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "Northern Goshawk")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "Northern Goshawk") 
nn <- bind_rows(nn, nn2)
st_write(nn, file.path("outputs", "northerngoshawk_pt.gpkg"))

        
# -red-backed vole
ee <- ww %>% filter(SPECIES_ENGLISH_NAME == "Southern Red-backed Vole")  
st_write(ee , file.path("outputs", "sthredbackedvole_pt.gpkg"))


#black bear
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "American Black Bear")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "American Black Bear") 
nn <- bind_rows(nn, nn2)
st_write(nn , file.path("outputs", "blackbear_pt.gpkg"))

# grizz 
nn <- ww %>% filter(SPECIES_ENGLISH_NAME == "Grizzly Bear")      
nn2 <- wwt %>% filter(SPECIES_ENGLISH_NAME == "Grizzly Bear") 
nn <- bind_rows(nn, nn2)
st_write(nn , file.path("outputs", "grizbear_pt.gpkg"))

                            


############################################################

# intersect with terrestrial barcodes

sk_lf_barcode.tif


