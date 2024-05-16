## lakes Barcode assembly

library(dplyr)
library(sf)
library(tidyr)



in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))

## LAKES  - barcode 

##https://catalogue.data.gov.bc.ca/dataset/eaubc-lakes

lac_sf <- read_sf(file.path("inputs", "eaubc_lakes.gpkg")) %>% 
  select("WSA_LAKE_ID", "WSA_SURFACE_AREA_HECTARE", "WSA_SHORELINE_COMPLEXITY",  "WSA_ELEVATION_IN_METER", 
         "LAKE_ECOSYSTEM_CLASS", "BEDROCK_GEOLOGY_CLASSES")


        
ldf <- lac_sf |> 
  st_drop_geometry() 

#1) WSA_SURFACE_AREA_HECTARE
ldf <- ldf %>%
  mutate(surface_area = case_when(
    WSA_SURFACE_AREA_HECTARE <= 1 ~ 1, 
    WSA_SURFACE_AREA_HECTARE > 1 &  WSA_SURFACE_AREA_HECTARE <=10 ~ 2, 
    WSA_SURFACE_AREA_HECTARE > 10 &  WSA_SURFACE_AREA_HECTARE <=100 ~ 3,
    WSA_SURFACE_AREA_HECTARE > 100 &  WSA_SURFACE_AREA_HECTARE <=1000 ~ 4,
    WSA_SURFACE_AREA_HECTARE > 1000 &  WSA_SURFACE_AREA_HECTARE <=10000 ~ 5,
    WSA_SURFACE_AREA_HECTARE > 10000 ~ 6
  ))

#comp <- lac |> st_drop_geometry() |> group_by(surface_area) |> count()


#2) Lake Ecosystem class

ldf <- ldf %>%
  mutate(ecosystem_class = case_when(
    LAKE_ECOSYSTEM_CLASS == "U" ~ 1, 
    LAKE_ECOSYSTEM_CLASS == "D" ~ 2, 
    LAKE_ECOSYSTEM_CLASS == "I" ~ 3, 
    LAKE_ECOSYSTEM_CLASS == "- 999" ~ 4
  ))



#3) "WSA_SHORELINE_COMPLEXITY"

ldf <- ldf %>%
  mutate(shoreline_complex = case_when(
    WSA_SHORELINE_COMPLEXITY <= 1.02 ~ 1, 
    WSA_SHORELINE_COMPLEXITY > 1.02 &  WSA_SHORELINE_COMPLEXITY <=2.03 ~ 2, 
    WSA_SHORELINE_COMPLEXITY > 2.03 &  WSA_SHORELINE_COMPLEXITY <=4 ~ 3, 
    WSA_SHORELINE_COMPLEXITY > 4 ~ 4
  ))


#3)  "WSA_ELEVATION_IN_METER", 
ldf <- ldf %>%
  mutate(wsa_elevation = case_when(
    WSA_ELEVATION_IN_METER <= 100 ~ 1, 
    WSA_ELEVATION_IN_METER> 100 &  WSA_ELEVATION_IN_METER <=600 ~ 2, 
    WSA_ELEVATION_IN_METER > 600 &  WSA_ELEVATION_IN_METER <=1000 ~ 3, 
    WSA_ELEVATION_IN_METER > 1000 ~ 4
  ))


#3)  "WSA_ELEVATION_IN_METER", 
ldf <- ldf %>%
    mutate(bedrock_class = case_when(
           BEDROCK_GEOLOGY_CLASSES == - 999 ~ 0, 
           .default = BEDROCK_GEOLOGY_CLASSES
           ))


## combine datasets 

ldf_sum <- ldf |> 
  select(WSA_LAKE_ID, "surface_area","ecosystem_class",
         "wsa_elevation", "shoreline_complex" ,
        "bedrock_class") %>% 
  mutate(lake_code = bedrock_class + (10 * shoreline_complex)+ (100*wsa_elevation)+
           (1000* ecosystem_class) + (10000 * surface_area)) 

lac_sf <- lac_sf %>% 
  left_join(ldf_sum)

st_write(lac_sf, file.path("inputs", "sk_lakes_barcode_poly.gpkg"))

ldf_ss <- ldf_sum |> 
  select(-WSA_LAKE_ID)

summ <- ldf_ss %>%
  group_by(lake_code) %>%
  summarise(count= n())


write_csv(summ, file.path("inputs", "fw_lakes_summary.csv"))



#quantile(summ$count, seq(0,1, 0.1))





# EAUBC freshwater rivers
ri <- st_read(file.path("inputs", "eaubc_rivers.gpkg"))

# filtyer slivers
ri <- ri %>% 
  filter(FEATURE_AREA_SQM > 100000)


## FIX BEDROCK CODE - COMBINE INTO 1 column (predomient) 

ribr <- ri %>%
  st_drop_geometry( ) %>% 
  select("RIVER_ID", "PCT_WS_UNT_BEDROCK_ALLUVIUM", "PCT_WS_UNT_BEDROCK_CARB_SEDS",
         "PCT_WS_UNT_BEDROCK_CHEM_SEDS",  "PCT_WS_UNT_BEDROCK_HARD_SEDS",   
         "PCT_WS_UNT_BEDROCK_INT_META", "PCT_WS_UNT_BEDROCK_SOFT_SEDS", 
         "PCT_WS_UNT_BEDROCK_VOLCANIC") %>% 
  pivot_longer(!"RIVER_ID", names_to = "bedrock_type", values_to = "percent_type") %>% 
  mutate(bedrock_code = case_when(
    bedrock_type == "PCT_WS_UNT_BEDROCK_ALLUVIUM" ~ 3,
    bedrock_type == "PCT_WS_UNT_BEDROCK_CARB_SEDS" ~ 4,
    bedrock_type == "PCT_WS_UNT_BEDROCK_CHEM_SEDS" ~ 5,
    bedrock_type == "PCT_WS_UNT_BEDROCK_HARD_SEDS" ~ 6,
    bedrock_type == "PCT_WS_UNT_BEDROCK_INT_META" ~ 2,
    bedrock_type == "PCT_WS_UNT_BEDROCK_SOFT_SEDS" ~7 ,
    bedrock_type == "PCT_WS_UNT_BEDROCK_VOLCANIC" ~1
  ))

# select the maximum percent bedrock type
ribr_max <- ribr %>%
  group_by(RIVER_ID) %>% 
  slice_max(percent_type)


# select the river ids where more than one max value (i.e. equal percent or -9999 no data )
multi_id <- ribr_max %>%
  group_by(RIVER_ID) %>% 
  #filter(percent_type != -999) %>%
  mutate(count = n()) %>%
  filter(count > 1)

# get list of all imulti idas
multi_id_list = unique(multi_id$RIVER_ID)
all_id_list = unique(ribr$RIVER_ID)

single_max_ri <- setdiff(all_id_list, multi_id_list)


# get subset of multilist where no data (-999 and convert to 0)
ribr_nodata <- ribr_max %>% 
  filter(percent_type == -999) %>% 
  select(RIVER_ID) %>% 
  distinct()%>% 
  mutate(bedrock_code = 0) 
           
# get unique ids
nodat_list = unique(ribr_nodata$RIVER_ID)


# get list of units were equal between two values
ribr_multi <- multi_id %>% 
  filter(percent_type != -999) %>% 
  slice_head(n = 1)%>% 
  select(RIVER_ID, bedrock_code)


# reassemble values 
single_max <- ribr_max %>% 
  filter(RIVER_ID %in% single_max_ri) %>% 
  select(RIVER_ID, bedrock_code)


out <- bind_rows(ribr_nodata, ribr_multi )
bedrockout <- bind_rows(single_max, out)
# join back to spatial data
ri <- left_join(ri, bedrockout)
           
           

ri <- ri %>%
  select("RIVER_ID", RIVER_ECOSYSTEM_CLASS,
         MEAN_WATERSHED_ELEVATION,
         MAX_STREAM_ORDER,
         MELTONS_RATIO,
         STREAM_GRADIENT_MODEL, bedrock_code)


rdf <- ri |> 
  st_drop_geometry() 

#1) RIVER_ECOSYSTEM_CLASS
rdf<- rdf %>%
  mutate(ecosystem_class = case_when(
    RIVER_ECOSYSTEM_CLASS == "C" ~ 1, 
    RIVER_ECOSYSTEM_CLASS == "H" ~ 2,
    RIVER_ECOSYSTEM_CLASS == "M" ~ 3,
    RIVER_ECOSYSTEM_CLASS == "T" ~ 4,
    RIVER_ECOSYSTEM_CLASS == "- 999" ~ 9
  ))

#2) Stream Order 
rdf <- rdf %>%
  mutate(streamorder_max = case_when(
    MAX_STREAM_ORDER == 1 ~ 1,
    MAX_STREAM_ORDER %in% c(2,3) ~ 2,
    MAX_STREAM_ORDER %in% c(4,5,6) ~ 3,
    MAX_STREAM_ORDER %in% c(7,8) ~ 4,
    MAX_STREAM_ORDER == - 999 ~ 9
  ))

#3)  " MEAN ELEVATION_IN_METER", 
rdf <- rdf %>%
  mutate(elevation = case_when(
    MEAN_WATERSHED_ELEVATION <= 100 ~ 1, 
    MEAN_WATERSHED_ELEVATION> 100 &  MEAN_WATERSHED_ELEVATION <=600 ~ 2, 
    MEAN_WATERSHED_ELEVATION > 600 &  MEAN_WATERSHED_ELEVATION <=1000 ~ 3, 
    MEAN_WATERSHED_ELEVATION> 1000 ~ 4
  ))

#3) "RUGGEDNESS
rdf <- rdf %>%
  mutate(meltons = case_when(
    MELTONS_RATIO <= 0.01 ~ 1, 
    MELTONS_RATIO > 0.01 &  MELTONS_RATIO<=0.02 ~ 2, 
    MELTONS_RATIO > 0.02 &  MELTONS_RATIO <=0.05 ~ 3, 
    MELTONS_RATIO > 0.05 ~ 4
  ))



## combine datasets 

rdf_sum <- rdf |> 
  select(RIVER_ID, "ecosystem_class" , "streamorder_max", "elevation" , 
         "meltons",  "bedrock_code") %>% 
  mutate(river_code = bedrock_code + (10 * meltons)+ (100* elevation)+
           (1000* streamorder_max) + (10000 * ecosystem_class)) 


ri_sf <- ri %>% 
  left_join(rdf_sum)

st_write(ri_sf, file.path("inputs", "sk_river_barcode_poly.gpkg"), append = FALSE)

rdf_ss <-rdf_sum  |> 
  select(-RIVER_ID)

summ <- rdf_ss %>%
  group_by(river_code) %>%
  summarise(count= n())


write_csv(summ, file.path("inputs", "fw_river_summary.csv"))


#quantile(summ$count, seq(0,1, 0.1))




































rrrreg <- sf::st_intersection(rreg,  in_aoi) 

st_write(rrrreg, file.path("inputs", "eaubc_reg.gpkg"), append = FALSE)

[1] "ACCUMULATIVE_PRECIP_YIELD"      "ANNUAL_MEAN_PRECIP_PER_WS_UNIT" "ANNUAL_MEAN_TEMP_PER_WS_UNIT"  
[4] "APR_MEAN_PRECIP_PER_WS_UNIT"    "APR_MEAN_TEMP_PER_WS_UNIT"      "AUG_MEAN_PRECIP_PER_WS_UNIT"   
[7] "AUG_MEAN_TEMP_PER_WS_UNIT"      "DAYS_ABOVE_0C"                  "DEC_MEAN_PRECIP_PER_WS_UNIT"   
[10] "DEC_MEAN_TEMP_PER_WS_UNIT"      "FEATURE_AREA_SQM"               "FEATURE_CODE"                  
[13] "FEATURE_LENGTH_M"               "FEB_MEAN_PRECIP_PER_WS_UNIT"    "FEB_MEAN_TEMP_PER_WS_UNIT"     
[16] "FLOW_REGIME_MODEL"              "FRESHWATER_ECO_DRAINAGE_UNIT"   "FRESHWATER_ECOREGION"          
[19] "geom"                           "id"                             "JAN_MEAN_PRECIP_PER_WS_UNIT"   
[22] "JAN_MEAN_TEMP_PER_WS_UNIT"      "JUL_MEAN_PRECIP_PER_WS_UNIT"    "JUL_MEAN_TEMP_PER_WS_UNIT"     
[25] "JULY_MAX_TEMP_PER_WS"           "JUN_MEAN_PRECIP_PER_WS_UNIT"    "JUN_MEAN_TEMP_PER_WS_UNIT"     
[28] "K_FACTOR"                       "MAGNITUDE_RATIO"                "MAINSTEM_GRAD_CLASS_002_008"   
[31] "MAINSTEM_GRAD_CLASS_008_012"    "MAINSTEM_GRAD_CLASS_012_016"    "MAINSTEM_GRAD_CLASS_016_020"   
[34] "MAINSTEM_GRAD_CLASS_GRT_020"    "MAINSTEM_GRAD_CLASS_LESS_002"   "MAR_MEAN_PRECIP_PER_WS_UNIT"   
[37] "MAR_MEAN_TEMP_PER_WS_UNIT"      "MAX_STREAM_MAG_PRIMARY_WS_UNIT" "MAX_STREAM_MAG_WS_UNIT"        
[40] "MAX_STREAM_ORDER"               "MAXIMUM_WATERSHED_ELEVATION"    "MAY_MEAN_PRECIP_PER_WS_UNIT"   
[43] "MAY_MEAN_TEMP_PER_WS_UNIT"      "MEAN_VALLEY_FLAT_WIDTH"         "MEAN_WATERSHED_ELEVATION"      
[46] "MELTONS_RATIO"                  "MINIMUM_WATERSHED_ELEVATION"    "MLF_Kehm_2012"                 
[49] "NOV_MEAN_PRECIP_PER_WS_UNIT"    "NOV_MEAN_TEMP_PER_WS_UNIT"      "NUMBER_OF_LAKES"               
[52] "NUMBER_OF_WETLANDS"             "NUTRIENT_MODEL"                 "OBJECTID"                      
[55] "OCEAN_TERMINUS"                 "OCT_MEAN_PRECIP_PER_WS_UNIT"    "OCT_MEAN_TEMP_PER_WS_UNIT"     
[58] "PCT_GLACIAL_INFLUENCE"          "PCT_LAKE_AREA"                  "PCT_TUNDRA_INFLUENCE"          
[61] "PCT_WETLAND"                    "PCT_WS_UNT_BEDROCK_ALLUVIUM"    "PCT_WS_UNT_BEDROCK_CARB_SEDS"  
[64] "PCT_WS_UNT_BEDROCK_CHEM_SEDS"   "PCT_WS_UNT_BEDROCK_HARD_SEDS"   "PCT_WS_UNT_BEDROCK_INT_META"   
[67] "PCT_WS_UNT_BEDROCK_SOFT_SEDS"   "PCT_WS_UNT_BEDROCK_VOLCANIC"    "PCT_WSA_UNIT_BAFA"             
[70] "PCT_WSA_UNIT_BG"                "PCT_WSA_UNIT_BWBS"              "PCT_WSA_UNIT_CDF"              
[73] "PCT_WSA_UNIT_CMA"               "PCT_WSA_UNIT_CWH"               "PCT_WSA_UNIT_ESSF"             
[76] "PCT_WSA_UNIT_ICH"               "PCT_WSA_UNIT_IDF"               "PCT_WSA_UNIT_IMA"              
[79] "PCT_WSA_UNIT_MH"                "PCT_WSA_UNIT_MS"                "PCT_WSA_UNIT_PP"               
[82] "PCT_WSA_UNIT_SBPS"              "PCT_WSA_UNIT_SBS"               "PCT_WSA_UNIT_SWB"              
[85] "PRIMARY_DRAINAGE_WS_UNIT_ID"    "REVISED_41_HYDROLOGIC_ZONES"    "RIVER_ECOSYSTEM_CLASS"         
[88] "RIVER_ECOSYSTEM_SUB_TYPE"       "RIVER_ECOSYSTEM_TYPE"           "RIVER_ID"                      
[91] "RIVER_SUBTYPE_LABEL"            "SE_ANNO_CAD_DATA"               "SEP_MEAN_PRECIP_PER_WS_UNIT"   
[94] "SEP_MEAN_TEMP_PER_WS_UNIT"      "STND_DEV_WATERSHED_ELEVATION"   "STREAM_GRADIENT_MODEL"         
[97] "TEMPERATURE_MODEL"              "TRIBUTARY_GRAD_CLASS_002_008"   "TRIBUTARY_GRAD_CLASS_008_012"  
[100] "TRIBUTARY_GRAD_CLASS_012_016"   "TRIBUTARY_GRAD_CLASS_016_020"   "TRIBUTARY_GRAD_CLASS_GRT_020"  
[103] "TRIBUTARY_GRAD_CLASS_LESS_002"  "UPSTREAM_DRAINAGE_AREA"         "WS_CODE_ORDER"                 
[106] "WSA_AREA"                       "WSA_DOWNSTREAM_WATERSHED_CODE"  "WSA_DOWNSTREAM_WATERSHED_ID"   
[109] "WSA_GAZETTEER_NAME"             "WSA_LOCAL_STREAM_NAME1"         "WSA_OLD_WATERSHED_CODE"        
[112] "WSA_PERIMETER"                  "WSA_REVERSE_STREAM_ORDER"       "WSA_STREAM_MAGNITUDE"          
[115] "WSA_STREAM_ORDER_50K"           "WSA_WATERSHED_CODE"             "WSA_WATERSHED_ID"              
[118] "WSA_XREF_WATERSHED_CODE"       
