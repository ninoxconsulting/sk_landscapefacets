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
# Filter out the -999 for ecosystem class. 


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

# go straight to reading in...
summ <- read_csv(file.path("inputs", "fw_lakes_summary.csv"))

## Assign rarity code based on percent 
ids = summ %>% 
  mutate(total = sum(count))%>%
  rowwise() %>%
  mutate(pc = (count/total)*100)%>%
  arrange(count)

ids <-within(ids, acc_sum <- cumsum(pc))

rare <- ids %>% 
  mutate(rare_id = case_when(
    acc_sum <= 1 ~ 6, 
    acc_sum > 1 & acc_sum <=2 ~ 5,
    acc_sum > 2 & acc_sum <=4 ~ 4,
    acc_sum > 4 & acc_sum <=8 ~ 3,
    acc_sum > 8 & acc_sum <=16 ~ 2,
    .default = as.numeric(1)
  ))

# re join the rarity code back with the lake id and then group with the rivers polygon. 


rlac_sf <- lac_sf %>%
  left_join(rare) %>% 
  select("WSA_LAKE_ID", "lake_code", "rare_id") 

ri <- st_read(file.path("inputs", "eaubc_rivers.gpkg"))%>% 
  select(RIVER_ID)

# intersect the watershed
lac_river_poly <- st_intersection(rlac_sf, ri)


# summarise into a single polygon by taking the rarest value persent in the polygon

sum_df <- lac_river_poly %>%
  st_drop_geometry()%>%
  select(-WSA_LAKE_ID)


# number of lakes per watershed 
no_lakes_per_river_watershed <- sum_df %>% 
  group_by(RIVER_ID) %>% 
  add_count() %>% 
  select(RIVER_ID, n) %>% 
  distinct()



# number of lakes per code per watershed
no_lakes_per_river_watershed_per_code <- sum_df %>% 
  group_by(RIVER_ID, lake_code) %>% 
  add_count() %>% 
  select(RIVER_ID, n, lake_code) %>% 
  distinct()
  

# calculate diversity of barcodes per watershed (ie. no of different code)

div_lake <- no_lakes_per_river_watershed_per_code %>% 
  group_by(RIVER_ID)%>% 
  select(-n) %>% 
  count(RIVER_ID) %>%
  rename( "unique_lakecodes" = n)


div_lak <- left_join(div_lake, no_lakes_per_river_watershed)%>%
  rename( "no_lakes_total" = n) %>% 
  mutate(div_prop = unique_lakecodes/ no_lakes_total)

# calculate a ratio of number of distinct lake types/ total no of lakes 
# ie nuique codes / no lakes total 
# scaled from 0 - 1 where 0 = lower diversity and 1 = high diversity 

# rejoin to the spatial data and export 

ri_diversity <- left_join(ri, div_lak )

st_write(ri_diversity , file.path("inputs", "lake_div_raw.gpkg"), append = FALSE)




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
  select("RIVER_ID", #"WSA_WATERSHED_CODE",
         RIVER_ECOSYSTEM_CLASS,
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
    MELTONS_RATIO <= 0.1 ~ 1, 
    MELTONS_RATIO > 0.1 &  MELTONS_RATIO<=0.2 ~ 2, 
    MELTONS_RATIO > 0.2 &  MELTONS_RATIO <=0.5 ~ 3, 
    MELTONS_RATIO > 0.5 ~ 4
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
































