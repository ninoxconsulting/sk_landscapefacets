## lakes Barcode assembly

library(dplyr)
library(sf)


## LAKES IS IN TABULAR FORM. 

in_aoi <- read_sf(file.path("inputs", "sk_poly_template.gpkg"))

# lakes 
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




# # #4)  "no inlets", 
# # hist()
# # 
# # lac<- lac %>%
# #   mutate(outlets  = case_when(
# #     WSA_ELEVATION_IN_METER <= 200 ~ "low", 
# #     WSA_ELEVATION_IN_METER> 200 &  WSA_ELEVATION_IN_METER <=600 ~ "medium", 
# #     WSA_ELEVATION_IN_METER > 600 &  WSA_ELEVATION_IN_METER <=1000 ~ "large", 
# #     WSA_ELEVATION_IN_METER > 1000 ~ "very large"
# #   ))
# # 
# # # 5) no outlets. 
# # 
# # lac <- lac %>%
# #   mutate(wsa_elevation = case_when(
# #     WSA_ELEVATION_IN_METER <= 200 ~ "low", 
# #     WSA_ELEVATION_IN_METER> 200 &  WSA_ELEVATION_IN_METER <=600 ~ "medium", 
# #     WSA_ELEVATION_IN_METER > 600 &  WSA_ELEVATION_IN_METER <=1000 ~ "large", 
# #     WSA_ELEVATION_IN_METER > 1000 ~ "very large"
# #   ))
# 
# #5) upstream drainage area 
# 
# #hist(ldf$UPSTREAM_DRAINAGE_AREA) 
# 
# ldf <- ldf %>%
#   mutate(upstream_drainage_code = case_when(
#     UPSTREAM_DRAINAGE_AREA <= 1 ~ "very small", 
#     UPSTREAM_DRAINAGE_AREA > 1 &  UPSTREAM_DRAINAGE_AREA  <= 5 ~ "small",
#     UPSTREAM_DRAINAGE_AREA > 5 &  UPSTREAM_DRAINAGE_AREA  <= 25 ~ "medium", 
#     UPSTREAM_DRAINAGE_AREA > 25 &  UPSTREAM_DRAINAGE_AREA  <= 75 ~ "large", 
#     UPSTREAM_DRAINAGE_AREA > 75   ~ "very large"))
# 
# comp <- ldf |> st_drop_geometry() |> group_by(upstream_drainage_code) |> count()
# #comp
# 
# 
# #6) Lake Ecosystem class
# 
# ldf <- ldf %>%
#   mutate(ecosystem_class = case_when(
#     LAKE_ECOSYSTEM_CLASS == "- 999" ~ "NA", 
#     .default = as.character(LAKE_ECOSYSTEM_CLASS)
#     ))
#   
# #comp <- ldf |> st_drop_geometry() |> group_by(LAKE_ECOSYSTEM_CLASS) |> count()
# 
# ## 7) MOdelled mean depth 
# 
# #hist(lac$MODELLED_MEAN_DEPTH) 
# 
# ldf <- ldf %>%
#   mutate(mean_depth = case_when(
#     MODELLED_MEAN_DEPTH <= 5 ~ "shallow", 
#     MODELLED_MEAN_DEPTH > 5 &  MODELLED_MEAN_DEPTH  <= 10 ~ "medium", 
#     MODELLED_MEAN_DEPTH > 10   ~ "deep"))
# 
# #comp <- ldf |> st_drop_geometry() |> group_by(mean_depth) |> count()
# 





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






Paul# countNA()# EAUBC freshwater ecoregions 
# https://catalogue.data.gov.bc.ca/dataset/eaubc-freshwater-ecoregion
#https://catalogue.data.gov.bc.ca/dataset/f8c3dc01-0fdc-41ce-a156-cc9cc0a80092

wet <- st_read(file.path("inputs", "eaubc_rivers.gpkg"))

rrrreg <- sf::st_intersection(rreg,  in_aoi) 

st_write(rrrreg, file.path("inputs", "eaubc_reg.gpkg"), append = FALSE)

