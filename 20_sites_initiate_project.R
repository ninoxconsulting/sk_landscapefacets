# Script to convert to sites, this is largely based on scripts on where to work 
#https://github.com/NCC-CNC/wtw-data-prep/blob/main/scripts/01_initiate_project.R


# downloaded and modified by G.Perkins Jan 26th 2025


# Authors: Marc Edwards
#
# Date: October 3rd, 2024
#
# Description: This script sets up the folder structure for generating input 
#              data for a Where To Work project
#
# Inputs:  1. The project folder path
#          2. An AOI shapefile
#          3. The data type for the project (National or regional)
#
# Outputs: 1. Project folder structure
#
# Tested on R Versions: 4.4.1
#
#===============================================================================

# See https://github.com/NCC-CNC/wtw-data-prep for an explanation of the
# various workflows

## load packages
library(sf)
library(terra)
library(dplyr)
library(readr)
library(tools)
source(fs::path("outputs/final/sites/scripts", "extra_sites_functions.r"))

# ## Install wheretowork if not yet installed or is not updated to current version (HARD CODED FOR NOW)
# if (!require(wheretowork) || packageVersion("wheretowork") != "1.2.3") { 
#   if (!require(remotes)) install.packages("remotes")
#   remotes::install_github("NCC-CNC/wheretowork", ref = "master")  
# }

# If WTW fails to install, follow these steps ----
## 1. Create Github account
## 2. Generate a personal access token (classic) PAT
## 3. Create a .Renviron file in your Documents directory
## 4. Paste your PAT in your .Renviron file

# If rcbc fails to install, be sure to have Rtools44 installed
## https://cran.r-project.org/bin/windows/Rtools/

library(wheretowork)


# 2.0 Set up -------------------------------------------------------------------

# Set project parameters
project_folder <- fs::path("outputs", "final", "sites_202505") # <--- CHANGE TO YOUR ROOT PROJECT FOLDER
PROJECT_TYPE <- "REGIONAL" 


# 3.0 Processing ----------------------------------------------------------------
# create folder structure

#dir.create(fs::path(project_folder, "PU"), recursive = TRUE)
#dir.create(file.path(project_folder, "scripts"), recursive = TRUE)
#dir.create(file.path(project_folder, "Tiffs"), recursive = TRUE)
#dir.create(file.path(project_folder, "WTW/metadata"), recursive = TRUE)

#if(PROJECT_TYPE == "REGIONAL"){
#  dir.create(file.path(project_folder, "Regional"), recursive = TRUE)
#}

# 2.0 Set up AOI -------------------------------------------------------------------

final_data_dir <- fs::path(project_folder ,"raw_tiffs")

#final_data_dir <- fs::path("outputs","final")
rr <- rast(fs::path(final_data_dir, "template_1km.tif"))
names(rr) <- "template"

# Convert all cell values to 0
rr[rr > 0] <- 0
writeRaster(rr, file.path(project_folder, "PU", "PU0.tif"), datatype = "INT1U", overwrite = TRUE)

# make a version where values are all zero
rr1 <- rr
rr1[rr1 == 0] <- 1

terra::writeRaster(rr1, file.path(project_folder, "PU","PU.tif"), datatype = "INT1U", overwrite = TRUE)


#===============================================================================

project_name <- "skeena" # <--- SET PROJECT NAME HERE FOR OUT FILE
themes_dir <- fs::path(project_folder, "Regional", "Themes" )# <--- Themes data folder
includes_dir <- fs::path(project_folder, "Regional","Includes") # <--- Includes data folder
excludes_dir <- fs::path(project_folder, "Regional","Excludes") # <--- Excludes data folder
weights_dir <- fs::path(project_folder,"Regional","Weights") # <--- Weights data folder

pu_units <- "m2" # <--- SET DESIRED UNITS
pu_cell_area <- 1000 # <--- SET PLANNING UNIT AREA IN units


# put the relevant files in the folders as specified above 

# Build vectors of data paths --------------------------------------------------

get_all_tifs_gdbs <- function(search_dir){
  
  tifs <- list.files(search_dir, full.names = TRUE, recursive = TRUE, pattern = ".tif$|.tiff$")
  
  gdbs_paths <- list.dirs(search_dir)[grepl(".gdb$", list.dirs(search_dir))]
  
  gdb_layers <- unlist(
    lapply(gdbs_paths, function(x){
      get_gdb_layers(x)}))
  
  return(c(tifs, gdb_layers))
}

themes_list <- get_all_tifs_gdbs(themes_dir)
includes_list <- get_all_tifs_gdbs(includes_dir)
excludes_list <- get_all_tifs_gdbs(excludes_dir)
weights_list <- get_all_tifs_gdbs(weights_dir)


#themes_list <- themes_list[11:18]

# Fill table -------------------------------------------------------------------

file_list <- c(themes_list, includes_list, excludes_list, weights_list)
#file_list <- c(themes_list,includes_list)
#file_list <- c(themes_list,includes_list, weights_list)
#file_list<-file_list[1:69]
## Build empty data.frame (template for metadata.csv) ----
df <- init_metadata()

# 5.0 Populate metadata --------------------------------------------------------
## Loop over each tiff file:
for (i in seq_along(file_list)) {
  
 # i <- 1 #71
  
  rname <- file_list[i]
  
  ### Read-in raster
  wtw_raster <- rast(file_list[i])
  
  ### Get raster stats
  if (!is.factor(wtw_raster)) {
    ## df
    wtw_raster_df <- terra::as.data.frame(wtw_raster, na.rm=TRUE)
    ## number of unique value
    u_values <- nrow(unique(wtw_raster_df)) %>% as.numeric()
    
    ## max raster value
    max_value <- max(wtw_raster_df) %>% as.numeric() # <- CAN NOT GET MAX ON CATEGORICAL DATA
  }
  
  ## FILE ----------------------------------------------------------------------
  file_no_ext <- paste0(tools::file_path_sans_ext(basename(file_list[i])))
  file <-  paste0(file_no_ext, ".tif")
  
  #### message
  print(paste0(file, " (", i, "/", length(file_list), ")"))
  
  #Get Type
  type <- case_when(rname %in% themes_list ~ "theme",
                    rname %in% includes_list ~ "include",
                    rname %in% excludes_list ~ "exclude",
                    rname %in% weights_list ~ "weight")
        
  ## NAME ----------------------------------------------------------------------
  name <- names(wtw_raster)
    
  ## THEME ---------------------------------------------------------------------
  if (type == "theme") {
    theme <- gsub("outputs/final/sites_202505/Regional/Themes/", "", rname)
    theme <- gsub(paste0("/", file), "", theme)
     } else {
    theme <- ""
  }
  
  theme <- theme
    
  ## LEGEND --------------------------------------------------------------------
  legend <- if (u_values > 2) "continuous" else "manual"
  
  ## VALUES --------------------------------------------------------------------
    if (identical(u_values, 2) && identical(max_value, 1)) {
      values <- "0, 1" # IUCN, NSC, KBA, Includes 
    } else if (identical(u_values, 2)) {
      values <- paste0("0,", max_value) # ECCC: rare case if only 2 unique values
    } else if (identical(u_values, 1)) {
      values <- max_value # covers entire AOI
    } else {
      values <- "" # continuous data does not need values
    }
    
    ## COLOR ---------------------------------------------------------------------
    ## there is no "Color" column in species metadata 
    color <- case_when(
      # includes
      identical(type, "include") && identical(u_values, 2) ~ "#00000000, #7fbc41",
      #excludes 
      #identical(type, "exclude") && identical(u_values, 2) ~ "#00000000, #756bb1",
      
      #themes - Aquatic
      identical(theme, "Freshwater features") && identical(legend, "continuous")  ~  "Blues",   # theme
      #identical(theme, "Freshwater features") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(file_no_ext, "aq_lake_rarity_4_cover") && identical(u_values, 2) ~ "#00000000, #85c1e9",
      identical(file_no_ext, "aq_lake_rarity_5_cover") && identical(u_values, 2) ~ "#00000000, #2874a6",
      identical(file_no_ext, "aq_lakes_divens_4") && identical(u_values, 2) ~ "#00000000, #85c1e9",
      identical(file_no_ext, "aq_lakes_divens_5") && identical(u_values, 2) ~ "#00000000, #2874a6",
      identical(file_no_ext, "rivers_rarity_4") && identical(u_values, 2) ~ "#00000000, #85c1e9",
      identical(file_no_ext, "rivers_rarity_5") && identical(u_values, 2) ~ "#00000000, #2874a6",
      identical(file_no_ext, "sk_rivers_diversity_4") && identical(u_values, 2) ~ "#00000000, #85c1e9",
      identical(file_no_ext, "sk_rivers_diversity_5") && identical(u_values, 2) ~ "#00000000, #2874a6",
      
      # themes - fed species 
      identical(theme, "Federal species at risk critical habitat (ECCC)") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(theme, "Federal species at risk critical habitat (ECCC)") && identical(legend, "continuous")  ~  "PuBu",
      # themes - bc species
      identical(theme, "Species at risk (BC Conservation Data Centre)") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(theme, "Species at risk (BC Conservation Data Centre)") && identical(legend, "continuous")  ~  "PuRd",
      # themes - focal species BC
      identical(theme, "Regional focal species") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(theme, "Regional focal species") && identical(u_values, 3) ~ "#00000000, #00000000, #756bb1",
      identical(theme, "Regional focal species") && identical(legend, "continuous")  ~  "Purples",
      
      #themes -  
      #identical(theme, "Productivity") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(file_no_ext, "ndvi_mean_0.58_0.68") && identical(u_values, 2) ~ "#00000000, #d7bde2",
      identical(file_no_ext, "ndvi_mean_0.68") && identical(u_values, 2) ~ "#00000000, #7d3c98",
      identical(file_no_ext, "gdd_mean_1000") && identical(u_values, 2) ~ "#00000000, #7d3c98",
      identical(file_no_ext, "gdd_mean_800") && identical(u_values, 2) ~ "#00000000, #d2b4de",
      identical(file_no_ext, "macrorefugia_70threshold") && identical(u_values, 2) ~ "#00000000, #d7bde2",
      identical(file_no_ext, "microrefugia_70threshold") && identical(u_values, 2) ~ "#00000000, #7d3c98",
      identical(file_no_ext, "sk_pither_resistence_40threshold") && identical(u_values, 2) ~ "#00000000, #d7bde2",
      identical(file_no_ext, "sk_pither_resistence_90threshold") && identical(u_values, 2) ~ "#00000000, #7d3c98",
      
      identical(theme, "Productivity") && identical(legend, "continuous")  ~  "Purples",
      
      # themes - terrestrial 
      identical(file_no_ext, "iba") && identical(u_values, 2) ~ "#00000000, #7fbc41",
      identical(file_no_ext, "TAP_intact_watershed") && identical(legend, "continuous")  ~  "Greens",
      identical(file_no_ext, "TAP_bigtrees") && identical(legend, "continuous")  ~  "Greens",
      identical(file_no_ext, "TAP_ancient_forest") && identical(legend, "continuous")  ~  "Greens",
      #identical(file_no_ext, "ter_diversity_c") && identical(legend, "continuous")   ~  "magma",
      identical(file_no_ext, "ter_rarity_c") && identical(legend, "continuous")  ~  "Purples",
      identical(file_no_ext, "ter_div45_class") && identical(u_values, 2) ~ "#00000000, #7fbc41",
      identical(file_no_ext, "ter_rarity45_class") && identical(u_values, 2) ~ "#00000000, #7fbc41",
      
      # themes - Climate resilience
      identical(file_no_ext, "ancientforest_2_cover") && identical(legend, "continuous")  ~  "BuGn",
      identical(file_no_ext, "ancientforest_1_cover") && identical(legend, "continuous")  ~  "Greens",
      identical(file_no_ext, "intactwatershed_10_cover") && identical(legend, "continuous")  ~  "Greens",
      identical(file_no_ext, "intactwatershed_9_cover") && identical(legend, "continuous")  ~  "Greens",
      identical(file_no_ext, "intactwatershed_8_cover") && identical(legend, "continuous")  ~  "Greens",
      identical(file_no_ext, "bigtree_1_cover") && identical(legend, "continuous")  ~  "Greens",
      identical(file_no_ext, "bigtree_2_cover") && identical(legend, "continuous")  ~  "BuGn",
      identical(file_no_ext, "wilderness_cover") && identical(legend, "continuous")  ~  "Purples",
      
      # themes - terrestrial
      identical(file_no_ext, "jokulhaups") && identical(u_values, 2) ~ "#00000000, #7fbc41",
      identical(file_no_ext, "ter_diversity_4_cover") && identical(legend, "continuous")  ~  "Oranges",
      identical(file_no_ext, "ter_diversity_5_cover") && identical(legend, "continuous")  ~  "OrRd",
      identical(file_no_ext, "ter_rarity_4_cover") && identical(legend, "continuous")  ~  "YlOrRd",
      identical(file_no_ext, "ter_rarity_5_cover") && identical(legend, "continuous")  ~  "YlOrBr",
      
      # weight 
      identical(file_no_ext, "carbon_total") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "gdd_w") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "human_footprint_2022") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "iba_cover") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "macrorefugia_w") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "microrefugia_c") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "mining_OG_cover") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "ndvi_w") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "npp_w") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "resistance_w") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "rivers_rarity_mean_101c") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "rivers_diversity_101_c") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "roads_cover") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "sk_lake_div_ens_101c") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "sk_lake_rarity_prop_2025") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "ter_diversity_c") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "ter_rarity_continuous") && identical(legend, "continuous")  ~  "Spectral",
      identical(file_no_ext, "urban_cover") && identical(legend, "continuous")  ~  "Spectral",
      
  
      
      
      # identical(theme, "ECCC_CH") && identical(u_values, 2) ~  "#00000000, #756bb1",
      # identical(source, "ECCC_CH") && identical(u_values, 1) ~  "#756bb1", 
      # identical(source, "ECCC_CH") && identical(legend, "continuous")  ~  "Purples",
      # identical(source, "ECCC_SAR") && identical(u_values, 2) ~  "#00000000, #fb9a99",
      # identical(source, "ECCC_SAR") && identical(u_values, 1) ~  "#fb9a99", 
      # identical(source, "ECCC_SAR") && identical(legend, "continuous") ~  "Reds",
      # identical(source, "IUCN_AMPH") && identical(u_values, 2) ~  "#00000000, #a6cee3",
      # identical(source, "IUCN_REPT") && identical(u_values, 1) ~  "#b2df8a",
      # identical(source, "NSC_END") && identical(u_values, 2) ~  "#00000000, #4575b4",
      # identical(source, "NSC_END") && identical(u_values, 1) ~  "#4575b4",
    
      TRUE ~ "" 
    )
  
    ## LABELS --------------------------------------------------------------------
    ## there is no "Label" column in species metadata
    labels <- case_when(
      
      #includes 
      identical(file_no_ext, "cancelled_lands_0.2") && identical(u_values, 2) ~ "not conservation lands, conservation lands",
      identical(file_no_ext, "crown_lands0.2") && identical(u_values, 2) ~ "Not crown land, crown land",
      #identical(file_no_ext, "ecoregion_nass") && identical(u_values, 2) ~ "Not ecoregion, ecoregion",
      identical(file_no_ext, "not_cancelled_lands_0.2") && identical(u_values, 2) ~ "Not conservation lands, conservation lands",
      identical(file_no_ext, "protected_lands_0.2") && identical(u_values, 2) ~ "Not protected, Protected",
      identical(file_no_ext, "bulkley_morice_tsa") && identical(u_values, 2) ~ "outside TSA, within TSA",
      
      # excludes 
      identical(type, "exclude") && identical(u_values, 2) ~ "not altered, permanently altered",
     
      # weights
      identical(file_no_ext, "carbon_total") && identical(legend, "continuous")  ~  "",
      identical(file_no_ext, "gdd_mean") && identical(legend, "continuous")  ~  "growing degree days",
      identical(file_no_ext, "human_footprint_2022") && identical(legend, "continuous")  ~  "human footprint index",
      identical(file_no_ext, "iba_cover") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "macrorefugia_C") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "microrefugia_c") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "mining_OG_cover") && identical(legend, "continuous")  ~  "cover",
      identical(file_no_ext, "ndvi_mean") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "npp") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "res_mean") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "rivers_rarity_mean_101c") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "roads_cover") && identical(legend, "continuous")  ~  "cover",
      identical(file_no_ext, "sk_lake_div_ens_101c") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "sk_lake_rarity_prop_2025") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "ter_diversity_c") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "ter_rarity_continuous") && identical(legend, "continuous")  ~  "index",
      identical(file_no_ext, "urban_cover") && identical(legend, "continuous")  ~  "cover",
     
      
      # themes - Aquatic
      identical(theme, "Freshwater features") && identical(legend, "continuous") ~  "",
      identical(theme, "Freshwater features") && identical(u_values, 2) ~  "Absence, Presence",
      #identical(type, "include") && identical(u_values, 2) ~ "not included, included",
      
      identical(theme, "Productivity") && identical(legend, "continuous")   ~  "",
      identical(theme, "Productivity") && identical(u_values, 2) ~  "Absence, Presence",   
        
      identical(theme, "Federal species at risk critical habitat (ECCC)") && identical(u_values, 2) ~  "Non Habitat, Habitat",
      identical(theme, "Federal species at risk critical habitat (ECCC)") && identical(legend, "continuous")   ~  "",
      identical(theme, "Species at risk (BC Conservation Data Centre)") && identical(u_values, 2) ~  "Uncertain occurrence, Known occurrence",
      identical(theme, "Species at risk (BC Conservation Data Centre)") && identical(legend, "continuous")   ~  "",
      identical(theme, "Regional focal species") && identical(u_values, 2) ~  "Absence, Presence",
      identical(theme, "Regional focal species") && identical(u_values, 3) ~  "Nan, Absence, Presence",
      identical(theme, "Regional focal species") && identical(legend, "continuous")  ~  "Presence",
      
      # themes - habitat 
      identical(file_no_ext, "ancientforest_2_cover") && identical(legend, "continuous")   ~  "",
      identical(file_no_ext, "ancientforest_1_cover") && identical(legend, "continuous")   ~  "",
      identical(file_no_ext, "intactwatershed_10_cover") && identical(legend, "continuous")   ~  "",
      identical(file_no_ext, "intactwatershed_9_cover") && identical(legend, "continuous")   ~  "",
      identical(file_no_ext, "intactwatershed_8_cover") && identical(legend, "continuous")   ~  "",
      identical(file_no_ext, "bigtree_1_cover") && identical(legend, "continuous")   ~  "",
      identical(file_no_ext, "bigtree_2_cover") && identical(legend, "continuous")   ~  "",
      
      # themes - climate resilience
      identical(file_no_ext, "wilderness_cover") && identical(legend, "continuous")   ~  "",
      identical(theme, "Climate resilience") && identical(u_values, 2) ~  "Absence, Presence",
        
      # themes - terrestrial
      identical(file_no_ext, "jokulhaups") && identical(u_values, 2) ~ "Absence, Presence",
      identical(file_no_ext, "ter_diversity_4_cover") && identical(legend, "continuous")  ~  "",
      identical(file_no_ext, "ter_diversity_5_cover") && identical(legend, "continuous")  ~  "",
      identical(file_no_ext, "ter_rarity_4_cover") && identical(legend, "continuous")  ~  "",
      identical(file_no_ext, "ter_rarity_5_cover") && identical(legend, "continuous")  ~  "",
      identical(theme, "Enduring landscape features") && identical(u_values, 2) ~  "Absence, Presence",
      identical(theme, "Enduring landscape features") && identical(legend, "continuous")  ~  "",
      TRUE ~ ""
    )
    
    ## UNITS ---------------------------------------------------------------------
    ## there is no "Unit" column in species metadata
    unit <- case_when(
      type == "include" ~ "km2",
      type == "exclude" ~ "km2",
      theme == "Federal species at risk critical habitat (ECCC)" ~ "km2",
      theme == "Species at risk (BC Conservation Data Centre)" ~ "km2",
      theme == "Regional focal species" ~ "km2",
      theme == "Freshwater features" ~ "km2",
      theme == "Old growth forests" ~ "km2",
      theme == "Climate resilience" ~ "km2",
      theme == "Enduring landscape features" ~ "km2",
      theme == "Productivity" ~ "km2",
      file_no_ext == "iba" ~ "km2",
      file_no_ext %in%  c("ter_diversity_c","ter_rarity_c") ~ "index",
      file_no_ext %in%  c("TAP_intact_watershed", "TAP_bigtrees", "TAP_ancient_forest") ~ "km2",
      # weights 
      file_no_ext == "carbon_total" ~ "index", 
      file_no_ext == "gdd_w" ~ "index", 
      file_no_ext == "human_footprint_2022" ~ "index",
      file_no_ext %in%  c("iba_cover","urban_cover","roads_cover") ~ "percentage",
      file_no_ext == "mining_OG_cover" ~ "percentage",
      file_no_ext %in%  c("macrorefugia_w", "microrefugia_c") ~ "index",
      file_no_ext == "ndvi_w" ~ "index", 
      file_no_ext == "npp_w" ~ "kgC/m2/yr", # check this!
      file_no_ext =="resistance_w" ~ "index",
      file_no_ext == "rivers_rarity_mean_101c" ~ "index",
      file_no_ext == "sk_lake_div_ens_101c" ~ "index",
      file_no_ext == "sk_lake_rarity_prop_2025" ~ "index",
      file_no_ext == "ter_diversity_c" ~ "km2",
      file_no_ext == "ter_rarity_continuous" ~ "index",
      TRUE ~ ""
    )   
    
    ## PROVENANCE ----------------------------------------------------------------
    provenance <- "regional"
    
    ## ORDER ---------------------------------------------------------------------
    order <- "" # manual assignment in csv
    
    ## VISIBLE -------------------------------------------------------------------
    visible <- if (startsWith(file_no_ext, "I_NAT")) "TRUE" else "FALSE" 
    
    ## HIDDEN --------------------------------------------------------------------
    hidden <- "FALSE"
    
    ## DOWNLOADABLE ------------------------------------------------------------
    downloadable <- if (startsWith(file_no_ext, "T_NAT_NSC")) "FALSE" else "TRUE"     
    
    ## GOAL ----------------------------------------------------------------------
    ## only set goals for themes
    if (identical(type, "theme")) {
      ## only set Rodrigues goals on species data
      # species_sources <- c(
      #   "ECCC_CH", "ECCC_SAR", 
      #   "IUCN_AMPH", "IUCN_BIRD", "IUCN_MAMM", "IUCN_REPT",
      #   "NSC_END", "NSC_SAR", "NSC_SPP"
      # )
      # # if (source %in% species_sources) {
         goal <- 0.2 #wtw_meta_row$Goal # species
      # } else {
      #   goal <- "0.2" # forest, wetland, rivers, lakes, shoreline
      # }
    } else {
      goal <- "" # weights, includes, excludes
    }    
    
    ## Build new national row ----
    new_row <- c(
      type, theme, file, name, legend, 
      values, color, labels, unit, provenance, 
      order, visible, hidden, downloadable, goal
    )
    
  # } else {
  #   
  #   ## Build new regional row ----
  #   new_row <- c(
  #     "", "", file, "", "", 
  #     "", "", "", "", "regional", 
  #     "", "", "", "" , "0.2"
  #   )
  # }
  
  ## Append to DF
  df <- structure(rbind(df, new_row), .Names = names(df))
  
} 

my_order <- c('Climate resilience','Enduring landscape features','Freshwater features',"Old growth forests",
"Productivity",'Federal species at risk critical habitat (ECCC)','Species at risk (BC Conservation Data Centre)','Regional focal species', "")



dff <-  df  %>% arrange(match(Theme, my_order))


file_order <-  c("microrefugia_c.tif" , "macrorefugia_w.tif" ,"resistance_w.tif", 
                 "carbon_total.tif", "npp_w.tif" ,   "ndvi_w.tif"   ,  "gdd_w.tif", 
                 "ter_diversity_c.tif",  "ter_rarity_continuous.tif" ,
                 "sk_lake_div_ens_101c.tif" , "sk_lake_rarity_prop_2025.tif",
                 "rivers_diversity_101_c.tif", "rivers_rarity_mean_101c.tif" ,
                 "iba_cover.tif" , "human_footprint_2022.tif", "mining_OG_cover.tif", 
                 "roads_cover.tif", "urban_cover.tif")

dff_w <- dff |>  filter(Type == "weight") %>% arrange(match(File, file_order))
      
dff <- dff |> filter(Type != "weight") %>% 
  rbind(dff_w) # re order weights

#Seems to be missing river diversity. 





# Write to csv ----
write.csv(
  dff,
  fs::path(project_folder, "WTW", "metadata", "_metadata.csv"),
  row.names = FALSE
)

## review the metadata table and update as needed. 

# need to reorder the theme groups 
# ensure species is FEDS, BC, other species 

# re order weights

#New order of weights: climate refugia, connectivity, carbon, npp, ndvi, gdd, 
#ter div and rare, lake div and rare, river div and rare, iba, human footprint, 
#mining, roads. Seems to be missing river diversity. Grouping them together with similar types.





# 2.0 Set up -------------------------------------------------------------------

## Set path where a QC'd metadata.csv version is located
meta_path <- fs::path(project_folder, "WTW", "metadata")

PRJ_PATH  <- fs::path("outputs", "final", "sites_202505") # <--- CHANGE TO YOUR ROOT PROJECT FOLDER
META_NAME <- "_metadata.csv" 

AUTHOR <- "Gen Perkins" 
EMAIL <- "gperkins@ninoxconsulting.ca" 
GROUPS <- "private" 

meta_path <- file.path(PRJ_PATH, paste0("WTW/metadata/", META_NAME)) 
tiffs_path <- file.path(PRJ_PATH,"raw_tiffs")
pu_path <- file.path(PRJ_PATH,"PU/PU.tif")

# shift files to tiffs folder - currently manual transfer

list.files(tiffs_path)
allfiles<- list.files(fs::path(project_folder, "Regional"), recursive = TRUE)
# copy all files to tiff folder
for (i in 1:length(allfiles)){
  #i <- 1
  file.copy(fs::path(project_folder, "Regional", allfiles[i]), fs::path(tiffs_path , basename(allfiles[i])))
}


# 3.0 Import meta data and PUs -------------------------------------------------

## Import formatted csv (metadata) as tibble 
metadata <- tibble::as_tibble(
  utils::read.table(
    meta_path, stringsAsFactors = FALSE, sep = ",", header = TRUE,
    comment.char = "", quote="\""
  )
)

## Assort by order column - optional
metadata <- dplyr::arrange(metadata, Order) 

## Validate metadata
assertthat::assert_that(
  all(metadata$Type %in% c("theme", "include", "weight", "exclude")),
  all(file.exists(file.path(tiffs_path, metadata$File)))
)

## Import study area (planning units) raster
pu <- terra::rast(pu_path)


# 3.1 Import rasters -----------------------------------------------------------

## Import theme, weight, include and exclude rasters as a list of SpatRasters 
## objects. If raster variable does not compare to study area, re-project raster 
## variable so it aligns to the study area.
raster_data <- lapply(file.path(tiffs_path, metadata$File), function(x) {
  #x <- file.path(tiffs_path, metadata$File)[1]
  raster_x <- terra::rast(x)
  names(raster_x) <- tools::file_path_sans_ext(basename(x)) # file name
  if (terra::compareGeom(pu, raster_x, stopOnError=FALSE)) {
    raster_x
  } else {
    print(paste0(names(raster_x), ": can not stack"))
    print(paste0("... aligning to ", names(pu)))
    terra::project(raster_x, y = pu, method = "near")
  }
}) 

## Convert list to a combined SpatRaster
raster_data <- do.call(c, raster_data)

# 4.0 Pre-processing -----------------------------------------------------------

## Prepare theme inputs ----
theme_data <- raster_data[[which(metadata$Type == "theme")]]
names(theme_data) <- gsub(".", "_", names(theme_data), fixed = TRUE)
theme_names <- metadata$Name[metadata$Type == "theme"]
theme_groups <- metadata$Theme[metadata$Type == "theme"]
theme_colors <- metadata$Color[metadata$Type == "theme"]
theme_units <- metadata$Unit[metadata$Type == "theme"]
theme_visible <- metadata$Visible[metadata$Type == "theme"]
theme_provenance <- metadata$Provenance[metadata$Type == "theme"]
theme_hidden <- metadata$Hidden[metadata$Type == "theme"]
theme_legend <- metadata$Legend[metadata$Type == "theme"]
theme_labels <- metadata$Labels[metadata$Type == "theme"]
theme_values <- metadata$Values[metadata$Type == "theme"]
theme_goals <- metadata$Goal[metadata$Type == "theme"]
theme_downloadble <- metadata$Downloadable[metadata$Type == "theme"]

## Prepare weight inputs (if there are any) ----
if ("weight" %in% unique(metadata$Type)) {
  weight_data <- raster_data[[which(metadata$Type == "weight")]]
  weight_data <- terra::clamp(weight_data, lower = 0) ## might need to check this 
  weight_names <- metadata$Name[metadata$Type == "weight"]
  weight_colors <- metadata$Color[metadata$Type == "weight"]
  weight_units <- metadata$Unit[metadata$Type == "weight"]
  weight_visible <- metadata$Visible[metadata$Type == "weight"]
  weight_hidden <- metadata$Hidden[metadata$Type == "weight"]
  weight_provenance <- metadata$Provenance[metadata$Type == "weight"]
  weight_legend <- metadata$Legend[metadata$Type == "weight"]
  weight_labels <- metadata$Labels[metadata$Type == "weight"]
  weight_values <- metadata$Values[metadata$Type == "weight"]
  weight_downloadble <- metadata$Downloadable[metadata$Type == "weight"]
} else {
  weight_data <- c() # no weights in project
}

## Prepare include inputs (if there are any) ----
if ("include" %in% unique(metadata$Type)) {
  include_data <- raster_data[[which(metadata$Type == "include")]]
  include_data <- terra::classify(include_data, matrix(c(-Inf,0.5,0, 0.5,Inf,1), ncol = 3, byrow = TRUE))
  include_names <- metadata$Name[metadata$Type == "include"]
  include_colors <- metadata$Color[metadata$Type == "include"]
  include_units <- metadata$Unit[metadata$Type == "include"]
  include_visible <- metadata$Visible[metadata$Type == "include"]
  include_provenance <- metadata$Provenance[metadata$Type == "include"]
  include_legend <- metadata$Legend[metadata$Type == "include"]
  include_labels <- metadata$Labels[metadata$Type == "include"]
  include_hidden <- metadata$Hidden[metadata$Type == "include"]
  include_downloadble <- metadata$Downloadable[metadata$Type == "include"]
} else {
  include_data <- c() # no includes in project
}

## Prepare exclude inputs (if there are any) ----
if ("exclude" %in% unique(metadata$Type)) {
  exclude_data <- raster_data[[which(metadata$Type == "exclude")]]
  exclude_data <- terra::classify(exclude_data, matrix(c(-Inf,0.5,0, 0.5,Inf,1), ncol = 3, byrow = TRUE))
  exclude_names <- metadata$Name[metadata$Type == "exclude"]
  exclude_colors <- metadata$Color[metadata$Type == "exclude"]
  exclude_units <- metadata$Unit[metadata$Type == "exclude"]
  exclude_visible <- metadata$Visible[metadata$Type == "exclude"]
  exclude_provenance <- metadata$Provenance[metadata$Type == "exclude"]
  exclude_legend <- metadata$Legend[metadata$Type == "exclude"]
  exclude_labels <- metadata$Labels[metadata$Type == "exclude"]
  exclude_hidden <- metadata$Hidden[metadata$Type == "exclude"]
  exclude_downloadble <- metadata$Downloadable[metadata$Type == "exclude"]
} else {
  exclude_data <- c() # no excludes in project
}


# 5.0 Build wheretowork objects ------------------------------------------------

# Requires wheretowork package (version >= 1.2.3)

## Create dataset ----
dataset <- wheretowork::new_dataset_from_auto(c(theme_data, weight_data, include_data))

## Create themes (must have) ----
### loop over unique theme groups (ex. Endemic Species, Species at Risk, etc.)
themes <- lapply(seq_along(unique(theme_groups)), function(i) {
  
  # start test lin
  #i <- 4
  # end test line 
  
  #### store temp variables associated with group (i)
  curr_theme_groups <- unique(theme_groups)[i]
  curr_theme_data <- theme_data[[which(theme_groups == curr_theme_groups)]]
  curr_theme_data_names <- names(curr_theme_data)
  curr_theme_names <- theme_names[theme_groups == curr_theme_groups]
  curr_theme_colors <- theme_colors[theme_groups == curr_theme_groups]
  curr_theme_labels <- theme_labels[theme_groups == curr_theme_groups]
  curr_theme_units <- theme_units[theme_groups == curr_theme_groups]
  curr_theme_visible <- theme_visible[theme_groups == curr_theme_groups]
  curr_theme_hidden <- theme_hidden[theme_groups == curr_theme_groups]
  curr_theme_provenance <- theme_provenance[theme_groups == curr_theme_groups] 
  curr_theme_legend <- theme_legend[theme_groups == curr_theme_groups]
  curr_theme_values <- theme_values[theme_groups == curr_theme_groups]
  curr_theme_goals <- theme_goals[theme_groups == curr_theme_groups]
  curr_theme_downloadable <- theme_downloadble[theme_groups == curr_theme_groups]
  
  #### create list of features (j) associated with group
  curr_features <- lapply(seq_along(curr_theme_names), function(j) {
  #  j = 10
    #print(curr_theme_legend[j])
    
    #### create variable (if manual legend)
    if (identical(curr_theme_legend[j], "manual")) {
      v <- wheretowork::new_variable(
        dataset = dataset,
        index = curr_theme_data_names[j],
        units = curr_theme_units[j],
        total = terra::global(curr_theme_data[[j]], fun ="sum", na.rm = TRUE)$sum,
        legend = wheretowork::new_manual_legend(
          values = c(as.numeric(trimws(unlist(strsplit(curr_theme_values[j], ","))))),
          colors = c(trimws(unlist(strsplit(curr_theme_colors[j], ",")))),
          labels = c(trimws(unlist(strsplit(curr_theme_labels[j], ","))))
        ),
        provenance = wheretowork::new_provenance_from_source(curr_theme_provenance[j])
      )
      
      #### create variable (if continuous legend)    
    } else if (identical(curr_theme_legend[j], "continuous")) {
      v <-  wheretowork::new_variable_from_auto(
        dataset = dataset,
        index = curr_theme_data_names[j],
        units = curr_theme_units[j],
        type = "continuous",
        colors = curr_theme_colors[j],
        provenance = curr_theme_provenance[j],
        labels = "missing",
        hidden = curr_theme_hidden[j]
      )
      
      #### create variable (if null legend)   
    } else if (identical(curr_theme_legend[j], "null")) {
      v <- wheretowork::new_variable(
        dataset = dataset,
        index = curr_theme_data_names[j],
        units = " ",
        total = terra::global(curr_theme_data[[j]], fun ="sum", na.rm = TRUE)$sum,
        legend = wheretowork::new_null_legend(),
        provenance = wheretowork::new_provenance_from_source("missing")
      )
    }
    
    #### create new feature
    wheretowork::new_feature(
      name = curr_theme_names[j],
      goal = curr_theme_goals[j],
      current = 0,
      limit_goal = 0,
      visible = curr_theme_visible[j],
      hidden = curr_theme_hidden[j],
      variable = v,
      downloadable = curr_theme_downloadable[j]
    )    
  })
  
  #### create theme from list of features
  curr_theme <- wheretowork::new_theme(curr_theme_groups,curr_features)
  
  #### return theme
  curr_theme
})

## Create weights (if there are any) ----
if (!is.null(weight_data)) {
  weights <- lapply(seq_len(terra::nlyr(weight_data)), function(i) {
    
    #i <- 5
    
    #### prepare variable (if manual legend)
    if (identical(weight_legend[i], "manual")) {
      v <- wheretowork::new_variable_from_auto(
        dataset = dataset,
        index = names(weight_data)[i],
        units = weight_units[i],
        type = "manual",
        colors = trimws(unlist(strsplit(weight_colors[i], ","))),
        provenance = weight_provenance[i],
        labels = trimws(unlist(strsplit(weight_labels[i], ",")))
      )
      
      #### prepare variable (if null legend)    
    } else if (identical(weight_legend[i], "null")) {
      v <- wheretowork::new_variable(
        dataset = dataset,
        index = names(weight_data)[i],
        units = " ",
        total = terra::global(weight_data[[i]], fun = "sum", na.rm=TRUE)$sum,
        legend = new_null_legend(),
        provenance = wheretowork::new_provenance_from_source("missing")
      )
      
      ### prepare variable (if continuous legend)    
    } else if (identical(weight_legend[i], "continuous")) { 
      v <- wheretowork::new_variable_from_auto(
        dataset = dataset,
        index = names(weight_data)[i],
        units = weight_units[i],
        type = "continuous",
        colors = weight_colors[i],
        provenance = weight_provenance[i]
      )
    }
    
    #### create weight
    wheretowork::new_weight(
      name = weight_names[i], variable = v, 
      visible = weight_visible[i], hidden = weight_hidden[i],
      downloadable = weight_downloadble[i]
    )
  })
}


## Create includes (if there are any) ----
if (!is.null(include_data)) {
  includes <- lapply(seq_len(terra::nlyr(include_data)), function(i) {
    
    #i <- 1
    
    ### build legend
    if (identical(include_legend[i], "null")) {
      legend <- wheretowork::new_null_legend()
    } else {
      legend <- wheretowork::new_manual_legend(
        values = c(0, 1),
        colors = trimws(unlist(strsplit(include_colors[i], ","))),
        labels = trimws(unlist(strsplit(include_labels[i], ",")))
      )
    }
    
    ### build include
    wheretowork::new_include(
      name = include_names[i],
      visible = include_visible[i],
      hidden = include_hidden[i],
      downloadable = include_downloadble[i],
      variable = wheretowork::new_variable(
        dataset = dataset,
        index = names(include_data)[i],
        units = include_units[i],
        total = terra::global(include_data[[i]], fun = "sum", na.rm = TRUE)$sum,
        legend = legend,
        provenance = wheretowork::new_provenance_from_source(include_provenance[i])
      )
    )
  })
}

## Create excludes (if there are any) ----
if (!is.null(exclude_data)){
  excludes <- lapply(seq_len(terra::nlyr(exclude_data)), function(i) {
    
   # i <- 1
    
    ### build legend
    if (identical(exclude_legend[i], "null")) {
      legend <- wheretowork::new_null_legend()
    } else {
      legend <- wheretowork::new_manual_legend(
        values = c(0, 1),
        colors = trimws(unlist(strsplit(exclude_colors[i], ","))),
        labels = trimws(unlist(strsplit(exclude_labels[i], ",")))
      )
    }
    
    ### build exclude
    wheretowork::new_exclude(
      name = exclude_names[i],
      visible = exclude_visible[i],
      hidden = exclude_hidden[i],
      downloadable = exclude_downloadble[i],
      variable = wheretowork::new_variable(
        dataset = dataset,
        index = names(exclude_data)[i],
        units = exclude_units[i],
        total = terra::global(exclude_data[[i]], fun = "sum", na.rm = TRUE)$sum,
        legend = legend,
        provenance = wheretowork::new_provenance_from_source(exclude_provenance[i])
      )
    )
  })
}

# 6.0  Export Where To Work objects --------------------------------------------

if (!is.null(weight_data)) {
  wtw_objects <- append(themes, weights) # Themes and Weights
} else{
  wtw_objects <- themes # Themes
}

if (!is.null(include_data)) {
  wtw_objects <- append(wtw_objects, includes) # Themes, Weights and Includes
} 

if (!is.null(exclude_data)) {
  wtw_objects <- append(wtw_objects, excludes) # Themes, Weights Includes and Excludes
} 

PRJ_NAME = "skeena"
PRJ_PATH = fs::path("outputs", "final", "sites_202505") 
PRJ_FILE_NAME = "skeena"

## Save project to disk ---- https://github.com/NCC-CNC/wheretowork/blob/12de775a50c68623c40f72d01b5c5706e82518a2/R/fct_write_project.R#L75
wheretowork::write_project(
  x = wtw_objects,
  dataset = dataset,
  name = PRJ_NAME, 
  path = file.path(PRJ_PATH, "WTW", paste0(PRJ_FILE_NAME, ".yaml")),
  spatial_path = file.path(PRJ_PATH, "WTW", paste0(PRJ_FILE_NAME, ".tif")),
  attribute_path = file.path(PRJ_PATH, "WTW", paste0(PRJ_FILE_NAME, "_attribute.csv.gz")), 
  boundary_path = file.path(PRJ_PATH, "WTW", paste0(PRJ_FILE_NAME, "_boundary.csv.gz")),
  mode = "advanced",
  user_groups = GROUPS,
  author_name = AUTHOR, 
  author_email = EMAIL 
)

