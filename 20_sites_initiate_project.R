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
project_folder <- fs::path("outputs", "final", "sites") # <--- CHANGE TO YOUR ROOT PROJECT FOLDER
PROJECT_TYPE <- "REGIONAL" 


# 3.0 Processing ----------------------------------------------------------------
# create folder structure

dir.create(fs::path(project_folder, "PU"), recursive = TRUE)
dir.create(file.path(project_folder, "scripts"), recursive = TRUE)
dir.create(file.path(project_folder, "Tiffs"), recursive = TRUE)
dir.create(file.path(project_folder, "WTW/metadata"), recursive = TRUE)

if(PROJECT_TYPE == "REGIONAL"){
  dir.create(file.path(project_folder, "Regional"), recursive = TRUE)
}

# 2.0 Set up AOI -------------------------------------------------------------------

final_data_dir <- fs::path("outputs","final")
rr <- rast(fs::path(final_data_dir, "sk_rast_template.tif"))
rr<- aggregate(rr, fact = 10)
names(rr) <- "template"

# Convert all cell values to 0
rr[rr > 0] <- 0
writeRaster(rr, file.path(project_folder, "PU", "PU0.tif"), datatype = "INT1U", overwrite = TRUE)

# make a version where values are all zero
rr1 <- rr
rr1[rr1 == 0] <- 1

terra::writeRaster(rr1, file.path(project_folder, "PU","PU.tif"), datatype = "INT1U", overwrite = TRUE)




# 3.0 Set up metadata ------------------------------------------------------------

# Authors: Marc Edwards
#
# Date: April 24th, 2023
#
# Description: Initialize the meta data table using all tifs and .gdb feature 
# classes in the root folder. Attempts to guess the metadata values where
# possible. User needs to QC and complete the metadata table manually before
# proceeding.
#
# Inputs:  1. project name for output csv
#          2. file paths to themes, includes, excludes and weights data
#             all input data should be placed in the correct folder and be in
#             .tif format for rasters, or as feature classes within a .gdb for
#             vectors. For theme data, all data for a given theme should be in
#             a subfolder where the subfolder name is the Theme name (e.g. 
#             Regional/Themes/Forest)
#          3. The areal units and area value for planning units 
#             (e.g. for a 10 km x 10 km grid we would use 'km2' and '10')
#
# Outputs: 1. Meta data csv
#
#===============================================================================

project_name <- "nb" # <--- SET PROJECT NAME HERE FOR OUT FILE
themes_dir <- fs::path(project_folder, "Regional", "Themes" )# <--- Themes data folder
includes_dir <- fs::path(project_folder, "Regional","Includes") # <--- Includes data folder
excludes_dir <- fs::path(project_folder, "Regional","Excludes") # <--- Excludes data folder
weights_dir <- fs::path(project_folder,"Regional","Weights") # <--- Weights data folder

pu_units <- "m2" # <--- SET DESIRED UNITS
pu_cell_area <- 1000 # <--- SET PLANNING UNIT AREA IN units


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



# Fill table -------------------------------------------------------------------

file_list <- c(themes_list, includes_list, excludes_list, weights_list)

## Build empty data.frame (template for metadata.csv) ----
df <- init_metadata()

# 5.0 Populate metadata --------------------------------------------------------
## Loop over each tiff file:
for (i in seq_along(file_list)) {
  
  #i <- 15
  
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
  
  #### get source
  #source <- NULL?
  
  #Get Type
  type <- case_when(rname %in% themes_list ~ "theme",
                    rname %in% includes_list ~ "include",
                    rname %in% excludes_list ~ "exclude",
                    rname %in% weights_list ~ "weight")
        
  ## TYPE ----------------------------------------------------------------------
  # type <- wtw_meta_row %>% 
  #     {if ("Type" %in% colnames(wtw_meta)) pull(., Type) else "theme"} 
  #   
  ## NAME ----------------------------------------------------------------------
  name <- names(wtw_raster)
    
  ## THEME ---------------------------------------------------------------------
  if (type == "theme") {
    theme <- gsub("outputs/final/sites/Regional/Themes/", "", rname)
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
      identical(theme, "aquatic") && identical(legend, "continuous")  ~  "Purples",
      identical(type, "include") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(type, "exclude") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(theme, "species_at_risk") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(file_no_ext, "iba") && identical(u_values, 2) ~ "#00000000, #756bb1",
      identical(file_no_ext, "TAP_intact_watershed") && identical(legend, "continuous")  ~  "Purples",
      identical(file_no_ext, "ter_diversity_c") && identical(legend, "continuous")  ~  "Purples",
      identical(file_no_ext, "ter_rarity_c") && identical(legend, "continuous")  ~  "Purples",
      identical(file_no_ext, "macrorefugia") && identical(legend, "continuous")  ~  "Purples",
      
      identical(file_no_ext, "npp") && identical(legend, "continuous")  ~  "Purples",
      
      # identical(theme, "ECCC_CH") && identical(u_values, 2) ~  "#00000000, #756bb1",
      # identical(source, "ECCC_CH") && identical(u_values, 1) ~  "#756bb1", 
      # identical(source, "ECCC_CH") && identical(legend, "continuous")  ~  "Purples",
      # identical(source, "ECCC_SAR") && identical(u_values, 2) ~  "#00000000, #fb9a99",
      # identical(source, "ECCC_SAR") && identical(u_values, 1) ~  "#fb9a99", 
      # identical(source, "ECCC_SAR") && identical(legend, "continuous") ~  "Reds",
      # identical(source, "IUCN_AMPH") && identical(u_values, 2) ~  "#00000000, #a6cee3",
      # identical(source, "IUCN_AMPH") && identical(u_values, 1) ~  "#a6cee3",
      # identical(source, "IUCN_BIRD") && identical(u_values, 2) ~  "#00000000, #ff7f00",
      # identical(source, "IUCN_BIRD") && identical(u_values, 1) ~  "#ff7f00",
      # identical(source, "IUCN_MAMM") && identical(u_values, 2) ~  "#00000000, #b15928",
      # identical(source, "IUCN_MAMM") && identical(u_values, 1) ~  "#b15928",
      # identical(source, "IUCN_REPT") && identical(u_values, 2) ~  "#00000000, #b2df8a",
      # identical(source, "IUCN_REPT") && identical(u_values, 1) ~  "#b2df8a",
      # identical(source, "NSC_END") && identical(u_values, 2) ~  "#00000000, #4575b4",
      # identical(source, "NSC_END") && identical(u_values, 1) ~  "#4575b4",
      # identical(source, "NSC_SAR") && identical(u_values, 2) ~  "#00000000, #d73027",
      # identical(source, "NSC_SAR") && identical(u_values, 1) ~  "#d73027",
      # identical(source, "NSC_SPP") && identical(u_values, 2) ~  "#00000000, #e6f598",
      # identical(source, "NSC_SPP") && identical(u_values, 1) ~  "#e6f598",
      TRUE ~ "" 
    )
    
  
  
  
    ## LABELS --------------------------------------------------------------------
    ## there is no "Label" column in species metadata
    labels <- case_when(
      identical(theme, "aquatic") && identical(legend, "continuous") ~  "",
      identical(type, "include") && identical(u_values, 2) ~ "not included, included",
      identical(type, "exclude") && identical(u_values, 2) ~ "low footprint, human footrint",
      identical(theme, "species_at_risk") && identical(u_values, 2) ~  "Non Habitat, Habitat",
      identical(file_no_ext, "iba") && identical(u_values, 2) ~ "Non Habitat, Habitat",
      identical(file_no_ext, "TAP_intact_watershed") && identical(legend, "continuous") ~  "",
      identical(file_no_ext, "ter_diversity_c") && identical(legend, "continuous")  ~  "",
      identical(file_no_ext, "ter_rarity_c") && identical(legend, "continuous")  ~  "",
      identical(file_no_ext, "macrorefugia") && identical(legend, "continuous")  ~  "",
      identical(file_no_ext, "npp") && identical(legend, "continuous")  ~  "",
      
      # identical(source, "ECCC_CH") && identical(u_values, 2) ~  "Non Habitat, Habitat",
      # identical(source, "ECCC_CH") && identical(u_values, 1) ~  "Habitat",
      # identical(source, "ECCC_CH") && identical(legend, "continuous") ~  "",
      # identical(source, "ECCC_SAR") && identical(u_values, 2) ~  "Non Range, Range",
      # identical(source, "ECCC_SAR") && identical(u_values, 1) ~  "Range",
      # identical(source, "ECCC_SAR") && identical(legend, "continuous") ~  "",
      # #identical(substring(source, 1, 4), "IUCN") && identical(u_values, 2) ~  "Non Habitat, Habitat",
      #identical(substring(source, 1, 4), "IUCN") && identical(values, 1) ~  "Habitat",
      #identical(substring(source, 1, 3), "NSC") && identical(u_values, 2) ~  "Non Occurrence, Occurrence",
      #identical(substring(source, 1, 3), "NSC") && identical(values, 1) ~  "Occurrence",
      TRUE ~ ""
    )
    
    ## UNITS ---------------------------------------------------------------------
    ## there is no "Unit" column in species metadata
    unit <- case_when(
      type == "include" ~ "km2",
      type == "exclude" ~ "km2",
      theme == "species_at_risk" ~ "km2",
      file_no_ext == "iba" ~ "km2",
      file_no_ext ==  "TAP_intact_watershed" ~ "km2",
      file_no_ext %in%  c("ter_diversity_c","ter_rarity_c", "macrorefugia") ~ "km2",
      file_no_ext == "npp" ~ "kgC/m2/yr", # check this!
      # (identical(source, "ECCC_CH")) ~ "ha",
      # (identical(source, "ECCC_SAR")) ~  "ha",
      # identical(source, "IUCN_AMPH") ~  "km2",
      # identical(source, "IUCN_BIRD") ~  "km2",
      # identical(source, "IUCN_MAMM") ~  "km2",
      # identical(source, "IUCN_REPT") ~  "km2",
      # identical(source, "NSC_END") ~  "km2",
      # identical(source, "NSC_SAR") ~  "km2",
      # identical(source, "NSC_SPP") ~  "km2",
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

# populate unique ID
#df$unique_id <- paste0("ID_", seq(1:nrow(df)))

# Write to csv ----
write.csv(
  df,
  fs::path(project_folder, "WTW", "metadata", "_metadata.csv"),
  row.names = FALSE
)







# 
# 
# 
# # Add Regional specific columns
# df$unique_id <- as.character()
# df$threshold <- as.character()
# df$source <- as.character()
# 
# for(x in c(themes_list, includes_list, excludes_list, weights_list)){
#   
#   #x <- c(themes_list, includes_list, excludes_list, weights_list)[1]
#   
#   # Get Type
#   type <- case_when(x %in% themes_list ~ "theme",
#                     x %in% includes_list ~ "include",
#                     x %in% excludes_list ~ "exclude",
#                     x %in% weights_list ~ "weight")
#   
#   # Get Theme
#   theme <- case_when(x %in% themes_list ~ basename(get_parent_dir(x)),
#                      .default = "")
#   
#   layer_name <- tools::file_path_sans_ext(basename(x))
#   
#   # Get final file name
#   file <- case_when(x %in% themes_list ~ paste0("T_", layer_name, ".tif"),
#                     x %in% includes_list ~ paste0("I_", layer_name, ".tif"),
#                     x %in% excludes_list ~ paste0("E_", layer_name, ".tif"),
#                     x %in% weights_list ~ paste0("W_", layer_name, ".tif"))
#   
#   # Guess legend
#   legend <- case_when(x %in% themes_list ~ "continuous", # usually continuous
#                       x %in% includes_list ~ "manual", # usually binary data
#                       x %in% excludes_list ~ "manual", # usually binary data
#                       x %in% weights_list ~ "continuous", # usually continuous
#                       .default = "continuous")
#   
#   # Guess values
#   values <- case_when(legend == "manual" ~ paste0("0, ", pu_cell_area),
#                       .default = "")
#   
#   # Guess colour
#   color <- case_when(legend == "manual" ~ "#00000000, #fb9a99",
#                      .default = "")
#   
#   # Guess units
#   unit <- case_when(legend == "manual" ~ pu_units,
#                     .default = "")
#   
#   # Get name
#   name <- gsub("_", " ", layer_name)
#   
#   # Set theme goals
#   goal <- case_when(type == "theme" ~ "0.2",
#                     .default = "")
#   
#   provenance <- "regional"
#   order <- ""
#   labels <- ""
#   threshold <- ""
#   visible <- FALSE
#   hidden <- FALSE
#   source <- x
#   id <- ""
#   
#   ## DOWNLOADABLE ------------------------------------------------------------
#  # file_no_ext <- paste0(tools::file_path_sans_ext(basename(file_list[i])))
#   downloadable <- "TRUE"  
#   
#   
#   # add row
#   new_row <- c(type, theme, file, name, legend, values, color, labels, unit, 
#                provenance, order, visible, hidden, goal, id, threshold, 
#                downloadable, source)
#   
#   df <- structure(rbind(df, new_row), .Names = names(df))
# }
# 


# 
# 
# 
# # populate continuous colors, same color for each theme
# themes <- unique(df$Theme)
# theme_colours <- sample(c("Greens", "Reds", "viridis", "YlOrBr", "Blues", "mako", "PuBuGn", "rocket"), length(themes), replace = TRUE)
# for(i in 1:nrow(df)){
#   if(df$Legend[i] == "continuous"){
#     df$Color[i] <- theme_colours[which(themes == df$Theme[i])]
#   }
# }
# 
# # populate unique ID
# df$unique_id <- paste0("ID_", seq(1:nrow(df)))
# 
# # save
# write.csv(df, fs::path(project_folder, "WTW", "metadata", "_metadata.csv"), row.names = FALSE)


## review the metadata table and update as needed. 



# 2.0 Set up -------------------------------------------------------------------

## Set path where a QC'd metadata.csv version is located
#PRJ_PATH <- "C:/Data/PRZ/WTW/CONSTECH/SW_ONTARIO_V3" # <--- CHANGE TO YOUR LOCAL WTW PROJECT FOLDER
#META_NAME <- "_metadata.csv" # <--- CHANGE TO NAME OF YOUR metadata.csv. NEED TO ADD ".csv" extension
meta_path <- fs::path(project_folder, "WTW", "metadata")

PRJ_PATH  <- fs::path("outputs", "final", "sites") # <--- CHANGE TO YOUR ROOT PROJECT FOLDER
META_NAME <- "_metadata.csv" # <--- CHANGE TO NAME OF YOUR metadata.csv. NEED TO ADD ".csv" extension


# ## Set output variables for WTW file names
# ### What regional operating or business unit?
# OU <- "IT"  # <--- REG_BC, REG_AB, REG SK, REG MB, REG ON, REG QC, REG AT, IT, CPP, SOS, MD etc.
# ### Planning unit scale
# SCALE <- "1km2" # <--- Set scale in ha or km2
# ### Unique name that describes the WTW project
# NAME <- "South Western Ontario Example" # <--- give a unique name
# 
# PRJ_NAME <- paste0(OU, ": ", NAME, ", ", SCALE)
# PRJ_FILE_NAME <- gsub(" ", "_", gsub("[[:punct:]]", "", PRJ_NAME))


AUTHOR <- "Gen Perkins" # <----- your name
EMAIL <- "gperkins@ninoxconsulting.ca" # <----- your email
GROUPS <- "private" # <---- options: public or private  

meta_path <- file.path(PRJ_PATH, paste0("WTW/metadata/", META_NAME)) 
tiffs_path <- file.path(PRJ_PATH,"Tiffs")
pu_path <- file.path(PRJ_PATH,"PU/PU.tif")


# shift files to tiffs folder - currently manual transfer




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
  weight_data <- terra::clamp(weight_data, lower = 0)
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
dataset <- wheretowork::new_dataset_from_auto(
  c(theme_data, weight_data, include_data, exclude_data)
)

## Create themes (must have) ----
### loop over unique theme groups (ex. Endemic Species, Species at Risk, etc.)
themes <- lapply(seq_along(unique(theme_groups)), function(i) {
  
  # start test lin
  #i <- 1
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
   # j = 3
    
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
    
   # i <- 1
    
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
    
   # i <- 1
    
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
PRJ_PATH = fs::path("outputs", "final", "sites")
PRJ_FILE_NAME = "skeena"

## Save project to disk ---- 
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


# 7.0 Clear R environment ------------------------------------------------------ 

## End timer
end_time <- Sys.time()
end_time - start_time

## Comment these lines below to keep all the objects in the R session
rm(list=ls())
gc()
