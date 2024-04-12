#03. Rarity

library(terra)
library(sf)
library(ggplot2)
library(dplyr)

## actual size 
#srast = terra::rast(file.path("inputs", "sk_lf_3005.tif"))

#srast = srastrc = terra::rast(file.path("outputs", "sk_lf_rockclass.tif"))
srast = terra::rast(file.path("outputs", "sk_lf_rockclassdet.tif"))


srast

# read in the rare csv file 
# rare <- read.csv(file.path("inputs","landscape_facets_summary.csv")) %>%
#  dplyr::select(skeena_lfacet_3005,count, rare_under_10 )

# read in the rare csv file 
#rare <- read.csv(file.path("outputs","landscape_facet_summary_rc.csv")) %>%
#  dplyr::select(layer1,count, rare_under_10 )

# read in the rare csv file 
rare <- read.csv(file.path("outputs","landscape_facet_summary_rcd.csv")) %>%
  dplyr::select(layer1,count, rare_under_10 )

#cutoff is count of 46127

rare10 <- rare %>%
  filter(rare_under_10 == "y")
  
hist(rare$count)
hist(rare10$count)
quantile(rare10$count)
 
#quantile(rare10$count)
#0%      25%      50%      75%     100% 
#1.00   326.00  2807.00 12374.75 46127.00 

# for rc (n = 5 option) 
#0%  25%  50%  75% 100% 
#1   28  170  438  953 


# for rcdet (n = 5 option) 
#0%   25%   50%   75%  100% 
#1.0  12.5  62.0 132.5 268.0 
 




# generate rare classes
# 
# rareq <- rare %>% 
#  dplyr::mutate(rare_id = case_when(
#     count < 326 ~ 1,
#     count > 326 & count < 2807 ~ 2,
#     count > 2807 & count < 12374 ~ 3,
#     count > 12374 & count < 46127~ 4,
#     .default = as.numeric(99)
#   ))
  
# # rock class (n = 5) 
# rareq <- rare %>% 
#   dplyr::mutate(rare_id = case_when(
#     count < 28 ~ 1,
#     count > 28 & count < 170 ~ 2,
#     count > 170 & count < 438 ~ 3,
#     count > 438 & count < 953 ~ 4,
#     .default = as.numeric(99)
#   ))

# rock class (n = 19) 
rareq <- rare %>% 
  dplyr::mutate(rare_id = case_when(
    count < 12 ~ 1,
    count > 12 & count < 62 ~ 2,
    count > 62 & count < 132 ~ 3,
    count > 132 & count < 268 ~ 4,
    .default = as.numeric(99)
  ))




# # try reclass? Not working 
# 
# raremx <- rareq %>%
#   select(skeena_lfacet_3005, rare_id) %>%
#   rename(from = skeena_lfacet_3005 ) %>%
#   rowwise() %>%
#   mutate(to = as.integer(from + 1), 
#          becomes = rare_id) %>%
#   select(-rare_id)
# 
# ra <- data.matrix(raremx, rownames.force = NA)
# # m = as.numeric(raremx[1,])

## from-to-becomes
# classify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.

## from-to-becomes
# classify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.

# 
# class1 <-rareq  %>% filter(rare_id == 1) %>% pull(skeena_lfacet_3005)
# class2 <-rareq  %>% filter(rare_id == 2) %>% pull(skeena_lfacet_3005)
# class3 <-rareq  %>% filter(rare_id == 3) %>% pull(skeena_lfacet_3005)
# class4 <-rareq  %>% filter(rare_id == 4) %>% pull(skeena_lfacet_3005)


# rock class

class1 <-rareq  %>% filter(rare_id == 1) %>% pull(layer1)
class2 <-rareq  %>% filter(rare_id == 2) %>% pull(layer1)
class3 <-rareq  %>% filter(rare_id == 3) %>% pull(layer1)
class4 <-rareq  %>% filter(rare_id == 4) %>% pull(layer1)

rr <- srast

uvr <- as.vector(unique(values(srast)))

# check if needs class 1: 

if(any(unique(uvr %in% class1)) == TRUE){
  print("reclass values")
  
  for(i in class1){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 1)
    
  }
  
}else {
  print("no class1 reclass needed")
}


# check if needs class 2: 

if(any(unique(uvr %in% class2)) == TRUE){
  print("reclass values")
  
  for(i in class2){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 2)
    
  }
  
}else {
  print("no class2 reclass needed")
}


# check if needs class 3: 

if(any(unique(uvr %in% class3)) == TRUE){
  print("reclass values")
  
  for(i in class3){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 3)
    
  }
  
}else {
  print("no class3 reclass needed")
}



# check if needs class 4

if(any(unique(uvr %in% class4)) == TRUE){
  print("reclass values")
  
  for(i in class4){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 4)
  }
}else {
  print("no class4 reclass needed")
}


reclass <- as.vector(unique(values(rr)))

reclass

## from-to-becomes
# classify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(1, 1, 1,
       2, 2, 2,
       3, 3, 3,
       4, 4, 4,
       5, 1000000, 999)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(rr, rclmat, include.lowest=TRUE)


# to do 
# might need a reclass after if less than 4 = NA
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(rr, rclmat, include.lowest=TRUE)


#terra::writeRaster(rc1, "sk_rarity_class.tif")

#terra::writeRaster(rc1,file.path("outputs", "sk_rarity_class.tif"))
#terra::writeRaster(rc1,file.path("outputs", "sk_rarity_class_rc.tif"))
terra::writeRaster(rc1,file.path("outputs", "sk_rarity_class_rcd.tif"))