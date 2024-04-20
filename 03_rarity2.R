#03. Rarity

library(terra)
library(sf)
library(ggplot2)
library(dplyr)

## actual size 
#srast = terra::rast(file.path("inputs", "sk_lf_3005.tif"))

#srast = srastrc = terra::rast(file.path("outputs", "sk_lf_rockclass.tif"))
srast = terra::rast(file.path("outputs", "sk_lf_rockclassdet.tif"))

# read in the rare csv file 
rare <- read.csv(file.path("outputs","landscape_facet_summary_rcd.csv")) %>%
  mutate(rare_id = rarity_class)

#cutoff is count of 46127

hist(rare$count)
hist(rare$rarity_class)

 
# rock class

class1 <-rare  %>% filter(rare_id == 1) %>% pull(layer1)
class2 <-rare  %>% filter(rare_id == 2) %>% pull(layer1)
class3 <-rare  %>% filter(rare_id == 3) %>% pull(layer1)
class4 <-rare  %>% filter(rare_id == 4) %>% pull(layer1)
class5 <-rare  %>% filter(rare_id == 5) %>% pull(layer1)

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


# check if needs class 5

if(any(unique(uvr %in% class5)) == TRUE){
  print("reclass values")
  
  for(i in class5){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 5)
  }
}else {
  print("no class5 reclass needed")
}

# up to here
reclass <- as.vector(unique(values(rr)))

reclass

## from-to-becomes
# classify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(1, 1, 1,
       2, 2, 2,
       3, 3, 3,
       4, 4, 4,
       5, 5, 5,
       6,1000000, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(rr, rclmat, include.lowest=TRUE)


# 
# # to do 
# # might need a reclass after if less than 4 = NA
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# rc1 <- classify(rr, rclmat, include.lowest=TRUE)
# 

#terra::writeRaster(rc1, "sk_rarity_class.tif")

#terra::writeRaster(rc1,file.path("outputs", "sk_rarity_class.tif"))
#terra::writeRaster(rc1,file.path("outputs", "sk_rarity_class_rc.tif"))
terra::writeRaster(rc1,file.path("outputs", "sk_rarity_class_rcd.tif"), overwrite = TRUE)




## Calculoate concentration 

## reclass the valyers to a conccentration 

con_rare <- rast(file.path("outputs", "facet_rcd_rarity_101c.tif"))

hist(con_rare$facet_rcd_rarity_101c , breaks = 40)

## WAITING ON INPUT FROM PAULA 

unique(values(con_rare))

## from-to-becomes
# classify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(0, 1, 1,
       1, 2, 2,
       2, 3, 3,
       3, 4, 4,
       4, 6, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(con_rare, rclmat, include.lowest=TRUE)

writeRaster(rc, file.path("outputs", "sk_rarity_conc.tif"))




