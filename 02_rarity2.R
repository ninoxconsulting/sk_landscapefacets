#02. Gneerate rarity codes for barcode 

library(terra)
library(sf)
library(ggplot2)
library(dplyr)

srast = terra::rast(file.path("outputs", "sk_lf_barcode.tif"))

# Read in the skeena facets 

# number of codes within Aoi 
uval = length(unique(values(srast)))

# Summarise values 
routdf <- as.data.frame(srast)

# polt histogram
ggplot2::ggplot(routdf, aes(lyr.1)) +
  ggplot2::geom_histogram(bins = uval) 

colnames(routdf) = "layer1"

ids = routdf %>% 
  group_by(layer1)%>%
  summarise(count = n())%>%
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


# generate Summary tables 
write_csv(rare , file.path("outputs", "lf_barcode_summary.csv"))



# if above is already run....

# read in the rare csv file 
rare <- read.csv(file.path("outputs", "lf_barcode_summary.csv")) 
rr = terra::rast(file.path("outputs", "sk_lf_barcode.tif"))

#cutoff is count of 46127

hist(rare$count)
hist(rare$rare_id)

 # assign rarity class
class1 <-rare  %>% filter(rare_id == 1) %>% pull(layer1)
class2 <- rare %>% filter(rare_id == 2)%>% pull(layer1)
class3 <-rare  %>% filter(rare_id == 3) %>% pull(layer1)
class4 <-rare  %>% filter(rare_id == 4) %>% pull(layer1)
class5 <-rare  %>% filter(rare_id == 5) %>% pull(layer1)
class6 <-rare  %>% filter(rare_id == 6) %>% pull(layer1)

uvr <- as.vector(unique(values(rr)))

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

# check if needs class 6 

if(any(unique(uvr %in% class15)) == TRUE){
  print("reclass values")
  
  for(i in class6){
    #  i = class1[1]
    print(i)
    rr <- subst(rr, i, 6)
    
  }
  
}else {
  print("no class 6 reclass needed")
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
#class5 <- aa
#aa <- class5 
#class5 <- aa[21:50]

#setdiff(reclass, class5)


if(any(unique(uvr %in% class5)) == TRUE){
  print("reclass values")
  
  for(i in class5){
     # i = class1[1]
    print(i)
    rr <- subst(rr, i, 5)
  }
}else {
  print("no class5 reclass needed")
}


reclass <- as.vector(unique(values(rr)))

sort(reclass)

## from-to-becomes
# classify the values into groups 

m <- c(1, 1, 1,
       1.5, 1.5, 1.5,
       2, 2, 2,
       3, 3, 3,
       4, 4, 4,
       5, 200000, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(rr, rclmat, include.lowest=TRUE)



unique(values(rc1))

terra::writeRaster(rc1,file.path("outputs", "sk_rarity_class_rcd.tif"), overwrite = TRUE)



## in QGIS run neighbourhood analysis 







## Calculoate concentration 

# clip to boundary 

# con_rare <- rast(file.path("outputs", "sk_lf_rdc_rarity_101c.tif"))
# names(con_rare)= "rarity"
# 
# temp <- rast(file.path("inputs", "sk_rast_template.tif"))
# 
# # mask the raster by aoi and clip and then export
# con_rarec <- mask(con_rare, temp)
# writeRaster(con_rarec, file.path("outputs","sk_lf_rdc_rarity_101c_clip.tif"))


# read in clipped dataset
con_rarec <- rast(file.path("outputs","sk_lf_rdc_rarity_101c_clip.tif"))

#reclass the valyers to a conccentration 

hist(con_rarec$rarity  , breaks = 40)

## WAITING ON INPUT FROM PAULA 

unique(values(con_rarec))

## from-to-becomes
# classify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.

# might need to update these....
m <- c(0, 1, 1,
       1, 2, 2,
       2, 3, 3,
       3, 4, 4,
       4, 6, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- classify(con_rarec , rclmat, include.lowest=TRUE)

writeRaster(rc, file.path("outputs", "sk_rarity_conc.tif"))



