#1. Gap analysis 

# review the proteced areas: 

library(bcdata)
library(dplyr)
library(terra)
library(sf)


## read in diverity layer ? or rarity layer ? 



## read in protected layers 




## read in ecoregions 




# calculate % protected for all of Skeena


# calculate % protected by type? per ecoregion? 




# 

sk_rc <- rast(file.path("outputs", "sk_lf_rockclass.tif"))
sk_rcd <- rast(file.path("outputs", "sk_lf_rockclassdet.tif"))

skrcdf <- as.data.frame(sk_rc)
skrcdff <- skrcdf %>% 
  group_by(lyr.1) |> 
  count()|> 
  mutate(type = "rockclass") %>%
  ungroup()%>% 
  select(-lyr.1)

hist(skrcdff$n)

skrcddf <- as.data.frame(sk_rcd)
skrcddff <- skrcddf %>% 
  group_by(lyr.1) |> 
  count() |> 
  mutate(type = "detailed") %>%
  ungroup()%>%
  select(-lyr.1)

aa <- bind_rows(skrcdff,skrcddff )

xx <- skrcddf

hist(skrcddff$n)

library(ggplot2)



hist(as.numeric(skrcdf$lyr.1))

hist(as.numeric(skrcddf$lyr.1))
