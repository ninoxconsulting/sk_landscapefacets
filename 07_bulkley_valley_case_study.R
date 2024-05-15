#07_Bulkley_valley_test_area 

library(grainscape)
library(terra)
library(sf)
library(tidyverse)
library(raster)

# read in diversity for template raster

template <- rast('C:/Users/j_evans/Downloads/sk_diversity_conc.tif')

test_patches <- st_read('F:/Other/Nixon/landscape_facets/connectivity/clipped_area.gpkg')

# clipping the template to the test patches then rasterizing

template_c <- crop(template, test_patches)

# rasterizing
patches <- rasterize(test_patches, template_c, field = "CEF_DISTURB_GROUP_RANK")

patches <- raster(patches)

# creating the MPG
patchyMPG <- MPG(patches, patch = (patches ==1 ))

plot(patchyMPG, quick = "mpgPlot", theme = FALSE)

scalarAnalysis <- threshold(patchyMPG, nThresh = 5)
## Use kable to render this as a table
scalarAnalysis$summary

scalarAnalysis <- threshold(patchyMPG, nThresh = 100)
ggplot(scalarAnalysis$summary, aes(x = maxLink, y = nComponents)) +
  geom_line(colour = "forestgreen") +
  xlab("Link Threshold (resistance units)") +
  ylab("Number of components") +
  scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
  scale_y_continuous(breaks = 1:20) +
  theme_light() +
  theme(axis.title = element_text())

ggplot() +
  geom_raster(
    data = ggGS(patchyMPG, "patchId"),
    aes(x = x, y = y, fill = value > 0)
  ) +
  scale_fill_manual(values = "grey") +
  geom_segment(
    data = ggGS(patchyMPG, "links"),
    aes(
      x = x1, y = y1, xend = x2, yend = y2,
      colour = lcpPerimWeight >= 250
    )
  ) +
  scale_colour_manual(values = c("forestgreen", NA)) +
  geom_point(
    data = ggGS(patchyMPG, "nodes"), aes(x = x, y = y),
    colour = "darkgreen"
  )