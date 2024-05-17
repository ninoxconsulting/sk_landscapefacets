#07_Bulkley_valley_test_area 

library(grainscape)
library(terra)
library(sf)
library(tidyverse)
library(raster)
library(tictoc)

# read in diversity for template raster

template <- rast(file.path("..", "inputs", "sk_rast_template.tif"))


test_patches <- sf::st_read("F:/Other/Nixon/landscape_facets/inputs/skeena_clip_ce_bulkley.gpkg")

# clipping the template to the test patches then rasterizing
template_c <- crop(template, test_patches)

# rasterizing
patches <- rasterize(test_patches, template_c, field = "CEF_DISTURB_GROUP_RANK")

patches <- raster(patches)

# creating the MPG
patchyMPG <- MPG(patches, patch = (patches ==1 ))

# plotting the MPG
plot(patchyMPG, quick = "mpgPlot", theme = FALSE)

# Lets plot the thresholds at different thresholds to see where we 
# see the amount of information cease to change


for(i in seq(10, 100, 10)) {
  
  scalarAnalysis <- threshold(patchyMPG, nThresh = i)
  
  g <- ggplot(scalarAnalysis$summary, aes(x = maxLink, y = nComponents)) +
    geom_line(colour = "forestgreen") +
    xlab("Link Threshold (resistance units)") +
    ylab("Number of components") +
    labs(title = paste("threshold = ", i)) +
    theme_light() +
    theme(axis.title = element_text())
  # we can see that having 100 thresholds is more than what is necessary to capture the 
  # variation in the landscape with the graph plateuing at 30 components. Lets redo this with 30
  # thresholds
  print(g)
  
}

# threshold around 50 seems reasonable to go with

# showing the components

## Use the grainscape::threshold() function to create a new network
## by thresholding links
fragTh <- threshold(patchyMPG, nThresh = 50)

# mapping the alpha_centrality at every scale:



centrality <- map(10:50, possibly(function(x) {

  
    print(x)
    ## Find the components in that thresholded network using
    ## an igraph package function
    fragThC <- components(fragTh$th[[x]])
    ## Extract the node table and append the
    ## component membership information
    fragThNodes <- data.frame(vertex_attr(fragTh$th[[x]]),
                              component = fragThC$membership
    )
    
    ## Assess centrality of a thresholded network
    ## made in the previous example (threshold = 20)
    fragThDegree <- try(alpha_centrality(fragTh$th[[x]]))
    ## Add degree to the node table
    fragThNodes <- data.frame(vertex_attr(fragTh$th[[x]]), centrality = fragThDegree) %>%
      mutate(threshold = x)
    
    fragThNodes$x <- fragThNodes$centroidX
    fragThNodes$y <- fragThNodes$centroidY
    
    return(fragThNodes)

    
})) %>%
  keep(~ !is.null(.x))
  

files <- list()
# plot each as a function of node size:
for (i in seq_along(centrality)) {
  
  print(i)
  
  figure <- ggplot() +
    geom_raster(
      data = ggGS(patchyMPG, "patchId"),
      aes(x = x, y = y, fill = value > 0)
    ) +
    scale_fill_manual(values = "grey") +
    geom_segment(
      data = ggGS(patchyMPG, "links"),
      aes(
        x = x1, y = y1, xend = x2, yend = y2,
        colour = lcpPerimWeight > 20
      )
    ) +
    scale_colour_manual(values = c("forestgreen", NA)) +
    geom_point(
      data = centrality[[i]],
      aes(x = x, y = y, size = centrality), colour = "darkgreen"
    ) +
    ggtitle(paste("Node importance metrics thresh = ", i))
  
  filename <- paste0('F:/Other/Nixon/landscape_facets/connectivity/images/centrality_thresh_', i, ".png")
  
  files[[i]] <- filename
  
  ggsave(filename, plot = figure, width = 862, height = 551, units = "px", dpi = 150)
  
}


gif_file <- "F:/Other/Nixon/landscape_facets/connectivity/node_importance.gif"
gifski(unlist(files), gif_file, 862, 551, delay = 0.5)

utils::browseURL(gif_file)



print(figure)

# building a grains of connectivity analysis 
# Use 100 thresholds 
tic()
patchyGOC <- GOC(patchyMPG, nThresh = 50)
toc() # 94 seconds



# lets plot the connected regions at various thresholds:
for(i in seq(1, 50, 5)) {
  
  g <- plot(grain(patchyGOC, whichThresh = i), quick = "grainPlot", theme = FALSE) +
    ggtitle(paste("threshold = ", i))
  
  print(g)
  
}

#
