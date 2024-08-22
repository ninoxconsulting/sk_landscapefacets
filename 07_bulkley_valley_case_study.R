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

writeRaster(patches, 'F:/Other/Nixon/landscape_facets/connectivity/patches.tif')
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

#-------------------------------------------------------------------------------
# Link Thresholding to show components
# ------------------------------------------------------------------------------

## Use the grainscape::threshold() function to create a new network
## by thresholding links

for (i in seq(1, 50, 5)) {
  
  fragTh <- threshold(patchyMPG, doThresh = i)
  
  
  ## Find the components in that thresholded network using
  ## an igraph package function
  fragThC <- components(fragTh$th[[1]])
  ## Extract the node table and append the
  ## component membership information
  fragThNodes <- data.frame(vertex_attr(fragTh$th[[1]]),
                            component = fragThC$membership
  )
  
  ## We don't want to show nodes that are in components with
  ## only one node, so remove them
  singleNodes <- fragThNodes$component %in% which(fragThC$csize == 1)
  fragThNodes <- fragThNodes[!(singleNodes), ]
  ## Rename some columns to improve readability
  fragThNodes$x <- fragThNodes$centroidX
  fragThNodes$y <- fragThNodes$centroidY
  figure18 <- ggplot() +
    geom_raster(
      data = ggGS(patchyMPG, "patchId"),
      aes(x = x, y = y, fill = value > 0)
    ) +
    scale_fill_manual(values = "grey") +
    geom_segment(
      data = ggGS(fragMPG, "links"),
      aes(
        x = x1, y = y1, xend = x2, yend = y2,
        colour = lcpPerimWeight > 20
      )
    ) +
    scale_colour_manual(values = c("forestgreen", NA)) +
    geom_point(
      data = fragThNodes,
      aes(x = x, y = y), shape = 19, size = 4, colour = "darkgreen"
    ) +
    geom_text(
      data = fragThNodes, aes(x = x, y = y, label = component),
      colour = "white", size = 2
    ) +
    ggtitle(paste("Link thresholding to show components n_thresh =", i))
  
  
  print(figure18)
}


# having around 15 components seems reasonable 

## Use the grainscape::threshold() function to create a new network
## by thresholding links
fragTh <- threshold(patchyMPG, nThresh = 50)

# mapping the alpha_centrality at every scale:



centrality <- map(1:50, possibly(function(x) {
  
  
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
    mutate(threshold = x) %>%
    mutate(centrality = (centrality - min(centrality))/(max(centrality) - min(centrality)))
  
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


library(gifski)

gif_file <- "F:/Other/Nixon/landscape_facets/connectivity/node_importance.gif"
gifski(unlist(files), gif_file, 862, 551, delay = 0.5)

utils::browseURL(gif_file)


#-------------------------------------------------------------------------------
# Finding the Aysmptote of change
# ------------------------------------------------------------------------------

# binding the rows 
combined <- centrality %>% 
  purrr::reduce(., bind_rows)

delta <- combined %>%
  mutate(p_centrality = lag(centrality)) %>%
  group_by(threshold) %>%
  mutate(centrality_dif = mean(centrality - p_centrality))

ggplot(delta, aes(x = threshold, y = centrality_dif)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(y = "mean delta centrality", x = "number of thresholds")


# we can see that around n threshold = 30 we see a stabilization of centrality

#-------------------------------------------------------------------------------
# Do the patch numbers line up with the MPG? 
# ------------------------------------------------------------------------------

test <- st_as_sf(centrality[[1]], coords = c(10:11)) %>%
  st_set_crs(., st_crs(test_patches))

library(tmap)

tmap_mode("view")

tm_shape(test) +
  tm_text("patchId", col = "red") +
  tm_dots() 




#-------------------------------------------------------------------------------
# Finding the most important nodes
# ------------------------------------------------------------------------------

combined <- centrality %>% 
  purrr::reduce(., bind_rows) %>%
  group_by(patchId) %>%
  summarise(mean_centrality = mean(centrality, na.rm= T), x = median(x), y = median(y))

quantile(combined$mean_centrality)

# filter to the top 25% of points
combined <- combined %>%
  filter(mean_centrality >= 0.5790522)

img <- rast(patches)

img <- subst(img, NA, 100)

x <- leastcostpath::create_slope_cs(img, exaggeration = TRUE)

df <- igraph::as_data_frame(patchyMPG@mpg)

origins <- terra::cellFromXY(img, as.matrix(df[ , 5:6]))

destinations <- terra::cellFromXY(img, as.matrix(df[ , 7:8]))

cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)

igraph::E(cm_graph)$weight <- ((1/igraph::E(cm_graph)$weight))

cm_df <- igraph::as_data_frame(cm_graph, what = c("edges"))

cell_df <- data.frame('ID' = terra::cells(img), crds(img, df = T, na.rm = T))

graph <- cppRouting::makegraph(cm_df, directed = T, coords = cell_df, aux = ((1/igraph::E(cm_graph)$weight))) # WHAT ISTHIS??

d_matrix <- cppRouting::get_distance_pair(graph, origins, destinations, aggregate_aux = T)

pairs <- cppRouting::get_path_pair(graph, origins, destinations)


get_lcps_singular <- function(x, pairs, img) {
  
  lcp_xy <- purrr::map(pairs[[x]], ~ as.data.frame(terra::xyFromCell(img, as.numeric(.x)))) %>%
    reduce(., bind_rows)
  
  points <- st_as_sf(lcp_xy, coords = c(1:2), crs = sf::st_crs(img)) 
  
  linestring <- st_combine(points) %>% st_cast("LINESTRING")
  
}

map(get_lcps_singular)



get_lcps <- function(x, pairs, img) {
  
  lcp_xy <- purrr::map(pairs[[x]], ~ terra::xyFromCell(img, as.numeric(.x)))
  
  lcps <- purrr::map(lcp_xy, function(.x) sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(.x)), crs = sf::st_crs(img))) %>%
    purrr::reduce(., dplyr::bind_rows) %>%
    mutate(cost = d_matrix[x, ]) %>%
    mutate(from_vertex = names(pairs)[x]) %>%
    mutate(to_vertex = names(d_matrix[1,]))
  
  return(lcps)
  
}

# get all the least cost paths 
paths <- map(1:length(pairs), get_lcps, pairs = pairs, img = img) %>%
  bind_rows() %>%
  filter(cost > 0)

edges <- st_drop_geometry(paths) %>%
  dplyr::select(from_vertex, to_vertex, cost)



# generate the graph with just the nodes
directed_graph <- makegraph(edges,directed=TRUE)


# get all possible pairs
trips <- data.frame(from = origins, to = origins, demand = combined$mean_centrality)
from <- data.frame(from = origins)
to <- data.frame(to = origins)


cross <- tidyr::crossing(from, to) 

trips <- cross %>%
  mutate(demand = plyr::mapvalues(cross$from, trips$from, trips$demand)) %>%
  filter(from != to)


aon <- get_aon(Graph = directed_graph, from = trips$from, to = trips$to, demand = trips$demand)


paths <- inner_join(paths, aon, by = "cost")

st_write(paths, 'F:/Other/Nixon/landscape_facets/connectivity/paths_wt_flow.gpkg')
