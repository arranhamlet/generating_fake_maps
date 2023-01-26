if(!require(pacman)) install.packages("pacman")

#Load packages
pacman::p_load(
  rgdal,
  rgeos,
  maptools,
  raster,
  janitor,
  tidyverse
)

#Load in functions
invisible(sapply(list.files("R/functions/", full.names = TRUE), function(x) source(x)))

#Plot maps
mini_map_replace <- random_map_maker(seed = 3,
                             mapcol = 25,
                             maprow = 25,
                             nstart = 30,
                             nrun = 100,
                             ncat = 30,
                             direction = 4,
                             nsample = 10)

larger_map <- random_map_maker(seed = 15,
                               mapcol = 100,
                               maprow = 100,
                               nstart = 30,
                               ncat = 30,
                               nrun = 15,
                               replace = F,
                               nsample = 1000)

#Slow mo plot
mini_map_replace <- random_map_maker(seed = 3,
                                     mapcol = 25,
                                     maprow = 25,
                                     nstart = 30,
                                     nrun = 100,
                                     ncat = 30,
                                     direction = 4,
                                     nsample = 10,
                                     slowmo = 0.05)

#Create a shapefile from the plot and add in fake data
fake_worms <- data.frame(id = as.character(unique(mini_map_replace)),
                         worminess = rnbinom(length(unique(mini_map_replace)), size = 1, prob = 0.05))

map_ggplot_obj <- raster_to_ggplot_obj(mini_map_replace,
                                       data = fake_worms)

#Generate centroid points of the object for labelling
cnames <- aggregate(cbind(long, lat) ~ id,
                    data = map_ggplot_obj,
                    FUN = function(x) mean(range(x)))

#Plot new map
ggplot() +
  geom_polygon(data = map_ggplot_obj,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = worminess),
               color = "black") +
  geom_text(data = cnames,
            aes(x = long,
                y = lat,
                group = id,
                label = id)) +
  theme_void() +
  labs(fill = "Worminess") +
  coord_map() +
  scale_fill_distiller(palette = "YlOrRd",
                       direction = 1)









