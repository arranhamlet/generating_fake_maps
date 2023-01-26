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
mini_map <- random_map_maker(seed = 3,
                             mapcol = 25,
                             maprow = 25,
                             nstart = 10,
                             nrun = 150,
                             ncat = 10,
                             nsample = 10)

mini_map_replace <- random_map_maker(seed = 3,
                             mapcol = 25,
                             maprow = 25,
                             nstart = 30,
                             nrun = 100,
                             ncat = 30,
                             direction = 4,
                             nsample = 10,
                             replace = F)

larger_map <- random_map_maker(seed = 15,
                               mapcol = 100,
                               maprow = 100,
                               nstart = 30,
                               ncat = 30,
                               nrun = 15,
                               replace = F,
                               nsample = 1000)

#Slow mo plot
random_map_maker(seed = 1,
                 mapcol = 25,
                 maprow = 25,
                 nstart = 10,
                 nsample = 10,
                 ncat = 10,
                 replace = F,
                 catweight = 1,
                 slowmo = 0.1)

map_ggplot_obj <- raster_to_ggplot_obj(mini_map_replace)
fake_worms <- data.frame(id = as.character(unique(mini_map_replace)),
                         worminess = rnbinom(length(unique(mini_map_replace)), size = 1, prob = 0.05))

map_ggplot_obj_data <- left_join(map_ggplot_obj,
                                 fake_worms,
                                 by = "id")

cnames <- aggregate(cbind(long, lat) ~ id,
                    data = map_ggplot_obj_data,
                    FUN = function(x) mean(range(x)))

ggplot() +
  geom_polygon(data = map_ggplot_obj_data,
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
  labs(fill = "Adm unit") +
  coord_map() +
  scale_fill_distiller(palette = "YlOrRd",
                       direction = 1)



pbinom(q = 1:100, size = 30, prob = 1:100/sum(1:100))







