
raster_to_ggplot_obj <- function(raster_file, data){

  rastpol <- rasterToPolygons(raster_file, dissolve = T)
  levels <- levels(raster_file)[[1]]
  rastpol$id <- as.character(1:nrow(rastpol))
  rastpol@data <- left_join(as.data.frame(rastpol),
                       levels,
                       by = c("layer" = "ID"))

  rastpol_fort <- fortify(rastpol)

  rastpol_fort_data <- left_join(rastpol_fort,
                                 as.data.frame(rastpol),
                                 by = "id") %>%
    rename("numeric_value" = layer)

  map_ggplot_obj_data <- left_join(rastpol_fort_data,
                                   data,
                                   by = "id")

}
