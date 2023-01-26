
raster_to_ggplot_obj <- function(raster_file){

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
    rename("numeric_value" = layer,
           "letter_value" = VALUE)

  rastpol_fort_data

}
