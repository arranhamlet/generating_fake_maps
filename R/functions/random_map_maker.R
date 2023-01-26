#
random_map_maker <- function(seed = 1,
                             mapcol = 100,
                             maprow = 100,
                             nstart = 10,
                             ncat = 5,
                             catweight = 1,
                             nrun = 100,
                             nsample = 10,
                             direction = 8,
                             replace = T,
                             slowmo = F){


  #Generate the base vaue raster
  base_raster <- raster(matrix(NA,
                               ncol = mapcol,
                               nrow = maprow))
  base_raster[] <- factor(base_raster[],
                          levels = 1:ncat)

  #Run through randomly sampling
  set.seed(seed)

  #Set up initial locations
  random_raster <- base_raster
  prob_use <- if(length(catweight) == 1) rep(catweight, ncat) else catweight/sum(catweight)
  random_raster[sample(1:length(random_raster), nstart)] <- factor(sample(1:ncat,
                                                                          size = nstart,
                                                                          prob = prob_use,
                                                                          replace = if(ncat < nstart) T else F),
                                                                   levels = 1:ncat)

  #Generate random raster
  if(slowmo == F){
    for(i in 1:nrun){
      some_of_these <- adjacent(x = random_raster,
                                cells = which(!is.na(random_raster[])),
                                directions = direction)
      if(replace == F){
        some_of_these <- some_of_these[which(is.na(random_raster[some_of_these[, 2]])), ]
        if(nrow(some_of_these) == 0) break
      }

      take_these <- some_of_these[sample(1:nrow(some_of_these), nsample, replace = TRUE), ]
      random_raster[take_these[, 2]] <- random_raster[take_these[, 1]]
    }
    plot(random_raster, zlim = c(1, max(ncat)))
  } else {
    for(i in 1:nrun){
      plot(random_raster, zlim = c(1, max(ncat)))
      Sys.sleep(slowmo)
      some_of_these <- adjacent(x = random_raster,
                                cells = which(!is.na(random_raster[])),
                                directions = direction)
      if(replace == F){
        some_of_these <- some_of_these[which(is.na(random_raster[some_of_these[, 2]])), ]
        if(nrow(some_of_these) == 0) break
      }

      take_these <- some_of_these[sample(1:nrow(some_of_these), nsample, replace = TRUE), ]
      random_raster[take_these[, 2]] <- random_raster[take_these[, 1]]
    }
  }

  random_raster

}
