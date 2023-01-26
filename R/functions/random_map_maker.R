random_map_maker <- function(seed = 1,           #The seed for your random number generation
                             mapcol = 100,       #The number of columns
                             maprow = 100,       #The number of rows
                             nstart = 10,        #How many starting points you want
                             ncat = 5,           #How many categories you want
                             catweight = 1,      #The weight of the categories, accepts a vector the same length as ncat
                             nrun = 100,         #The number of iterations of sampling
                             nsample = 10,       #How many samples you want per run
                             direction = 4,      #What direction do you want the sampling to move? Chess movement, 4 = king, 8 = queen, 16 = Knight +  one cell queen moves
                             replace = F,        #Do you want the categories to be able to replace each other? Makes more jagged maps if set to T
                             slowmo = F){        #Do you want each sample to be plotted, accepts F for no or time in seconds


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
