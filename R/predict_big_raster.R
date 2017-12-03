

#' Title Predict big raster
#' @title predict big raster
#' @param model model to be predicted
#' @param st stack with co-variable
#' @param path_file path to storage results
#' @param suffix suffix to be adictioned to name of file results
#' @param format graphics format of result file
#' @param tiles number of  divisions in x and y to create tiles
#' @param cpu_cores numeber of cpu cores
#' @param verbose verbose
#' @importFrom progress progress_bar
#' @importFrom raster stack beginCluster endCluster clusterR writeRaster crop res mosaic raster
#' @return
#' @examples
#' \dontrun{
#' predict_big_raster(model = modelo_calibra, st = stcovar,
#'                    path_file ='./mosaic/', format = "GTiff",
#'                    suffix = 'etp_', tiles = 3, cpu_cores = 7 )
#' }
#' @export

predict_big_raster <- function(model, st, path_file, suffix = "pred_",
                               format = "GTiff", tiles = 5, cpu_cores = 7,
                               verbose = TRUE) {
  td <- NULL
  td <- create_temp_dir()
  tile_stack(st = st, dir = td, num_tiles = tiles)
  i <- 1

  for (i in 1:length(model)) {
    nm <- model[[i]]$modelInfo$label
    tile_predict(model <- model[[i]], dir = td, cpu_cores = cpu_cores,
                 verbose = verbose)
    mosaic_tiles(dir = td,  name_mosaic = paste0(path_file, suffix, nm, ".tif"),
                 format = "GTiff")
  }
  if (dir.exists(td)) {
    unlink(td)
  }
}


create_temp_dir <- function() {
  nd <- paste0("./temp_", sample(10000:100000, 1))
  if (!dir.exists(file.path(".", nd)))  {
    dir.create(file.path(".", nd))
    dir.create(file.path(".", paste0(nd, "/tiles")))
    dir.create(file.path(".", paste0(nd, "/predict")))
    return(nd)
  }
}

tile_stack <- function(st, dir, num_tiles, verbose = TRUE) {
  n.side <-  num_tiles
  dx     <- (extent(st)[2] - extent(st)[1]) / n.side
  dy     <- (extent(st)[4] - extent(st)[3]) / n.side
  xs     <- seq(extent(st)[1], by = dx, length = n.side)
  ys     <- seq(extent(st)[3], by = dy, length = n.side)
  cs     <- expand.grid(x = xs, y = ys)

  i <- 1
  nr <- nrow(cs)
  if (verbose == TRUE) {
    pb <- progress_bar$new(total = nr,
                           format("Running [:bar] :percent elapsed:
                                  :elapsed eta: :eta"),
                           clear = FALSE)

  }
  inicio <- Sys.time()
  print("creating tiles")
  for (i in 1:nrow(cs)) {
    #  for(i in 1:4) {
    ex1 <- c(cs[i, 1], cs[i, 1] + dx, cs[i, 2], cs[i, 2] + dy)
    cl1 <- raster::crop(st, ex1)
    raster::writeRaster(x = cl1, format = "raster",
                        filename = paste0(dir, "/tiles/tl_", i, ".grd"),
                        overwrite = T)
    if (verbose == TRUE) {
      pb$tick()
    }

  }
  print(paste("tile", hms_span(inicio, Sys.time())))

}

tile_predict <- function(model, dir, cpu_cores =  4, verbose = TRUE) {

  dir_tile <- paste0(dir, "/tiles/")
  dir_predict <- paste0(dir, "/predict/")
  rasters0 <- list.files(dir_tile, pattern = "*.grd", full.names = F,
                         recursive = TRUE)
  i <- 1
  print("predict tiles")
  nr <- length(rasters0)
  inicio <- Sys.time()
  if (verbose == TRUE) {
    pb <- progress_bar$new(total = nr,
                           format("Running [:bar] :percent elapsed:
                                  :elapsed eta: :eta"),
                           clear = FALSE)

  }

  for (i in 1:nr){
    nf <- rasters0[i]
    s1 <- unlist(stringr::str_split(nf, "[.]"))[1]
    rt <- stack(paste0(dir_tile, nf))
    ddd <- rt[[1]]@data@min
    if (is.na(ddd)) {
      rna <- raster(extent(rt))
      raster::res(rna) <- raster::res(rt)
      raster::writeRaster(rna, filename <- paste0(dir_predict, s1, ".tif"),
                  format = "GTiff", overwrite = T )
    } else {
      raster::beginCluster(cpu_cores)
      pred <- raster::clusterR(rt,  raster::predict, args = list(model),
                               filename = paste0(dir_predict, s1, ".tif"),
                               format = "GTiff",
                               progress = T, overwrite = T)
      raster::endCluster()
    }
    if (verbose == TRUE) {
      pb$tick()
    }

  }
  print(paste("predict ", hms_span(inicio, Sys.time())))
}

mosaic_tiles <- function(dir, name_mosaic, format = "GTiff" ) {
  print("mosaicing tiles")
  dir_predict <- paste0(dir, "/predict/")
  rasters1 <- list.files(dir_predict,
                         pattern = "*.tif",
                         full.names = TRUE, recursive = TRUE)
  rast.list <- list()
  for (i in 1:length(rasters1)) {
    rast.list[i] <- list(raster(rasters1[i]))
  }
  rast.list$fun <- mean
  rast.mosaic <- do.call(raster::mosaic, rast.list)
  raster::writeRaster(rast.mosaic, filename = name_mosaic,
                      format = format, overwrite = TRUE   )
  print("end")
}
