library(raster)
library(dplyr)

path_raster = "../BrunoMendonca/imagem" 
raster_type = "*.tif"
path_poligon = "../BrunoMendonca/amostras" 
name_poligon = "amostra_A_idpol"
field_class = "gridcode" 
field_poligon = "Id"

extract_data_polygon <- function(path_raster, raster_type = ".asc",
                                 path_poligon, name_poligon,
                                 field_class = "gridcode",
                                 field_poligon = "Id", remove_NA = TRUE) {
  inicio <- Sys.time()
  l <- list.files(path_raster, pattern = raster_type,
                  include.dirs = T, full.names = T)
  st <- raster::stack(l[1])
  shpfile <- rgdal::readOGR(path_poligon, name_poligon, stringsAsFactors = F)
  shpfile@data$gridcode = as.integer(shpfile@data$GRID_CODE)
  shpfile@data$Id = as.integer(shpfile@data$RASTERVALU)
  table(shpfile@data$Id)
  table(shpfile@data$gridcode)
  
  r <- raster::raster(extent(shpfile))
  raster::res(r) <- raster::res(st)
#  raster::res(r) <- c(30,30)
  rclasse <- raster::rasterize(shpfile, field = field_class, r)
  table(rclasse@data@values)
  rpolig <- raster::rasterize(shpfile, field = field_poligon, r)
  table(rpolig@data@values)
  
  pclasse <- raster::rasterToPoints(rclasse) %>%  data.frame()
  ppolig <- raster::rasterToPoints(rpolig) %>%     data.frame()
  names(pclasse)[3] <- "classe"
  names(ppolig)[3] <- "polig"
  pt <- dplyr::left_join(pclasse, ppolig) %>%
    data.frame()
  xy <- pt[, c(1, 2)]
  spdf <- sp::SpatialPointsDataFrame(coords = xy, data = pt,
                                     proj4string = shpfile@proj4string)
  dp <- raster::extract(st, spdf)
  dp <- data.frame(pt, dp)
  if (remove_NA == TRUE) {
    dp <- na.omit(dp)
  }
  
  print(paste("time elapsed:",hms_span(inicio, Sys.time())))
  return(dp)
}
