# Extract data Polygon
#'
#' @title extract_data_polygon
#' @description This function allows extract data from raster layers from polygon areas
#' @param path_raster  Path para a pasta onde estão os rasters
#' @param raster_type  Tipo do arquivo raster a ser lido (.tif, .asc, .img ... )
#' @param path_poligon Path para a pasta onde esta os poligonos da amostragem
#'
#' @keywords extract raster data
#' @export df - dataframe com os resultados
#' @details
#' @examples
#' extract_data_polygon(pathraster,".tif", path_pol, "amostras_cafe")

extract_data_polygon <- function(path_raster, raster_type = ".asc", path_poligon, name_poligon,
                         cpu_cores = 2, remove_NA = T) {
  require(raster)
  require(rgdal)
  require(dplyr)
  pat = paste("*.", raster_type, sep = "")
  l =list.files(path_raster, pattern = pat,include.dirs = T,full.names = T)
  st = stack(l)
  p = readOGR(path_poligon, name_poligon)
  inicio = Sys.time()
  beginCluster(cpu_cores)
  dp = extract(st, p, df = T)
  endCluster()
  print(Sys.time() - inicio)
  dc = left_join(p@data, dp, by = c("OBJECTID","Id"))
  if (remove_NA == T) {
    dc = na.omit(dc)
  }
  return(dc)
}
