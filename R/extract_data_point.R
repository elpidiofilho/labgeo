# Extract data point
#'
#' @title extract_data_point
#' @description This function allows extract data from raster layers from point
#' @param path_raster  Path para a pasta onde estão os rasters
#' @param raster_type  Tipo do arquivo raster a ser lido (.tif, .asc, .img ... )
#' @param path_poligon Path para a pasta onde esta os pontos de amostragem
#'
#' @keywords extract raster data point
#' @export df - dataframe com os resultados
#' @details
#' @examples
#' extract_data_point(pathraster,".tif", path_point, "amostras_cafe")

extract_data_point <- function(path_raster, raster_type = ".asc", path_point, name_point,
                                 cpu_cores = 2, remove_NA = T) {

  require(raster)
  require(rgdal)
  require(dplyr)
  pat = paste("*.", raster_type, sep = "")
  l =list.files(path_raster, pattern = pat,include.dirs = T,full.names = T)
  st = stack(l)
  p = readOGR(path_point, name_point)
  inicio = Sys.time()
  beginCluster(cpu_cores)
  dp = extract(st, p, df = T)
  endCluster()
  print(Sys.time() - inicio)
  dc = data.frame(p@data, dp)
  if (remove_NA == T) {
    dc = na.omit(dc)
  }
  return(dc)
}
