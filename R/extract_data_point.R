# Extract data point
#'
#' @title extract_data_point
#' @description This function allows extract data from raster layers from point
#' @param path_raster  Path para a pasta onde estão os rasters
#' @param raster_type  Tipo do arquivo raster a ser lido (*.tif, *.asc, *.img ... )
#' @param path_point Path para a pasta onde esta os pontos de amostragem
#' @param name_point name of shapefile of points without extension
#' @param cpu_cores number of cpu cores used in processing
#' @param remove_NA remove NA values after extraction
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @keywords extract raster data point
#' @importFrom raster beginCluster endCluster extract stack
#' @importFrom rgdal readOGR
#' @importFrom utils glob2rx
#' @importFrom stats na.omit
#' @details details
#' @examples
#' \dontrun{
#' extract_data_point(pathraster,"*.tif", path_point, "amostras_cafe")
#' }
#' @export

extract_data_point <- function(path_raster, raster_type = "*.asc",
                               path_point, name_point,
                                 cpu_cores = 1, remove_NA = TRUE) {
  if (dir.exists(path_raster) == FALSE) stop(paste(path_raster, "does not exists"))
  if (dir.exists(path_point) == FALSE) stop(paste(path_point, "does not exists"))
  pat <- glob2rx(raster_type)
  l <- list.files(path_raster, pattern = pat, include.dirs = TRUE, full.names = TRUE)
  st <- raster::stack(l)
  p <- rgdal::readOGR(path_point, name_point)
  inicio <- Sys.time()
  if (cpu_cores > 1) {
    raster::beginCluster(cpu_cores)
  }
  dp <- raster::extract(st, p)
  if (cpu_cores > 1) {
    raster::endCluster()
  }
  print(Sys.time() - inicio)
  dc <- data.frame(p@data, dp)
  if (remove_NA == TRUE) {
    dc <- na.omit(dc)
  }
  return(dc)
}
