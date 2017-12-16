# Extract data Polygon
#'
#' @title extract_data_polygon
#' @description This function allows extract data from raster layers from polygon areas

#' @param raster_type  Tipo do arquivo raster a ser lido (.tif, .asc, .img ... )
#' @param path_poligon Path para a pasta onde esta os poligonos da amostragem
#' @param name_poligon nome do arquivo de poligono sem a raster_type
#' @param field_class nome do campo do shape de poligonos conte a identificacao da classe
#' @param field_poligon nome do campo do shape de poligonos conte a identificacao do poligono
#' @param remove_NA remover os valores NA durante a extracao dos dados
#' @keywords extract raster data
#' @importFrom  rgdal readOGR
#' @importFrom dplyr left_join
#' @importFrom raster stack raster rasterToPoints rasterize
#' @importFrom raster res extract extent
#' @importFrom sp SpatialPointsDataFrame
#' @details  details
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' df = extrai_poligono_raster(pathraster = "./bandas", extensao = "*.asc",
#'                             pathpoligono = "./amostragem", filepoligono = "poligonos",
#'                             fieldclasse = "gridcode", fieldpoligono = "Id")
#' }
#' @export

extract_data_polygon <- function(path_raster, raster_type = ".asc",
                                 path_poligon, name_poligon,
                                 field_class = "gridcode",
                                 field_poligon = "Id", remove_NA = TRUE) {
  inicio <- Sys.time()
  l <- list.files(
    path_raster, pattern = raster_type,
    include.dirs = TRUE, full.names = TRUE
  )
  st <- raster::stack(l)
  shpfile <- rgdal::readOGR(path_poligon, name_poligon)
  r <- raster::raster(extent(shpfile))
  raster::res(r) <- raster::res(st)
  rclasse <- raster::rasterize(shpfile, field = field_class, r)
  rpolig <- raster::rasterize(shpfile, field = field_poligon, r)
  pclasse <- raster::rasterToPoints(rclasse) %>%
    data.frame()
  ppolig <- raster::rasterToPoints(rpolig) %>%
    data.frame()
  names(pclasse)[3] <- "classe"
  names(ppolig)[3] <- "polig"
  pt <- dplyr::left_join(pclasse, ppolig) %>%
    data.frame()
  xy <- pt[, c(1, 2)]
  spdf <- sp::SpatialPointsDataFrame(
    coords = xy, data = pt,
    proj4string = shpfile@proj4string
  )
  dp <- raster::extract(st, spdf)
  dp <- data.frame(pt, dp)
  if (remove_NA == TRUE) {
    dp <- na.omit(dp)
  }

  print(paste("time elapsed:", hms_span(inicio, Sys.time())))
  return(dp)
}
