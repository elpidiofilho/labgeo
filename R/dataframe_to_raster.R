# dataframe to raster
#'
#' @title dataframe_to_raster
#' @description This function convertes a dataframe in XYZ format, to a raster
#' @param df  dataframe in xYZ format where x and y are coordinates and z is the variable to create raster.
#' @keywords dataframe raster correlation
#' @export raster
#' @details
#' @examples
#' dataframe_to_raster(df)

dataframe_to_raster <- function(df) {
  library(raster)
  rasterFromXYZ(df) }
