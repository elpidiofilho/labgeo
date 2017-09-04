# Dataframe predict to raster
#'
#'
#' This function creates a raster as a result of predicting an adjusted model, given x and y, and a dataframe containing the data needed to predict. The result is a raster map.
#' @param fit model
#' @param xy dataframe with x and y coordinates. Names of column must be "x" and "y"
#' @param df dataframe with data neeeded to predict the model. Names of columns must be the same of vars that was be used to train the model
#' @keywords predict, map, raster
#' @details  details
#' @importFrom  sp gridded coordinates
#' @importFrom  raster raster
#' @importFrom stats predict
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' ptr = df_predict_to_raster(fit, xy, df)
#' }
#' @export


df_predict_to_raster <- function(fit, xy, df) {
  v <- predict(fit, df)
  dfmap <- data.frame(xy, v)
  sp::coordinates(dfmap) <- ~ x + y
  sp::gridded(dfmap) <- TRUE
  r <- raster::raster(dfmap)
  return(r)
}
