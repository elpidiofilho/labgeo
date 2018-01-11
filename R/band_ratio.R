# Band Ratio
#'
#' @title band_ratio
#' @description This function calculates band ratios in the form bd = (a-b) / (a + b). The result is a dataframe containing the combinations of all the bands.
#' @param df dataframe with bands
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @keywords band ratio ndvi
#' @details calculates band ratios in the form bd = (a-b) / (a + b)
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' br = band_ratio(df)
#' }
#' @export

band_ratio <- function(df) {
  nb <- ncol(df)
  cont <- 1
  for (i in 1:(nb - 1)) {
    for (j in (i + 1):nb) {
      rb <- (df[, j] - df[,i]) / (df[, j] + df[, i])
      if (cont == 1) {
        drb <- data.frame(rb)
      } else {
        drb <- cbind(drb, rb)
      }
      ni <- gsub("band", "", names(df)[i])
      nj <- gsub("band", "", names(df)[j])
      names(drb)[cont] <- paste("b_", ni, "_", nj, sep = "")
      cont <- cont + 1
    }
  }
  return(drb)
}


# Band Ratio to Raster
#'
#' @title band_ratio_to_Raster
#' @description This function calculates band ratios in the form bd = (a-b) / (a + b). The result is write in a raster file with the same extension of input files
#' @param raster_ext extension of input raster file (.asc, .tif)
#' @param raster_input_path path of input raster files
#' @param  raster_output_path path of output raster files
#' @importFrom raster stack writeRaster
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @keywords band ratio ndvi
#' @details calculates band ratios in the form bd = (a-b) / (a + b)
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' raster_input_path = "./asc"
#' raster_ext =  ".asc"
#' raster_output_path = "./band_ratio"
#' band_ratio_to_raster(raster_ext,raster_input_path,raster_output_path )
#' }
#' @export

band_ratio_to_raster <- function(raster_ext, raster_input_path,
                                 raster_output_path) {
  l <- list.files(path = raster_input_path,
                  pattern = glob2rx(paste0("*", raster_ext)), full.names = TRUE)
  st <- raster::stack(l)
  nf <- names(st)
  nb <- length(nf)
  for (i in 1:(nb - 1)) {
    for (j in (i + 1):nb) {
      rb <- (st[[j]] - st[[i]]) / (st[[j]] + st[[i]])
      ni <- gsub("band", "", nf[i])
      nj <- gsub("band", "", nf[j])
      nfile <- paste0("br_", ni, "_", nj, raster_ext)
      print(nfile)
      path_nf <- paste0(raster_output_path, "/", nfile)
      raster::writeRaster(rb, filename = path_nf, overwrite = TRUE)
    }
  }
}
