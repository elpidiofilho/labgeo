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
      rb <- (df[, i] - df[j]) / (df[, i] + df[j])
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
