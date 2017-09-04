# Factor variable detection
#'
#' @title factor_detect
#' @description This function detect factor variables
#' @param df  dataframe
#' @param max.unique number maximum of unique values to be considered as a factor varaiable
#' @keywords detect factor
#' @details details
#' @examples
#' \dontrun{
#' factor_detect (df, max.unique = 10)
#' }
#' @export


factor_detect <- function(df, max.unique = 20) {
  name.col <- names(df)
  vf <- as.integer()
  for (i in seq_len(ncol(df))){
    if ( (is.numeric(df[, i]) == TRUE) | (is.character(df[, i]) == TRUE)) {
      unique.val <- length(unique(df[, i]))
      if (unique.val <= max.unique) {
        vf <- c(vf, i)
        print(name.col[i])
      }
    }
  }
  return(vf)
}
