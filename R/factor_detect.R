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
  vunique <- sapply(df, function(y) length(unique(y)))
  vf <- vunique[vunique <= max.unique]
  vn <- match(names(vf), names(df))
  if (length(vn) > 0) {
    print(names(df)[vn])
  }
  return(vn)
}
