#'
#' @title unique_count
#' @description This function counts unique values by  column and calculate relative value of uniquiness
#' @param df  dataframe
#' @keywords counts uniquevalues
#' @details details
#' @examples
#' \dontrun{
#' unique_count(df)
#' }
#' @export

unique_count <- function(df) {
  dfu <- data.frame(var = names(df), uniq = sapply(df, function(x) length(unique(x))), stringsAsFactors = F)
  vunique <- (dfu$uniq == 1)
  dfu$flag_unique <- (dfu$uniq == 1)
  dfu$relative_unique <- dfu$uniq / nrow(df)
  return(dfu)
}
