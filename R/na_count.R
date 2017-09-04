# Counts NA values
#'
#' @title na_count
#' @description This function counts NA by column and calculate relative value of NA
#' @param df  dataframe
#'
#' @keywords counts NA values
#' @export NACount  - dataframe with absolute and relative NA counts
#' @details
#' @examples
#' na_count(df)


na_count = function(df){
  NAcount <-sapply(df, function(y) sum(length(which(is.na(y)))))
  NAcount <- data.frame(var = names(df), absolute = NAcount, relative = numeric(ncol(df)), stringsAsFactors = F)
  NAcount$relative = NAcount$absolute / nrow(df)
  return(NAcount)
}
