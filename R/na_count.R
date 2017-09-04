# Counts NA values
#'
#' @title na_count
#' @description This function counts NA by column and calculate relative value of NA
#' @param df  dataframe
#' @keywords counts NA values
#' @details details
#' @examples
#' \dontrun{
#' na_count(df)
#' }
#' @export


na_count <- function(df){
  na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
  na_count <- data.frame(var = names(df), absolute = na_count,
                        relative = numeric(ncol(df)), stringsAsFactors = FALSE)
  na_count$relative <- na_count$absolute / nrow(df)
  return(na_count)
}
