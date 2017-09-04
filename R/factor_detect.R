# Factor variable detection
#'
#' @title factor_detect
#' @description This function detect factor variables
#' @param df  dataframe
#' @param max.uniques number maximum of unique values to be considered as a factor varaiable
#'
#' @keywords detect factor
#' @export df - vector of names of columns candidate to be a factor variable
#' @details
#' @examples
#' factor_detect (df, max.unique = 10)


factor_detect <- function(df,max.unique = 20) {
  name.col = names(df)
  vf = as.integer()
  for(i in 1:ncol(df)){
    if ((is.numeric(df[,i]) == T) | (is.character(df[,i]) == T)) {
      unique.val = length(unique(df[,i]))
      if (unique.val <= max.unique) {
        vf = c(vf,i)
        print(name.col[i])
      }
    }
  }
  return(vf)
}
