# Moda Mediana
#'
#' @title moda_median
#' @description This function calculates moda or median for each column of a dataframe
#' @param df dataframe
#' @keywords moda median
#' @details details
#' @importFrom stats cor median
#' @examples
#' \dontrun{
#' moda_median(df)
#' }
#' @export



moda_median <- function(df){
  Freq = NULL
  DF <- as.data.frame(df)
  #x = DF[,2]
  MODE2 <- function(x){
    if (is.numeric(x) == FALSE){
      df <- as.data.frame(table(x))
      df <- df[order(df$Freq), ]
      m <- max(df$Freq)
      MODE1 <- as.vector(as.character(subset(df, Freq == m)[, 1]))
      if (sum(df$Freq) / length(df$Freq) == 1){
        warning("No Mode: Frequency of all values is 1", call. = FALSE)
      }else{
        return(MODE1)
      }

    }else{
      df <- median(x, na.rm = TRUE)
    }
  }
  return(as.vector(lapply(DF, MODE2)))
}
