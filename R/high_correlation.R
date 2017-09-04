# High correlation variables
#'
#' @title high_correlation
#' @description This function searches through a correlation matrix and returns a vector of integers corresponding to columns to remove to reduce pair-wise correlations.
#' @param df  input dataframe
#' @param cuttof   numeric value for the pair-wise absolute correlation cutoff
#' @param verbose  A boolean for printing the details and plot graphic
#'
#' @keywords high correlated
#' @export VC - dataframe com os resultados
#' @details
#' @examples
#' high_correlation(df, 0.95, TRUE)


high_correlation <- function(df, cutoff = 0.95, verbose = F) {
  library(dplyr)
  library(knitr)
  library(caret)
  library(corrplot)
  df = df %>% select_if(is.numeric)
  mcor = cor(df)
  vc = findCorrelation(mcor, cutoff)
  if (length(vc) == 0) {
    print(paste("nenhum par de variaveis com corrrelacao maior que ",cutoff))
    return(vc)
  } else {
    vnam = names(df)[vc]
    if (verbose == T) {
      print(paste("high correlated variables :", length(vc)))
      m = vector2matrix(vnam)
      print(kable(m, col.names = c(1:ncol(m))))
      mcor = mcor[vc,]
      mcor[abs(mcor) < cutoff] <- 0
      mcor = mcor[,colSums(mcor) > 0]
      corrplot(mcor,tl.cex = 0.6)
    }
    return(vnam)
  }
}

vector2matrix <- function(vhc) {
  l1 = length(vhc)
  l2 = (ceiling(sqrt(length(vhc)))) ^2
  ladd  = l2 - l1
  ncol = sqrt(l2)
  nrow = ncol
  if (ladd > 0) {
    v1 = c(vhc, rep(" ",ladd))
  } else {
    v1 = vhc
  }
  m = matrix(v1,nrow, ncol)
  return(m)
}

