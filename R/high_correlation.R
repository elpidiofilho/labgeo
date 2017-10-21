# High correlation variables
#'
#' @title high_correlation
#' @description This function searches through a correlation matrix and returns a vector of integers corresponding to columns to remove to reduce pair-wise correlations.
#' @param df  input dataframe
#' @param cutoff   numeric value for the pair-wise absolute correlation cutoff
#' @param correl type of correlation. linear ou rdc
#' @param verbose  A boolean for printing the details and plot graphic
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @keywords high correlated
#' @details details
#' @importFrom caret findCorrelation
#' @importFrom  knitr kable
#' @importFrom  corrplot corrplot
#' @importFrom  dplyr select_if
#' @importFrom  stats cor
#' @examples
#' \dontrun{
#' high_correlation(df, 0.95, correl = "linear", TRUE)
#' }
#' @export

high_correlation <- function(df, cutoff = 0.95,
                             correl = "linear", verbose = FALSE) {
  df <- df %>%
    dplyr::select_if(is.numeric)
  if (correl == "linear") {
    mcor <- cor(df)
  } else {
    ## rdc
    mcor <- as.matrix(non_linear_correl(df))
  }
  vc <- caret::findCorrelation(mcor, cutoff)
  if (length(vc) == 0) {
    print(paste("no pair of variables with correlation greater than ", cutoff))
    return(vc)
  } else {
    vnam <- names(df)[vc]
    if (verbose == TRUE) {
      print(paste("high correlated variables :", length(vc)))
      m <- vector2matrix(vnam)
      print(knitr::kable(m, col.names = c(seq_len(ncol(m)))))
      mcor <- mcor[vc, ]
      mcor[abs(mcor) < cutoff] <- 0
      mcor <- mcor[, colSums(mcor) > 0]
      corrplot::corrplot(mcor, tl.cex = 0.6)
    }
    return(vnam)
  }
}

vector2matrix <- function(vhc) {
  l1 <- length(vhc)
  l2 <- (ceiling(sqrt(length(vhc)))) ^ 2
  ladd <- l2 - l1
  ncol <- sqrt(l2)
  nrow <- ncol
  if (ladd > 0) {
    v1 <- c(vhc, rep(" ", ladd))
  } else {
    v1 <- vhc
  }
  m <- matrix(v1, nrow, ncol)
  return(m)
}
