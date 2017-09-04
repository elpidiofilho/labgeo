# Non Linear Correlation
#'
#' @title non_linear_correl
#' @description This function calculates non linear correlation between variables
#' @param df  dataframe with numerical data
#'
#' @keywords RDC non linera correlation
#' @export mcnl - matrix with non linear correlagion
#' @details
#' @examples
#' non_linear_correl(df)



non_linear_correl <- function(df) {
  stopifnot(is.data.frame(df))
  c = c("peraser")
  mcnl = matrix(nrow = ncol(df), ncol = ncol(df))
  for (i in 1:ncol(df)) {
    for (j in 1:ncol(df)) {
      mcnl[i,j] = rdc(df[,i],df[,j])
    }
  }
  mcnl =as.data.frame(mcnl)
  colnames(mcnl) = colnames(df)
  rownames(mcnl) = colnames(df)
  return(mcnl)
}



rdc <- function(x,y,k = 20,s = 1/6, f = sin) {
  x <- cbind(apply(as.matrix(x),2,function(u)rank(u)/length(u)),1)
  y <- cbind(apply(as.matrix(y),2,function(u)rank(u)/length(u)),1)
  set.seed(313)
  x <- s/ncol(x)*x %*% matrix(rnorm(ncol(x)*k),ncol(x))
  set.seed(313)
  y <- s/ncol(y)*y %*% matrix(rnorm(ncol(y)*k),ncol(y))
  cancor(cbind(f(x),1),cbind(f(y),1))$cor[1]
}
