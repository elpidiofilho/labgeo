# Descriptive statistics polygon
#'
#' @title Descriptive statistics polygon
#' @description This function calculates Descriptive statistics for samples collected in polygon
#' @param df dataframe
#' @param poligon_id name of column thats contains polygon id
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise_all
#' @importFrom dplyr select_if
#' @importFrom dplyr mutate_if
#' @importFrom dplyr left_join
#' @importFrom stats quantile
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @keywords statics polygon
#' @details calculates Descriptive statistics for samples collected in polygon
#' @examples
#' \dontrun{
#' b = descriptive_stat_polygon(df,"id")
#'}
#' @export



descriptive_stat_polygon <- function(df, poligon_id) {

  dnum <- df %>% dplyr::group_by(!!poligon_id) %>% dplyr::select_if(is.numeric) %>%
    dplyr::summarise_all(c("min", "median", "mean", "max", "sd",
                    "skewness", "kurtosis", "p1", "p10", "p25",
                    "p33", "p66", "p75", "p90", "p99"))

  dcat = df %>% dplyr::group_by(!!poligon_id) %>% dplyr::select_if(is.factor) %>%
    dplyr::summarise_all(moda) %>% dplyr::mutate_if(is.character, as.factor)

  df = dplyr::left_join(dcat,dnum)
  return(df)
}

p1 <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  quantile(x, 0.01)
}

p10 <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  quantile(x, 0.10)
}

p25 <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  quantile(x, 0.25)
}

p33 <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  quantile(x, 0.33)
}

p66 <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  quantile(x, 0.66)
}


p75 <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  quantile(x, 0.75)
}

p90 <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  quantile(x, 0.90)
}

p99 <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  quantile(x, 0.99)
}


moda <- function(x) {
  w = table(x);
  w1 = w[max(w)==w]
  return(names(w1)[1])
}


kurtosis <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  summ <- sums(x, 4)
  n <- length(x)
  p1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  p2 <- (3 * (n - 1) ^ 2) / ((n - 2) * (n - 3))
  kurt <- (p1 * summ) - p2
  return(kurt)
}


skewness <- function(x) {
  if(!is.numeric(x)) {
    stop('x must be numeric')
  }
  summ <- sums(x, 3)
  n <- length(x)
  skew <- (n / ((n - 1) * (n - 2))) * summ
  return(skew)
}


