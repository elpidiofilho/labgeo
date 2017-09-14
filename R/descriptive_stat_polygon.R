# Descriptive statistics polygon
#

#' @title Descriptive statistics polygon
#' @description This function calculates Descriptive statistics for samples collected in polygon
#' @param df dataframe
#' @param poligon name of column thats contains polygon id
#' @param desc parameters os descritive statistics to be calculated.Choices are : c("min", "median", "mean", "max", "sd","skewness", "kurtosis", "p1", "p10", "p25","p33", "p66", "p75", "p90", "p99")
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise_all
#' @importFrom dplyr select_if
#' @importFrom dplyr mutate_if
#' @importFrom dplyr left_join
#' @importFrom dplyr collect
#' @importFrom multidplyr cluster_copy partition
#' @importFrom stats quantile
#' @importFrom rlang quo
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @keywords statics polygon
#' @details calculates Descriptive statistics for samples collected in polygon
#' @examples
#' \dontrun{
#' b = descriptive_stat_polygon(df,"id")
#' }
#' @export



descriptive_stat_polygon <- function(df, poligon,
                                     desc = c("min", "mean", "max", "sd")) {
  #  c("min", "median", "mean", "max", "sd","skewness", "kurtosis",
  # "p1", "p10", "p25","p33", "p66", "p75", "p90", "p99")
  inicio <- Sys.time()
  vpc <- c("p1", "p10", "p25", "p33", "p66", "p75", "p90", "p99")
  pdesc <- desc[vpc %in% desc]
  dfselnum <- df %>%
    dplyr::select_if(is.numeric)

  poligon_id = rlang::quo(poligon)
  d1 <- multidplyr::partition(dfselnum, polig)
  multidplyr::cluster_copy(d1, p1)
  multidplyr::cluster_copy(d1, p10)
  multidplyr::cluster_copy(d1, p25)
  multidplyr::cluster_copy(d1, p33)
  multidplyr::cluster_copy(d1, p66)
  multidplyr::cluster_copy(d1, p75)
  multidplyr::cluster_copy(d1, p90)
  multidplyr::cluster_copy(d1, p99)
  multidplyr::cluster_copy(d1, px)
  multidplyr::cluster_copy(d1, poligon_id)
  dnum <- d1 %>%
    dplyr::summarise_all(desc) %>%
    dplyr::collect()
  dcat <- df %>%
    dplyr::group_by(polig) %>%
    dplyr::select_if(is.factor) %>%
    dplyr::summarise_all(moda) %>%
    dplyr::mutate_if(is.character, as.factor)
  df <- dplyr::left_join(dcat, dnum)
  print(paste("time elapsed", hms_span(inicio, Sys.time())))
  return(df)
}

px <- function(x, q) {
  n <- length(x)
  vs <- sort(x, method = "shell")
  np <- round(q * length(x), 0)
  if (np > 1) {
    vq <- vs[np]
    names(vq) <- paste(q, "%", sep = "")
    return(vq)
  } else {
    return(NA)
  }
}


p1 <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  px(x, 0.01)
}

p10 <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  px(x, 0.10)
}

p25 <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  px(x, 0.25)
}

p33 <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  px(x, 0.33)
}

p66 <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  px(x, 0.66)
}


p75 <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  px(x, 0.75)
}

p90 <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  px(x, 0.90)
}

p99 <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  px(x, 0.99)
}


moda <- function(x) {
  w <- table(x)
  w1 <- w[max(w) == w]
  return(names(w1)[1])
}


sums <- function(x, pot) {
  return(sum((x - mean(x)) ^ pot))
}

kurtosis <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  summ <- sums(x, 4)
  n <- length(x)
  p1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  p2 <- (3 * (n - 1) ^ 2) / ((n - 2) * (n - 3))
  kurt <- (p1 * summ) - p2
  return(kurt)
}


skewness <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  summ <- sums(x, 3)
  n <- length(x)
  skew <- (n / ((n - 1) * (n - 2))) * summ
  return(skew)
}
