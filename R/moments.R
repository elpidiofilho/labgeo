# moments
#'
#' @title moments - This function calculates the univariate statistics of a variable in vector form
#' @param x vetor wiht data
#' @param plot  define if plot the graphics
#' @param na.rm   remove nodata before calcululate moments
#' @details details
#' @description code from moments package https://cran.r-project.org/web/packages/moments/index.html
#' @examples
#' \dontrun{
#' vt = c(12,22,34,26,5,19)
#' print(moments(v))
#' }
#' @export




moments <- function(x, plot = FALSE, na.rm = TRUE) {
  if (!length(x) >= 3)
    stop("Not enought values to represent a distribution")
  .skew <- function(x, na.rm = FALSE) {
    if (na.rm)
      x <- x[!is.na(x)]
    sum((x - mean(x)) ^ 3) / (length(x) * stats::sd(x) ^ 3)
  }
  .kurt <- function(x, na.rm = FALSE) {
    if (na.rm)
      x <- x[!is.na(x)]
    sum((x - mean(x)) ^ 4) / (length(x) * stats::var(x) ^ 2) - 3
  }
  .cv <- function(x) {
    (stats::sd(x) / mean(x)) * 100
  }
  .means <- function(x) {
    arithmetic <- function(x) {
      sum(x) / length(x)
    }
    (x <- c(arithmetic(x)))
  }
  .dmode <- function(x) {
    den <- stats::density(x, kernel = c("gaussian"))
    (den$x[den$y == max(den$y)])
  }
  .n.modes <- function(x) {
    den <- stats::density(x, kernel = c("gaussian"))
    den.s <- stats::smooth.spline(den$x, den$y, all.knots = TRUE, spar = 0.8)
    s.0 <- stats::predict(den.s, den.s$x, deriv = 0)
    s.1 <- stats::predict(den.s, den.s$x, deriv = 1)
    s.derv <- data.frame(s0 = s.0$y, s1 = s.1$y)
    nmodes <- length(rle(den.sign <- sign(s.derv$s1))$values) / 2
    if ((nmodes > 10) == TRUE) {
      nmodes <- 10
    }
    if (is.na(nmodes) == TRUE) {
      nmodes <- 0
    }
    (nmodes)
  }
  r <- c(
    min(x), stats::quantile(x, 0.25), .means(x)[1], stats::quantile(x, 0.5), stats::quantile(x, 0.75),
    max(x), stats::sd(x), stats::var(x), .cv(x), stats::mad(x), .skew(x), .kurt(x), .n.modes(x), .dmode(x)
  )
  names(r) <- c(
    "min", "25th", "mean", "median", "75th", "max", "stdv", "var", "cv", "mad", "skew",
    "kurt", "nmodes", "mode"
  )
  if (plot == TRUE) {
    graphics::plot(stats::density(x), type = "n", main = "", ylab = "DENSITY", xlab = "RANGE", )
    graphics::polygon(stats::density(x), col = "blue")
    graphics::abline(v = min(x), lty = 1, lwd = 1, col = "black")
    graphics::abline(v = max(x), lty = 1, lwd = 1, col = "black")
    graphics::abline(v = stats::quantile(x, 0.25), lty = 2, lwd = 1, col = "black")
    graphics::abline(v = stats::quantile(x, 0.75), lty = 2, lwd = 1, col = "black")
    graphics::abline(v = .dmode(x), lty = 3, lwd = 1, col = "red")
    graphics::legend(
      "topright", lty = c(1, 1, 2, 2, 3), lwd = c(1, 1, 1, 1, 1), bty = "n",
      legend = c(
        "MIN", "MAX", "25th",
        "75th", "MODE"
      ), col = c("black", "black", "black", "black", "red")
    )
  }
  return(r)
}

