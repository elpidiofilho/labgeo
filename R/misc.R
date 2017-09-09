

"%nin%" <- function(x, y) match(x, y, nomatch = 0) == 0

## from scales package
## https://github.com/hadley/scales

zero_range <- function(x, tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1)
    return(TRUE)
  if (length(x) != 2)
    stop("x must be length 1 or 2")
  if (any(is.na(x)))
    return(NA)
  if (x[1] == x[2])
    return(TRUE)
  if (all(is.infinite(x)))
    return(FALSE)
  m <- min(abs(x))
  if (m == 0)
    return(FALSE)
  abs((x[1] - x[2]) / m) < tol
}

rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) {
  if (zero_range(from) || zero_range(to)) {
    return(ifelse(is.na(x), NA, mean(to)))
  }
  (x - from[1]) / diff(from) * diff(to) + to[1]
}


## from library MASS
## https://github.com/cran/MASS/blob/master/R/kde2d.R

kde2d <- function(x, y, h, n = 25, lims = c(range(x), range(y))) {
  nx <- length(x)
  if (length(y) != nx)
    stop("data vectors must be the same length")
  if (any(!is.finite(x)) || any(!is.finite(y)))
    stop("missing or infinite values in the data are not allowed")
  if (any(!is.finite(lims)))
    stop("only finite values are allowed in 'lims'")
  n <- rep(n, length.out = 2L)
  gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
  gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
  h <- if (missing(h)) c(bandwidth.nrd(x), bandwidth.nrd(y))
    else rep(h, length.out = 2L)
  if (any(h <= 0))
    stop("bandwidths must be strictly positive")
  h <- h / 4 # for S's bandwidth scale
  ax <- outer(gx, x, "-") / h[1L]
  ay <- outer(gy, y, "-") / h[2L]
  z <- tcrossprod(matrix(dnorm(ax),, nx), matrix(dnorm(ay),, nx)) / (nx * h[1L] * h[2L])
  list(x = gx, y = gy, z = z)
}
