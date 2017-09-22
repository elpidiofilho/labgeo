

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

rescale <- function(x, to = c(0, 1),
                    from = range(x, na.rm = TRUE, finite = TRUE)) {
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

## code by Nathan Russell https://github.com/nathan-russell
## https://stackoverflow.com/questions/32100133/print-the-time-a-script-has-been-running-in-r
#' @export
hms_span <- function(start, end) {
  dsec <- as.numeric(difftime(end, start, unit = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor((dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600 * hours - 60 * minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }), collapse = ":"
  )
}


#' @export
to_table <- function(txt, n.col, num.digits = 3) {
  n.col <- 4
  num.elem <- length(txt)
  divint <- ceiling(num.elem / n.col) * n.col
  dif <- divint - num.elem
  if (dif > 0) {
    complete <- rep(" ", times = dif)
    txt_complete <- c(txt, complete)
  } else {
    txt_complete <- txt
  }
  mx <- matrix(txt_complete, ncol = n.col)
  names(mx) <- 1:n.col
  dd <- knitr::kable(mx, col.names = 1:n.col, digits = num.digits)
  return(dd)
}



#' @export
save_gggraphics <- function(object, graphic_format = c("jpg", "png"),
                         file_path, width = 15, height = 15,
                         units = "cm", dpi = 100) {
  nf = length(format)
  for (i in 1:nf) {
    ggplot2::ggsave(object, file = paste0(file_path, ".", graphic_format[i]),
                    width = width, height = height, units = units, dpi = dpi)
  }
  invisible(NULL)
}

#' @export
getdist_rectangle <- function(px,py,nx,ny) {
  maxy <- max(py)
  maxx <- max(px)
  miny <- min(py)
  minx <- min(px)
  vx <- seq(from = minx, to = maxx, length.out = nx)
  vy <- seq(from = miny, to = maxy, length.out = ny)
  gr <- expand.grid(x = vx, y = vy)
  l <- length(py)
  c <- nrow(gr)
  df1 <- matrix(nrow = l, ncol = c)
  cont <- 1
  for (i in 1:c) {
    df1[,cont] <- sqrt((gr$y[i] - py) ^ 2 + (gr$x[i] - px) ^ 2)
    names(df1)[i] <- paste("dist_",i,sep="")
    cont <- cont + 1
  }
  df1 <- data.frame(df1)
  return(df1)
}




mclapply.hack <- function(...) {
library(parallel)
    ## Create a cluster
  ## ... How many workers do you need?
  ## ... N.B. list(...)[[1]] returns the first
  ##          argument passed to the function. In
  ##          this case it is the list to iterate over
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster( min(size.of.list, detectCores()) )

  ## Find out the names of the loaded packages
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))

  ## N.B. tryCatch() allows us to properly shut down the
  ##      cluster if an error in our code halts execution
  ##      of the function. For details see: help(tryCatch)
  tryCatch( {

    ## Copy over all of the objects within scope to
    ## all clusters.
    ##
    ## The approach is as follows: Beginning with the
    ## current environment, copy over all objects within
    ## the environment to all clusters, and then repeat
    ## the process with the parent environment.
    ##
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    ## repeat for the global environment
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())

    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        ## N.B. the character.only option of
        ##      require() allows you to give the
        ##      name of a package as a string.
        require(yy , character.only=TRUE)})
    })

    ## Run the lapply in parallel
    return( parLapply( cl, ...) )
  }, finally = {
    ## Stop the cluster
    stopCluster(cl)
  })
}
