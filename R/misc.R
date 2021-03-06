



#' format time data
#' @title hms difrence time
#' @param start initial time
#' @param end final time
#' @author  Nathan Russell https://github.com/nathan-russell
#' @export
hms_span <- function(start, end) {
  dsec <- as.numeric(difftime(end, start, units = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor( (dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600 * hours - 60 * minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }),
    collapse = ":"
  )
}

#' calculate and format time until now
#' @title time until now
#' @param start initial time
#' @export
until_now <- function(start) {
  end <- Sys.time()
  dsec <- as.numeric(difftime(end, start, units = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor( (dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600 * hours - 60 * minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }),
    collapse = ":"
  )
}

#' create tablea from a list of strings
#' @title  transform a vector of strings in a table do be printed
#' @param txt vector of characters (strings)
#' @param n.col number  of columns to be created
#' @param num.digits number of decimal places to numeric values
#' @importFrom knitr kable
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

#' Save ggplot graphics
#' @title Save ggplot graphics
#' @param object ggplot graphic
#' @param graphic_format format to be saved ("jpg", "png")
#' @param file_path path and file name of graphic arquive to be saved
#' @param units unit of measure ("cm", "inch")
#' @param width width of graphic in units of measure
#' @param height height  of graphic in units of measure
#' @param dpi dot per inch resoluition
#' @importFrom ggplot2 ggsave
#' @export
save_gggraphics <- function(object, graphic_format = c("jpg", "png"),
                            file_path, width = 15, height = 15,
                            units = "cm", dpi = 100) {
  nf <- length(format)
  for (i in 1:nf) {
    ggplot2::ggsave(
      object, file = paste0(file_path, ".", graphic_format[i]),
      width = width, height = height, units = units, dpi = dpi
    )
  }
  invisible(NULL)
}
#' Calculates distance from points
#' @title calculates distance from points
#' @param px vector of x coordinates
#' @param py vector of y coordinates
#' @param nx number of divisions in x axis
#' @param ny number of divisions in y axis
#' @export
getdist_rectangle <- function(px, py, nx, ny) {
  maxy <- max(py, na.rm = T)
  maxx <- max(px, na.rm = T)
  miny <- min(py, na.rm = T)
  minx <- min(px, na.rm = T)
  vx <- seq(from = minx, to = maxx, length.out = nx)
  vy <- seq(from = miny, to = maxy, length.out = ny)
  gr <- expand.grid(x = vx, y = vy)
  l <- length(py)
  c <- nrow(gr)
  df1 <- matrix(nrow = l, ncol = c)
  cont <- 1
  for (i in 1:c) {
    df1[, cont] <- sqrt( (gr$y[i] - py) ^ 2 + (gr$x[i] - px) ^ 2)
    names(df1)[i] <- paste("dist_", i, sep = "")
    cont <- cont + 1
  }
  df1 <- data.frame(df1)
  return(df1)
}

#' Rasterize coordinates
#' @title Rasterize coordinates
#' @description  Create rasters of coordinates x and y
#' @param fileraster raster file raster to extract bounds, resolution and projection information
#' @param path_output path to output rasters files
#' @param prefix_nama prefix to use to create name of rasters files
#' @param format format of raster files ('.asc', '.tif', '.grd', '.sdat'..etc). See Help of writeRaster function of raster package to more formats
#' @importFrom raster rasterToPoints rasterFromXYZ writeRaster raster
#' @examples
#' \dontrun{0
#' rasterize_coordinates(fileraster = './raster/bio_14.asc', path_output = './raster', prefix_name = 'coord_', format = '.tif')
#' }
#' @export
rasterize_coordinates <- function(fileraster, path_output = '.', prefix_name = 'coord_', format = '.asc') {
  r <- raster::raster(fileraster)
  p <- raster::rasterToPoints(r, spatial = T)
  xyzx <- data.frame(x = p@coords[,1], y = p@coords[,2], z = p@coords[,1], stringsAsFactors = F)
  xyzy <- data.frame(x = p@coords[,1], y = p@coords[,2], z = p@coords[,2], stringsAsFactors = F)
  rx <- raster::rasterFromXYZ(xyz = xyzx, res = c(xres(r), yres(r)), crs = r@crs)
  ry <- raster::rasterFromXYZ(xyz = xyzy, res = c(xres(r), yres(r)), crs = r@crs)
  fnx = paste0(path_output,'/',prefix_name,'x',format)
  fny = paste0(path_output,'/',prefix_name,'y',format)
  raster::writeRaster(rx, fnx,  overwrite = TRUE)
  raster::writeRaster(ry, fny,  overwrite = TRUE)
}


#' grid distance rasterize
#' @title Grid Distance rasterize
#' @description  Create rasters of distances to a grid
#' @param fileraster raster file raster to extract bounds, resolution and projection information
#' @param path_output path to output rasters files
#' @param nx number of divisions in x axis
#' @param ny number of divisions in y axis
#' @param format format of raster files ('.asc', '.tif', '.grd', '.sdat'..etc). See Help of writeRaster function of raster package to more formats
#' @importFrom raster rasterToPoints rasterFromXYZ writeRaster raster
#' @examples
#' \dontrun{0
#' grid_rasterize('bio_14.asc', path_output = '.', nx = 3, ny = 4, format = '.asc')
#' }
#' @export

grid_dist_rasterize <- function(fileraster, path_output = '.', nx = 3, ny = 3,
                           format = '.asc') {
  r <- raster(fileraster)
  p <- raster::rasterToPoints(r, spatial = T)
  xy <- data.frame(p@coords)
  dxy <- labgeo::getdist_rectangle(xy$x, xy$y, nx, ny)
  nr <- nx * ny
  for (i in 1:nr) {
    xyz <- data.frame(xy,dxy[i])
    nv <- names(dxy)[i]
    names(xyz)[3] = nv
    rx <- raster::rasterFromXYZ(xyz = xyz, crs = r@crs)
    fnx <- paste0(path_output,'/', nv, format)
    raster::writeRaster(rx, fnx,  overwrite = TRUE)
    print(fnx)
  }

}





#' @importFrom utils sessionInfo
#' @importFrom parallel clusterExport makeCluster detectCores parLapply stopCluster
mclapply.hack <- function(...) {
  library(parallel)
  size_of_list <- length(list(...)[[1]])
  cl <- parallel::makeCluster(min(size_of_list, parallel::detectCores()))
  loaded_package_names <- c(
    utils::sessionInfo()$basePkgs,
    names(utils::sessionInfo()$otherPkgs)
  )
  tryCatch({
      this_env <- environment()
      while (identical(this_env, globalenv()) == FALSE) {
        parallel::clusterExport(
          cl,
          ls(all_names = TRUE, env = this_env),
          envir = this_env
        )
        this_env <- parent.env(environment())
      }
      parallel::clusterExport(
        cl,
        ls(all.names = TRUE, env = globalenv()),
        envir = globalenv()
      )

      parallel::parLapply(cl, 1:length(cl), function(xx) {
        lapply(loaded_package_names, function(yy) {
          require(yy, character.only = TRUE)
        })
      })
      return(parallel::parLapply(cl, ...))
    },
    finally = {
      parallel::stopCluster(cl)
    }
  )
}

# create code for snipptes using information in clipboar
clip2snippet <- function() {
  x <- readClipboard(format = 1, raw = FALSE)

  for (i in seq_along(x)) {
    s1 <- "`r paste('"
    s2 <- x[i]
    if (s2 == "") {
      s2 <- "\n"
    }
    s3 <- "')`"
    vx[i] <- paste(s1, s2, s3, sep = "")
  }
  vx
  writeClipboard(vx, 1)
}
