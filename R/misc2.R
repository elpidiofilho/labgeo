#' @export
train_test = function(df, p = 0.75, groups = min(10,length(y)), seed = NULL){
  if (is.null(seed) == FALSE) {
    set.seed(seed)
  }
  vcdp <- caret::createDataPartition(df[,1], p = p, list = FALSE)
  train = df[vcdp, ]
  test = df[-vcdp, ]
  return(list(train = train, test = test))
}

#' @export
create_dummy <- function(df) {
  od = caret::dummyVars(~ ., data = df)
  dfr = predict(od, df) %>% data.frame()
  return(dfr)
}

#' @export
remove_extra_spaces <- function(x) {
  return(gsub("^ *|(?<= ) | *$", "", x, perl = TRUE))
}


#' Convert comma to point
#'
#' @export
comma_to_point <- function(x, tonumeric = TRUE) {
  if (tonumeric) {
    return(as.numeric(gsub(",", ".", x)))
  } else {
    return(gsub(",", ".", x))
  }
}

# Create polygon of Region of Interest (ROI)
#' code by Jeffrey Evans
#' @importFrom raster extent
#' @importFrom sp proj4string
#' @param extent extent of region of interest
#' @param extent projection of region of  interest
#' @export
#' @examples
#' \dontrun{
#' ext <- as(raster::extent(78.46801, 78.83157, 19.53407, 19.74557), "SpatialPolygons")
#' proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' pol.roi = roio(ext, proj)
#'}

roi <- function(extent, project) {
  e = as(raster::extent(extent), "SpatialPolygons")
  proj4string(e) <- project
  return(e)
}
