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
#' @param x vector
#' @param tonumeric logical TRUE transform to numeric after change comma to point
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

#' @export
#' @importFrom caret modelLookup
caret.models <- function(model.regression = TRUE) {
  if (model.regression == TRUE) {
    m <- unique(modelLookup()[modelLookup()$forClass,c(1)])
  } else {
    m <- unique(modelLookup()[modelLookup()$forReg,c(1)])
  }
  return(m)
}

#' @export
numeric_to_factor <- function(vx, npercentil = 4) {
 return(dplyr::ntile(vx, npercentil))
}

#' @export
remove_acento <- function(vx) {
  return(chartr("áàãâéêíóõôúÚçª", "aaaaeeiooouUca", vx))
}

#' @export
spaceless <- function(x) {x <- gsub(" ", "_", x);x}


#' @export
eliminate_symbol <- function(vx, replace = " ") {
"." <- NULL
vxclean <- vx %>%
    str_to_lower() %>%
    chartr("áàãéêíóõúÚçª", "aaaeeioouUca", .) %>%
    #  iconv(., "utf-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("\\.", replace) %>%
    str_replace_all("\\,", replace) %>%
    str_replace_all(" ", replace) %>%
    str_replace_all("-", replace) %>%
    str_replace_all(" - ", replace) %>%
    str_replace_all("\\+", replace) %>%
    str_replace_all("\\*", replace) %>%
    str_replace_all("\\/", replace) %>%
    str_replace_all("\\(", replace) %>%
    str_replace_all("\\)", replace) %>%
    str_replace_all("\\'", replace) %>%
    str_replace_all("\\&", replace) %>%
    str_replace_all('\\"', replace) %>%
    str_replace_all("___", replace) %>%
    str_replace_all("__", replace)
  return(vxclean)
}

#' @export
remove_first_symbol <- function(vx) {
  return(gsub("^\\P{L}*", "", vx, perl=T))
}

#' @export
space_to_symbol <- function(vx, symbol){
  return(vx %>% str_replace_all(" ", symbol))
}

#' @export
point_to_camel <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}

#' @export
abbrev <- function(df, maxlength) {
  colnames(df) <- df %>%  names() %>% abbreviate()
}

#' @export
balanced_sample <- function(df, target, n = 100) {
  dfsample = df %>%
    group_by(!!target) %>% sample_n(size = n, replace = T)  %>%
    na.omit()  %>% data.frame()
  return(dfsample)
}
