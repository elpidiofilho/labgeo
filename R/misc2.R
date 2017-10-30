#' @title train test split
#' @importFrom caret createDataPartition
#' @param df dataframe
#' @param p proportion between  train and test sets
#' @param groups numbre of groups to be used in stratified sample
#' @export
train_test <- function(df, p = 0.75, groups = 10, seed = NULL){
  if (is.null(seed) == FALSE) {
    set.seed(seed)
  }
  vcdp <- caret::createDataPartition(df[, 1], p = p, list = FALSE, groups = groups)
  train <- df[vcdp, ]
  test <- df[-vcdp, ]
  return(list(train = train, test = test))
}

#' convert factor to dummy variables
#' @title convert factor to dummy variables
#' @importFrom caret dummyVars
#' @importFrom dplyr '%>%'
#' @param df dataframe
#' @export
factor_to_dummy <- function(df) {
  od <- caret::dummyVars(~ ., data = df)
  dfr <- predict(od, df) %>% data.frame()
  return(dfr)
}

#' remove extra spaces
#' @param vx character vector
#' @export
remove_extra_spaces <- function(vx) {
  return(gsub("^ *|(?<= ) | *$", "", vx, perl = TRUE))
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
#' @importFrom sp proj4string
#' @importFrom raster extent
#' @param extent projection of region of  interest
#' @export
#' @examples
#' \dontrun{
#' ext <- as(raster::extent(78.46801, 78.83157, 19.53407, 19.74557), "SpatialPolygons")
#' proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' pol.roi = roio(ext, proj)
#'}

roi <- function(extent, project) {
  e <- as(raster::extent(extent), "SpatialPolygons")
  sp::proj4string(e) <- project
  return(e)
}

#' @param model.regression TRUE or FALSE
#' @export
#' @importFrom caret modelLookup
caret.models <- function(model.regression = TRUE) {
  if (model.regression == TRUE) {
    m <- unique(modelLookup()[modelLookup()$forClass, c(1)])
  } else {
    m <- unique(modelLookup()[modelLookup()$forReg, c(1)])
  }
  return(m)
}

#' convert numeric data to factor
#' @title Numeric to Factor
#' @param vx vector of numeric
#' @param percentil numeric the number of percintil to be splited the vector vx
#' @return vector of factors
#' @importFrom dplyr ntile
#' @export
numeric_to_factor <- function(vx, npercentil = 4) {
 return(dplyr::ntile(vx, npercentil))
}


#' Remove acentuation
#' @param vx character vector
#' @return vector of character whitout acentuation
#' @export
remove_accentuation <- function(vx) {
  return(chartr("áàãâéêíóõôúÚçª", "aaaaeeiooouUca", vx))
}


#' remove spaces from a character vector
#' @param vx character vector
#' @param symbol symbol or character to replace space
#' @export
spaceless <- function(vx, symbol = '-') {x <- gsub(" ", symbol, vx); vx}

#' Eliminate symbol
#' @param vx character vector
#' @param replace symbol the will replace eliminated symbol character
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

#' Remove first symbol
#' Remove symbol when it is the first character of a string
#' @param vx vector of character
#' @return vector of character with first symbol removed
#' @export
remove_first_symbol <- function(vx) {
  return(gsub("^\\P{L}*", "", vx, perl = T))
}


#' Convert string space to any symbol
#' @param vx character vector
#' @param symbol symbol the will replace space character
#' @export
space_to_symbol <- function(vx, symbol){
  return(vx %>% str_replace_all(" ", symbol))
}

#' Convert points to uppercase letter in a string
#' @param vx character vector
#' @return vector with points replaced by uppercase character
#' @export
point_to_camel <- function(vx){
  capit <- function(vx) paste0(toupper(substring(vx, 1, 1)),
                              substring(vx, 2, nchar(vx)))
  s1 = sapply(strsplit(vx, "\\."), function(vx) paste(capit(vx), collapse = ""))
  return(s1)
}

#' Abbreviate column names
#'
#' @param df dataframe
#' @param maxlength max length of column names after abbreviation
#' @importFrom dplyr %>%
#' @return dataframe with names os columns abbreviated
#' @export

abbrev_colnames <- function(df, maxlength) {
  colnames(df) <- df %>%  names() %>% abbreviate()
  return(df)
}

#' Balanced Sample
#' @name  balanced_sample
#' @param df dataframe
#' @param target name of outcome variable
#' @param n number of samples
#' @importFrom dplyr group_by sample_n %>%
#' @return dataframe with balanced sample by outcome
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @export
balanced_sample <- function(df, target, n = 100) {
  dfsample <- df %>%
    group_by(!!target) %>% sample_n(size = n, replace = T)  %>%
    na.omit()  %>% data.frame()
  return(dfsample)
}
