#' Train Test split
#' @title train test split
#' @importFrom caret createDataPartition
#' @param df dataframe
#' @param y outcome variable
#' @param p proportion between  train and test sets
#' @param groups numbre of groups to be used in stratified sample
#' @param seed seed
#' @export
train_test <- function(df, y,  p = 0.75, groups = min(5,length(y)), seed = NULL) {
  if (is.null(seed) == FALSE) {
    set.seed(seed)
  }
  vcdp <- caret::createDataPartition(y = y, p = p,  list = FALSE, groups = groups)
  train <- df[vcdp, ]
  test <- df[-vcdp, ]
  dffactor = df %>% select_if((is.factor)) %>% data.frame()
  nc = ncol(dffactor)
  if (nc > 0) {
    dfftrain = train %>% select_if((is.factor)) %>% data.frame()
    dfftest  = test %>%  select_if((is.factor)) %>% data.frame()
    i = 1
    for(i in 1:nc) {
      lg = levels(droplevels(dffactor[,i]))
      lt = levels(droplevels(dfftrain[,i]))
      lv = levels(droplevels(dfftest[,i]))
      v_train = (!(lg %in% lt))
      v_test = (!(lg %in% lv))

      if (sum(v_train) > 0) {
        print('---------- Warning --------------------------')
        print(paste('train set - factor', names(dffactor))[i])
        print(paste('total levels', paste(lg, collapse = ',')))
        print(paste('test levels', paste(lt, collapse = ',')))
        print(paste('missing levels', paste(lg[v_train], collapse = ',')))
      }

      if (sum(v_test) > 0) {
        print('---------- Warning --------------------------')
        print(paste('test set - factor', names(dffactor))[i])
        print(paste('total levels', paste(lg, collapse = ',')))
        print(paste('test levels', paste(lv, collapse = ',')))
        print(paste('missing levels', paste(lg[v_test], collapse = ',')))
      }
    }
  }

  return(list(train = train, test = test))
}

#' convert factor to dummy variables
#' @title convert factor to dummy variables
#' @importFrom caret dummyVars contr.ltfr
#' @importFrom dplyr '%>%'
#' @param df dataframe
#' @export
factor_to_dummy <- function(df) {
  od <- caret::dummyVars(~ ., data = df)
  dfr <- predict(od, df) %>% data.frame()
  return(dfr)
}

#' remove extra spaces
#' @title Remove extra spaces
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

#' Create polygon of Region of Interest (ROI)
#' code by Jeffrey Evans
#' @importFrom raster extent
#' @importFrom sp proj4string
#' @importFrom sp proj4string
#' @importFrom raster extent
#' @param extent projection of region of  interest
#' @param project projection string (ex: "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#' @export
#' @examples
#' \dontrun{
#' ext <- as(raster::extent(78.46801, 78.83157, 19.53407, 19.74557), "SpatialPolygons")
#' proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' pol.roi = roio(ext, proj)
#' }

roi <- function(extent, project) {
  e <- as(raster::extent(extent), "SpatialPolygons")
  sp::proj4string(e) <- project
  return(e)
}

#' Retrive list of caret supported models
#' @title caret models
#' @param regression model regression TRUE or FALSE
#' @export
#' @importFrom caret modelLookup
caret_models <- function(regression = TRUE) {
  if (regression == FALSE) {
    m <- unique(caret::modelLookup()[caret::modelLookup()$forClass, c(1)])
  } else {
    m <- unique(caret::modelLookup()[caret::modelLookup()$forReg, c(1)])
  }
  return(m)
}

#' model_tags : return list of models tags supported by caret package
#' @title models tags
#' @param is.regression if TRUE return regression models tags if FALSE return classification models tags
#' @importFrom caret getModelInfo
#' @export
model_tags <- function(is.regression = TRUE) {
  models <- caret_models(regression = is.regression)
  vtag <- character()
  for (i in seq_along(models)) {
    vi <- caret::getModelInfo(models[i])
    tags <- vi[[1]]$tags
    vtag <- c(vtag, tags)
  }
  return(sort(unique(vtag)))
}

# Select classification/ regression models by tag
#' @title select model by tag
#' @param is.regression if TRUE return regression models tags if FALSE return classification models tags
#' @param vtags vetctor of taga used in model selection
#' @importFrom caret getModelInfo
#' @export
select_model <- function(is.regression = TRUE,
                         vtags = c("Boosting", "Bagging")) {
  models <- caret_models(regression = is.regression)
  vsel <- character()
  for (i in seq_along(models)) {
    vi <- caret::getModelInfo(models[i])
    tg <- vi[[1]]$tags
    if (length(tg) > 0) {
      if (is.null(tg) == FALSE) {
        if (sum(tg %in% vtags) > 0) {
          vsel <- c(vsel, models[i])
        }
      }
    }
  }
  return(sort(vsel))
}


#' convert numeric data to factor
#' @title Numeric to Factor
#' @param vx vector of numeric
#' @param npercentil numeric the number of percintil to be splited the vector vx
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

}


#' remove spaces from a character vector
#' @title spaceless
#' @param vx character vector
#' @param symbol symbol or character to replace space
#' @export
spaceless <- function(vx, symbol = "-") {
  vx <- gsub(" ", symbol, vx)
  return(vx)
}

#' Eliminate symbol
#' @title eliminate symbols from string
#' @param vx character vector
#' @param replace symbol the will replace eliminated symbol character
#' @export
eliminate_symbol <- function(vx, replace = " ") {

  vxclean <- vx %>%
    str_to_lower() %>%

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

#' Remove symbol when it is the first character of a string
#' @title Remove first symbol
#' @param vx vector of character
#' @return vector of character with first symbol removed
#' @export
remove_first_symbol <- function(vx) {
  return(gsub("^\\P{L}*", "", vx, perl = TRUE))
}


#' Convert string space to any symbol
#' @title space to symbol
#' @param vx character vector
#' @param symbol symbol the will replace space character
#' @export
space_to_symbol <- function(vx, symbol) {
  return(vx %>% str_replace_all(" ", symbol))
}

#' Convert points to uppercase letter in a string
#' @title point to camel
#' @param vx character vector
#' @return vector with points replaced by uppercase character
#' @export
point_to_camel <- function(vx) {
  capit <- function(vx) paste0(
      toupper(substring(vx, 1, 1)),
      substring(vx, 2, nchar(vx))
    )
  s1 <- sapply(
    strsplit(vx, "\\."),
    function(vx) paste(capit(vx), collapse = "")
  )
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
  `.` <- NULL
  colnames(df) <- df %>% names() %>% abbreviate(., maxlength)
  return(df)
}

#' Balanced Sample
#' @title balanced sample
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
    group_by(!! target) %>%
    sample_n(size = n, replace = TRUE) %>%
    na.omit() %>%
    data.frame()
  return(dfsample)
}

#' Group rare levels of a factor variable
#' @param vx vector with factor variable
#' @param min_num minumun frequency of a factor
#' @param other name of group of rare factos
#' @importFrom dplyr %>%  select filter pull
#' @importFrom forcats fct_collapse
#' @examples
#' \dontrun{
#' vx = train$solo_tex; min_num = 20; other = 99
#' }
#' @export

group_rare_levels <- function(vx, min_num = 20, other = 99) {
  freq <- NULL
  dd <- vx
  ddt <- table(dd)
  lev <- as.character(names(table(dd)))
  ddd <- data.frame(lev = lev, freq = as.matrix(ddt))
  ddd$lev <- as.character(ddd$lev)
  group_lev <- ddd %>% dplyr::filter(freq < min_num) %>% dplyr::pull(lev)
  newf <- forcats::fct_collapse(dd, other = group_lev)
  return(newf)
}


#' Separete Train and test samples whit group variable
#' @param df dataframe
#' @param perfilt groupp vector
#' @param p sample proportion
#' @param seed random number seed
#' @importFrom dplyr %>%  filter
#' @examples
#' \dontrun{
#' vs = train_validatio_perfil(d, d$perfil, p = 0.8, seed = 313)
#' }
#' @export

train_validation_perfil <- function(df, perfil, p = 0.8, seed = NULL) {
  vu = unique(perfil)
  n = length(vu)
  np = round(n * p)
  set.seed(seed)
  snp = sample(vu,np)
  treino = df %>% filter( perfil %in% snp)
  teste = df %>% filter(!(perfil %in% snp))
  return(list(train = treino, test = teste))
}

