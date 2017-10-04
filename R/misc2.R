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

#' @export
comma_to_point <- function(x, tonumeric = TRUE) {
  if (tonumeric) {
    return(as.numeric(gsub(",", ".", x)))
  } else {
    return(gsub(",", ".", x))
  }
}
