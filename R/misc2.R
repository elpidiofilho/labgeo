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
