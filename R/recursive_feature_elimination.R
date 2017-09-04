# Recursive Feature Elimination
#'
#'
#' This function performs an recursive feature elimination using the rfe function from caret package
#' @param df   dataframe, with income variable in the first column
#' @param index  Users cross validation folds. Default = NULL
#' @param nfolds   Number of folds to be build in cross-validation
#' @param sizes A numeric vector of integers corresponding to the number of features that should be retained
#' @param fun A list of functions for model fitting, prediction and variable importance
#' @param cpu_cores  Number of CPU cores to be used in parallel processing
#' @keywords Recursive Feature Elimination
#' @export
#' @examples
#' rfe(df,sizes = c(2:10,12,15,20), ncores = 4)




recursive_feature_elimination <- function(df, sizes = c(2:5,10), index = NULL, nfolds = 5,
                                          fun = rfFuncs, ncores = 6) {
  suppressPackageStartupMessages(require(caret))
  suppressPackageStartupMessages(require(doParallel))
  if (!is.data.frame(df)) stop("df is not a dataframe")
  inicio = Sys.time()
  set.seed(313)
  #if (is.null(index)) index = createFolds(df[,1], nfolds)
  seeds <- vector(mode = "list", length = nfolds + 1)
  for(i in 1:nfolds) seeds[[i]] = sample.int(n=1000, 50)
  seeds[[nfolds + 1]] <- sample.int(1000, 1)
  #index = createFolds(df[,1], k = nfolds, list = T, returnTrain = T)
  formula = as.formula(paste(names(df)[1],"~ .",sep = ""))
  if (ncores > 0) {
    cl <- makePSOCKcluster(cpu_cores)
    registerDoParallel(cl)
  }
  if (is.null(index)) {
  rfProfile <- rfe(formula = formula, data = df,  sizes = sizes,
                   rfeControl = rfeControl(method = "cv",functions = fun, number = nfolds,
                                           seeds = seeds))
  } else {
    rfProfile <- rfe(formula = formula, data = df,  sizes = sizes,
                     rfeControl = rfeControl(method = "cv",functions = fun, number = nfolds,
                                             seeds = seeds, index = index))

  }
  if (!is.null(cl)) {
    stopCluster(cl)
  }

  print(paste("time elapsed :", Sys.time() - inicio))
  return(rfProfile)
}
