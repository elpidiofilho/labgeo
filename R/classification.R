# Classification
#'
#'
#' This function performs the training of the chosen classifier
#' @param df.train   Training dataframe
#' @param classifier Choice of classifier to be used to train model. Uses classifier names of Caret package
#' @param nfolds     Number of folds to be build in crossvalidation
#' @param cpu_cores  Number of CPU cores to be used in parallel processing
#' @keywords Train kappa
#' @export
#' @examples
#' kappa_cv_evaluation(train,"rf",10,6)


classification <- function(df.train, classifier = "rf", index = NULL, nfolds = 10, cpu_cores = 3){
  suppressPackageStartupMessages(require(caret))
  suppressPackageStartupMessages(require(doParallel))
  if (!is.data.frame(df)) stop("df.train is not a dataframe")
  beg = Sys.time()
  formula = as.formula(paste(names(df.train)[1], "~ ."))
  set.seed(313)
  if (ncores > 0) {
    cl <- makePSOCKcluster(cpu_cores)
    registerDoParallel(cl)
  }
  if (is.null(index)) {
  fit = train(formula,data = df.train, method = classifier,
              trControl = trainControl(method = "cv", number = nfolds))
  } else {

    fit = train(formula,data = df.train, method = classifier,index = index,
                trControl = trainControl(method = "cv", number = nfolds))
  }
if (!is.null(cl)) {
    stopCluster(cl)
  }
  print(paste("Execution time: ",(Sys.time() - beg)))
  return(fit)
}


