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
#' @param metric metric used to evaluate model fit. For numeric outcome ("RMSE", "Rsquared) and for categorical outcome ("Accuracy","Kappa")
#' @param seeds seeds
#' @param verbose verbose
#' @keywords Recursive Feature Elimination
#' @details  details
#' @importFrom parallel makePSOCKcluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom caret trainControl rfe rfeControl rfFuncs
#' @importFrom stats as.formula
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' rfe(df,sizes = c(2:10,12,15,20), cpu_cores = 4)
#' }
#' @export


recursive_feature_elimination <- function(df,
                                          sizes = c(2:5, 10),
                                          index = NULL,
                                          nfolds = 5,
                                          fun = rfFuncs,
                                          cpu_cores = 6,
                                          metric = ifelse(is.factor(df[,1]),"Kappa", "Rsquared"),
                                          seeds = NULL,
                                          verbose = TRUE){
  if (is.null(metric)){
    if (is.numeric(df[, 1])) {
      metric <- "Rsquared"
    } else {
      metric <- "Kappa"
    }
  }
  if (nfolds == 0 ){
    method <- "none"
    tune_length <- NULL
  } else {
    if (nfolds >= nrow(df)){
      method <- "LOOCV"
    } else {
      method <- "CV"
    }
  }
  if (!is.data.frame(df)) stop("df is not a dataframe")
  inicio <- Sys.time()
  #index = createFolds(df[,1], k = nfolds, list = T, returnTrain = T)
  formula <- as.formula(paste(names(df)[1], "~ .", sep = ""))
  if (cpu_cores > 0) {
    cl <- parallel::makePSOCKcluster(cpu_cores)
    doParallel::registerDoParallel(cl)
  }
  if (is.null(index)) {
  rfProfile <- caret::rfe(formula = formula, data = df,  sizes = sizes, metric = metric,
                   rfeControl = rfeControl(method = "cv", functions = fun, number = nfolds,
                                           seeds = seeds))
  } else {
    rfProfile <- caret::rfe(formula = formula, data = df,  sizes = sizes, metric = metric,
                     rfeControl = rfeControl(method = "cv", functions = fun, number = nfolds,
                                             seeds = seeds, index = index))

  }
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  if (verbose == TRUE) {
    print("=======================================================================")
    print(paste("outcome : ",names(df)[1],sep=""))
    print(paste('Selected vars :',paste(rfProfile$optVariables, collapse = ",")))
    print(paste("time elapsed :", round((Sys.time() - inicio),3)))
  }
  return(rfProfile)
}
