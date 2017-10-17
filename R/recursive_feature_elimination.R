#' Recursive Feature Elimination
#'
#' This function performs an recursive feature elimination using the rfe function from caret package.
#' This implentation of RFE uses parallel processing
#'
#' This function implements backwards selection of predictors based on
#' predictor importance ranking. The predictors are ranked and the less
#' important ones are sequentially eliminated prior to modeling. The goal is to
#' find a subset of predictors that can be used to produce an accurate model.
#' The web page \url{http://topepo.github.io/caret/recursive-feature-elimination.html#rfe}
#' has more details and examples related to this function.
#'
#'
#' @param df   dataframe, with income variable in the first column
#' @param index  Users cross validation folds. Default = NULL
#' @param nfolds   Number of folds to be build in cross-validation. Default = 10
#' @param repeats repeats
#' @param sizes A numeric vector of integers corresponding to the number of features
#'  that should be retained. Default = c(2:5,10)
#' @param fun Default = rfFuncs , get importance values from Random Forest model.
#' @param cpu_cores  Number of CPU cores to be used in parallel processing.
#'                   Default = 6. For avoid parallel execution set this parameter to zero.
#' @param metric metric used to evaluate model fit. For numeric outcome possible values are
#'               ("RMSE", "Rsquared) and for categorical outcome ("Accuracy","Kappa")
#' @param seeds seeds
#' @param verbose  print results and execution time of function
#' @keywords Recursive Feature Elimination
#' @details  details
#' @importFrom parallel makePSOCKcluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom caret trainControl rfe rfeControl rfFuncs
#' @importFrom stats as.formula
#' @author RFE by Max Kuhn
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
                                          repeats = 1,
                                          fun = rfFuncs,
                                          cpu_cores = 6,
                                          metric = ifelse(is.factor(df[, 1]), "Kappa", "Rsquared"),
                                          seeds = NULL,
                                          verbose = TRUE) {

  cl = NULL
  if (!is.data.frame(df)) stop("df is not a dataframe")
  if (is.null(metric)) {
    if (is.numeric(df[, 1])) {
      metric <- "Rsquared"
    } else {
      metric <- "Kappa"
    }
  }

  if (nfolds == 0) {
    method <- "none"
    tune_length <- NULL
  } else {
    if (nfolds >= nrow(df)) {
      method <- "LOOCV"
    } else {
      method <- "CV"
    }
    if (repeats > 1) method <- "repeatedcv"
  }


  totalSize <- if(any(sizes == (ncol(df)) -1)) length(sizes) else length(sizes) + 1
  if(is.null(seeds)){
    seedsvec <- NULL
  } else {
    set.seed(seeds)
    vseeds <- vector(mode = "list", length = (nfolds * repeats) + 1)
    vseeds <- lapply(vseeds, function(x) sample.int(n = 100000, size = totalSize))
    vseeds[[nfolds * repeats + 1]] <- sample.int(n = 100000, size = 1)
    seedsvec <- vseeds
  }

#  if (is.null(seeds)) {
#    seedsvec <- NULL
#  } else {
#    set.seed(seeds)
#    seedsvec <- vector(mode = "list", length = nfolds + 1)
#    for (i in 1:nfolds) seedsvec[[i]] <- sample.int(n = 1000, 400)
#    seedsvec[[nfolds + 1]] <- sample.int(1000, 1)
#  }


  inicio <- Sys.time()
  formula <- as.formula(paste(names(df)[1], "~ .", sep = ""))
  if (cpu_cores > 0) {
    cl <- parallel::makePSOCKcluster(cpu_cores)
    doParallel::registerDoParallel(cl)
  }
  if (is.null(index)) {
    rfProfile <- caret::rfe(
      formula = formula, data = df, sizes = sizes, metric = metric,
      rfeControl = rfeControl(
        method = "cv", functions = fun, number = nfolds,
        seeds = seedsvec
      )
    )
  } else {
    rfProfile <- caret::rfe(
      formula = formula, data = df, sizes = sizes, metric = metric,
      rfeControl = rfeControl(
        method = "cv", functions = fun, number = nfolds,
        seeds = seedsvec, index = index
      )
    )
  }
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  if (verbose == TRUE) {
    print("=======================================================================")
    print("Recursive Feature Elimination")
    print(paste("outcome : ", names(df)[1], sep = ""))
    print(paste("Selected vars :", paste(rfProfile$optVariables, collapse = ",")))
    print(paste("time elapsed", hms_span(inicio, Sys.time())))
  }
  return(rfProfile)
}



#' Recursive Feature Elimination plot results
#'
#' This function plot results of Recursive Feature Elimination (RFE)
#'
#'
#' @param fit.rfe Results from recursive_feature_elimination  function
#' @keywords Recursive Feature Elimination results plot
#' @details  details
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @importFrom dplyr %>% filter select
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_point geom_line
#' @importFrom ggplot2 geom_smooth scale_x_continuous
#' @importFrom ggplot2 geom_label facet_wrap ylab  xlab
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 theme_bw
#' @importFrom knitr kable
#' @examples
#' \dontrun{
#' rfe_result(fi.rfe)
#' }
#' @export

rfe_result <- function(fit.rfe) {
  value <- variables <- tol <- Value <- Variables <- NULL
  ddd <- fit.rfe$results
  mx <- max(ddd$results[,3])
  wm <- which.max(ddd[,3])
  ddd$tol <- NA
  for (i in 1:wm) {
    ddd$tol[i] <- abs(ddd[i, 3] - ddd[wm, 3])/  ddd[wm, 3] * 100
  }
  dddg <- ddd %>% na.omit %>% tidyr::gather(key = var, value = Value, -Variables )
  g1 <- ggplot2::ggplot(dddg, aes(x = Variables, y = Value)) +
    ggplot2::geom_line()  +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::scale_x_continuous(breaks = dddg$Variables) +
    ggplot2::facet_wrap(~var, scales = 'free')

  ddd.na <- ddd %>% na.omit
  g2 <- ggplot2::ggplot(data = ddd.na , aes(x = Variables, y = tol)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_label(label = round(ddd.na$tol, 1), nudge_y = 0.5) +
    # ggplot2::xlim(min(ddd.na$Variables), ddd.na$Variables[wm])  +
    ggplot2::scale_x_continuous(breaks = ddd.na$Variables) +
    ggplot2::scale_y_continuous(breaks = c(0:round(max(ddd.na$tol)))) +
    ggplot2::ylab("tolerance (%)") + xlab('number of selected variables')
  ggplot2::theme_bw()
  print(g1)
  print(g2)
  knitr::kable(ddd[1:wm,], digits = 3, col.names = names(ddd))
  return(ddd)
}
