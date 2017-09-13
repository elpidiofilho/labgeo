#' fold poligon - separete samples of whole polygons to cross validation
#'
#' This function separates samples into polygons for cross-validation so that all pixels of a
#' polygon are allocated to a single fold. This avoids overfitting of the result due to spatial
#' correlation present in geographically close samples.
#'
#' @param nfold Number of folds to be created
#' @param poligon Vector with the sampling polygons
#' @param seed number tp seed generator
#' @return  A list with folds;. Each fold contains line number of data to be uses in cross validation
#' @details details
#' @importFrom caret createFolds
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' fold_poligon(nfold = 10, poligon =df$poligon)
#' }
#' @export


fold_poligon <- function(nfold, poligon, seed = 123) {
  lfold <- list(nfold)
  dfdata <- data.frame(id = seq_len(length(poligon)), poligon)
  set.seed(seed)
  vu <- unique(poligon)
  dp <- caret::createFolds(vu, nfold, returnTrain = TRUE)

  for (i in seq_len(nfold)) {
    v <- vu[dp[[i]]]
    vp <- (dfdata %>%
      dplyr::filter(poligon %in% v))[, 1]
    lfold[[i]] <- vp
  }
  return(lfold)
}
