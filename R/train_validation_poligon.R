
# train_validation_poligon
#'
#'
#' This function performs the separation of training samples and validation for samples collected in the form of polygons.
#' Note that the class has to be the first column and the polygon number has to be the second column of the dataframe.
#' @param dfall Dataframe with class and poligon columns
#' @param class string with name of variable with class value
#' @param poligon string with name of variable with poligon id
#' @param p double the percentage of data that goes to training
#' @param seed  numeric seed to control random numbers generation
#' @importFrom caret createDataPartition
#' @importFrom dplyr group_by_ slice filter select
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' dfall = df %>% select(classe, poligono, everything())
#' vt = treino_validacao_poligono(dfall, class = 'classe', poligon = 'poligono', p = 0.80, seed = 123)
#' treino = dfall[treino,]
#' valida = dfall[-treino,]
#' }
#' @export


train_validation_poligon = function (dfall, class, poligon, p = 0.75, seed = 313){
  "." <- poligono <- NULL
  stopifnot(is.data.frame(dfall), p > 0, p < 1, any(names(dfall) == class), any(names(dfall) == poligon))
  idam <- NULL
  dfuy <- dplyr::select(dfall, class, poligon) %>% dplyr::mutate(idam = seq_len(nrow(.)))
  nv <- c(class, poligon)
  dfuy$idam <- seq_len(nrow(dfuy))
  dots <- lapply(nv, as.symbol)
  dfu <- dfuy %>% dplyr::group_by_(.dots = dots) %>% dplyr::slice(1) %>%
    data.frame()
  set.seed(seed)
  vc <- caret::createDataPartition(dfu[, 1], p = p, list = FALSE)[,
                                                                  1]
  vp <- dfu[vc, 2]
  names(dfuy)[2] <- c("poligono")
  vtreino <- (dfuy %>% dplyr::filter(poligono %in% vp) %>%
                dplyr::select(idam))[, 1]
  return(vtreino)
}




# create_folds_poligon
#'
#'
#' This function  creates folds  for samples collected in the form of polygons.In this case
#' all samples inside a polygon are placed in a fold avoiding the division in multiples
#' folds..
#'
#' @param outcome vector with  outcome variable
#' @param poligon vector with  poligon id
#' @param nfolds number of folds to be created
#' @param seed  numeric seed to control random numbers generation
#' @importFrom caret createFolds
#' @importFrom dplyr group_by_ slice filter select pull
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' dfall = df %>% select(classe, poligono, everything())
#' index = create_folds_poligon(dfall, class = classe, poligon = poligono, nfolds = 5, seed = 123)
#' }
#' @export


create_folds_poligon <- function(outcome, polig, nfolds = 10, seeds = 313){
  groupfolds = list()
  vpol <- unique(polig)
  vclass <- unique(outcome)
  dffold <- data.frame(polig, outcome,stringsAsFactors = F)
  dffold$id <- seq(1:nrow(dffold))
  vclass <- dffold %>% group_by(polig, outcome) %>% slice(1) %>% dplyr::select(outcome, polig)
  set.seed(seeds)
  folds <- caret::createFolds(y = vclass$outcome, list = T, k = nfolds, returnTrain = F)
  for (i in 1:nfolds) {
    vlpolig <- vpol[folds[[i]]]
    f1 <- dffold %>% filter(id %in% vlpolig)
    groupfolds[[i]] <- dffold %>% dplyr::filter(polig %in% vlpolig ) %>% pull(id)
    names(groupfolds)[i] <- paste0("Fold",str_pad(i,2,"left","0"))

  }
  return(groupfolds)
}


