# train_validation_poligon
#'
#'
#' This function performs the separation of training samples and validation for samples collected in the form of polygons.
#' Note that the class has to be the first column and the polygon number has to be the second column of the dataframe.
#' @param dfall   Dataframe with class and poligon columns
#' @param p the percentage of data that goes to training
#' @param seed     seed to control random numbers generation
#' @importFrom caret createDataPartition
#' @importFrom dplyr group_by_ slice filter select
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' dfall = df %>% select(classe, poligono, everything())
#' vt = treino_validacao_poligono(dfall, p = 0.80, seed = 123)
#' treino = dfall[treino,]
#' valida = dfall[-treino,]
#'}
#' @export

train_validation_poligon <- function(dfall, p = 0.75, seed = 313) {
  '.' <- poligono <- NULL
  stopifnot(is.data.frame(dfall), p > 0, p < 1)
  idam = NULL
  dfuy <- dplyr::select(dfall,1:2) %>% dplyr::mutate(idam = seq_len(nrow(.)))
  nv <- names(dfall)[1:2]
  dfuy$idam <- seq_len(nrow(dfuy))
  dots <- lapply(nv, as.symbol)
  dfu <-  dfuy %>% dplyr::group_by_(.dots = dots) %>% dplyr::slice(1) %>% data.frame()
  set.seed(seed)
  vc <- caret::createDataPartition(dfu[,1], p = p, list = FALSE)[, 1]
  vp <- dfu[vc,2]
  names(dfuy)[2] <- c("poligono")
  vtreino <- (dfuy %>% dplyr::filter(poligono %in% vp) %>% dplyr::select(idam))[, 1]
  return(vtreino)
}
