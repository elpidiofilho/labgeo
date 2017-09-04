# Data preparation
#'
#'
#' This function preprare data to analisys using vtreat library
#' @param df   global dataframe
#' @param p the percentage of data that goes to training
#' @param prune suppress variables with significance above this level
#' @param seed seed
#' @keywords data prepartion vtreat
#' @importFrom vtreat prepare designTreatmentsN
#' @importFrom caret createDataPartition
#' @importFrom  dplyr everything select one_of
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' dp = data_preparation(df = c_stock, p = 0.75, prune = 0.99)
#' treino = dp$treino
#' teste = dp$teste
#' }
#' @export

data_preparation <- function(df, prune = 0.99, p = 0.75, seed = 313) {
  outcome <- df[, 1]
  nome.out <- names(df)[1]
  names(outcome) <- nome.out
  set.seed(seed)
  cp <- caret::createDataPartition(outcome, p = p, list = FALSE, group = 6)
  dtrainn <- df[cp, ]
  dtestn <-  df[-cp, ]
  treatmentsn <- vtreat::designTreatmentsN(dtrainn, colnames(dtrainn),
                                   nome.out, verbose = FALSE)
  treino <- vtreat::prepare(treatmentsn, dtrainn, pruneSig = prune) %>%
    dplyr::select(one_of(nome.out), everything())
  teste <- vtreat::prepare(treatmentsn, dtestn, pruneSig = prune) %>%
    dplyr::select(one_of(nome.out), everything())
  return(list(treino = treino, teste = teste, tratamento = treatmentsn))
}
