# Data preparation Categoric
#'
#'
#' This function preprare data categoric to analisys using vtreat library
#' @param df   global dataframe
#' @param p the percentage of data that goes to training
#' @param positivo Value/level of outcome to be considered "success"
#' @param prune suppress variables with significance above this level
#' @param seed seed
#' @keywords data prepartion vtreat
#' @importFrom vtreat designTreatmentsC prepare
#' @importFrom caret createDataPartition
#' @importFrom dplyr select pull %>%
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @details details
#' @examples
#' \dontrun{
#' data_prepatarion_categoric(df, 0.75)
#' }
#' @export

data_preparation_categoric <- function(df, p = 0.75, positivo = 1,
                                       prune = 0.99, seed = 313) {
  outcome <- df %>%
    pull(1)
  nome.out <- names(df)[1]
  names(outcome) <- nome.out
  set.seed(seed)
  cp <- caret::createDataPartition(outcome, p = p, list = FALSE)
  dtrainc <- df[cp, ]
  dtestc <- df[-cp, ]
  treatmentsc <- vtreat::designTreatmentsC(
    dtrainc, colnames(dtrainc),
    nome.out, positivo
  )
  treino <- vtreat::prepare(treatmentsc, dtrainc, pruneSig = prune) %>%
    select(one_of(nome.out), everything())
  teste <- vtreat::prepare(treatmentsc, dtestc, pruneSig = prune) %>%
    select(one_of(nome.out), everything())
  return(list(treino = treino, teste = teste, tratamento = treatmentsc))
}
