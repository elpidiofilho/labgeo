# Data preparation
#'
#'
#' This function preprare data to analisys using vtreat library
#' @param df   global dataframe
#' @param p the percentage of data that goes to training
#' @param prune suppress variables with significance above this level
#' @param seed seed
#' @keywords data prepartion vtreat
#' @import vtreat
#' @import caret
#' @import dplyr
#' @export
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' data_prepatarion(df, 0.75)

data_preparation <- function(df, prune = 0.99, p = 0.75 , seed = 313) {
  outcome = df[,1]
  nome.out = names(df)[1]
  names(outcome) = nome.out
  set.seed(seed)
  cp = createDataPartition(outcome, p=p, list=F, group = 6)
  dTrainN = df[cp,]
  dTestN =  df[-cp,]
  treatmentsN = designTreatmentsN(dTrainN,colnames(dTrainN),nome.out,verbose = F)
  treino <- prepare(treatmentsN,dTrainN,pruneSig=prune) %>% select(one_of(nome.out), everything())
  teste <- prepare(treatmentsN,dTestN,pruneSig=prune) %>% select(one_of(nome.out), everything())
  return(list(treino = treino, teste = teste, tratamento = treatmentsN ))
}
