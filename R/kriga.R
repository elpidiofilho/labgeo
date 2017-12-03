#' kriga - Automatric krige
#' @param df   dataframe with targer variable, coordinate x  and coordinate y
#' @param target_var Name of column with target_var
#' @param nrep  Number of repetitions
#' @param name_px  names of column with coordinate x
#' @param name_py  names of column with coordinate y
#' @param p proportion between training and validation
#' @param seed seed number to allow reprodutibility
#' @keywords krige geostatics
#' @importFrom automap autoKrige
#' @importFrom dplyr select filter
#' @importFrom caret createDataPartition
#' @importFrom sp coordinates
#' @importFrom stats as.formula
#' @importFrom knitr kable
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @details details
#' @export
#' @examples
#' \dontrun{
#' result.krig = kriga(df = d,target_var = "argila", name_px = 'point_x',
#'                     name_py = 'point_y',nrep = 100, p = 0.75 , seed = 313)
#' media = result.krig %>% select(-(model:repet))
#'         %>% summarise_all(funs(med = mean, desvpad = sd ))
#' print(knitr::kable(media))
#' }

kriga <- function(df, target_var, nrep = 10,
                  name_px, name_py, p = 0.75, seed = NULL) {
  repet <- NULL
  ng <- nrep
  if (!is.null(seed)) {
    set.seed(seed)
  }
  ld <- caret::createDataPartition(df[, 1], times = ng)
  varsel <- c(target_var, name_px, name_py)
  nl <- length(ld)
  dsel <- df %>% select(one_of(varsel))
  for (i in 1:nl) {
    df1 <- dsel[unlist(ld[[i]]), ]
    df1$repet <- i
    df2 <- dsel[-unlist(ld[[i]]), ]
    df2$repet <- i
    if (i == 1) {
      dftreino <- df1
      dfvalida <- df2

    } else {
      dftreino <- rbind(dftreino, df1)
      dfvalida <- rbind(dfvalida, df2)
    }
  }
  dfresult <- data.frame(model = character(ng), vars = character(ng),
                         repet = numeric(ng),
                        r2 = numeric(ng), rmse = numeric(ng),
                        mae = numeric(ng), mbe = numeric(ng),
                        stringsAsFactors = F)
  cont <- 1
  i <- 1
  for (i in 1:ng){
    dsel.treino <- dftreino %>% filter(repet == i)
    dsel.valida <- dfvalida %>% filter(repet == i)
    f1 <- as.formula(paste("~", name_px, "+", name_py))
    f2 <- as.formula(paste(target_var, " ~ 1"))
    sp::coordinates(dsel.treino) <- f1
    sp::coordinates(dsel.valida) <- f1
    kr <- automap::autoKrige(f2, dsel.treino, dsel.valida, model = c("Ste"))
    ddd <- data.frame(predito = kr$krige_output$var1.pred,
                      observado = select(dsel.valida@data, one_of(target_var)))
    names(ddd) <- c("predito", "observado")
    result <- pred_acc(ddd$observado, ddd$predito)
    dfresult$model[cont] <- "krigging"
    dfresult$vars[cont] <- as.character(kr$var_model[2, 1])
    dfresult$repet[cont] <- i
    dfresult$r2[cont]  <- result$rsquared
    dfresult$rmse[cont] <- result$root_mean_square_error
    dfresult$mae[cont] <- result$mean_absolute_error
    dfresult$mbe[cont] <- result$mean_bias_error
    cont <- cont + 1
  }
  print(knitr::kable(dfresult))
  return(dfresult)
}
