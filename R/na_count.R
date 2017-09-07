# Counts NA values
#'
#' @title na_count
#' @description This function counts NA by column and calculate relative value of NA
#' @param df  dataframe
#' @param verbose if TRUE plot graphics of values NA absolute and relative
#'        by variable
#' @keywords counts NA values
#' @details details
#' @importFrom ggplot2 ggplot geom_col xlab ylab theme labs aes element_text
#' @examples
#' \dontrun{
#' na_count(df)
#' }
#' @export


na_count <- function(df, verbose = TRUE){
 classe <- na_absolute <- na_relative <- variav <- NULL
  nc = ncol(df)
  na_count <- data.frame(variav = character(nc),
                        class_var = character(nc),
                        na_absolute = integer(nc),
                        na_relative = numeric(nc),
                        uniques_values = integer(nc),
                        stringsAsFactors = FALSE)
  na_count$variav <- names(df)
  na_count$class_var<- sapply(df, function(y) class(y))
  na_count$na_absolute <- sapply(df, function(y) sum(length(which(is.na(y)))))
  na_count$na_relative <- na_count$na_absolute / nrow(df)
  na_count$uniques_values <- sapply(df, function(y) length(unique(y)))
  na_count$classe <- cut((na_count$relative+0.001) * 100, c(0,25,50,75,100), dig.lab = 4)

  if (verbose == TRUE) {
    nr = nrow(df)
    g1 = ggplot2::ggplot(data = na_count, ggplot2::aes(x = stats::reorder(variav, na_relative),
                                                  y = na_relative * 100,
                                                  fill = as.factor(classe))) +
      ggplot2::geom_col() + ggplot2::labs(fill='% NA') +
      ggplot2::xlab("variaveis") + ggplot2::ylab('% NA') +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

    g2 = ggplot2::ggplot(data = na_count, ggplot2::aes(x = stats::reorder(variav, nr - na_absolute),
                                                  y = nr - na_absolute, fill= as.factor(classe))) +
      ggplot2::geom_col() + ggplot2::ylab('Amostras validas') + ggplot2::xlab("variaveis") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(fill='% NA')
    print(g1)
    print(g2)
  }

  return(na_count)
}
