# stack_boxplot
#'
#' @title stack_boxplot
#' @description This function plot two variables
#' @keywords boxplot
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @param x x
#' @param y y
#' @param y.limit y.limit
#' @param name1 name1
#' @param name2 name2
#' @param color1 color1
#' @param color2 color2
#' @param title title
#' @importFrom graphics boxplot par
#' @importFrom plotrix color.legend
#' @details details
#' @examples
#' \dontrun{
#' stack_boxplot(b1, b2, 2000, "banda1", "banda2","red","blue", "band1 x band2")
#' }
#' @export


stack_boxplot <- function (x, y, y.limit, name1, name2, color1, color2, title){
  boxplot(x, main = title, col = color1, ylim = y.limit)
  par(new = TRUE)
  boxplot(y, main = "", col = color2, ylim = y.limit)
  par(new = TRUE)
  title(main = title)
  testcol <- c(color1, color2)
  col.labels <- c(name1, name2)
  par(new = TRUE)
  color.legend(0, 1, 3, 0.94, col.labels, testcol)
}
