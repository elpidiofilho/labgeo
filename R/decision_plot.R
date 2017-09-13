# Decision plot
#'
#' @title decisionplot dimension reduction
#' @description  The following plot adds the decision boundary by evaluating the classifier at evenly spaced grid points
#' @param model model adjusted
#' @param df dataframe with data
#' @param class name of outcome variable
#' @param predict_type name of outcome variable
#' @param resolution resolution of graphic in ppi. Default is 100ppi
#' @param showgrid Logical. Control if show or not the grid over the graphic.
#' @param ... aditional parameters
#' @keywords decision boundary
#' @source <http://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html>
#' @details details
#' @importFrom graphics plot points contour
#' @importFrom stats predict
#' @examples
#' \dontrun{
#' model <- knn3(Species ~ ., data=x, k = 1)
#' decisionplot(model, x, class = "Species", main = "kNN (1)")
#' }
#' @export

decision_plot <- function(model, df, class = NULL, predict_type = "class",
                          resolution = 100, showgrid = TRUE, ...) {
  if (!is.null(class)) cl <- df[, class] else cl <- 1
  df <- df %>%
    dplyr::select(-dplyr::one_of(class))
  k <- length(unique(cl))

  plot(df, col = as.integer(cl) + 1L, pch = as.integer(cl) + 1L, ...)

  # make grid
  r <- sapply(df, range, na.rm = TRUE)
  xs <- seq(r[1, 1], r[2, 1], length.out = resolution)
  ys <- seq(r[1, 2], r[2, 2], length.out = resolution)
  g <- cbind(rep(xs, each = resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)

  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g)
  if (is.list(p)) p <- p$class
  p <- as.factor(p)

  if (showgrid) points(g, col = as.integer(p) + 1L, pch = ".")

  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(
    xs, ys, z, add = TRUE, drawlabels = FALSE,
    lwd = 2, levels = (1:(k - 1)) + .5
  )

  invisible(z)
}
