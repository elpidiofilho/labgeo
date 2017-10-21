# Extract data
#'
#' @title separability.measures
#' @description This function calculates Jeffries-Matusita distance, Bhattacharya distance and the transformed divergence.
#' @param Vector_1  data of class 1
#' @param Vector_2  data of class 2
#' @keywords distance divergence
#' @references https://stats.stackexchange.com/questions/78849/measure-for-separability
#' @details details
#' @importFrom stats cov
#' @examples
#' \dontrun{
#' sample.1 <- c (1362, 1411, 1457, 1735, 1621, 1621, 1791, 1863, 1863, 1838)
#' sample.2 <- c (1362, 1411, 1457, 10030, 1621, 1621, 1791, 1863, 1863, 1838)
#'
#' #separability between these two samples
#' separability.measures( sample_1 , sample_2 )
#' }
#' @export



separability_measures <- function(Vector_1, Vector_2) {
  Matrix_1 <- as.matrix(Vector_1)
  Matrix_2 <- as.matrix(Vector_2)
  mean_matrix_1 <- mean(Matrix_1)
  mean_matrix_2 <- mean(Matrix_2)
  mean_difference <- mean_matrix_1 - mean_matrix_2
  cv_matrix_1 <- cov(Matrix_1)
  cv_matrix_2 <- cov(Matrix_2)
  p <- (cv_matrix_1 + cv_matrix_2) / 2
  bh_distance <- 0.125 * t(mean_difference) * p ^ (-1) * mean_difference +
    0.5 * log(det(p) / sqrt(det(cv_matrix_1) * det(cv_matrix_2)
    ))
  jm_distance <- 2 * (1 - exp(-bh_distance))
  trace_of_matrix <- function(squarematrix) {
    sum(diag(squarematrix))
  }
  divergence_term_1 <- 1 / 2 *
    trace_of_matrix( (cv_matrix_1 - cv_matrix_2) *
                       (cv_matrix_2 ^ (-1) - cv_matrix_1 ^ (-1))
    )
  divergence_term_2 <- 1 / 2 *
    trace_of_matrix( (cv_matrix_1 ^ (-1) +
                        cv_matrix_2 ^ (-1)) *
                       (mean_matrix_1 - mean_matrix_2) *
                       t(mean_matrix_1 - mean_matrix_2)
    )
  divergence <- divergence_term_1 + divergence_term_2
  transformed_divergence <- 2 * (1 - exp( -(divergence / 8)))
  indices <- data.frame(
    jm = jm_distance, bh = bh_distance, div = divergence,
    tdiv = transformed_divergence
  )
  return(indices)
}
