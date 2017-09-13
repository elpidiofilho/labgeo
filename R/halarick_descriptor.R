# Halarick texture descriptors from glcm library
#'
#' @title halarick_descriptor
#' @description This function calculates halarick texture descriptors
#' @param band  raster with a single band
#' @param ngrey number of grey levels to use in texture calculation
#' @param window 2 element list with row and column dimensions of the texture window
#' @param shift a matrix where each row gives an (x, y) shift to use when computing co-occurrency matrices. Textures will be calculated for each shift, and the average over all shifts will be returned.
#' @param statistics a list of strings naming the texture statistics to calculate
#' @keywords halaric
#' @importFrom glcm glcm
#' @details details
#' @examples
#' \dontrun{
#' descriptor = halarick_descriptor(band4)
#' plot(descriptor)
#' }
#' @export

halarick_descriptor <- function(band, ngrey = 16, window = c(3, 3),
                                shift=list(c(0, 1), c(1, 1), c(1, 0), c(1, -1)),
                                statistics = c(
                                  "mean", "mean_ENVI", "variance",
                                  "variance_ENVI", "homogeneity",
                                  "contrast", "dissimilarity", "entropy",
                                  "second_moment", "correlation"
                                )) {
  result <- glcm::glcm(band, window = window, shift = shift, statistics = statistics)
  return(result)
}
