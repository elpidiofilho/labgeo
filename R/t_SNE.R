# t_SNE_plot dimension reduction
#'
#' @title t_SNE_plot dimension reduction
#' @description  t-SNE is a method for constructing a low dimensional embedding of high-dimensional data, distances or similarities.
#' @param dx covariates
#' @param y responde variable
#' @details details
#' @keywords t_SNE dimension reduction
#' @importFrom ggplot2 ggplot geom_point geom_text guides xlab ylab ggtitle theme theme_light
#' @importFrom Rtsne Rtsne
#' @examples
#' \dontrun{
#' dx = dxy[,-1]
#' dy = dxy[,1]
#' t_SNE_plot(dx,dy)
#' }
#' @export



t_SNE_plot <- function(dx, y) {
  tsne <- Rtsne::Rtsne(as.matrix(dx), check_duplicates = FALSE, pca = TRUE,
               perplexity = 30, theta = 0.5, dims = 5)

  embedding <- as.data.frame(tsne$Y)
  embedding$Class <- y
  embedding$amostra <- seq_len(nrow(dx))

  g <- ggplot2::ggplot(embedding, ggplot2::aes(x = V1, y = V2, color = Class)) +
    ggplot2::geom_point(size = 1.25, ggplot2::aes(shape = Class)) +
    ggplot2::geom_text(ggplot2::aes(label = embedding$id), size = 0.8, hjust = 0) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 6))) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::ggtitle("t-SNE 2D Embedding of 'Classe' Outcome") +
    ggplot2::theme_light(base_size = 20) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank())
  print(g)
  return(tsne)
}

