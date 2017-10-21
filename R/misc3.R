## Thanks RobertH for sharing this very useful function
## http://stackoverflow.com/a/9937083

fun.quickstack <- function(f) {
  r <- raster(f[1])
  ln <- extension(basename(f), '')
  s <- stack(r)
  s@layers <- sapply(1:length(f), function(x){ r@file@name = f[x];
  r@layernames=ln[x]; r@data@haveminmax=FALSE ; r })
  s@layernames <- ln
  s
}

#plot confusion matrix - alluvial graph
plotCM <- function(cm){
  cmdf <- as.data.frame(cm[["table"]])
  cmdf[["color"]] <- ifelse(cmdf[[1]] == cmdf[[2]], "green", "red")

  alluvial::alluvial(cmdf[,1:2]
                     , freq = cmdf$Freq
                     , col = cmdf[["color"]]
                     , alpha = 0.5
                     , hide  = cmdf$Freq == 0
  )
}


plotConfusionMatrix <- function(model, norm = "none"){

  cm <- confusionMatrix(model, norm = norm)

  conf_matrix <- matrix(cm$table, ncol = length(unique(model$trainingData$.outcome)))

  nr <- nrow(conf_matrix)

  M <- t(conf_matrix)[, nr:1]
  Mv <- as.vector(M)
  colnames(M) <- colnames(cm$table)[nr:1]
  rownames(M) <- colnames(cm$table)

  g <- ggplot2::ggplot(reshape2::melt(M), ggplot2::aes_string(x='Var1', y='Var2', fill='value')) + ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient2(low='blue', high='red') + ggplot2::xlab("True") + ggplot2::ylab("Predicted") +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1,vjust=1)) +
    ggplot2::geom_text(aes(label = round(Mv,2)), vjust = 1)

  return(g)

}


