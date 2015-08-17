## function adapted from Stefan Wager
  # https://github.com/swager/randomForestCI

rfPredVar <- function(rf,pred.data=NULL,CI=FALSE) {

  if (is.null(rf$inbag)) {
    stop("Random forest must be trained with keep.inbag = TRUE")
  }

  if (length(unique(colSums(rf$inbag))) > 1) {
    stop("The keep.inbag field must store the number of times each observation was used")
  }

  B <- rf$ntree
  n <- length(rf$y)
  s <- sum(rf$inbag) / B

  N <- Matrix::Matrix(rf$inbag,sparse=TRUE)
  N.avg <- Matrix::Matrix(Matrix::rowMeans(N),nrow(N),1)

  preds <- predict(rf, newdata=pred.data, predict.all = TRUE)
  pred <- preds$individual
  agg.preds <- rowMeans(pred)
  class(pred) <- "numeric" # in case classification

  pred.centered <- pred - rowMeans(pred)
  pred.centered.sums <- Matrix::Matrix(rowSums(pred.centered), 1, nrow(pred.centered))

  C = N %*% t(pred.centered) - N.avg %*% pred.centered.sums
  raw.IJ = Matrix::colSums(C^2) / B^2

  N.var <-  mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
  boot.var <-  rowSums(pred.centered^2) / B
  bias.correction <-  n * N.var * boot.var / B
  pred.ij.var <- raw.IJ - bias.correction

  out <- data.frame('prediction' = agg.preds,pred.ij.var)
  if (CI) {
    out <- data.frame(out,
                      'l.ci' = out$prediction - (out$pred.ij.var * qnorm(0.975,lower.tail=T)),
                      'u.ci' = out$prediction + (out$pred.ij.var * qnorm(0.975,lower.tail=T)))
  }
  return(out)
}


