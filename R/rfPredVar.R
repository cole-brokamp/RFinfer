#' @title rfPredVar
#' @description
#' Generate predictions and prediction variances from a random forest based on the infinitesimal jackknife.
#'
#' @param rf A random forest trained with \code{keep.inbag=TRUE}. See details for more information.
#' @param pred.data The data used to \code{predict} with the \code{randomForest}.
#' @param CI Should 95\% confidence intervals be returned along with predictions and prediction variances?
#' @return A data frame with the predictions and prediction variances (and optionally 95% confidence interval)
#' @details
#' The original version of \code{randomForest} with the \code{keep.inbag=TRUE}
#' only keeps track if each training data point was or was not included in each resample.
#' Install a tweaked version of \code{randomForest} that amends this to include the number of times each
#' training data point was included in each resample by running the following code in R:
#' \code{devtools::install_github('cole-brokamp/randomForest')}
#' @examples
#' library(randomForest)
#' data(airquality)
#' d <- na.omit(airquality)
#' rf <- randomForest(Ozone ~ .,data=d,keep.inbag=T,sampsize=30,replace=FALSE,ntree=500)
#' rfPredVar(rf,pred.data=d,CI=TRUE)


#'
#'
#'

## function adapted from Stefan Wager
  # https://github.com/swager/randomForestCI

rfPredVar <- function(rf,pred.data=NULL,CI=FALSE) {

  if (is.null(rf$inbag)) {
    stop("Random forest must be trained with keep.inbag = TRUE\nSee Details in function documentation")
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

  out <- data.frame('pred' = agg.preds,pred.ij.var)
  if (CI) {
    out <- data.frame(out,
                      'l.ci' = out$pred - (out$pred.ij.var * qnorm(0.975,lower.tail=T)),
                      'u.ci' = out$pred + (out$pred.ij.var * qnorm(0.975,lower.tail=T)))
  }
  return(out)
}


