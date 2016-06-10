#' @title rfPredVar
#' @description Generate predictions and prediction variances from a random
#'   forest based on the infinitesimal jackknife.
#'
#' @export
#' @import randomForest
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats qnorm
#'
#' @param random.forest A random forest trained with \code{keep.inbag=TRUE}. See
#'   details for more information.
#' @param rf.data The data used to train \code{rf}
#' @param pred.data The data used to predict with the forest; defaults to
#'   \code{rf.data} if not given
#' @param tree.type either 'ci' for conditional inference tree or 'rf' for
#'   traditional CART tree
#' @param prog.bar should progress bar be shown? (only applicable when
#'   \code{tree.type='ci'})
#' @param CI Should 95\% confidence intervals based on the CLT be returned along
#'   with predictions and prediction variances?
#' @return A data frame with the predictions and prediction variances (and
#'   optionally 95\% confidence interval)
#'
#' @details The random forest trained with \code{keep.inbag=TRUE} is supplied
#'   only for the purpose of defining the resampling scheme. The function builds
#'   a new random forest based on the \code{tree.type} setting.  However, the
#'   resamples are maintained identically to the supplied random forest.  This
#'   allows for direct comparison of the tree methods without having to account
#'   for variation in resampling.
#'
#'   Currently, the CI methods are much more computationally intensive because
#'   there is no C implementation of the CI random forest method that indicates
#'   the number of times that each sample is included in each resample.  In
#'   order to carry out our simulations using \eqn{\hat{V}_{IJ}^B}{V_IJ^B}, we had to use a
#'   pure R implementation of CI random forests.  This is different for CART
#'   random forests, where a C implementation already exists in the
#'   \code{randomForest} package.  However, it should be noted that the
#'   difference in computational times is due to the random forest creation
#'   step, not the implementation of \eqn{\hat{V}_{IJ}^B}{V_IJ^B}.  This should not be an
#'   issue in the future when a C implementation of CI random forests is
#'   created.
#'
#'   Note: This function does not use the default predict method for forests
#'   produced by \code{cforest}. The predictions here are the direct averages of
#'   all tree predictions, instead of using the observation weights.  Therefore,
#'   predictions from this function will likely differ from
#'   \code{predict.cforest} when using subsampling.
#'
#'   This function currently only works with regression forests -- not
#'   classification forests.
#'
#' @examples
#' library(randomForest)
#' data(airquality)
#' d <- na.omit(airquality)
#' rf <- randomForest(Ozone ~ .,data=d,keep.inbag=TRUE,sampsize=30,replace=FALSE,ntree=500)
#' rfPredVar(rf,rf.data=d,CI=TRUE,tree.type='rf')

## function adapted from Stefan Wager
  # https://github.com/swager/randomForestCI

rfPredVar <- function(random.forest,rf.data,pred.data=rf.data,CI=FALSE,tree.type='rf',prog.bar=FALSE) {

  if (is.null(random.forest$inbag)) {
    stop("Random forest must be trained with keep.inbag = TRUE")
  }
  if (length(unique(colSums(random.forest$inbag))) > 1) {
    stop("The keep.inbag field must store the number of times each observation was used
           \nMake sure the latest version of the randomForest package is installed from CRAN")
  }
  N.weights <- random.forest$inbag

  B <- ncol(N.weights)
  n <- nrow(N.weights)
  s <- sum(N.weights[ ,1])
  N <- Matrix::Matrix(N.weights,sparse=TRUE)
  N.avg <- Matrix::Matrix(Matrix::rowMeans(N),nrow(N),1)

  if (tree.type=='rf') pred <- predict(random.forest,newdata=pred.data,predict.all=TRUE)$individual
  if (tree.type=='ci') pred <- CB_cforest(rf=random.forest,pb=prog.bar,rf.d=rf.data,p.d=pred.data)$preds

  agg.preds <- rowMeans(pred)
  pred.centered <- pred - agg.preds
  pred.centered.sums <- Matrix::Matrix(rowSums(pred.centered), 1, nrow(pred.centered))

  C = N %*% t(pred.centered) - N.avg %*% pred.centered.sums
  raw.IJ <- Matrix::colSums(C^2) / B^2

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


