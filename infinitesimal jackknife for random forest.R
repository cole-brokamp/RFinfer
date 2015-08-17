## function adapted from Stefan Wager
  # https://github.com/swager/randomForestCI

# change the randomForest function so it counts times in bag, not yes/no
  # if error, make sure the updated version of randomForest is installed
  # devtools::install_github('cole-brokamp/randomForest')

library(randomForest)

data(airquality)
d <- na.omit(airquality)
set.seed(6655)

rf <- randomForest(Ozone ~ ., data=d,
                   keep.inbag=T,
                   # sampsize=nrow(d),
                   sampsize=floor(nrow(d)^0.7),
                   # replace=FALSE,
                   ntree=nrow(d)*50)
#### my attempt ####
# rfPredVar <- function(rf,pred.data=NULL,num.cores=1,CI=FALSE) {
#   # must have in.bag
#   # rf$inbag is n x B matrix
#   if (is.null(rf$inbag)) {
#     stop("Random forest must be trained with keep.inbag = TRUE")
#   }
#   
#   # by default the keep.inbag produces indicator matrix does NOT count how many times
#   # only that it was or was not used
#   # valid for subsampling, but HOW TO FIX THIS? for bootstrap sampling
#   if (length(unique(colSums(rf$inbag))) > 1) {
#     stop("The keep.inbag field must store the number of times each observation was used")
#   }
#   
#   B <- rf$ntree
#   n <- length(rf$y)
#   s <- sum(rf$inbag) / B
#   N <- rf$inbag
#   
#   preds <- predict(rf, newdata=pred.data, predict.all = TRUE)
#   agg.preds <- preds$aggregate
#   tree.preds <- preds$individual
#   
#   ij.var <- function(t.pred) {
#     C.i <- function(i) sum(sapply(1:B,function(b) {
#       (N[i,b] - (s/n)) * (tree.preds[t.pred,b] - agg.preds[t.pred])
#     }))/B
#     raw.ij.var <- sum(sapply(1:n,C.i)^2)
#     v.hat <- sum(sapply(1:B,function(b) (tree.preds[t.pred,b] - agg.preds[t.pred])^2)) / B
#     correction <- ( (s*(n - s)) / n ) * (v.hat / B)
#     return(data.frame('pred.ij.var'=raw.ij.var - correction))
#   }
#   
#   out <- data.frame('prediction' = agg.preds,
#                     CB::CBapply(seq_along(agg.preds),ij.var,num.cores=num.cores))
#   if (CI) {
#     out <- data.frame(out,
#                       'l.ci' = out$prediction - (out$pred.ij.var * qnorm(0.975,lower.tail=T)),
#                       'u.ci' = out$prediction + (out$pred.ij.var * qnorm(0.975,lower.tail=T)))
#   }
#   return(out)
# }

##### ####

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

rf.preds <- rfPredVar(rf,pred.data=d,CI=TRUE)

d.preds <- cbind(d,rf.preds)

library(ggplot2)

ggplot(d.preds,aes(Ozone,prediction)) +
  geom_abline(intercept=0,slope=1,lty=2,color='darkgrey') +
  geom_point() +
  coord_cartesian(ylim=c(-50,200)) +
  geom_errorbar(aes(ymax = u.ci, ymin=l.ci)) +
  theme_bw()

ggplot(d.preds,aes(Temp,prediction)) +
  geom_point() +
  stat_smooth() +
  coord_cartesian(ylim=c(-50,200)) +
  geom_errorbar(aes(ymax = u.ci, ymin=l.ci)) +
  theme_bw()

ggplot(d.preds,aes(prediction,pred.ij.var)) +
  geom_point() +
  theme_bw()

