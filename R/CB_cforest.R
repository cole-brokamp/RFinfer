# pull from random forest: mtry, ntree, matching sub/bootstrap samples
# must supply same data.frame used in random forest

# returns list:
  # c_trees: individual trees as constructed by party::ctree based on same subsamples as in RF
  # preds: predictions of individual trees for each data point in d (n x B matrix)

CB_cforest <- function(rf,rf.d,p.d=rf.d,pb=FALSE) {
  new.samples <- match_sample(rf,rf.d)
  # tc <- party::ctree_control(teststat='max',testtype='Univariate',mincriterion=0,mtry=rf$mtry)
  tc <- party::ctree_control(teststat='max',testtype='Univariate',
                             mincriterion=0,mtry=rf$mtry,minbucket=1,minsplit=2)
  pb.fun <- ifelse(pb,pbapply::pblapply,lapply)
  out <- list()
  if (pb) print('making ci trees')
  out$c_trees <- pb.fun(new.samples,function(x) party::ctree(as.formula(rf$call$formula),data=x,controls=tc))
  if (pb) print('predicting new data for each tree')
  out$preds <- do.call(cbind,pb.fun(1:length(out$c_trees),function(x) predict(out$c_trees[[x]],newdata=p.d)))
  return(out)
  }


match_sample <- function(rf,d) {
  n <- nrow(d)
  matched.samples <- lapply(1:ncol(rf$inbag),function(B){
    n.uses <- rf$inbag[ ,B]
    names(n.uses) <- 1:n
    match.sample.rows <- unlist(mapply(rep,x=as.numeric(names(n.uses)),each=n.uses))
    matched.sample <- d[match.sample.rows, ]
    return(matched.sample)
  })
  return(matched.samples)
}
