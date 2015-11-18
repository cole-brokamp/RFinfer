# change the randomForest function so it counts times in bag, not yes/no
# if error, make sure the updated version of randomForest is installed
# devtools::install_github('cole-brokamp/randomForest')

library(randomForest)
library(RFinfer)

data(airquality)
d <- na.omit(airquality)
set.seed(6655)

rf.ss <- randomForest(Ozone ~ .,data=d,keep.inbag=T,sampsize=30,replace=FALSE,ntree=1000,mtry=2)
rf.bs <- randomForest(Ozone ~ .,data=d,keep.inbag=T,replace=TRUE,ntree=1000,mtry=2)

rf.ss.preds <- rfPredVar(random.forest=rf.ss,rf.data=d,pred.data=d,CI=TRUE,tree.type='rf',prog.bar=T)
rf.bs.preds <- rfPredVar(random.forest=rf.bs,rf.data=d,pred.data=d,CI=TRUE,tree.type='rf',prog.bar=T)

cf.ss.preds <- rfPredVar(random.forest=rf.ss,rf.data=d,pred.data=d,CI=TRUE,tree.type='ci',prog.bar=T)
cf.bs.preds <- rfPredVar(random.forest=rf.bs,rf.data=d,pred.data=d,CI=TRUE,tree.type='ci',prog.bar=T)

rf.ss.preds$resample <- cf.ss.preds$resample <- 'subsample'
rf.bs.preds$resample <- cf.bs.preds$resample <- 'bootstrap'
rf.ss.preds$forest_type <- rf.bs.preds$forest_type <- 'rf'
cf.ss.preds$forest_type <- cf.bs.preds$forest_type <- 'ci'

d.preds <- data.frame(d,rbind(rf.ss.preds,rf.bs.preds,cf.ss.preds,cf.bs.preds))
d.preds$resample <- factor(d.preds$resample)
d.preds$forest_type <- factor(d.preds$forest_type)

library(ggplot2)

ggplot(d.preds,aes(Ozone,pred)) +
  geom_abline(intercept=0,slope=1,lty=2,color='darkgrey') +
  geom_point() +
  coord_cartesian(xlim=c(0,100),ylim=c(-150,350)) +
  geom_errorbar(aes(ymax = u.ci, ymin=l.ci)) +
  facet_grid(resample ~ forest_type) +
  theme_bw()
# save_pdf('ozone_actual_v_pred.pdf',width=15,height=6)

ggplot(d.preds,aes(Temp,pred)) +
  geom_point() +
  stat_smooth() +
  # coord_cartesian(ylim=c(-200,400)) +
  # geom_errorbar(aes(ymax = u.ci, ymin=l.ci)) +
  facet_grid(resample ~ forest_type) +
  theme_bw()
# save_pdf('ozone_temp_v_pred.pdf',width=15,height=6)

ggplot(d.preds,aes(pred,pred.ij.var)) +
  geom_smooth() +
  geom_point() +
  # coord_cartesian(ylim=c(-20,400)) +
  facet_grid(resample ~ forest_type) +
  theme_bw()
# save_pdf('ozone_pred_v_predvar.pdf',width=15,height=6)

