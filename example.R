# change the randomForest function so it counts times in bag, not yes/no
# if error, make sure the updated version of randomForest is installed
# devtools::install_github('cole-brokamp/randomForest')

library(randomForest)
library(RFinfer)

data(airquality)
d <- na.omit(airquality)
set.seed(6655)

rf <- randomForest(Ozone ~ .,data=d,keep.inbag=T,sampsize=30,replace=FALSE,ntree=10000)

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
