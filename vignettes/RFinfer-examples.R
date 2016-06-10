## ----echo=FALSE,warning=FALSE--------------------------------------------
knitr::opts_chunk$set(prompt=TRUE,warning=FALSE,message=FALSE,collapse=TRUE)
suppressPackageStartupMessages(library(RFinfer))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(nhanesA))

## ----eval=FALSE----------------------------------------------------------
#  install.packages('RFinfer')

## ----eval=FALSE----------------------------------------------------------
#  install_github('cole-brokamp/RFinfer')

## ------------------------------------------------------------------------
library(RFinfer)
data('airquality')
d.aq <- na.omit(airquality)
d.aq <- d.aq[d.aq$Ozone < 100, ]

## ------------------------------------------------------------------------
rf <- randomForest(Ozone ~ .,data=d.aq,keep.inbag=T)

## ------------------------------------------------------------------------
rf.preds <- rfPredVar(rf,rf.data=d.aq,CI=TRUE)
str(rf.preds)

## ---- fig.width=4,fig.height=2-------------------------------------------
library(ggplot2)
ggplot(rf.preds,aes(d.aq$Ozone,pred)) +
  geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
  geom_point() +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci,height=0.15)) +
  xlab('Actual') + ylab('Predicted') +
  theme_bw()

## ----fig.width=4,fig.height=2--------------------------------------------
qplot(d.aq$Ozone - rf.preds$pred,rf.preds$pred.ij.var,
     xlab='prediction error',ylab='prediction variance') + theme_bw()

## ------------------------------------------------------------------------
library(nhanesA)
bmx_d <- nhanes('BMX_D')
demo_d <- nhanes('DEMO_D')
d_merged <- merge(bmx_d,demo_d)
d <- data.frame('bmi' = d_merged$BMXBMI,
                'age' = d_merged$RIDAGEYR,
                'race' = factor(d_merged$RIDRETH1),
                'poverty_income_ratio' = d_merged$INDFMPIR,
                'edu' = factor(d_merged$DMDHREDU,levels=1:9))

## ------------------------------------------------------------------------
d <- na.omit(d)
set.seed(24512399)
d <- d[sample(1:nrow(d),size=100), ]
summary(d)

## ------------------------------------------------------------------------
library(RFinfer)
rf <- randomForest(bmi ~ .,data=d,keep.inbag=TRUE,
                   sampsize=nrow(d)^0.7,replace=FALSE,ntrees=5000)
rf.preds_cart <- rfPredVar(rf,rf.data=d,CI=TRUE,tree.type='rf')
rf.preds_ci <- rfPredVar(rf,rf.data=d,CI=TRUE,tree.type='ci',prog.bar=FALSE)

## ------------------------------------------------------------------------
preds <- rbind(data.frame(rf.preds_cart,'tree'='CART'),
               data.frame(rf.preds_ci,'tree'='CI'))
preds$pred.ij.var <- NULL

## ----warning=FALSE-------------------------------------------------------
LM <- lm(bmi ~ .,data=d)
LM.preds <- data.frame(predict(LM,interval='prediction'))
names(LM.preds) <- c('pred','l.ci','u.ci')
LM.preds$tree <- 'LM'
preds <- rbind(preds,LM.preds)

## ------------------------------------------------------------------------
preds$measured <- d[ ,'bmi']

preds$deviance <- preds$pred - preds$measured
preds$CI_length <- (preds$u.ci - preds$l.ci)

preds$age <- rep(d$age,3)

## ---- fig.height=3,fig.width=7-------------------------------------------
ggplot(preds,aes(measured,pred,color=tree)) +
  geom_point(size=0.5) +
  geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Actual') + ylab('Predicted') +
  theme_bw() +
  facet_grid(~tree)


## ------------------------------------------------------------------------
tapply(preds$deviance,preds$tree,function(x) sqrt(sum(x^2)/length(x)))

## ----fig.height=3,fig.width=7--------------------------------------------
ggplot(preds,aes(deviance,CI_length,color=tree)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~tree)

## ------------------------------------------------------------------------
summary(LM)

## ----fig.height=3,fig.width=7--------------------------------------------
ggplot(preds,aes(age,pred,color=tree)) +
  geom_point(size=0.5) +
  geom_smooth() +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Age') + ylab('Predicted BMI') +
  theme_bw() +
  facet_grid(~tree)

## ------------------------------------------------------------------------
newdat <- expand.grid('age'=2:85,
                      'race'=factor(1,levels=1:5),
                      'poverty_income_ratio'=median(d$poverty_income_ratio),
                      'edu'=factor(5,levels=1:9))
class(newdat$age) <- c('labelled','integer')
class(newdat$poverty_income_ratio) <- c('labelled','numeric')
newdat.preds <- rfPredVar(rf,rf.data=d,pred.data=newdat,
                          CI=TRUE,tree.type='ci',prog.bar=FALSE)
newdat.preds.plot <- cbind(newdat,newdat.preds)

## ----message=FALSE,warning=FALSE,fig.height=3,fig.width=7----------------
ggplot(newdat.preds.plot,aes(age,pred)) +
  geom_point(size=0.5) +
  geom_smooth() +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Age') + ylab('Predicted BMI') +
  theme_bw()

