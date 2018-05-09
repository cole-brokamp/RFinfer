# RFinfer

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RFinfer)](http://cran.r-project.org/package=RFinfer)
[![DOI](https://zenodo.org/badge/21831/cole-brokamp/RFinfer.svg)](https://zenodo.org/badge/latestdoi/21831/cole-brokamp/RFinfer)
[![Build Status](https://travis-ci.org/cole-brokamp/RFinfer.svg?branch=master)](https://travis-ci.org/cole-brokamp/RFinfer)
![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/RFinfer?color=orange)

**This software is longer maintained and has been removed from CRAN.  I recommend using the Generalized Random Forest package for R instead: https://github.com/swager/grf**

#### About

R package for generating prediction and prediction variances from random forests. Read the vignettes for an introduction to the package (`vignette('RFinfer-examples')`) and the theory behind the infinitesimal jackknife applied to random forest predictions (`vignette('IJ_vig')`).

Options for random forest include using a subsample or bootstrap sample for each tree and using either conditional inference tree or traditional CART used as base learners.

#### Install

Install from CRAN: `install.packages('RFinfer')`  
Install latest version: `devtools::install_github('cole-brokamp/RFinfer')`

#### Publication

This package is based on simulation experiments in:
Cole Brokamp, MB Rao, Patrick Ryan, Roman Jandarov. A comparison of resampling and recursive partitioning methods in random forest for estimating the asymptotic variance using the infinitesimal jackknife. *Stat*. 6(1). 360-372. 2017. [*Download*](https://colebrokamp-website.s3.amazonaws.com/publications/Brokamp_Stat_2017.pdf). 
