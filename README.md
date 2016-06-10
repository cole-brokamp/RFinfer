# RFinfer
[![Build Status](https://travis-ci.org/cole-brokamp/RFinfer.svg?branch=master)](https://travis-ci.org/cole-brokamp/RFinfer)
[![DOI](https://zenodo.org/badge/21831/cole-brokamp/RFinfer.svg)](https://zenodo.org/badge/latestdoi/21831/cole-brokamp/RFinfer)

#### About

R package for generating prediction and prediction variances from random forests. Read the vignettes for an introduction to the package (`vignette('RFinfer-examples')`) and the theory behind the infinitesimal jackknife applied to random forest predictions (`vignette('IJ_vig')`).

Options for random forest include using a subsample or bootstrap sample for each tree and using either conditional inference tree or traditional CART used as base learners.

#### Install

Install from CRAN: `install.packages('RFinfer')`  
Install latest version: `devtools::install_github('cole-brokamp/RFinfer')`

#### Infinitesimal Jackknife



This package is based on work completed for my dissertation. For more details, see: http://colebrokamp.com/dissertation
