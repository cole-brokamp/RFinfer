# RFinfer
[![Build Status](https://travis-ci.org/cole-brokamp/RFinfer.svg?branch=master)](https://travis-ci.org/cole-brokamp/RFinfer)
[![DOI](https://zenodo.org/badge/21831/cole-brokamp/RFinfer.svg)](https://zenodo.org/badge/latestdoi/21831/cole-brokamp/RFinfer)

R package for generating prediction and prediction variances from random forests.  Options for random forest include using a subsample or bootstrap sample for each tree and using either conditional inference tree or traditional CART used as base learners.

Install this package within `R` by running: `devtools::install_github('cole-brokamp/RFinfer')`. To also install the vignettes, use `devtools::install_github('cole-brokamp/RFinfer',build_vignettes = TRUE)`.

Read the vignette for an introduction to the package and the theory behind the infinitesimal jackknife applied to random forest predictions.

This package is based on work completed for my dissertation. For more details, see: http://colebrokamp.com/dissertation
