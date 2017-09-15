
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hypertidy/obj.svg?branch=master)](https://travis-ci.org/hypertidy/obj) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/obj?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/obj) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/obj)](https://cran.r-project.org/package=obj) [![Coverage Status](https://img.shields.io/codecov/c/github/hypertidy/obj/master.svg)](https://codecov.io/github/hypertidy/obj?branch=master)

obj
===

The goal of obj is to ...

Installation
------------

You can install obj from github with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/obj")
```

Example
-------

This example shows how to read and plot a [Wavefront obj. file](https://en.wikipedia.org/wiki/Wavefront_.obj_file):

``` r
library(obj)
#> [1] TRUE
#> [1] TRUE
f <- system.file("extdata/greek_bust.obj.zip", package = "obj")
obj <- read_obj(f)
 
plot(obj)
```
