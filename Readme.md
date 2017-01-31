R-package for calculation of Nature Index.
================
Jens Åström
May 6, 2016

<!-- README.md is generated from README.Rmd. Please edit that file -->
Intro
=====

This file documents the steps for the creation of the package. Details of the packages itself is documented in R-function help pages and package Vignettes.

See <https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-RStudio> for instructions. There is also a link to Hadley Wickam's book on writing R-packages.

Workflow
--------

Three commands will be frequently used after you've made some changes to the package.

-   `devtools::document()` builds the documentation files. Use this to test and update the documentation, after you have changed the roxygen2 comments in a function.
-   `devtools::build_vignettes()` builds vignettes. Use after you have modified vignettes.
-   "Build and Reload"-button on the top right. Builds and reloads the new version of the package. Use when some functionality has changed in the package. This will check many types of errors in the package, and can be used often.

Set up package skeleton
=======================

I started with creating a directory called `NIcalc`. Then I ran `git init` to initialise file versioning through git (optional). Then the following commands creates a minimalist R-package structure in the existing folder.

``` r
require(roxygen2)
require(devtools)
#getwd()

setup("../NIcalc")
```

Create functions
================

Functions are stored in script files in the folder NIcalc/R. Typically (and best), each function has its own script file. It's easiest to use `Roxygen2` for documentation, and store help documentation in the same script file. Each function requires a minimum of documentation. Below is a minimal skeleton that can be used to create new functions.

``` r
#' Example function help title
#' 
#' Function description goes here.
#' 
#' Details of function goes here.
#' 
#'
#' @param firstParam Description of first param.
#' @param secondParam Description of second para,.
#' @return What the function returns
#' @examples
#' exampleFunction(1, 1)
```

As an example, I create the function `add()` in the file `NIcalc/R/add.R`. It looks like this:

``` r
#' Add together two numbers.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}
```

Set up first vignette
=====================

Vignettes are a nice way to provide a fuller manual of how to work with a package. A package can have many vignettes, with or without code. A skeleton to a vignette is created by the function `devtools::use_vignette`. The code below creates a first vignette called "Workflow". This will create a file called "NIcalc/vignettes/Workflow.Rmd". This file, when built through the command `devtools::build_vignettes`, will create a html file that is included with the package. Edit that file to your liking, or delete it and create a nother one. I think we could produce a pdf at the same time, if we want. Possibly using the NINA kortrapport template if we want that.

``` r
use_vignette("Workflow")
```

Include data in the package
===========================

Datasets can be included in the package as well. Easiest is to use the `devtools::use_data()` function. Here, I include a dummy dataset. This code stores the dummy dataframe "NIdata1" in the "NIcalc/data" folder.

``` r
set.seed(12345)
NIdata1<-as.data.frame(matrix(runif(200), ncol=5))
names(NIdata1)<-paste("column", 1:5, sep="_")

use_data(NIdata1)
```

Such datasets should also be documented. This is put in a file in the "R-folder" called "NIdata.R"

``` r
#' Dummy dataset
#'
#' A dataset of 200 random numbers put in a dataframe
#'
#' @format A data frame with 40 rows and 5 columns:
#' \describe{
#'   \item{column1}{random numbers}
#'   \item{column1}{random numbers}
#'   \item{column1}{random numbers}
#'   \item{column1}{random numbers}
#'   \item{column1}{random numbers}
#'   }
"NIdata1"
```

Once the package is installed (and loaded), this data can be loaded through `data(NIdata)`

Note that you can also include raw data. This is perhaps better, if you want to show users by examples of how to user their own data. See Hadley Wickham's R-package book.
