# R-package for calculation of Nature Index.
Jens  
May 6, 2016  

Intro
====================
This file documents the steps for the creation of the package. Details of the packages itself is documented in R-function help pages and package Vignettes.

Set up package skeleton
==========
I started with creating a directory called `NIcalc` wherein I ran `git init` to initialise file versioning through git (optional). Then these command create a minimalist R-package structure in an existing folder.


```r
require(roxygen2)
require(devtools)
#getwd()

setup("../NIcalc")
```


