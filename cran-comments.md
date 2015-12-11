
## Resubmission
This is a resubmission. In this version I have:

* Updated the Date field of the DESCRIPTION to 2015-10-12 (the day I first sent
  the package to CRAN).

* Included missing "importFrom" in the NAMESPACE for functions:

    + "lines", "plot", "points", "title" from the `graphics` package.
    + "approxfun", "coefficients", "complete.cases" from the `stats` package.
    
The Version field in DESCRIPTION has been changed to 1.0.1. to easier distinguish between this version and the first one I sent.

## Test environments
* Windows 7, R version 3.1.3
* OS X, R version 3.2.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alberto Garre Perez <garre.alberto@gmail.com>'
New submission

NOTE has been ignored as per:
http://stackoverflow.com/questions/23829978/checking-cran-incoming-feasibility-note-maintainer
