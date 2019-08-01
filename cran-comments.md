
## Resubmission
This is a resubmission. In this version I have:

* Improved the vignette, trying to fix the issues with CRAN submission.

## Test environments

* Windows 7, R version 3.4.2
* OS X Yosemite 10.10.2, R version 3.5.3

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE: 

>Possibly mis-spelled words in DESCRIPTION:
 isothermal (11:5, 13:93)

The word 'isothermal' is broadly used by food scientists,
to reffer to pasteurization treatments at constant temperature.
Hence, I would rather keep it in the text.

## Reverse dependencies

I have run devtools::revdep_check() and every package passed.
