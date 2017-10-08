# labgeo
Labgeo is a R package make classifications and regressions in an easy way

Labgeo is a package of routines for fit classification and regression models. This package addresses the teaching and research activities in digital soil mapping and image classification. The main purpose is to encapsulate within the package the steps of adjustment of models, such as:

  1- Reduction of data dimensionality through the removal of variables
  highly correlated and/or removal of few importance variables.
  
  2- Comparation between several models and selection of the best model for a
  certain set of data, automatically.
  
  3- Model a large number of response variables (outcome) by comparing the
  several models for each one.
  
  4 - Carry out the prediction of the best adjusted models using co-variables
  in the form of rasters files and satellite images. (Under construction)
  
  The functions were written for automatic execution in parallel whenever possible,
  taking advantage of multi-core CPUs, aiming agility in more complex modeling.


  The model settings are made using the Caret package written by Max Kuhn.
  The Caret package creates a standard interface for accessing a few hundred models,
  is a Swiss Army knife of data modeling.

## Installation 

To install the last version of Labgeo package follow this steps : 

1 - Install via github the multidplyr package, which is not yet available in CRAN. The install multidplyr use :
devtools::install_github("hadley/multidplyr")

2 - Install via github labgeo package devtools::install_github("elpidiofilho/labgeo")

3 - Warning : Caret Version 6.0.77 in CRAN has a bug. Max Kuhn the caret pakcage manteiner write that a new CRAN version of caret with bug correctionm will be submitted around December. Use version 6.0.76 instead, until correction in CRAN.

To install a prior version use the code bellow : 
devtools::install_version("caret", version = "6.0-76") 

*OR* 

To install 6.0.77 with bug correction version from gitub :
devtools::install_github('topepo/caret/pkg/caret')


 
