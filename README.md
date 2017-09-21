# labgeo
R package to classification and regression in easy way

 Labgeo is a package of routines for adjusting classification and regression models.
      This package addresses the teaching and research activities in digital soil mapping and
      image classification. The main purpose is to encapsulate within the packet the steps
      of adjustment of models, such as:
      
      1 - reduction of data dimensionality through the removal of variables
      highly correlated and / or removal of few importance variables.
      
      2 - Comparison between several models and selection of the best model for a
      certain set of data automatically.
      
      3 - Model a large number of variables (outcome) by comparing the
      several models for each one.
      
      4 - Carry out the prediction of the best adjusted models using covariables
      in the form of rasters and satellite images. (Under construction)
      
      The functions were written for automatic execution in parallel whenever possible,
      taking advantage of multi-core CPUs, aiming agility in more complex modeling.
      
      The model settings are made using the Caret package written by Max Kuhn.
      The Caret package creates a standard interface for accessing a few hundred models,
      is a Swiss Army knife of data modeling.


To install the last version of Labgeo package use :
devtools::install_github("elpidiofilho/labgeo")

To install package multidplyr use :
devtools::install_github("hadley/multidplyr")


Warning : Caret Version 6.0.77 in CRAN has a bug. Use version 6.0.76 instead, until correction in CRAN.
To install a prior version use the code bellow :
devtools::install_version("caret", version = "6.0-76")


devtools::install_github("hadley/multidplyr")
 
