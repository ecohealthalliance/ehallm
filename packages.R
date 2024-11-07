######################## LOAD R PACKAGES #######################################

################################################################################
#
#' R packages needed to run any/most {targets} workflows
#
################################################################################

library(targets)
library(tarchetypes)
library(tidyverse)
library(here)
library(knitr)
library(rmarkdown)
library(paws)
library(conflicted)
library(tidymodels)
library(DALEX)
library(DALEXtra)

################################################################################
#
#' Additional R packages needed to run your specific workflow
#' 
#' * Delete or hash out lines of code for R packages not needed in your workflow
#' * Insert code here to load additional R packages that your workflow requires
#
################################################################################

library(dotenv)
library(usethis)
library(kableExtra)