
# Packages required by the app.
library(shiny)
library(DBI)
library(RPostgres)
library(data.table)
library(dplyr)
library(rmarkdown)
 
# Source all package functions.
lapply(list.files("R", full.names = TRUE), FUN = source)
 