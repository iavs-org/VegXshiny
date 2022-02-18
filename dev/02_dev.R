# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("bslib")
usethis::use_package("dplyr")
usethis::use_package("DT")
usethis::use_package("golem")
usethis::use_package("jsonlite")
usethis::use_package("lubridate")
usethis::use_package("rhandsontable")
usethis::use_package("shiny")
usethis::use_package("shinyAce")
usethis::use_package("shinyjs")
usethis::use_dev_package("shinyTree")
usethis::use_package("shinyWidgets")
usethis::use_package("shinythemes")   # Style with bslib and remove
usethis::use_package("stringr")
usethis::use_package("tidyr")         # Style with bslib and remove
usethis::use_package("xml2")

usethis::use_package("covr", type = "Suggests")
usethis::use_package("testthat", type = "Suggests")

## Add modules ----
golem::add_module("about")
golem::add_module("fileManagement")
golem::add_module("documentCreation")
golem::add_module("elementMapping")
golem::add_module("rowGenerator")
golem::add_module("viewXML")
golem::add_module("actionLog")

## Add helper functions ----
golem::add_fct("xml_schema")
golem::add_fct("xml_export")
golem::add_fct("misc")

## External resources
golem::add_js_file("custom")
golem::add_js_handler("node_tooltip")
golem::add_css_file("custom")


## Add internal datasets ----
usethis::use_data_raw("data_preparation")

## Tests ----
## Add one line by test you want to create
usethis::use_testthat()

# Documentation

## Vignette ----
usethis::use_vignette("VegXshiny")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage("test-coverage.yaml", type = "codecov")

# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 

# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_standard() 

# Add action for PR
usethis::use_github_action_pr_commands()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

