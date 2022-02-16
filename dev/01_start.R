# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
## 
## /!\ Note: if you want to change the name of your app during development, 
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
## 
golem::fill_desc(
  pkg_name = "VegXshiny", # The Name of the package containing the App 
  pkg_title = "VegXshiny", # The Title of the package containing the App 
  pkg_description = "A web application for the conversion of vegetation data into a common exchange format defined by the VegX standard.", # The Description of the package containing the App 
  author_first_name = "Christian", # Your First Name
  author_last_name = "König", # Your Last Name
  author_email = "chr.koenig@outlook.com", # Your Email
  repo_url = "https://github.com/ChrKoenig/VegXshiny" # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license(copyright_holder = "Christian König")  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_lifecycle_badge( "Experimental" )

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file. 
golem::remove_favicon()

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

