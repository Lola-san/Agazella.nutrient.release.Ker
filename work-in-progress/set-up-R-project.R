################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2024
# set_up-R-project.R
################################################################################

# initiate renv
renv::init()

# create folder that will contain all R function scripts
dir.create("R")

# create data and output folders
dir.create("data")
dir.create("output")


# install packages 
renv::install("devtools")
renv::install("targets")
# triggers problem with igraph
renv::install("igraph", type = "binary") # working!
renv::install("targets") # but still not working... 
utils::install.packages("targets") # but still not working

getOption("download.file.method") # wininet
renv:::renv_download_method() # curl but it should match
options(renv.download.override = utils::download.file) # but still didnt work afterward
Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
renv::install("targets") # but again still not working... 
Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "wininet")
renv::install("targets") # but again still not working... 
Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "curl")

renv::install("openxlsx")
renv::install("tidyverse")
renv::install("purrr")
renv::install("sensobol")
renv::install("ncdf4")
renv::install("tarchetypes")
renv::install("kableExtra")
renv::install("imager")
renv::install("ghibli")
renv::install("rmarkdown")
renv::install("here")
renv::install("kdensity")
renv::install("wesanderson")
renv::install("fpc")

# checking for updates
renv::status()

# check for problems
getOption("repos") 
renv::diagnostics()
renv::dependencies()

# 1 Insert the following code to your .Rprofile file: options(renv.download.override = utils::download.file)
# 2
renv::activate()
# 3 remove all packages except renv
renv::remove("actuar")
renv::remove("askpass")
renv::remove("assertthat")
renv::remove("backports")
renv::remove("base64enc")
renv::remove("base64url")
renv::remove("bit")
renv::remove("bit64")
renv::remove("blob")
renv::remove("brew")
renv::remove("brio")
renv::remove("broom")
renv::remove("bslib")
renv::remove("cachem")
renv::remove("callr")
renv::remove("cellranger")
renv::remove("cli")
renv::remove("clipr")
renv::remove("colorspace")
renv::remove("commonmark")
renv::remove("conflicted")
renv::remove("cpp11")
# or all of them manually in the folder, in fact EXCEPT RENV 

# 4 
utils::install.packages("renv", dependencies = TRUE)

# 5 Restart R (
.rs.restartR()

# 6 retry
renv::install("targets")

