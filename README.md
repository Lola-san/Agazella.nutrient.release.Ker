# Agazella.nutrient.release.Ker
Assessing the variability in the multi-nutrient content of scats of Antarctic 
fur seals in Kerguelen and using this data to estimate their nutrient release.

This R project is associated to the article Gilbert et al. (2025), (add journal
and doi).

This R project contains all the analysis of data conducted for this project
from data exploration to modeling and result analysis.

## Targets set up
The project uses the targets packages (https://books.ropensci.org/targets/), and is 
therefore organized as follows:

- the R folder contains scripts containing functions and only functions (no
 direct command)
- functions are called in the _targets.R script at the root of this project, in 
 items of a unique list.
 Each item in a list is defined by function tar_target(y, function(x)), in which 
 y is a target object created by function(x) where function must be defined in 
 the R folder and x must be a known object (usually, a target objet previously 
 defined in the list)

Each object from the list can be read using targets::tar_read(object) or loaded
in the local environment with targets::tar_load(object). 

The full analysis can be run using targets::tar_make(), and only tasks associated 
to objects that are not up-to-date will be run. 

Note that the folder "work-in-progress" contains R scripts with direct commands which
were used throughout the development of the project. 

## Renv set up
The project also uses the Renv package (https://rstudio.github.io/renv/articles/renv.html).
It helps in creating reproducible environments into R projects. Basic commands 
regarding Renv (e.g., update the environment) are contained in 
/dev-scripts/set-up-project.R