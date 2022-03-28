## Data preprocessing script

library("roxygen2", "sinew", "usethis")
# Load data
ensemblebiomass <- read.csv("data-raw/ensemble_biomass.csv")
usethis::use_data(ensemblebiomass, overwrite = TRUE)


ensemblenumbersage <- read.csv("data-raw/ensemble_numbers_age.csv")
usethis::use_data(ensemblenumbersage, overwrite = TRUE)
usethis::use_r('ensemblenumbersage')
sinew::makeOxygen('ensemblenumbersage')

salmongroups <- read.csv("data-raw/salmon_groups.csv")
usethis::use_data(salmongroups, overwrite = TRUE)
usethis::use_r('salmongroups')
sinew::makeOxygen('salmongroups')

functionalgroups <- read.csv("data-raw/PugetSoundAtlantisFunctionalGroups.csv")
usethis::use_data(functionalgroups, overwrite = TRUE)
usethis::use_r('functionalgroups')
sinew::makeOxygen('functionalgroups')

ppreymatrix <- read.csv("data-raw/ps_prey_matrix_final.csv")
usethis::use_data(ppreymatrix, overwrite = TRUE)
usethis::use_r('ppreymatrix')
sinew::makeOxygen('ppreymatrix')

