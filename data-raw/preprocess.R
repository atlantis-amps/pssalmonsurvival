## Data preprocessing script

# Load data
ensemblebiomass <- read.csv("data-raw/ensemble_biomass.csv")
usethis::use_data(ensemblebiomass, overwrite = TRUE)


ensemblenumbersage <- read.csv("data-raw/ensemble_numbers_age.csv")
usethis::use_data(ensemblenumbersage, overwrite = TRUE)
usethis::use_r('ensemblenumbersage')
makeOxygen('ensemblenumbersage')
