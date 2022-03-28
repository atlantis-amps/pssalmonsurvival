## ----setup--------------------------------------------------------------------
install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
library(pssalmonsurvival)


## ----map model extent, echo=TRUE----------------------------------------------

file.name <- "amps_model_map.png"

make_map(file.name)


## -----------------------------------------------------------------------------
#Plot model food web

data("ppreymatrix")
plot.name <- "ps_foodweb.png"
  
plot_foodweb(ppreymatrix, plot.name)



## ----plot biomass base runs, echo=FALSE---------------------------------------

#biomass comparison between model variants, base runs
data("ensemblebiomass")
plotmodels <- c(1,6) # eliminated model versions 1 & 6

plot_ensemblebiomass(ensemblebiomass, plotmodels)


## ----plot survival base runs, echo=FALSE--------------------------------------
#numbers at age from base runs
data("ensemblenumbersage")
data("salmongroups")
plotmodels <- c(1,6) # eliminated model versions 1 & 6

plot_ensemble_survival(ensemblenumbersage, salmongroups, plotmodels)


## ----make scenario forcings, include=FALSE------------------------------------

#groups for scenarios
sim.groups <- c("Pinniped", "California_sea_lion", "Harbor_seal", 
                "PinkSY_Fish", "ChumFSY_Fish", 
                "Herring_PS", "Herring_Cherry",
                "Porpoise",
                "Spinydog_Fish", 
                "Pisc_Seabird","NonPisc_Seabird",
                "Gel_Zoo", 
                "ChinookY_Fish","ChinookSY_Fish","CohoHY_Fish","ChumHSY_Fish",
                #chinook hatchery are repeated twice for a scenario with just Chinook
                "ChinookY_Fish_2","ChinookSY_Fish_2")

data("functionalgroups")
data("salmongroups")

ncfile = "AMPS.nc"
num.forcing.years = 50
bash.file = "amps_cal.sh"
force.prm = "PugetSound_force.prm"
func.groups = functionalgroups

#scenario 20% decrease
rate.multiplier = 0.8

lapply(sim.groups, make_forcing, ncfile, func.groups, num.forcing.years, rate.multiplier, bash.file, force.prm, salmongroups)

#scenario 20% increase
rate.multiplier = 1.2

lapply(sim.groups, make_forcing, ncfile, func.groups, num.forcing.years, rate.multiplier, bash.file, force.prm, salmongroups)


