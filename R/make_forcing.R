#' @title make forcing files for salmon survival
#' @param ncfile, AMPS
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @descriptions Code to modify AMPS nc file to create forcing files for sensitivity simulations
#' @descriptions function to create forcing files, force prm and bash file
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002

make_forcing <-
  function(fungroupname,
           ncfile,
           func.groups,
           num.forcing.years,
           rate.multiplier,
           bash.file,
           force.prm) {

    #rate multiplier name
    rate.name <- stringr::str_replace(rate.multiplier, "[.]", "_")

    #new forcing file name
    force.prm.name <-
      paste0("PugetSound_force_", fungroupname, "_", rate.name, ".prm")

    #include together marine mammals for predation scenario

    if (fungroupname %in% c("Pinniped", "California_sea_lion", "Harbor_seal")) {
      force.prm.name <-
        paste0("PugetSound_force_mammal_predation_", rate.name, ".prm")
    }

    #include seabirds in predation scenario
    if (fungroupname %in% c("Pisc_Seabird", "NonPisc_Seabird")) {
      force.prm.name <-
        paste0("PugetSound_force_seabirds_predation", rate.name, ".prm")
    }

    #include herring groups in herring trajectory
    if (fungroupname %in% c("Herring_PS", "Herring_Cherry")) {
      force.prm.name <-
        paste0("PugetSound_force_herring_trend_", rate.name, ".prm")
    }

    #include pink and chum in salmon competition
    if (fungroupname %in% c("PinkSY_Fish", "ChumFSY_Fish")) {

      force.prm.name <-
        paste0("PugetSound_force_salmon_competition_",
               rate.name,
               ".prm")
    }

    #include hatchery salmon in hatchery competition
    if (fungroupname %in% c("ChinookY_Fish","ChinookSY_Fish","CohoHY_Fish","ChumHSY_Fish")) {

      force.prm.name <-
        paste0("PugetSound_force_hatchery_competition_",
               rate.name,
               ".prm")
        }

    if (fungroupname %in% c("ChinookY_Fish_2","ChinookSY_Fish_2")) {

      force.prm.name <-
        paste0("PugetSound_force_chinookhatchery_competition_",
               rate.name,
               ".prm")

        fungroupname <- stringr::str_replace(fungroupname,"_2","")

    }

#get functional group name
this.group <- func.groups %>%
  dplyr::filter(name == fungroupname)
#number of cohorts, one nc forcing file per cohort
no.cohorts <- this.group$NumCohorts

if (this.group$isVertebrate == 1) {
  #if the group is vertebrate there is one Nums forcing file per cohort
  cdf.variable <-
    paste(fungroupname, 1:no.cohorts, "_Nums", sep = "")
  print(cdf.variable)

} else if (this.group$isVertebrate == 0) {
  #invertebrates are biomass pools with only one forcing file
  cdf.variable <- paste0(this.group$name, "_N")
  print(cdf.variable)
}



    #name new directory
    new.dir <-
      stringr::str_replace(force.prm.name, "[.]prm", "")
    #create new directory
    dir.create(here::here("modelfiles", new.dir))


    #open original nc file
    nc <- ncdf4::nc_open(here::here("modelfiles", ncfile))
    #list names in forcing file
    attribute.names <- attributes(nc$var)$names

    #loop over age classes to create a new nc forcing file for each
    for (eachageclass in cdf.variable) {
      print(eachageclass)
      #get age class from original nc file
      orig.ncvar <- ncdf4::ncvar_get(nc, eachageclass)
      #get attributes
      orig.attributes <-
        ncdf4::ncatt_get(nc, eachageclass, attname = NA, verbose = FALSE)
      # create an array with the number of simulation years
      forcing.array <- array(NA, dim = c(7, 89, num.forcing.years))

      #copy the abundance trend in the age class for each year
      for (yearIndex in 1:num.forcing.years)
      {
        forcing.array[, , yearIndex] <- orig.ncvar * rate.multiplier

      }

      #define dimensions for nc file
      maxSeconds <- 31536000 * (num.forcing.years - 1)

      b <- ncdf4::ncdim_def("b", "boxNum", 0:88)
      z <- ncdf4::ncdim_def("z", "depthBin", 1:7)
      t <-
        ncdf4::ncdim_def("t",
                         "seconds since 2011-01-01",
                         seq(0, maxSeconds, by = 31536000),
                         unlim = TRUE)

      #define variables
      variable.definition <-
        ncdf4::ncvar_def(
          eachageclass,
          units = orig.attributes$units,
          dim = list(z, b, t),
          missval = 0,
          longname = orig.attributes$long_name,
          prec = "double",
          shuffle = FALSE,
          compression = NA,
          chunksizes = NA,
          verbose = FALSE
        )


      #new name of nc forcing file
      new.nc.forcing.name <-
        paste(eachageclass, "_ncforcing_", rate.name, ".nc", sep = "")
      # new name of cdf forcing file
      new.cdf.forcing.name <-
        stringr::str_replace(new.nc.forcing.name, "[.]nc", ".cdf")

      #After a variable is defined with this call, and created on disk using nc_create, then data values for the variable can be written to disk using ncvar_put.

      new.nc.forcing <-
        ncdf4::nc_create(
          here::here("modelfiles", new.dir, new.nc.forcing.name),
          variable.definition,
          force_v4 = FALSE,
          verbose = FALSE
        )

      #put in new variable definitions
      ncdf4::ncvar_put(new.nc.forcing, variable.definition, forcing.array)
      # check and see what's in the new NC file
      # varid=0 means it is a global attribute

      ncdf4::ncatt_put(new.nc.forcing, 0, "title", "Salmon survival forcings")
      ncdf4::ncatt_put(new.nc.forcing,
                       0,
                       "geometry",
                       "PugetSound_89b_070116.bgm")
      ncdf4::ncatt_put(new.nc.forcing, 0, "parameters", "")

      attributes(new.nc.forcing$var)$names

      #close new forcing file
      ncdf4::nc_close(new.nc.forcing)

      new.nc <- RNetCDF::open.nc(here::here("modelfiles", new.dir, new.nc.forcing.name), write = TRUE)

      RNetCDF::att.put.nc(new.nc, eachageclass, "valid_min", "NC_DOUBLE", 0)
      RNetCDF::att.put.nc(new.nc, eachageclass, "valid_max", "NC_DOUBLE", 10000000000000)
      RNetCDF::close.nc(new.nc)
      setwd(here::here("modelfiles", new.dir))
      #dump nc file into cdf
      system(paste("ncdump", new.nc.forcing.name , ">", new.cdf.forcing.name),
             wait = TRUE)

      setwd(here::here())
    }

    #close the original nc file
    ncdf4::nc_close(nc)


    #copy bash file
    file.copy(
      from = here::here("modelfiles", bash.file),
      to = here::here("modelfiles", new.dir, bash.file)
    )

    #add new prm forcing file to bash file

    cat(
      paste0(
        "atlantisMerged -i AMPS.nc 0 -o AMPS_OUT.nc -r PugetSound_run.prm -f ",
        force.prm.name
      ),
      file = here::here("modelfiles", new.dir, bash.file),
      append = TRUE
    )
    cat(
      " -p PugetSound_physics.prm -b AMPSbioparamV6_B1984.prm  -h PugetSound_harvest_mfc_tuned.prm -e VMPA_setas_economics.prm -s isExternal_PugetSoundAtlantisFunctionalGroups.csv -q PugetSound_fisheries.csv -d outputFolder 2>outamps",
      file = here::here("modelfiles", new.dir, bash.file),
      append = TRUE
    )

    #copy new prm forcing file
    file.copy(
      from = here::here("modelfiles", force.prm),
      to = here::here("modelfiles", new.dir, force.prm.name)
    )

    #add number of tracers and tracer names
    cat(
      "\n",
      file = here::here("modelfiles", new.dir, force.prm.name),
      append = TRUE
    )
    cat(
      "# The number of tracers to search for in the files and the names of those tracers\n",
      file = here::here("modelfiles", new.dir, force.prm.name),
      append = TRUE
    )
    cat(
      paste0("nforceTracers ", no.cohorts, "\n"),
      file = here::here("modelfiles", new.dir, force.prm.name),
      append = TRUE
    )
    cat(
      paste0("tracerNames ", no.cohorts, "\n"),
      file = here::here("modelfiles", new.dir, force.prm.name),
      append = TRUE
    )
    cat(
      cdf.variable,
      sep = "\n",
      file = here::here("modelfiles", new.dir, force.prm.name),
      append = TRUE
    )
    cat(
      "\n",
      file = here::here("modelfiles", new.dir, force.prm.name),
      append = TRUE
    )

    #add list of variables to prm forcing file
    for (eachvariable in cdf.variable) {
      cat(
        paste0(eachvariable, "_nFiles 1"),
        sep = "\n",
        file = here::here("modelfiles", new.dir, force.prm.name),
        append = TRUE
      )
      cat(
        paste0(
          eachvariable,
          "_File0.name ",
          paste(eachvariable, "_ncforcing_", rate.name, ".nc", sep = "")
        ),
        sep = "\n",
        file = here::here("modelfiles", new.dir, force.prm.name),
        append = TRUE
      )
      cat(
        paste0(eachvariable, "_rewind 0"),
        sep = "\n",
        file = here::here("modelfiles", new.dir, force.prm.name),
        append = TRUE
      )
      cat(
        "\n",
        file = here::here("modelfiles", new.dir, force.prm.name),
        append = TRUE
      )

    }


  }
