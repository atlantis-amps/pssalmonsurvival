#' analyze trajectories
#'
#' @param ensemblenumbersagescenarios
#'
#' @return
#' @export
#'
#' @examples

make_trajectories <- function(ensemblenumbersagescenarios, salmongroups){

  salmon.max.nums <- ensemblenumbersagescenarios %>%
    dplyr::group_by(model_ver, Code, age, year_no, scenario_name, scenario_var) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by="Code") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="salmon competition","wild pink and chum salmon competition",
                                                 dplyr::if_else(scenario_name=="mammal predation","pinniped predation",
                                                                dplyr::if_else(scenario_name=="seabirds predation","seabird predation",
                                                                               dplyr::if_else(scenario_name="gelatinous zooplankton increase","gelatinous zooplankton abundance",
                                                                                              dplyr::if_else(scenario_name=="herring decrease","herring abundance",scenario_name)))))) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var=="0_8", "-20%","+20%"))

  #dplyr::filter(!model_ver%in% plotmodels)

  salmon.juv.nums <- salmon.max.nums %>%
    dplyr::filter(age == 1) %>%
    dplyr::rename(juv_nums = max_nums)

  salmon.return.nums <- salmon.max.nums %>%
    dplyr::filter(age == (years_away + 1)) %>%
    dplyr::mutate(cohort_yr = year_no - age) %>%
    dplyr::select(-years_away) %>%
    dplyr::rename(age_return = age, return_nums=max_nums, year_sim = year_no, year_no= cohort_yr) %>%
    dplyr::left_join(salmon.juv.nums, by=c("model_ver","Code","year_no","scenario_name","scenario_var", "Long.Name","NumCohorts","Name", "migiobox")) %>%
    dplyr::mutate(survival = return_nums/juv_nums) %>%
    dplyr::mutate(model_ver = as.factor(model_ver)) %>%
    dplyr::mutate(Year = year_sim - 2010) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(max_year = max(year_no)) %>%
    dplyr::mutate(ret_year = max_year-3) %>%
    dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
    dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Hatchery Chinook competition", "Hatchery competition", "Wild pink and chum salmon competition", "Gelatinous zooplankton abundance", "Herring abundance", "Pinniped predation",
                                                       "Porpoise predation", "Seabird predation" , "Spiny dogfish predation"))




  trajectory.data <- salmon.return.nums %>%
    dplyr::mutate(scenario_code = as.integer(factor(scenario_name, levels = unique(scenario_name))),
                  model_ver = as.numeric(model_ver)) %>%
    dplyr::mutate(Year = year_sim - 2010) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(max_year = max(year_no)) %>%
    dplyr::mutate(ret_year = max_year-3) %>%
    dplyr::filter(year_no<=ret_year) %>%
    dplyr::filter(year_no==max(year_no))


  scenariovars <- trajectory.data %>%
    dplyr::distinct(scenario_var) %>%
    dplyr::pull(scenario_var)

  salmon.codes <- salmongroups$Code

  for(eachscenariovar in scenariovars) {

    for(eachspecies in salmon.codes) {

      this.trajectory <- trajectory.data %>%
        dplyr::filter(scenario_var==eachscenariovar) %>%
        dplyr::filter(Code==eachspecies)

      juv.ret.trajectory <- cbind(this.trajectory$juv_nums, this.trajectory$return_nums)

      surv.dist <- dist(juv.ret.trajectory)

      model.ver <- this.trajectory$model_ver
      scenario.code <- this.trajectory$scenario_code

      oldpar <- par(mar=c(4,4,1,1))
      ecotraj::trajectoryPCoA(surv.dist, model.ver, scenario.code, traj.colors = c("black","red", "blue","yellow","green","darkgreen","darkblue","darkred","orange"), lwd = 2,
                              survey.labels = T)
      legend("topleft", col=c("black","red", "blue","yellow","green","darkgreen","darkblue","darkred","orange"),
             legend=c("Scenario 1", "Scenario 2", "Scenario 3","Scenario 4", "Scenario 5", "Scenario 6","Scenario 7", "Scenario 8", "Scenario 9"), bty="n", lty=1, lwd = 2)

    }

  }
}


