#' @title plot salmon survival of ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002




plot_ensemble_survival_scenarios <- function(ensemblenumbersagescenarios, salmongroups, plotmodels){

  salmon.max.nums <- ensemblenumbersagescenarios %>%
    dplyr::group_by(scenario_name, scenario_var, model_ver, Code, age, year_no) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by="Code") %>%
    dplyr::ungroup() %>%
    dplyr::filter(!model_ver%in% plotmodels)

  salmon.juv.nums <- salmon.max.nums %>%
    dplyr::filter(age == 1) %>%
    dplyr::rename(juv_nums = max_nums)

  salmon.return.nums <- salmon.max.nums %>%
    dplyr::filter(age == (years_away + 1)) %>%
    dplyr::mutate(cohort_yr = year_no - age) %>%
    dplyr::select(-years_away) %>%
    dplyr::rename(age_return = age, return_nums=max_nums, year_sim = year_no, year_no= cohort_yr) %>%
    dplyr::left_join(salmon.juv.nums, by=c("scenario_name","scenario_var","migiobox","model_ver","Code","year_no","Long.Name","NumCohorts","Name")) %>%
    dplyr::mutate(survival = return_nums/juv_nums) %>%
    dplyr::mutate(model_ver = as.factor(model_ver)) %>%
    dplyr::mutate(Year = year_sim - 2010) %>%
    filter(year_sim==2040 | year_sim==2041)


salmon.sur.low <- salmon.return.nums %>%
  filter(scenario_var=="0_8") %>%
  dplyr::rename(scenario_low=scenario_var,juv_nums_low = juv_nums, survival_low = survival, return_nums_low = return_nums)

salmon.sur.high <- salmon.return.nums %>%
  filter(scenario_var=="1_2") %>%
  dplyr::rename(scenario_high=scenario_var,juv_nums_high = juv_nums, survival_high = survival, return_nums_high = return_nums)


salmon.sur.int <- salmon.sur.low %>%
  left_join(salmon.sur.high, by=c("scenario_name","model_ver","Code","age_return","year_sim","Name","Long.Name","NumCohorts","migiobox","year_no","age","years_away","Year"))

  max_year  <- max(salmon.return.nums$year_no)

  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    nrow(salmongroups)/ 12
  )

#  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")

  print(n_pages)

  plot.list <- list()

  for (i in seq_len(n_pages)) {

    low <- salmon.sur.low %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=(max_year-3)) %>%
      dplyr::filter(scenario_name==thisscenario) %>%
      pull(survival_low)

    high <- salmon.sur.high %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=(max_year-3)) %>%
      dplyr::filter(scenario_name==thisscenario) %>%
      pull(survival_high)

    survival.plot <- salmon.return.nums %>%
      dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=(max_year-3)) %>%
      dplyr::filter(scenario_name==thisscenario) %>%
      dplyr::filter(scenario_var==thisvar) %>%
      ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, fill= model_ver, colour=model_ver))+
      ggplot2::geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2)
      ggplot2::geom_line() +
      ggplot2::ylim(0,1) +
      ggthemes::theme_few() +
      ggthemes::scale_colour_few(name = "Model version") +
      ggforce::facet_wrap_paginate(~ Long.Name, ncol = 3, nrow = 4, page = i, shrink = FALSE, labeller = 'label_value')+
      ggplot2::labs(y="Survival")

    plot.list[[i]] <- survival.plot

    thisplotname <- paste0("salmon_survival_plot_",i,".png")

    ggplot2::ggsave(thisplotname,plot = survival.plot, device = "png", width = 21, height = 24, units = "cm")

  }


  return(plot.list)
}
