#' @title plot time series of salmon survival from ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return
#' @export
#'
#' @description Code to plot survival of multiple models over time
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com June 2002
#'



plot_ensemble_survival_scenarios_timeseries <- function(ensemblenumbersagescenarios, salmongroups){

  salmon.max.nums <- ensemblenumbersagescenarios %>%
    dplyr::group_by(model_ver, Code, age, year_no, scenario_name, scenario_var) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by="Code") %>%
    dplyr::ungroup() # %>%
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
    tidyr::drop_na()

plot.salmon.data <- salmon.return.nums %>%
    dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
    tidyr::drop_na() %>%
    dplyr::filter(year_no<=(max_year-3)) %>%
    dplyr::group_by(scenario_name, scenario_var, Long.Name, Year) %>%
    dplyr::summarise(max_model = max(survival), min_model = min(survival), mean_model = mean(survival))

  #readr::write_csv(salmon.return.nums,"scenarios_survival.csv")

  max_year  <- max(salmon.return.nums$year_no)

  # Calculate the number of pages with 9 panels per page one per species
  n_pages <- ceiling(
    nrow(salmongroups)
  )

  #  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")

  print(n_pages)

  plot.list.line <- list()
  plot.list.range <- list()

  scenario.vars <- salmon.return.nums %>%
    dplyr::distinct(scenario_var) %>%
    dplyr::pull(scenario_var)


  col.pal <- paletteer::paletteer_d("rcartocolor::Vivid", n = 10)

  for(eachscenariovar in scenario.vars) {

  for (i in seq_len(n_pages)) {

    survival.plot.range <- salmon.return.nums %>%
      dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=(max_year-3)) %>%
      dplyr::filter(scenario_var == eachscenariovar) %>%
      left_join(plot.salmon.data, by = c("scenario_name", "scenario_var", "Long.Name", "Year")) %>%
      ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, ymax = max_model, ymin = min_model, group= scenario_name, fill=scenario_name))+
      ggplot2::geom_ribbon() +
      ggplot2::ylim(0,1) +
      ggthemes::theme_few() +
      ggplot2::scale_fill_manual(values = col.pal) +
      ggforce::facet_wrap_paginate(Long.Name ~ scenario_name, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y")+
      ggplot2::labs(y="Survival", subtitle = paste("Abundance multiplier", gsub("_",".", eachscenariovar))) +
      ggplot2::theme(strip.text.x = element_text(size = 9)) +
      ggplot2::guides(fill = "none")

    plot.list.range[[i]] <- survival.plot.range

    thisplotname <- paste0("salmon_survival_plot_range_",eachscenariovar,"_p",i,".png")

    ggplot2::ggsave(thisplotname,plot = survival.plot.range, device = "png", width = 24, height = 22, units = "cm")


    survival.plot.line <- salmon.return.nums %>%
      dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=(max_year-3)) %>%
      dplyr::filter(scenario_var == eachscenariovar) %>%
      left_join(plot.salmon.data, by = c("scenario_name", "scenario_var", "Long.Name", "Year")) %>%
      ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, ymax = max_model, ymin = min_model, group= model_ver, color = model_ver))+
      ggplot2::geom_line() +
      ggplot2::ylim(0,1) +
      ggthemes::theme_few() +
     # ggplot2::scale_fill_manual(values = col.pal) +
      ggplot2::scale_color_manual(values = col.pal, name = " Model version") +
      ggforce::facet_wrap_paginate(Long.Name ~ scenario_name, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y")+
      ggplot2::labs(y="Survival") +
      ggplot2::theme(strip.text.x = element_text(size = 9)) +
      ggplot2::theme(legend.position = "bottom")

    plot.list.line[[i]] <- survival.plot.line

    thisplotname <- paste0("salmon_survival_plot_line_",eachscenariovar,"_p",i,".png")

    ggplot2::ggsave(thisplotname,plot = survival.plot.line, device = "png", width = 21, height = 24, units = "cm")

  }

}
  return(plot.list)
}
