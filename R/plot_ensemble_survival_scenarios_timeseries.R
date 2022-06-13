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



plot_ensemble_survival <- function(ensemblenumbersagescenarios, salmongroups){

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

  #readr::write_csv(salmon.return.nums,"scenarios_survival.csv")

  max_year  <- max(salmon.return.nums$year_no)

  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    nrow(salmongroups)/ 12
  )

  #  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")

  print(n_pages)

  plot.list <- list()

  for (i in seq_len(n_pages)) {

    survival.plot <- salmon.return.nums %>%
      dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=(max_year-3)) %>%
      ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, group= model_ver, colour=model_ver, fill = model_ver))+
      ggplot2::geom_line() +
      ggplot2::ylim(0,2) +
      ggthemes::theme_few() +
      ggthemes::scale_colour_few(name = "Model version") +
      ggforce::facet_wrap_paginate(scenario_var ~ Long.Name, ncol = 3, nrow = 4, page = i, shrink = FALSE, labeller = 'label_value')+
      ggplot2::labs(y="Survival")

    plot.list[[i]] <- survival.plot

    thisplotname <- paste0("salmon_survival_plot_",i,".png")

    ggplot2::ggsave(thisplotname,plot = survival.plot, device = "png", width = 21, height = 24, units = "cm")

  }


  return(plot.list)
}
