#' @title plot salmon survival of ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @descriptions Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002




plot_ensemble_survival <- function(ensemblenumbersage, salmongroups, plotmodels){

  salmon.max.nums <- ensemblenumbersage %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(model_ver, Code, age, year_no) %>%
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
    dplyr::left_join(salmon.juv.nums, by=c("model_ver","Code","year_no","Long.Name","NumCohorts","Name")) %>%
    dplyr::mutate(survival = return_nums/juv_nums) %>%
    dplyr::mutate(model_ver = as.factor(model_ver))

  max_year  <- max(salmon.return.nums$year_no)

  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    nrow(salmongroups)/ 12
  )

  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")

  print(n_pages)
  for (i in seq_len(n_pages)) {

    survival.plot <- salmon.return.nums %>%
      dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=(max_year-3)) %>%
      ggplot2::ggplot(ggplot2::aes(x = year_no, y = survival, group= model_ver, colour=model_ver))+
      ggplot2::geom_line() +
      ggplot2::ylim(0,1) +
      ggplot2::scale_color_manual(values=col.pal, name = "Model version")+
      ggforce::facet_wrap_paginate(~ Long.Name, ncol = 3, nrow = 4, page = i, shrink = FALSE, labeller = 'label_value')

    thisplotname <- paste0("salmon_survival_plot_",i,".png")

    ggplot2::ggsave(thisplotname,plot = survival.plot, device = "png", width = 21, height = 24, units = "cm")

  }


  return(salmon.return.nums)
}
