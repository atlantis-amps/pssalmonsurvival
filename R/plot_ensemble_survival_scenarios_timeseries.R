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
    dplyr::ungroup() %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="salmon competition","wild pink and chum salmon competition",
                                               dplyr::if_else(scenario_name=="mammal predation","pinniped predation",
                                                              dplyr::if_else(scenario_name=="seabirds predation","seabird predation", scenario_name)))) %>%
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
    dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Hatchery Chinook competition", "Hatchery competition", "Wild pink and chum salmon competition", "Gelatinous zooplankton increase", "Herring decrease", "Pinniped predation",
                                                       "Porpoise predation", "Seabird predation" , "Spiny dogfish predation"))



plot.salmon.data <- salmon.return.nums %>%
    dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
    tidyr::drop_na() %>%
    dplyr::filter(year_no<=ret_year) %>%
    dplyr::group_by(scenario_name, scenario_var, Long.Name, Year) %>%
    dplyr::summarise(max_model = max(survival), min_model = min(survival), mean_model = mean(survival))

  #readr::write_csv(salmon.return.nums,"scenarios_survival.csv")


  plot.list.line <- list()
  plot.list.range <- list()

  scenario.vars <- salmon.return.nums %>%
    dplyr::distinct(scenario_var) %>%
    dplyr::pull(scenario_var)


  col.pal <- paletteer::paletteer_d("rcartocolor::Vivid", n = 10)

  bottom.up.sc <- c("Hatchery Chinook competition", "Hatchery competition", "Wild pink and chum salmon competition", "Gelatinous zooplankton increase", "Herring decrease")
  top.down.sc <- c("Pinniped predation", "Porpoise predation", "Seabird predation" , "Spiny dogfish predation")
  scenario.list <- list("Bottom up hypotheses" = bottom.up.sc, "Top down hypotheses" = top.down.sc)

  for(eachscenario in 1:length(scenario.list)){

    thislist <- scenario.list[eachscenario]

    thesescenarios <- thislist %>% unlist()
    thisname <- names(thislist)

    col.pal <- c("#ffbe0b","#0a9396")
    col.fill <- c("#ffbe0b","#0a9396", "#3fd2c7", "#99ddff", "#00458b", "#549896", "#83bb90", "#f1dd88", "#f7a482", "#81aa2c", "#f293b1", "#ed5181")



    plot.data <- salmon.return.nums %>%
      dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=ret_year) %>%
      dplyr::filter(scenario_name %in% thesescenarios) %>%
      left_join(plot.salmon.data, by = c("scenario_name", "scenario_var", "Long.Name", "Year"))

    # Calculate the number of pages with 9 panels per page one per species
    n_groups <- plot.data %>%
      distinct(Long.Name) %>%
      nrow

    n_pages <- ceiling(n_groups / 9)


    #  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")

    print(n_pages)

    for(eachscenariovar in c("-20%","+20%")) {

      for (i in seq_len(n_pages)) {

        survival.plot.range <- plot.data %>%
          filter(scenario_var == eachscenariovar) %>%
          ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, ymax = max_model, ymin = min_model, group= scenario_name, fill=scenario_name))+
          ggplot2::geom_ribbon(alpha =0.3) +
          ggplot2::geom_line(ggplot2::aes(x = Year, y = max_model, group=model_ver, color = scenario_name)) +
          ggplot2::ylim(0,1) +
          ggthemes::theme_few() +
          ggplot2::scale_fill_manual(values = col.pal) +
          ggforce::facet_wrap_paginate( ~ Long.Name, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y")+
          ggplot2::labs(y="Survival", subtitle = paste("Abundance multiplier", gsub("_",".", eachscenariovar))) +
          ggplot2::theme(strip.text.x = element_text(size = 9)) +
          ggplot2::guides(fill = "none")

        plot.list.range[[i]] <- survival.plot.range

        thisplotname <- paste0("salmon_survival_plot_range_",eachscenariovar,"_p",i,".png")

        ggplot2::ggsave(thisplotname,plot = survival.plot.range, device = "png", width = 24, height = 22, units = "cm")


        survival.plot.line <- salmon.return.nums %>%
          dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
          tidyr::drop_na() %>%
          dplyr::filter(year_no<=ret_year) %>%
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


}
  return(plot.list)
}
