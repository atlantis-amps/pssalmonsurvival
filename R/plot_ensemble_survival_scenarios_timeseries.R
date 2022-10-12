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



plot.salmon.data <- salmon.return.nums %>%
    dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
    tidyr::drop_na() %>%
    dplyr::filter(year_no<=ret_year) %>%
    dplyr::group_by(scenario_name, scenario_var, Long.Name, Year) %>%
    dplyr::summarise(max_model = max(survival), min_model = min(survival), mean_model = mean(survival))

 # readr::write_csv(salmon.return.nums,"scenarios_survival_timeseries.csv")


#  plot.list.line <- list()
 # plot.list.range <- list()

  scenario.vars <- salmon.return.nums %>%
    dplyr::distinct(scenario_var) %>%
    dplyr::pull(scenario_var)


  col.pal <- paletteer::paletteer_d("rcartocolor::Vivid", n = 10)

  bottom.up.sc <- c("Hatchery Chinook competition", "Hatchery competition", "Wild pink and chum salmon competition", "Gelatinous zooplankton abundance", "Herring abundance")
  top.down.sc <- c("Pinniped predation", "Porpoise predation", "Seabird predation" , "Spiny dogfish predation")
  scenario.list <- list("Bottom up hypotheses" = bottom.up.sc, "Top down hypotheses" = top.down.sc)

  for(eachscenario in 1:length(scenario.list)){

    thislist <- scenario.list[eachscenario]

    thesescenarios <- thislist %>% unlist()
    thisname <- names(thislist)

    col.pal <- c("#ffbe0b","#0a9396")
    col.fill <- c("#ffbe0b","#0a9396", "#3fd2c7", "#99ddff", "#00458b", "#549896", "#83bb90", "#f1dd88", "#f7a482", "#81aa2c", "#f293b1", "#ed5181")
    col.pal <- c("#ffbe0b","#0a9396")



    plot.data <- salmon.return.nums %>%
      dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year_no<=ret_year) %>%
      dplyr::filter(scenario_name %in% thesescenarios) %>%
      dplyr::left_join(plot.salmon.data, by = c("scenario_name", "scenario_var", "Long.Name", "Year"))

    # Calculate the number of pages with 9 panels per page one per species
    n_groups <- plot.data %>%
      dplyr::distinct(Long.Name) %>%
      nrow

    n_pages <- ceiling(n_groups / 9)


    #  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")

    print(n_pages)

    for(eachscenariovar in c("-20%","+20%")) {


      scenario.plot.data <- plot.data %>%
        dplyr::filter(scenario_var == eachscenariovar) %>%
        dplyr::mutate(Year = as.factor(Year))

      col.pal <- c("darkorange","darkorange3","darkorange4","darksalmon","coral2","darkseagreen1","darkseagreen4","darkolivegreen2","darkolivegreen4")

      for (i in seq_len(n_pages)) {

     survival.plot.bar.time <- scenario.plot.data %>%
          dplyr::filter(!survival>1) %>%
          ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, color = scenario_name))+
          ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.6, alpha = 0.6) +
          #ggplot2::ylim(0,1) +
          #ggnewscale::new_scale_color() +
          #ggplot2::geom_line(ggplot2::aes(group= scenario_name, colour=scenario_name)) +
          ggthemes::theme_few() +
          ggplot2::scale_colour_manual(values= col.pal, name = "Scenario") +
          ggforce::facet_wrap_paginate(.~ Long.Name, ncol = 1, nrow = 4, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y") +
          ggplot2::labs(y="Survival", subtitle = eachscenariovar) +
          ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)) +
          ggplot2::theme(legend.position="bottom")

   #     plot.list.range[[i]] <- survival.plot.bar.time

        thisplotname <- paste0("salmon_survival_barplot_time_",eachscenariovar,"_p",i,".png")

        ggplot2::ggsave(thisplotname,plot = survival.plot.bar.time, device = "png", width = 24, height = 22, units = "cm")


        survival.plot.line <- salmon.return.nums %>%
          dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
          tidyr::drop_na() %>%
          dplyr::filter(year_no<=ret_year) %>%
          dplyr::filter(scenario_var == eachscenariovar) %>%
          dplyr::left_join(plot.salmon.data, by = c("scenario_name", "scenario_var", "Long.Name", "Year")) %>%
          ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, ymax = max_model, ymin = min_model, group= model_ver, color = model_ver))+
          ggplot2::geom_line() +
          ggplot2::ylim(0,1) +
          ggthemes::theme_few() +
          # ggplot2::scale_fill_manual(values = col.pal) +
          ggplot2::scale_color_manual(values = col.pal, name = " Model version") +
          ggforce::facet_wrap_paginate(Long.Name ~ scenario_name, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y")+
          ggplot2::labs(y="Survival", subtitle = eachscenariovar) +
          ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)) +
          ggplot2::theme(legend.position = "bottom")

  #      plot.list.line[[i]] <- survival.plot.line

        thisplotname <- paste0("salmon_survival_plot_linetime_",eachscenariovar,"_p",i,".png")

        ggplot2::ggsave(thisplotname,plot = survival.plot.line, device = "png", width = 21, height = 24, units = "cm")

      }


    }


}
  return(scenario.plot.data)
}
