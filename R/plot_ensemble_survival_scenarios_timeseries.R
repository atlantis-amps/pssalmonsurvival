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



plot_ensemble_survival_scenarios_timeseries <- function(ensemblenumbersagescenarios, salmongroups, indsalmoneffect) {

    salmon.max.nums <- ensemblenumbersagescenarios %>%
        dplyr::group_by(model_ver, Code, age, year_no, scenario_name, scenario_var) %>%
        dplyr::summarise(max_nums = max(nums)) %>%
        dplyr::left_join(salmongroups, by = "Code") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(scenario_name = tolower(scenario_name)) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "salmon competition", "wild pink & chum salmon competition", dplyr::if_else(scenario_name ==
            "mammal predation", "pinniped predation", dplyr::if_else(scenario_name == "seabirds predation", "seabird predation", dplyr::if_else(scenario_name ==
            "gelatinous zooplankton increase", "gelatinous zooplankton abundance", dplyr::if_else(scenario_name == "herring decrease", "herring abundance", scenario_name)))))) %>%
        dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "0_8", "-20%", "+20%"))

    # dplyr::filter(!model_ver%in% plotmodels)

    salmon.juv.nums <- salmon.max.nums %>%
        dplyr::filter(age == 1) %>%
        dplyr::rename(juv_nums = max_nums)

    salmon.return.nums <- salmon.max.nums %>%
        dplyr::filter(age == (years_away + 1)) %>%
        dplyr::mutate(cohort_yr = year_no - age) %>%
        dplyr::select(-years_away) %>%
        dplyr::rename(age_return = age, return_nums = max_nums, year_sim = year_no, year_no = cohort_yr) %>%
        dplyr::left_join(salmon.juv.nums, by = c("model_ver", "Code", "year_no", "scenario_name", "scenario_var", "Long.Name", "NumCohorts", "Name", "migiobox")) %>%
        dplyr::mutate(survival = (return_nums/juv_nums) * 100) %>%
        dplyr::mutate(survival = dplyr::if_else(survival > 100, 100, survival)) %>%
        dplyr::mutate(model_ver = as.factor(model_ver)) %>%
        dplyr::mutate(Year = year_sim - 2010) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(max_year = max(year_no)) %>%
        dplyr::mutate(ret_year = max_year - 3) %>%
        dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "Hatchery chinook competition", "Hatchery Chinook competition", scenario_name)) %>%
        dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition",
            "Gelatinous zooplankton abundance", "Herring abundance", "Pinniped predation", "Porpoise predation", "Seabird predation", "Spiny dogfish predation"))



    plot.salmon.data <- salmon.return.nums %>%
        dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
        tidyr::drop_na() %>%
        dplyr::filter(year_no <= ret_year) %>%
        dplyr::group_by(scenario_name, scenario_var, Long.Name, Year) %>%
        dplyr::summarise(max_model = max(survival), min_model = min(survival), mean_model = mean(survival)) %>%
        dplyr::left_join(indsalmoneffect, by = c("scenario_name", "scenario_var"))

    # readr::write_csv(salmon.return.nums,'scenarios_survival_timeseries.csv')


    # plot.list.line <- list() plot.list.range <- list()

    scenario.vars <- salmon.return.nums %>%
        dplyr::distinct(scenario_var) %>%
        dplyr::pull(scenario_var)


    bottom.up.sc <- c("Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition", "Gelatinous zooplankton abundance", "Herring abundance")
    top.down.sc <- c("Pinniped predation", "Porpoise predation", "Seabird predation", "Spiny dogfish predation")
    scenario.list <- list(`Bottom up hypotheses` = bottom.up.sc, `Top down hypotheses` = top.down.sc)

    for (eachhypothesis in 1:length(scenario.list)) {

        thislist <- scenario.list[eachhypothesis]

        thesescenarios <- thislist %>%
            unlist()

        thisname <- names(thislist)

        plot.data <- salmon.return.nums %>%
            dplyr::mutate(Long.Name = as.factor(Long.Name)) %>%
            tidyr::drop_na() %>%
            dplyr::filter(year_no <= ret_year) %>%
            dplyr::filter(scenario_name %in% thesescenarios) %>%
            dplyr::left_join(plot.salmon.data, by = c("scenario_name", "scenario_var", "Long.Name", "Year"))

        # Calculate the number of pages with 9 panels per page one per species
        n_groups <- plot.data %>%
            dplyr::distinct(Long.Name) %>%
            nrow

        n_pages <- ceiling(n_groups/9)


        # col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), 'qMSOSlp')

        print(n_pages)

       salmon.impacts <- c("Negative impacts on salmon", "Positive impacts on salmon")


        for (eachscenariovar in 1:length(salmon.impacts)) {

            sl.impact <- salmon.impacts[eachscenariovar]


            scenario.plot.data <- plot.data %>%
                dplyr::filter(salmon_effect == sl.impact) %>%
                dplyr::mutate(Year = as.factor(Year)) %>%
                droplevels()


            col.pal <- c(`Gelatinous zooplankton abundance` = "#B2CD5D", `Hatchery Chinook competition` = "#9CC439", `Hatchery competition` = "#86C117", `Herring abundance` = "#699412",
                `Wild pink & chum salmon competition` = "#51750E", `Pinniped predation` = "#9E64AA", `Porpoise predation` = "#8B4997", `Seabird predation` = "#89308C",
                `Spiny dogfish predation` = "#67256C")

            plot.list.range <- list()

            for (i in seq_len(n_pages)) {

                survival.plot.bar.time <- scenario.plot.data %>%
                  ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, color = scenario_name)) +
                  ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.6, alpha = 0.6) +
                  # ggplot2::ylim(0,1) + ggnewscale::new_scale_color() + ggplot2::geom_line(ggplot2::aes(group= scenario_name, colour=scenario_name)) +
                  ggthemes::theme_few() + ggplot2::scale_colour_manual(values = col.pal, name = "Scenario") +
                  ggforce::facet_wrap_paginate(. ~ Long.Name, ncol = 1, nrow = 4, page = i, shrink = FALSE, labeller = "label_value", scales = "free_y") +
                  ggplot2::labs(y = "Survival", subtitle = sl.impact) + ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)) +
                  ggplot2::theme(legend.position = "bottom")

                plot.list.range[[i]] <- survival.plot.bar.time

                thisplotname <- paste0("salmon_survival_barplot_time_", eachscenariovar, "_p", i, ".png")

                ggplot2::ggsave(thisplotname, plot = survival.plot.bar.time, device = "png", width= 14.43, height = 12.25, scale = 1, dpi = 600)

            }


        }


    }
    return(plot.list.range)
}
