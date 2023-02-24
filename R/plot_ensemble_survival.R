#' @title plot salmon survival of ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002




plot_ensemble_survival <- function(ensemblenumbersage, salmongroups) {

    salmon.max.nums <- ensemblenumbersage %>%
        dplyr::group_by(scenario_name, model_ver, Code, age, year_no) %>%
        dplyr::summarise(max_nums = max(nums)) %>%
        dplyr::left_join(salmongroups, by = "Code") %>%
        dplyr::ungroup()  # %>%
    # dplyr::filter(!model_ver%in% plotmodels)

    salmon.juv.nums <- salmon.max.nums %>%
        dplyr::filter(age == 1) %>%
        dplyr::rename(juv_nums = max_nums)

    salmon.return.nums <- salmon.max.nums %>%
        dplyr::filter(age == (years_away + 1)) %>%
        dplyr::mutate(cohort_yr = year_no - age) %>%
        dplyr::select(-years_away) %>%
        dplyr::rename(age_return = age, return_nums = max_nums, year_sim = year_no, year_no = cohort_yr) %>%
        dplyr::left_join(salmon.juv.nums, by = c("scenario_name", "model_ver", "Code", "year_no", "Long.Name", "NumCohorts", "Name")) %>%
        dplyr::mutate(survival = (return_nums/juv_nums) * 100) %>%
        dplyr::mutate(survival = dplyr::if_else(survival > 100, 100, survival)) %>%
        dplyr::mutate(model_ver = as.factor(model_ver)) %>%
        dplyr::mutate(Year = year_sim - 2010) %>%
        dplyr::mutate(max_year = max(year_no)) %>%
        dplyr::mutate(ret_year = max_year - 3) %>%
        dplyr::mutate(scenario_name = tolower(scenario_name)) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "salmon competition", "wild pink & chum salmon competition", dplyr::if_else(scenario_name ==
            "mammal predation", "pinniped predation", dplyr::if_else(scenario_name == "seabirds predation", "seabird predation", dplyr::if_else(scenario_name ==
            "gelatinous zooplankton increase", "gelatinous zooplankton abundance", dplyr::if_else(scenario_name == "herring decrease", "herring abundance", scenario_name)))))) %>%
        dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "Hatchery chinook competition", "Hatchery Chinook competition", scenario_name))

    salmon.plot.data <- salmon.return.nums %>%
        dplyr::filter(year_no <= ret_year) %>%
        dplyr::mutate(Long.Name = as.factor(Long.Name), Year = as.factor(Year), scenario_name = as.factor(scenario_name)) %>%
        droplevels() %>%
        tidyr::drop_na() %>%
        dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition",
            "Gelatinous zooplankton abundance", "Herring abundance", "Pinniped predation", "Porpoise predation", "Seabird predation", "Spiny dogfish predation"))



    readr::write_csv(salmon.return.nums, here::here("modelfiles", "base_survival.csv"))


    # Calculate the number of pages with 12 panels per page

    sc.cases <- salmon.return.nums %>%
        dplyr::distinct(Long.Name, scenario_name)

    n_pages <- ceiling(nrow(salmongroups)/4)

    # col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), 'qMSOSlp')

    print(n_pages)

    plot.list <- list()

    # col.pal <- paletteer::paletteer_d('rcartocolor::Vivid', n = 10)
    col.pal <- c(`Gelatinous zooplankton abundance` = "#B2CD5D", `Hatchery Chinook competition` = "#9CC439", `Hatchery competition` = "#86C117", `Herring abundance` = "#699412",
        `Wild pink & chum salmon competition` = "#51750E", `Pinniped predation` = "#9E64AA", `Porpoise predation` = "#8B4997", `Seabird predation` = "#89308C", `Spiny dogfish predation` = "#67256C")

    for (i in seq_len(n_pages)) {

        survival.plot <- salmon.plot.data %>%
            ggplot2::ggplot(ggplot2::aes(x = Year, y = survival, color = scenario_name)) + ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.6, alpha = 0.6) +
            # ggplot2::ylim(0,1) + ggnewscale::new_scale_color() + ggplot2::geom_line(ggplot2::aes(group= scenario_name, colour=scenario_name)) +
        ggthemes::theme_few() + ggplot2::scale_colour_manual(values = col.pal, name = "Scenario") + ggforce::facet_wrap_paginate(. ~ Long.Name, ncol = 1, nrow = 4,
            page = i, shrink = FALSE, labeller = "label_value") + ggplot2::labs(y = "Survival") + ggplot2::theme(legend.position = "bottom")

        plot.list[[i]] <- survival.plot

        thisplotname <- paste0("salmon_survival_plot_", i, ".png")

        ggplot2::ggsave(thisplotname, plot = survival.plot, device = "png", width = 12.55, height = 9.81, scale = 1, dpi= 600)

    }


    return(plot.list)
}
