#' @title plot salmon survival of ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2022



plot_ensemble_survival_scenarios <- function(ensemblenumbersagescenarios, salmongroups, plotmodels, base.survival, salmonbybasin, indsalmoneffect) {

    salmon.max.nums <- ensemblenumbersagescenarios %>%
        dplyr::group_by(scenario_name, scenario_var, model_ver, Code, age, year_no) %>%
        dplyr::summarise(max_nums = max(nums)) %>%
        dplyr::left_join(salmongroups, by = "Code") %>%
        dplyr::ungroup() %>%
        # dplyr::filter(!model_ver%in% plotmodels) %>%
    dplyr::mutate(model_ver = as.double(model_ver)) %>%
        dplyr::mutate(scenario_name = tolower(scenario_name)) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "salmon competition", "wild pink & chum salmon competition", dplyr::if_else(scenario_name ==
            "mammal predation", "pinniped predation", dplyr::if_else(scenario_name == "seabirds predation", "seabird predation", dplyr::if_else(scenario_name ==
            "gelatinous zooplankton increase", "gelatinous zooplankton abundance", dplyr::if_else(scenario_name == "herring decrease", "herring abundance", scenario_name)))))) %>%
        dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "0_8", "-20%", "+20%"))

    salmon.juv.nums <- salmon.max.nums %>%
        dplyr::filter(age == 1) %>%
        dplyr::rename(juv_nums = max_nums)

    salmon.return.nums <- salmon.max.nums %>%
        dplyr::filter(age == (years_away + 1)) %>%
        dplyr::mutate(cohort_yr = year_no - age) %>%
        dplyr::select(-years_away) %>%
        dplyr::rename(age_return = age, return_nums = max_nums, year_sim = year_no, year_no = cohort_yr) %>%
        dplyr::left_join(salmon.juv.nums, by = c("scenario_name", "scenario_var", "migiobox", "model_ver", "Code", "year_no", "Long.Name", "NumCohorts", "Name")) %>%
        dplyr::mutate(survival = (return_nums/juv_nums) * 100) %>%
        dplyr::mutate(survival = dplyr::if_else(survival > 100, 100, survival)) %>%
        dplyr::mutate(Year = year_sim - 2010) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(max_year = max(year_no)) %>%
        dplyr::mutate(ret_year = max_year - 3)


    salmon.return.nums.yr <- salmon.return.nums %>%
        dplyr::filter(year_no <= ret_year) %>%
        dplyr::filter(year_no == max(year_no)) %>%
        dplyr::select(scenario_name, scenario_var, model_ver, Code, Long.Name, survival, juv_nums, return_nums) %>%
        dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "Hatchery chinook competition", "Hatchery Chinook competition", scenario_name))

    salmon.rel.survival <- base.survival %>%
        dplyr::select(scenario_name, model_ver, Code, Long.Name, survival, juv_nums, return_nums) %>%
        dplyr::rename(base_survival = survival) %>%
        dplyr::left_join(salmon.return.nums.yr, by = c("scenario_name", "model_ver", "Code", "Long.Name")) %>%
        dplyr::mutate(rel_survival = (survival - base_survival))


    salmon.return.nums %>%
        dplyr::filter(year_no <= ret_year) %>%
        dplyr::filter(year_no == max(year_no)) %>%
        readr::write_csv(., here::here("modelfiles", "survival_rates.csv"))


    salmon.basin <- salmonbybasin %>%
        dplyr::select(Code, basin, geo_order, salmon_genus)

    salmon.return.nums %>%
        dplyr::filter(year_no <= ret_year) %>%
        dplyr::filter(year_no == max(year_no)) %>%
        readr::write_csv(., here::here("modelfiles", "survival_rates.csv"))


    salmon.basin <- salmonbybasin %>%
        dplyr::select(Code, basin, geo_order, salmon_genus)

    salmon.lollipop.data <- salmon.rel.survival %>%
        dplyr::mutate(longname = as.factor(Long.Name)) %>%
        dplyr::mutate(model_ver = as.factor(model_ver)) %>%
        dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
        dplyr::group_by(scenario_name, scenario_var, Code, longname) %>%
        dplyr::mutate(max_model = max(rel_survival)) %>%
        dplyr::mutate(min_model = min(rel_survival)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(longname = gsub("Subyearling", "SY", longname), longname = gsub("Yearling", "Y", longname), longname = dplyr::if_else(longname == "Chum Hood Canal summer run SY",
            "Chum Hood Canal SY", dplyr::if_else(longname == "Strait of Georgia salmonids", "St. of Georgia salmonids", longname))) %>%
        dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition",
            "Gelatinous zooplankton abundance", "Herring abundance", "Pinniped predation", "Porpoise predation", "Seabird predation", "Spiny dogfish predation")) %>%
        dplyr::left_join(salmon.basin, by = c("Code")) %>%
        dplyr::left_join(indsalmoneffect, by = c("scenario_name", "scenario_var"))

    readr::write_csv(salmon.lollipop.data, here::here("modelfiles", "ensemble_survival.csv"))

    # bottom.up.sc <- c('Hatchery competition', 'Wild pink and chum salmon competition', 'Gelatinous zooplankton increase', 'Herring decrease')
    bottom.up.sc <- c("Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition", "Gelatinous zooplankton abundance", "Herring abundance")
    top.down.sc <- c("Pinniped predation", "Porpoise predation", "Seabird predation", "Spiny dogfish predation")

    scenario.list <- list(`Bottom-up hypotheses` = bottom.up.sc, `Top-down hypotheses` = top.down.sc)

    for (eachscenario in 1:length(scenario.list)) {

        thislist <- scenario.list[eachscenario]

        print(thislist)

        thesescenarios <- thislist %>%
            unlist()
        thisname <- names(thislist)

       salmon.order <- salmon.lollipop.data %>%
            dplyr::distinct(longname, basin, geo_order, salmon_genus) %>%
            dplyr::arrange(salmon_genus, geo_order) %>%
            dplyr::pull(longname)

        plot.data <- salmon.lollipop.data %>%
            dplyr::filter(scenario_name %in% thesescenarios) %>%
            droplevels() %>%
            dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound",
                "Hood Canal")) %>%
            dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
            dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye"))

        # Calculate the number of pages with 12 panels per page

        n_pages <- plot.data %>%
            dplyr::distinct(longname) %>%
            nrow(.)/9
        pages.num <- ceiling(n_pages)

        # col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), 'qMSOSlp')

        max.violin <- plot.data %>%
            dplyr::pull(rel_survival) %>%
            max() %>%
            ceiling(.)

        min.violin <- plot.data %>%
            dplyr::pull(rel_survival) %>%
            min() %>%
            floor(.)

        col.fill <- c(`-20%` = "#ffbe0b", `+20%` = "#0a9396")

        for (i in seq_len(pages.num)) {

            print("creating range plot")

          range.plot <- plot.data %>%
            ggplot2::ggplot() + ggplot2::geom_segment(ggplot2::aes(x = min_model, xend = max_model, y = scenario_name, yend = scenario_name, color = scenario_var),
                                                      size = 5, alpha = 0.1) + #  ggplot2::scale_color_manual(values = col.fill, name = 'Change in key group abundance') +  ggplot2::geom_point(ggplot2::aes(y = scenario_name, x = rel_survival, fill = model_ver, color = model_ver, shape = scenario_var)) + size
            ggplot2::geom_jitter(ggplot2::aes(y = scenario_name, x = rel_survival, shape = scenario_var, fill = model_ver), width = 0.25, height = 0.25, size = 1.5) +
            ggplot2::scale_shape_manual(values = c(21, 24), name = "Change in key group abundance") +
            ggplot2::scale_fill_manual(values = col.fill, guide = "none") +
            ggplot2::scale_color_manual(values = col.fill, guide = "none") + #    ggsci::scale_fill_d3('category20', name = 'Model version') + ggplot2::facet_wrap(ggplot2::vars(long_name), scales = 'free_y') + ggplot2::scale_color_manual(values
            ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = "label_value") + ggplot2::coord_flip() + ggplot2::geom_vline(xintercept = 0) +
            ggplot2::xlab("% change in survival (scenario-base)") +
            ggplot2::ylab("Scenario") +
            ggplot2::labs(title = thisname) +
            ggthemes::theme_base() +
            ggplot2::theme(legend.position = "bottom") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
            ggplot2::guides(fill = "none") +
            ggplot2::xlim(min.violin, max.violin)



            ggplot2::ggsave(paste0(thisname, "_", i, "_survival.png"), plot = range.plot, device = "png", width= 14.75, height = 13.58, scale = 1, dpi = 600)

            print("creating violin plot")

            violin.plot <- plot.data %>%
                ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
                ggplot2::geom_violin(trim = FALSE, position = ggplot2::position_dodge(),
                draw_quantiles = c(0.5)) +
                ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = "label_value", scales = "free_y") +
                ggplot2::geom_boxplot(ggplot2::aes(fill = scenario_var), width = 0.1, color = "black", position = ggplot2::position_dodge(width = 0.9)) +
                ggplot2::scale_fill_manual(values = col.fill, name = "Change in key group abundance") +
                ggplot2::ylab("% change in survival (scenario-base)") + ggplot2::xlab("Scenario") +
                ggplot2::labs(title = thisname) +
                ggplot2::geom_hline(yintercept = 0) +
                ggthemes::theme_base() +
                ggplot2::theme(legend.position = "bottom") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
                ggplot2::ylim(min.violin, max.violin)


            ggplot2::ggsave(paste0(thisname, "_", i, "_violin_survival.png"), plot = violin.plot, device = "png", width= 11.55, height = 13.58, scale = 1, dpi = 600)

            print("creating violin plot scale")

            violin.plot.scale <- plot.data %>%
              ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
              ggplot2::geom_violin(trim = FALSE, position = ggplot2::position_dodge(),
                                   draw_quantiles = c(0.5)) +
              ggplot2::geom_hline(yintercept = 0) +
              ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE,
                                           labeller = "label_value", scales = "free_y") +
              ggplot2::geom_boxplot(ggplot2::aes(fill = scenario_var), width = 0.1, color = "black", position = ggplot2::position_dodge(width = 0.9)) +
              ggplot2::scale_fill_manual(values = col.fill, name = "Change in key group abundance") +
              ggplot2::ylab("Proportional change in survival") +
              ggplot2::xlab("Scenario") +
              ggplot2::labs(title = thisname) +
              ggthemes::theme_base() +
              ggplot2::theme(legend.position = "bottom") +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                 vjust = 0.5))  #+
            # ylim(min.violin, max.violin)


            ggplot2::ggsave(paste0(thisname, "_", i, "_violin_survival_scale.png"), plot = violin.plot.scale, device = "png", width= 11.55, height = 13.58, scale = 1, dpi = 600)

            print("creating box plot scale")

            box.plot.scale <- plot.data %>%
              ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
              ggplot2::geom_boxplot() + ggplot2::geom_hline(yintercept = 0) +
              ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = "label_value", scales = "free_y") + ggplot2::scale_fill_manual(values = col.fill,
                                                                                                                                                                                 name = "Change in key group abundance") +
              ggplot2::labs(title = thisname, y = "% change in survival (scenario-base)", x = "Scenario", face = "bold") +
              ggthemes::theme_base() +
              ggplot2::theme(legend.position = "bottom") +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5,
                                                                 hjust = 0.95)) +
              ggplot2::ylim(min.violin, max.violin)


            ggplot2::ggsave(paste0(thisname, "_", i, "_boxplot_survival_scale.png"), plot = box.plot.scale, device = "png", width= 11.55, height = 13.58, scale = 1, dpi = 600)

        }



        print("creating plots for scenario effects on salmon")

        salmon.impacts <- indsalmoneffect %>%
          dplyr::distinct(salmon_effect) %>%
          dplyr::pull(salmon_effect)

        for(thisimpact in salmon.impacts) {



        these.rows <- ceiling(length(thesescenarios)/2)

        basin.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
                        `Hood Canal` = "#032F5C")

        box.plot.basin <- plot.data %>%
          dplyr::filter(salmon_effect==thisimpact) %>%
          ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = longname, fill = basin)) +
          ggplot2::geom_boxplot() +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::facet_wrap(. ~ scenario_name, ncol = 2, nrow = these.rows, scales = "free_y") +
          ggplot2::scale_fill_manual(values = basin.fill, name = "Basin") +
          ggplot2::labs(title = thisname, subtitle = thisimpact, y = "% change in survival (scenario-base)", x = "Functional group", face = "bold") +
          ggthemes::theme_base() +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))

        # text.label <- plot.data %>% # dplyr::filter(salmon_effect==this.multiplier) %>% dplyr::distinct(scenario_name, scenario_var) %>%
        # dplyr::mutate(longname = 'Pink Salmon SY', basin = 'Puget Sound') ggplot2::geom_text( data = text.label, mapping = ggplot2::aes(x = -Inf, y = -Inf,
        # label = scenario_var), hjust = -0.5, vjust = -12 )

        short.name <- thisimpact %>%
          stringr::str_split(" ") %>%
          unlist %>% .[1]

        ggplot2::ggsave(paste0(thisname,"_",short.name, "_boxplot_survival_basin_.png"), plot = box.plot.basin, device = "png", width= 9.45, height = 11.62, scale = 1, dpi = 600)

}

        # col.fill <- c('#ffbe0b','#ed5181', '#3fd2c7', '#00458b', '#f7a482', '#83bb90', '#f1dd88', '#81aa2c', '#f293b1')

        effect.fill <- c(`Positive impacts on salmon` = "#D8B70A", `Negative impacts on salmon` = "#02401B")

        these.rows <- ceiling(length(thesescenarios)/2)


        box.plot.effect <- plot.data %>%
          ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = longname, fill = salmon_effect)) +
          ggplot2::geom_boxplot() +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::facet_wrap(. ~ scenario_name, ncol = 2, nrow = these.rows, scales = "free_y") +
          ggplot2::scale_fill_manual(values = effect.fill, name = "Salmon effect") +
          ggplot2::labs(title = thisname, y = "% change in survival (scenario-base)", x = "Functional group", face = "bold") +
          ggthemes::theme_base() +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))


        # text.label <- plot.data %>% # dplyr::filter(salmon_effect==this.multiplier) %>% dplyr::distinct(scenario_name, scenario_var) %>%
        # dplyr::mutate(longname = 'Pink Salmon SY', basin = 'Puget Sound') ggplot2::geom_text( data = text.label, mapping = ggplot2::aes(x = -Inf, y = -Inf,
        # label = scenario_var), hjust = -0.5, vjust = -12 )

          ggplot2::ggsave(paste0(thisname, "_boxplot_survival_effect_.png"), plot = box.plot.effect, device = "png", width= 9.45, height = 11.62, scale = 1, dpi = 600)

    }

    return(box.plot.effect)

    }


