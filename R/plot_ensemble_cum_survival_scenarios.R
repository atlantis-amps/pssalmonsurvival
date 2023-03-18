#' @title plot salmon survival of cumulative impact scenarios
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon_cum_return_nums.csv, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2022



plot_ensemble_cum_survival_scenarios <- function(ensemblenumberscum, salmongroups, base.cum.survival, salmonbybasin, salmoneffect) {


    salmon.max.nums <- ensemblenumberscum %>%
        dplyr::group_by(scenario_name, scenario_var, model_ver, Code, age, year_no) %>%
        dplyr::summarise(max_nums = max(nums)) %>%
        dplyr::left_join(salmongroups, by = "Code") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(model_ver = as.double(model_ver)) %>%
        dplyr::filter(scenario_var != 1) %>%
        dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "0_8", "-20%", "+20%")) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "bottom top", "Bottom-up & Top-down", dplyr::if_else(scenario_name == "bottom up", "Bottom-up",
            dplyr::if_else(scenario_name == "top down", "Top-down", scenario_name))))

    print(str(salmon.max.nums))
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
        dplyr::select("scenario_name", "scenario_var", "model_ver", "Code", "Long.Name", "survival")

    # Individual and interaction effects for cumulative impact scenarios. To estimate the individual effect we used the natural log response ratio (lnRR),
    # calculated as: lnRR = ln(Xs / Xb).  where Xs and Xb are mean survival under each scenario and the base run respectively.

    salmon.rel.survival <- base.cum.survival %>%
        dplyr::select("scenario_name", "model_ver", "Code", "Long.Name", "survival") %>%
        dplyr::rename(base_survival = survival) %>%
        dplyr::left_join(salmon.return.nums.yr, by = c("scenario_name", "model_ver", "Code", "Long.Name")) %>%
        dplyr::mutate(rel_survival = (survival - base_survival)) %>%
      dplyr::mutate(prop_survival = ((survival / base_survival)-1) *100) #make it proportional

    salmon.return.nums %>%
        dplyr::filter(year_no <= ret_year) %>%
        dplyr::filter(year_no == max(year_no)) %>%
        readr::write_csv(., here::here("modelfiles", "survival_cum_rates.csv"))

    salmon.basin <- salmonbybasin %>%
        dplyr::select(Code, basin, geo_order, salmon_genus)

    salmon.lollipop.data <- salmon.rel.survival %>%
        dplyr::mutate(longname = as.factor(Long.Name)) %>%
        dplyr::mutate(model_ver = as.factor(model_ver)) %>%
        dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
        dplyr::group_by(scenario_var, Code, longname) %>%
        dplyr::mutate(max_model = max(rel_survival)) %>%
        dplyr::mutate(min_model = min(rel_survival)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(longname = gsub("Subyearling", "SY", longname), longname = gsub("Yearling", "Y", longname), longname = dplyr::if_else(longname == "Chum Hood Canal summer run SY",
            "Chum Hood Canal SY", dplyr::if_else(longname == "Strait of Georgia salmonids", "St. of Georgia salmonids", longname))) %>%
        dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
        dplyr::left_join(salmon.basin, by = c("Code")) %>%
        dplyr::left_join(salmoneffect, by = c("scenario_name", "scenario_var"))

    readr::write_csv(salmon.lollipop.data, here::here("modelfiles", "ensemble_cum_survival.csv"))


    thisscenario <- salmon.lollipop.data %>%
        dplyr::distinct(scenario_name) %>%
        dplyr::pull(scenario_name)

    col.pal <- c("#ffbe0b", "#0a9396")
    col.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
        `Hood Canal` = "#032F5C")

    salmon.order <- salmonbybasin %>%
      # dplyr::filter(salmon_genus != 'Chum')
      dplyr::arrange(salmon_genus, geo_order, longname) %>%
      dplyr::pull(longname)

    plot.data <- salmon.lollipop.data %>%
        droplevels() %>%
        dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound", "Hood Canal")) %>%
        dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
        dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye")) %>%
        dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Bottom-up", "Top-down", "Bottom-up & Top-down")) %>%
        dplyr::mutate(salmon_effect = as.factor(salmon_effect))


    basin.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
        `Hood Canal` = "#032F5C")


    salmon.eff.basin <- plot.data %>%
      dplyr::filter(prop_survival >= 100) %>%
      dplyr::arrange(salmon_effect, longname) %>%
      dplyr::select(salmon_genus, scenario_name, salmon_effect, basin, prop_survival) %>%
      dplyr::mutate(label = round(prop_survival,0))%>%
      dplyr::group_by(salmon_genus, scenario_name, salmon_effect, basin) %>%
      dplyr::slice(which.max(label)) %>% # leaves only the maximum value
      #dplyr::select(-prop_survival) %>%
      #dplyr::bind_cols(prop.survival) %>%
      #dplyr::left_join(salmon.codes, by = "longname") %>%
      dplyr::mutate(basin_code = dplyr::if_else(basin == "South Puget Sound","SPS",
                                         "HC")) %>%
      dplyr::mutate(label = paste(as.character(label), basin_code), prop_survival = 95)

    genus.fill.all <- paletteer::paletteer_d("ggthemes::stata_economist",12)
    genus.fill <- genus.fill.all[c(5,10,8,6,9)]


    box.plot.scale.basin <- plot.data %>%
      dplyr::filter(prop_survival <= 100) %>%
      ggplot2::ggplot(ggplot2::aes(y = prop_survival, x = salmon_genus, fill = basin)) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_point(ggplot2::aes(color=basin), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      ggplot2::geom_hline(yintercept = 0) +
     # ggplot2::facet_wrap(salmon_effect ~ scenario_name, scales = "free_y") +
      ggplot2::facet_grid(ggplot2::vars(salmon_effect), ggplot2::vars(scenario_name)) +
      ggplot2::labs(title = "Salmon survival in cumulative scenarios",
                    y = "Proportional change in survival (scenario/base)", x = "Salmon group", face = "bold") +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position = "bottom") +
    #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.95, hjust = 0.95)) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
      ggplot2::ylim(-100, 100) +
      ggplot2::scale_fill_manual(values = basin.fill, name = "Basin") +
      ggplot2::scale_color_manual(values = basin.fill) +
      #  ggplot2::geom_text(data = salmon.fg.text, label=salmon.fg.text$label, size = 3.5) +
      ggrepel::geom_text_repel(
        data          = salmon.eff.basin,
        mapping       = ggplot2::aes(salmon_genus, prop_survival, label = label),
        force = 0.7,
        force_pull = 0.7,
        size          = 3,
        colour        = "black"
      ) +
      ggplot2::guides(color="none")

   box.plot.scale.basin.title <- gridExtra::grid.arrange(box.plot.scale.basin, right='Expected salmon impact')

    ggplot2::ggsave("boxplot_cum_survival_basin_scale.png", plot = box.plot.scale.basin.title, device = "png", width= 12.7, height = 14.3, scale = 1, dpi = 600)


    salmon.codes <- salmonbybasin %>%
      dplyr::select(Code, longname)

    salmon.eff.text <- plot.data %>%
      dplyr::filter(prop_survival >= 100) %>%
      dplyr::arrange(salmon_effect, longname) %>%
      dplyr::select(scenario_name, salmon_effect, longname, prop_survival) %>%
      dplyr::mutate(label = round(prop_survival,0))%>%
      dplyr::group_by(scenario_name, salmon_effect, longname) %>%
      dplyr::slice(which.max(label)) %>% # leaves only the maximum value
      #dplyr::select(-prop_survival) %>%
      #dplyr::bind_cols(prop.survival) %>%
      dplyr::left_join(salmon.codes, by = "longname") %>%
      dplyr::mutate(label = paste(as.character(label), Code), prop_survival = 95) %>%
      dplyr::select(-Code)

    salmon.colors <- salmon_colors()

    box.plot.scale.group <- plot.data %>%
      dplyr::filter(prop_survival <= 100) %>%
      ggplot2::ggplot(ggplot2::aes(y = prop_survival, x = salmon_effect, fill = longname)) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_point(ggplot2::aes(color=longname), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(. ~ scenario_name, scales = "free_y", ncol = 1) +
      ggplot2::labs(title = "Salmon survival in cumulative scenarios",
                    y = "Proportional change in survival (scenario/base)", x = "Expected salmon impact", face = "bold") +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position = "bottom") +
      #  ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.95, hjust = 0.95)) +
      #  ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
      ggplot2::ylim(-100, 100) +
      ggplot2::scale_fill_manual(values = salmon.colors, name = "Salmon group") +
      ggplot2::scale_color_manual(values = salmon.colors) +
      #  ggplot2::geom_text(data = salmon.fg.text, label=salmon.fg.text$label, size = 3.5) +
      ggrepel::geom_text_repel(
        data          = salmon.eff.text,
        mapping       = ggplot2::aes(salmon_effect, prop_survival, label = label),
        force = 0.7,
        force_pull = 0.7,
        size          = 3,
        colour        = "black"
      ) +
      ggplot2::guides(color="none") +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))

    ggplot2::ggsave("boxplot_cum_survival_group_scale.png", plot = box.plot.scale.group, device = "png", width= 14.5, height = 14, scale = 1, dpi = 600)

    return(box.plot.scale.group)

}

