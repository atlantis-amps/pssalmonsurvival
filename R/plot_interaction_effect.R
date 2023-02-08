#' @title plot interaction effect
#' @param ensemblesurvival, ensemblecum survival
#'
#' @return plots of survival interaction effect
#' @export
#'
#' @description Code to plot survival interaction effects
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2022


plot_interaction_effect <- function(ensemble.survival, ensemble.cum.survival, scenariocategories) {

    ind.survival <- ensemble.survival %>%
        dplyr::select(scenario_name, model_ver, longname, Code, basin, salmon_genus, geo_order, salmon_effect, rel_survival) %>%
        dplyr::left_join(scenariocategories, by = c("scenario_name")) %>%
        dplyr::mutate(prop_survival = 1 + (rel_survival/100)) %>%
        dplyr::group_by(scenario_category, model_ver, longname, Code, basin, salmon_genus, geo_order, salmon_effect) %>%
        dplyr::summarise(ind_survival = prod(prop_survival)) %>%
        dplyr::mutate(expec_cum = (ind_survival - 1) * 100) %>%
        dplyr::mutate(scenario_category = dplyr::if_else(scenario_category == "bottom-up", "Bottom-up", "Top-down")) %>%
        dplyr::ungroup()

    # ensemble.survival %>% dplyr::select(scenario_name, model_ver, longname, Code, basin, salmon_genus, geo_order, salmon_effect, rel_survival) %>%
    # dplyr::left_join(scenariocategories, by = c('scenario_name')) %>% dplyr::mutate(prop_survival = 1 + (rel_survival / 100)) %>%
    # dplyr::filter(longname=='Chinook Duwamish SY' & salmon_effect == 'Positive impacts on salmon' & model_ver == 1) %>% dplyr::group_by(scenario_category,
    # model_ver, longname, Code, basin, salmon_genus, geo_order, salmon_effect) %>% dplyr::summarise(ind_survival = prod(prop_survival)) %>%
    # dplyr::mutate(expec_cum = (ind_survival -1)*100)


    ind.cum.survival <- ensemble.survival %>%
        dplyr::select(scenario_name, model_ver, longname, Code, basin, salmon_genus, geo_order, salmon_effect, rel_survival) %>%
        dplyr::left_join(scenariocategories, by = c("scenario_name")) %>%
        dplyr::mutate(scenario_category = "Bottom-up & Top-down") %>%
        dplyr::mutate(prop_survival = 1 + (rel_survival/100)) %>%
        dplyr::group_by(scenario_category, model_ver, longname, Code, basin, salmon_genus, geo_order, salmon_effect) %>%
        dplyr::summarise(ind_survival = prod(prop_survival)) %>%
        dplyr::mutate(expec_cum = (ind_survival - 1) * 100) %>%
        dplyr::ungroup() %>%
        dplyr::bind_rows(ind.survival)

    salmon.order <- ensemble.cum.survival %>%
        # dplyr::filter(salmon_genus != 'Chum')
    dplyr::distinct(longname, basin, geo_order, salmon_genus) %>%
        dplyr::arrange(salmon_genus, geo_order) %>%
        dplyr::pull(longname)

    effect.size <- ensemble.cum.survival %>%
        dplyr::select(scenario_name, model_ver, longname, Code, basin, salmon_genus, geo_order, salmon_effect, rel_survival) %>%
        dplyr::rename(scenario_category = scenario_name, obs_cum = rel_survival) %>%
        dplyr::right_join(ind.cum.survival, by = c("scenario_category", "model_ver", "Code", "longname", "basin", "salmon_genus", "geo_order", "salmon_effect")) %>%
        dplyr::mutate(effect_size = (obs_cum - expec_cum)) %>%
        dplyr::mutate(scenario_category = forcats::fct_relevel(as.factor(scenario_category), "Bottom-up", "Top-down", "Bottom-up & Top-down")) %>%
        dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound", "Hood Canal")) %>%
        dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
        dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye"))


    col.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
        `Hood Canal` = "#032F5C")

    box.plot.effect <- effect.size %>%
        ggplot2::ggplot(ggplot2::aes(y = effect_size, x = longname, fill = basin)) +
        gplot2::geom_boxplot() +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::facet_wrap(scenario_category ~ salmon_effect, ncol = 2, nrow = 3, scales = "free_y") +
        ggplot2::scale_fill_manual(values = col.fill, name = "Basin of origin") +
        ggplot2::labs(title = "Effect size", y = "Effect size salmon survival", x = "Functional group", face = "bold") +
        ggthemes::theme_base() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))


    ggplot2::ggsave("boxplot_effect_size.png", plot = box.plot.effect, device = "png", width = 30, height = 40, units = "cm", dpi = 600)

    return(box.plot.effect)
}
