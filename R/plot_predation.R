#' @title plot predation
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @description Code to plot predation changes between scenarios
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com November 2022


plot_predation <- function(ensemblepredationcum, salmoneffect, salmonbybasin) {

    salmon.basin <- salmonbybasin %>%
        dplyr::rename(code = Code) %>%
        dplyr::select(-longname)


    pred.sc <- ensemblepredationcum %>%
        dplyr::filter(time == 28) %>%
        dplyr::filter(scenario_var != 1) %>%
        dplyr::mutate(scenario_var = if_else(scenario_var == "0_8", "-20%", "+20%")) %>%
        dplyr::mutate(scenario_name = if_else(scenario_name == "bottom-top", "Bottom-up & Top-down", scenario_name)) %>%
        dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
        dplyr::left_join(salmoneffect, by = c("scenario_name", "scenario_var")) %>%
        dplyr::group_by(code, longname, model_ver, scenario_name, salmon_effect) %>%
        dplyr::summarise(pred_mortality = sum(mortality)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(salmon.basin, by = c("code"))


    pred.base <- ensemblepredationcum %>%
        dplyr::filter(time == 28) %>%
        dplyr::filter(scenario_var == 1) %>%
        dplyr::mutate(scenario_name = if_else(scenario_name == "bottom-top", "Bottom-up & Top-down", scenario_name)) %>%
        dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
        dplyr::group_by(code, longname, model_ver, scenario_name) %>%
        dplyr::summarise(base_mortality = sum(mortality)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(salmon.basin, by = c("code"))


    pred.plot.salmon <- pred.sc %>%
        left_join(pred.base, by = c("code", "longname", "model_ver", "scenario_name", "name", "basin", "geo_order", "salmon_genus")) %>%
        dplyr::mutate(prop_mort = pred_mortality/base_mortality) %>%
        dplyr::mutate(excess_mort = (prop_mort - 1) * 100) %>%
        dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Bottom-up", "Top-down", "Bottom-up & Top-down")) %>%
        dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound", "Hood Canal")) %>%
        dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye"))

    # HEATMAP, not used because it can't incorporate variation in the model ensemble pred.plot.salmon %>% ggplot(aes(salmon_effect, longname)) +
    # geom_tile(aes(fill = log(excess_mort))) + scale_fill_gradient(low = 'lightyellow', high = 'firebrick4') + facet_wrap(.~scenario_name)

    col.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
        `Hood Canal` = "#032F5C")

    pred.boxplot <- pred.plot.salmon %>%
        ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = longname, fill = basin)) + ggplot2::geom_boxplot() + ggplot2::geom_hline(yintercept = 1) + ggplot2::facet_wrap(scenario_name ~
        salmon_effect, scales = "free_y", ncol = 2, nrow = 3) + ggplot2::scale_fill_manual(values = col.fill, name = "Basin of origin") + ggplot2::labs(title = "Cumulative scenarios",
        y = "% change in mortality relative to base scenario", x = "Functional group", face = "bold") + ggthemes::theme_base() + ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))

    # pred.plot.salmon %>% dplyr::filter(scenario_name=='Top-down') %>% ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = salmon_effect, fill = basin)) +
    # ggplot2::geom_boxplot(width = 0.1) + # ggplot2::geom_violin(width=1.4) + ggplot2::geom_hline(yintercept = 1) + ggplot2::facet_wrap( ~ longname, scales =
    # 'free_y') + ggplot2::scale_fill_manual(values = col.fill, name = 'Basin of origin') + ggplot2::labs(title = 'Cumulative scenarios', y = '% change in
    # mortality relative to base scenario', x = 'Functional group', face = 'bold') + ggthemes::theme_base() + ggplot2::theme(legend.position='bottom') +
    # ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.95)) pred.plot.salmon %>% dplyr::filter(scenario_name=='Bottom-up &
    # Top-down') %>% dplyr::filter(salmon_effect=='Positive impacts on salmon') %>% ggplot2::ggplot(ggplot2::aes(x = excess_mort, fill = basin)) +
    # ggplot2::geom_density(adjust = 1.5) + # ggplot2::geom_hline(yintercept = 1) + ggplot2::facet_wrap(~longname, scales = 'free_y') +
    # ggplot2::scale_fill_manual(values = col.fill, name = 'Basin of origin') + ggplot2::labs(title = 'Cumulative scenarios', y = '% change in mortality
    # relative to base scenario', x = 'Functional group', face = 'bold') + ggthemes::theme_base() + ggplot2::theme(legend.position='bottom') +
    # ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.95))


    ggplot2::ggsave("boxplot_predation_basin_scale.png", plot = pred.boxplot, device = "png", width = 30, height = 40, units = "cm", dpi = 600)

    return(pred.boxplot)


}
