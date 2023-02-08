#' @title plot biomass salmon predators
#' @param ensemblebiomasscum biomass cumulative scenarios
#' @param predgroups Salmon predator groups in guilds
#'
#' @return biomass plots of salmon predators
#' @export
#'
#' @description Code to plot biomass of salmon predators in cumulative scenarios
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2023


plot_predators <- function(ensemblebiomasscum, predgroups){

  salmon.preds <- predgroups %>%
    pull(Code)

  biomass.preds <- ensemblebiomasscum %>%
    dplyr::filter(Code %in% salmon.preds) %>%
    dplyr::left_join(predgroups, by = "Code") %>%
    dplyr::filter(Year > 29) %>%
    dplyr::group_by(Code, name, longname, guild, model_ver, scenario_name, scenario_var) %>%
    dplyr::summarise(max_biomass = max(biomass)) %>%
    ungroup

  base.biomass.preds <- biomass.preds %>%
    dplyr::filter(scenario_var=="1") %>%
    dplyr::rename(base_biomass=max_biomass) %>%
    dplyr::select(-scenario_var)

  rel.biomass.preds <- biomass.preds %>%
    dplyr::filter(scenario_var!="1") %>%
    dplyr::left_join(base.biomass.preds, by=c("Code","name","longname","guild","model_ver","scenario_name")) %>%
    dplyr::mutate(rel_biomass = max_biomass / base_biomass) %>%
    dplyr::mutate(scenario_name = as.factor(scenario_name)) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "1_2", "Negative impacts on salmon", "Positive impacts on salmon"))

  col.fill <- c(`Positive impacts on salmon` = "#D8B70A", `Negative impacts on salmon` = "#02401B")


  rel.biomass.preds %>%
    dplyr::filter(guild!="Demersal fish") %>%
    droplevels() %>%
    ggplot2::ggplot(ggplot2::aes(y = rel_biomass, x = scenario_name, fill = scenario_var)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(. ~ guild) + #, scales = "free_y"
    ggplot2::scale_fill_manual(values = col.fill, name = "Impact on salmon") +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::labs(title = "Predator biomass", y = "Biomass relative to base scenario", x = "Scenario name", face = "bold") +
    ggthemes::theme_base() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))


  pred.boxplot <- rel.biomass.preds %>%
    dplyr::filter(!rel_biomass > 1000) %>%
    ggplot2::ggplot(ggplot2::aes(y = rel_biomass, x = guild, fill = scenario_var)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(.~ scenario_name, scales = "free_y", ncol = 2, nrow = 3) +
    ggplot2::scale_fill_manual(values = col.fill, name = "Basin of origin") +
    ggplot2::labs(title = "Cumulative scenarios",
                  y = "% change in predation mortality relative to base scenario", x = "Functional group", face = "bold") +
    ggthemes::theme_base() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))

}
