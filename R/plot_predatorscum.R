#' @title plot biomass salmon predators in cumulative scenarios
#' @param ensemblebiomasscum biomass cumulative scenarios
#' @param predgroups Salmon predator groups in guilds
#'
#' @return biomass plots of salmon predators
#' @export
#'
#' @description Code to plot biomass of salmon predators in cumulative scenarios
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2023


plot_predatorscum <- function(ensemblebiomasscum, predgroups, thiscutoff){

  mammal.mod <- c("DOG","HSL","CSL","PIN","PHR", "SB", "SP")
  #groups which biomass was modified as part of the scenarios

  salmon.preds <- predgroups %>%
    dplyr::filter(!Code %in% mammal.mod) %>%
    dplyr::pull(Code)

  biomass.preds <- ensemblebiomasscum %>%
    dplyr::filter(Code %in% salmon.preds) %>%
    dplyr::left_join(predgroups, by = "Code") %>%
    dplyr::filter(Year == 28) %>%
    dplyr::group_by(Code, name, longname, guild, model_ver, scenario_name, scenario_var) %>%
    dplyr::summarise(max_biomass = max(biomass), .groups = "drop")

  base.biomass.preds <- biomass.preds %>%
    dplyr::filter(scenario_var=="1") %>%
    dplyr::rename(base_biomass=max_biomass) %>%
    dplyr::select(-scenario_var)

  rel.biomass.preds <- biomass.preds %>%
    dplyr::filter(scenario_var!="1") %>%
    dplyr::left_join(base.biomass.preds, by=c("Code","name","longname","guild","model_ver","scenario_name")) %>%
    dplyr::mutate(rel_biomass = ((max_biomass / base_biomass)-1)*100) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "1_2", "Negative", "Positive")) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "bottom top", "Bottom-up & Top-down",
                                                 dplyr::if_else(scenario_name=="top down","Top-down",
                                                                dplyr::if_else(scenario_name=="bottom up","Bottom-up",scenario_name)))) %>%
    dplyr::mutate(scenario_name = as.factor(scenario_name)) %>%
    dplyr::mutate(scenario_name = forcats::fct_relevel(scenario_name, "Bottom-up", "Top-down", "Bottom-up & Top-down"))


  col.fill <- c(`Positive` = "#002db3", `Negative` = "#ffd11a")

  guild.fill.all <- paletteer::paletteer_d("dutchmasters::pearl_earring",11)
  guild.fill <- guild.fill.all[c(1,6,3,7)]


  salmon.eff.text <- rel.biomass.preds %>%
    dplyr::filter(rel_biomass >= thiscutoff) %>%
    # dplyr::filter(guild!="Demersal fish") %>%
    dplyr::select(scenario_var, scenario_name, guild, rel_biomass) %>%
    dplyr::mutate(label = round(rel_biomass,0))%>%
    dplyr::group_by(scenario_name, scenario_var, guild) %>%
    dplyr::slice(which.max(label)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild_abv = dplyr::if_else(guild=="Demersal fish", "DemF",
                                             dplyr::if_else(guild=="Marine mammal", "MarM", guild))) %>%
    dplyr::mutate(label = paste(as.character(label), guild_abv), rel_biomass = 18) %>%
    dplyr::select(-guild_abv) %>%
    dplyr::arrange(scenario_name, scenario_var, guild)


  biomass.violin.pred <- rel.biomass.preds %>%
    dplyr::filter(rel_biomass <= thiscutoff) %>%
    #  dplyr::filter(guild!="Demersal fish") %>%
    #  droplevels() %>%
    ggplot2::ggplot(ggplot2::aes(y = rel_biomass, x = scenario_var, fill = guild)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_point(ggplot2::aes(color=guild), position = ggplot2::position_jitterdodge(), alpha=0.5) +
    ggplot2::facet_wrap(. ~ scenario_name, ncol = 1) + #, scales = "free_y"
    ggplot2::scale_fill_manual(values = guild.fill, name = "Species guild") +
    ggplot2::scale_color_manual(values = guild.fill) +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::labs(title = "Predator biomass in cumulative scenarios", y = "Proportional change in biomass (scenario/base)", x = "Expected impact on salmon", face = "bold") +
    ggthemes::theme_base() +
    ggplot2::ylim(-20, 20) +
    ggplot2::theme(legend.position = "bottom") +
    #ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
    ggplot2::guides(color="none") +
    ggrepel::geom_text_repel(
      data          = salmon.eff.text,
      mapping       = ggplot2::aes(scenario_var, rel_biomass, label = label),
      force = 0.9,
      force_pull = 0.7,
      size          = 3,
      colour        = "black"
    ) #+
  #ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))

  ggplot2::ggsave("predator_biomass_effect_.png", plot = biomass.violin.pred, device = "png", width= 9, height = 10, scale = 1, dpi = 600)

  return(biomass.violin.pred)

}
