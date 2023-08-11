#' @title plot biomass salmon predators
#' @param ensemblebiomass biomass individual scenarios
#' @param predgroups Salmon predator groups in guilds
#'
#' @return biomass plots of salmon predators
#' @export
#'
#' @description Code to plot biomass of salmon predators in individual scenarios
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com June 2023


plot_predators <- function(ensemblebiomass, predgroups, thiscutoff){

  salmon.preds <- predgroups %>%
   dplyr::pull(Code)

  mammal.pred <- c("HSL","CSL","PIN")
  #groups which biomass was modified as part of the scenarios

  biomass.preds <- ensemblebiomass %>%
    dplyr::filter(Code %in% salmon.preds) %>%
    dplyr::left_join(predgroups, by = c("Code","name")) %>%
    dplyr::filter(Year > 25)

  biomass.preds.mm <-  biomass.preds %>%
    dplyr::filter(dplyr::case_when(scenario_name == "mammal predation" ~ !Code %in% mammal.pred))

  biomass.preds.dog <-  biomass.preds %>%
    dplyr::filter(dplyr::case_when(scenario_name == "spiny dogfish predation" ~ Code != "DOG"))

  biomass.preds.bird <-  biomass.preds %>%
    dplyr::filter(dplyr::case_when(scenario_name == "seabirds predation" ~ !Code %in% c("SP","SB")))

  biomass.preds.por <-  biomass.preds %>%
    dplyr::filter(dplyr::case_when(scenario_name == "porpoise predation" ~ Code != "PHR"))

  biomass.preds.sc <- biomass.preds %>%
    dplyr::filter(!scenario_name %in% c("mammal predation", "spiny dogfish predation", "seabirds predation", "porpoise predation")) %>%
    dplyr::bind_rows(biomass.preds.por, biomass.preds.bird, biomass.preds.dog, biomass.preds.mm) %>%
    dplyr::group_by(Code, name, longname, guild, model_ver, scenario_name, scenario_var) %>%
    dplyr::summarise(max_biomass = mean(biomass), .groups = "drop")

  base.biomass.preds <- biomass.preds.sc %>%
    dplyr::filter(scenario_var=="1") %>%
    dplyr::rename(base_biomass=max_biomass) %>%
    dplyr::select(-scenario_var)

  col.fill <- c(`Positive` = "#002db3", `Negative` = "#ffd11a")

  guild.fill.all <- paletteer::paletteer_d("dutchmasters::pearl_earring")
  guild.fill <- c("Demersal fish"=guild.fill.all[c(1)], "Small planktivorous fish"=guild.fill.all[c(7)], "Elasmobranchs"=guild.fill.all[c(2)], "Marine mammals" = guild.fill.all[c(6)], "Seabirds"=guild.fill.all[c(3)])

  guild.order <- guild.fill %>% names()

  rel.biomass.preds <- biomass.preds.sc %>%
    dplyr::filter(scenario_var!="1") %>%
    dplyr::left_join(base.biomass.preds, by=c("Code","name","longname","guild","model_ver","scenario_name")) %>%
    dplyr::mutate(rel_biomass = ((max_biomass / base_biomass)-1)*100) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "1_2", "Negative", "Positive")) %>%
    dplyr::mutate(rel_biomass = dplyr::if_else(is.nan(rel_biomass), 0, rel_biomass)) %>%
    dplyr::mutate(scenario_type = dplyr::if_else(grepl("predation", scenario_name), "Top-down", "Bottom-up")) %>%
    dplyr::mutate(scenario_type = forcats::fct_relevel(scenario_type, "Bottom-up", "Top-down")) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "salmon competition", "wild pink & chum salmon competition",
                                                 dplyr::if_else(scenario_name =="mammal predation", "pinniped predation",
                                                                dplyr::if_else(scenario_name == "seabirds predation", "seabird predation",
                                                                               dplyr::if_else(scenario_name =="gelatinous zooplankton increase", "gelatinous zooplankton abundance",
                                                                                              dplyr::if_else(scenario_name == "herring decrease", "herring abundance", scenario_name)))))) %>%
    dplyr::mutate(scenario_name = stringr::str_to_sentence(scenario_name)) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="Hatchery chinook competition","Hatchery Chinook competition",scenario_name))




  salmon.eff.text <- rel.biomass.preds %>%
    dplyr::filter(rel_biomass >= thiscutoff) %>%
    dplyr::filter(guild!="Salmon") %>%
   # dplyr::filter(guild!="Demersal fish") %>%
    dplyr::select(scenario_type, scenario_var, scenario_name, guild, rel_biomass) %>%
    dplyr::mutate(label = round(rel_biomass,0))%>%
    dplyr::group_by(scenario_name, scenario_var, guild) %>%
    dplyr::slice(which.max(label)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild_abv = dplyr::if_else(guild=="Demersal fish", "DemF",
                                             dplyr::if_else(guild=="Marine mammal", "MarM",
                                                            dplyr::if_else(guild=="Elasmobranch", "Elasmo",guild)))) %>%
    dplyr::mutate(label = paste(as.character(label), guild_abv), rel_biomass = (thiscutoff-2)) %>%
    dplyr::select(-guild_abv) %>%
    dplyr::arrange(scenario_name, scenario_var, guild)


  biomass.violin.pred <- rel.biomass.preds %>%
    dplyr::mutate(guild = forcats::fct_relevel(as.factor(guild), "Demersal fish","Small planktivorous fish","Elasmobranchs","Marine mammals","Seabirds")) %>%
    dplyr::filter(rel_biomass <= thiscutoff) %>%
    dplyr::filter(guild!="Salmon") %>%
    ggplot2::ggplot(ggplot2::aes(y = rel_biomass, x = scenario_var, fill = guild)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_point(ggplot2::aes(color=guild), position = ggplot2::position_jitterdodge(), alpha=0.5) +
    ggplot2::facet_wrap(scenario_type ~ scenario_name, ncol = 2) + #, scales = "free_y"
    ggplot2::scale_fill_manual(values = guild.fill, name = "Species guild") +
    ggplot2::scale_color_manual(values = guild.fill) +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::labs(title = "Consumer biomass in bottom-up and top-down scenarios", y = "Proportional change in biomass (scenario/base)", x = "Expected impact on salmon", face = "bold") +
    ggthemes::theme_base() +
    ggplot2::ylim(-50, 50) +
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



  ggplot2::ggsave("predator_biomass_effect_indsc.png", plot = biomass.violin.pred, device = "png", width= 13, height = 12, scale = 1, dpi = 600)


  return(biomass.violin.pred)

}
