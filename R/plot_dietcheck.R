#' Plot Dietcheck time series
#'
#' @param dietcheck total predation by predators on salmon groups for individual scenarios
#' @param salmoneffect predicted scenario effects on salmon
#' @param groupguilds functional groups by guild
#'
#' @return pred.boxplot
#' @export
#'
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com May 2023
#' @examples
plot_dietcheck <- function(dietcheck, indsalmoneffect, predgroups, thiscutoff) {

  pred.guilds <- predgroups %>%
    dplyr::rename(longname=Predator, Predator= Code)

  dietcheck.names <- dietcheck %>%
    dplyr::mutate(scenario_var = as.character(scenario_var)) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "0.8", "-20%",
                                                dplyr::if_else(scenario_var == "1", "base",
                                                               dplyr::if_else(scenario_var == "1.2", "+20%", scenario_var)))) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "chinookhatchery-competition", "Hatchery Chinook competition",
                                                 dplyr::if_else(scenario_name == "Gel-Zoo", "Gelatinous zooplankton abundance",
                                                                dplyr::if_else(scenario_name == "hatchery-competition", "Hatchery competition",
                                                                               dplyr::if_else(scenario_name == "herring-trend", "Herring abundance",
                                                                                              dplyr::if_else(scenario_name == "mammal-predation", "Pinniped predation",
                                                                                                             dplyr::if_else(scenario_name == "Porpoise-predation",  "Porpoise predation",
                                                                                                                            dplyr::if_else(scenario_name == "salmon-competition",  "Wild pink & chum salmon competition",
                                                                                                                                           dplyr::if_else(scenario_name == "seabirds-predation",  "Seabird predation",
                                                                                                                                                          dplyr::if_else(scenario_name == "Spinydog-Fish",  "Spiny dogfish predation", scenario_name)))))))))) %>%     dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
    dplyr::left_join(indsalmoneffect, by = c("scenario_name","scenario_var")) %>%
    dplyr::left_join(pred.guilds, by = c("Predator")) %>%
    dplyr::mutate(year = Time/365) %>%
    dplyr::filter(year >= 25) %>%
    dplyr::group_by(longname, guild, scenario_name, scenario_var, model_ver, salmon_effect) %>%
    dplyr::summarise(av_mort = mean(tot_mort)) %>%
    dplyr::ungroup()


  pred.sc <- dietcheck.names %>%
    dplyr::filter(scenario_var != "base")

  pred.base <- dietcheck.names %>%
    dplyr::filter(scenario_var == "base") %>%
    dplyr::rename(ini_tot_mort = av_mort) %>%
    dplyr::select(-salmon_effect)

  pred.plot.salmon <- pred.sc %>%
    dplyr::left_join(pred.base, by = c("longname","guild","scenario_name", "model_ver")) %>%
    dplyr::mutate(excess_mort = (av_mort/ini_tot_mort - 1) * 100) %>%
    #excess mortality is proportional change in mortality
    #dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Bottom-up", "Top-down", "Bottom-up & Top-down")) %>%
    dplyr::mutate(excess_mort=dplyr::if_else(is.nan(excess_mort),0,excess_mort)) %>%
    dplyr::filter(excess_mort!=0) %>%
    dplyr::mutate(scenario_type = dplyr::if_else(grepl("predation", scenario_name), "Top-down", "Bottom-up"))


  # HEATMAP, not used because it can't incorporate variation in the model ensemble pred.plot.salmon %>% ggplot(aes(salmon_effect, longname)) +
  # geom_tile(aes(fill = log(excess_mort))) + scale_fill_gradient(low = 'lightyellow', high = 'firebrick4') + facet_wrap(.~scenario_name)


  # salmon.gen <- pred.plot.salmon %>%
  #   dplyr::filter(excess_mort >= 100) %>%
  #   dplyr::arrange(salmon_genus, longname) %>%
  #   dplyr::select(scenario_name, salmon_effect, longname, excess_mort, code) %>%
  #   dplyr::mutate(label = round(excess_mort,0))%>%
  #   dplyr::group_by(scenario_name, salmon_effect, longname) %>%
  #   dplyr::slice(which.max(label)) %>% # leaves only the maximum value
  #   #dplyr::select(-prop_survival) %>%
  #   #dplyr::bind_cols(prop.survival) %>%
  #   dplyr::mutate(label = paste(as.character(label), code), excess_mort = 95) %>%
  #   dplyr::select(-code)

  salmon.eff.text <- pred.plot.salmon %>%
    dplyr::filter(excess_mort >= thiscutoff) %>%
    dplyr::filter(guild!="Salmon") %>%
    # dplyr::filter(guild!="Demersal fish") %>%
    dplyr::select(scenario_type, scenario_name, guild, salmon_effect, excess_mort) %>%
    dplyr::mutate(label = round(excess_mort,0))%>%
    dplyr::group_by(scenario_name, salmon_effect, scenario_type, guild) %>%
    dplyr::slice(which.max(label)) %>%
    dplyr::mutate(label = formatC(label, format='e', digits=1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild_abv = dplyr::if_else(guild=="Demersal fish", "DemF",
                                             dplyr::if_else(guild=="Marine mammals", "MarM",
                                                            dplyr::if_else(guild=="Elasmobranchs", "Elasmo",
                                                                           dplyr::if_else(guild=="Small planktivorous fish","Sm. plank",guild))))) %>%
    dplyr::mutate(label = paste(as.character(label), guild_abv), excess_mort = (thiscutoff-2)) %>%
    dplyr::select(-guild_abv) %>%
    dplyr::arrange(salmon_effect, scenario_name, scenario_type, guild)

  guild.fill.all <- paletteer::paletteer_d("dutchmasters::pearl_earring")
  guild.fill <- c("Seabirds"=guild.fill.all[c(3)], "Marine mammals" = guild.fill.all[c(6)], "Elasmobranchs"=guild.fill.all[c(2)], "Demersal fish"=guild.fill.all[c(1)],"Small planktivorous fish"=guild.fill.all[c(7)])


  pred.boxplot <- pred.plot.salmon %>%
    dplyr::filter(guild!="Salmon") %>%
    dplyr::mutate(guild = forcats::fct_relevel(as.factor(guild), "Demersal fish","Small planktivorous fish","Elasmobranchs","Marine mammals","Seabirds")) %>%
    ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = salmon_effect, fill = guild)) +
    ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.6, alpha = 0.6) +
    #ggplot2::geom_point(ggplot2::aes(color=guild), position = ggplot2::position_jitterdodge(), alpha=0.5) +
    #ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(scenario_type ~ scenario_name, ncol=2, scales = "free_y") +
    ggplot2::scale_fill_manual(values = guild.fill, name = "Species guild") +
    ggplot2::scale_color_manual(values = guild.fill) +
    ggplot2::labs(title = "Salmon consumption by predators in survival scenarios",
                  y = "Proportional change in salmon consumption (scenario/base)", x = "Expected impact on salmon", face = "bold") +
    ggthemes::theme_base()  +
    ggplot2::ylim(-thiscutoff, thiscutoff) +
    ggplot2::theme(legend.position = "bottom") +
  #  ggplot2::guides(color="none") +
    ggrepel::geom_text_repel(
      data          = salmon.eff.text,
      mapping       = ggplot2::aes(salmon_effect, excess_mort, label = label),
      force = 0.9,
      force_pull = 0.7,
      size          = 3,
      colour        = "black"
    )


  ggplot2::ggsave("individualsc_dietcheck.png", plot = pred.boxplot, device = "png", width= 13, height = 16.50, scale = 1, dpi = 600)



  mammal.fill.all <- paletteer::paletteer_d("dutchmasters::view_of_Delft")
  mammal.fill <- c("California sea lions"=mammal.fill.all[c(2)], "Harbor seals" = mammal.fill.all[c(1)], "Resident Orca"=mammal.fill.all[c(6)], "Steller sea lions"=mammal.fill.all[c(10)])


  thiscutoff <- 10

  salmon.eff.text <- pred.plot.salmon %>%
    dplyr::filter(excess_mort >= thiscutoff) %>%
    dplyr::filter(guild=="Marine mammals") %>%
    droplevels() %>%
    # dplyr::filter(guild!="Demersal fish") %>%
    dplyr::select(scenario_type, scenario_name, longname, salmon_effect, excess_mort) %>%
    dplyr::mutate(label = round(excess_mort,0))%>%
    dplyr::group_by(scenario_name, salmon_effect, scenario_type, longname) %>%
    dplyr::slice(which.max(label)) %>%
    dplyr::mutate(label = formatC(label, format='e', digits=1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ln_abv = dplyr::if_else(longname=="California sea lions", "Cal. sea lions", longname)) %>%
    dplyr::mutate(label = paste(as.character(label), ln_abv), excess_mort = (thiscutoff-2)) %>%
    dplyr::arrange(salmon_effect, scenario_name, scenario_type, label)

  guild.fill.all <- paletteer::paletteer_d("dutchmasters::pearl_earring")
  guild.fill <- c("Seabirds"=guild.fill.all[c(3)], "Marine mammals" = guild.fill.all[c(6)], "Elasmobranchs"=guild.fill.all[c(2)], "Demersal fish"=guild.fill.all[c(1)],"Small planktivorous fish"=guild.fill.all[c(7)])


  pred.boxplot <- pred.plot.salmon %>%
    dplyr::filter(guild=="Marine mammals") %>%
    droplevels() %>%
    ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = salmon_effect, fill = longname)) +
    ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.6, alpha = 0.6) +
    #ggplot2::geom_point(ggplot2::aes(color=guild), position = ggplot2::position_jitterdodge(), alpha=0.5) +
    #ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(scenario_type ~ scenario_name, ncol=2, scales = "free_y") +
    ggplot2::scale_fill_manual(values = mammal.fill, name = "Group") +
    ggplot2::scale_color_manual(values = mammal.fill) +
    ggplot2::labs(title = "Salmon consumption by marine mammals in survival scenarios",
                  y = "Proportional change in salmon consumption (scenario/base)", x = "Expected impact on salmon", face = "bold") +
    ggthemes::theme_base()  +
    ggplot2::ylim(-thiscutoff, thiscutoff) +
    ggplot2::theme(legend.position = "bottom") +
    ggrepel::geom_text_repel(
      data          = salmon.eff.text,
      mapping       = ggplot2::aes(salmon_effect, excess_mort, label = label),
      force = 0.9,
      force_pull = 0.7,
      size          = 3,
      colour        = "black"
    )



  ggplot2::ggsave("individual_dietcheck_marinemamm.png", plot = pred.boxplot, device = "png", width= 11, height = 13.5, scale = 1, dpi = 600)


  return(pred.boxplot)


}
