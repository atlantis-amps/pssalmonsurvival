#' Plot Dietcheckcum time series
#'
#' @param dietcheckcum total predation by predators on salmon groups for cumulative scenarios
#' @param salmoneffect predicted scenario effects on salmon
#' @param groupguilds functional groups by guild
#'
#' @return pred.boxplot
#' @export
#'
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com May 2023
#' @examples
plot_dietcheckcum <- function(dietcheckcum, salmoneffect, predgroups, thiscutoff) {

  pred.sc <- dietcheckcum %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "bottom-top", "Bottom-up & Top-down", scenario_name)) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "1", "base",
                                                dplyr::if_else(scenario_var=="0_8","-20%",
                                                               dplyr::if_else(scenario_var=="1_2","+20%", scenario_var)))) %>%
    dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
    dplyr::left_join(salmoneffect, by = c("scenario_name", "scenario_var")) %>% #CONTINUE HERE
    dplyr::mutate(year = Time/365) %>%
    dplyr::filter(year >= 25) %>%
    dplyr::filter(tot_mort !=0) %>%
    dplyr::rename(Code=Predator) %>%
    dplyr::left_join(predgroups, by="Code") %>%
    dplyr::rename(longname=Predator) %>%
    dplyr::group_by(longname, guild, scenario_name, scenario_var, model_ver, salmon_effect) %>%
    dplyr::summarise(av_mort = mean(tot_mort)) %>%
    dplyr::ungroup()

  pred.base <- pred.sc %>%
    dplyr::filter(scenario_var == "base") %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "bottom-top", "Bottom-up & Top-down", scenario_name)) %>%
    dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
    dplyr::rename(ini_tot_mort = av_mort) %>%
    dplyr::select(-scenario_var, -salmon_effect)

  pred.plot.salmon <- pred.sc %>%
    dplyr::filter(scenario_var != "base") %>%
    dplyr::left_join(pred.base, by = c("longname","guild","scenario_name","model_ver")) %>%
    dplyr::mutate(excess_mort = (av_mort/ini_tot_mort - 1) * 100) %>%
    #excess mortality is proportional change in mortality
    dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Bottom-up", "Top-down", "Bottom-up & Top-down")) %>%
    dplyr::mutate(excess_mort=dplyr::if_else(is.nan(excess_mort),0,excess_mort))


  salmon.eff.text <- pred.plot.salmon %>%
    dplyr::filter(excess_mort >= thiscutoff) %>%
    dplyr::filter(guild!="Salmon") %>%
    # dplyr::filter(guild!="Demersal fish") %>%
    dplyr::mutate(label = round(excess_mort,0))%>%
    dplyr::group_by(guild, scenario_name, salmon_effect) %>%
    dplyr::slice(which.max(label)) %>%
    dplyr::mutate(label = formatC(label, format='e', digits=1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(guild_abv = dplyr::if_else(guild=="Demersal fish", "DemF",
                                             dplyr::if_else(guild=="Marine mammals", "MarM",
                                                            dplyr::if_else(guild=="Elasmobranchs", "Elasmo",
                                                                           dplyr::if_else(guild=="Small planktivorous fish","Sm. plank",guild))))) %>%
    dplyr::mutate(label = paste(as.character(label), guild_abv), excess_mort = (thiscutoff-2)) %>%
    dplyr::select(-guild_abv) %>%
    dplyr::arrange(guild, scenario_name, salmon_effect)

  guild.fill.all <- paletteer::paletteer_d("dutchmasters::pearl_earring")
  guild.fill <- c("Seabirds"=guild.fill.all[c(3)], "Marine mammals" = guild.fill.all[c(6)], "Elasmobranchs"=guild.fill.all[c(2)], "Demersal fish"=guild.fill.all[c(1)],"Small planktivorous fish"=guild.fill.all[c(7)])


  pred.boxplot <- pred.plot.salmon %>%
    dplyr::filter(guild!="Salmon") %>%
    dplyr::mutate(guild = forcats::fct_relevel(as.factor(guild), "Demersal fish","Small planktivorous fish","Elasmobranchs","Marine mammals","Seabirds")) %>%
    ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = salmon_effect, fill = guild)) +
    ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.6, alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(color=guild), position = ggplot2::position_jitterdodge(), alpha=0.5) +
    #ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(. ~ scenario_name, ncol=1, scales = "free_y") +
    ggplot2::scale_fill_manual(values = guild.fill, name = "Species guild") +
    ggplot2::scale_color_manual(values = guild.fill) +
    ggplot2::labs(title = "Salmon consumption by predators in cumulative scenarios",
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
    ) +
    ggplot2::guides(color="none")


  ggplot2::ggsave("cumsc_dietcheck.png", plot = pred.boxplot, device = "png", width= 12, height = 11, scale = 1, dpi = 600)


  mammal.fill.all <- paletteer::paletteer_d("dutchmasters::view_of_Delft")
  mammal.fill <- c("California sea lions"=mammal.fill.all[c(2)], "Harbor seals" = mammal.fill.all[c(1)], "Resident Orca"=mammal.fill.all[c(6)], "Steller sea lions"=mammal.fill.all[c(10)])


  thiscutoff <- pred.plot.salmon %>%
      dplyr::filter(guild=="Marine mammals") %>%
    dplyr::pull(excess_mort) %>%
    range() %>%
    abs() %>%
    max() %>%
    round(.,0)

  pred.boxplot.mamm <- pred.plot.salmon %>%
    dplyr::filter(guild=="Marine mammals") %>%
    ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = salmon_effect, fill = longname)) +
    ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.6, alpha = 0.6) +
    ggplot2::geom_point(ggplot2::aes(color=longname), position = ggplot2::position_jitterdodge(), alpha=0.5) +
    #ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(. ~ scenario_name, ncol=1, scales = "free_y") +
    ggplot2::scale_fill_manual(values = mammal.fill, name = "Group") +
    ggplot2::scale_color_manual(values = mammal.fill) +
    ggplot2::labs(title = "Salmon consumption by marine mammals in cumulative scenarios",
                  y = "Proportional change in salmon consumption (scenario/base)", x = "Expected impact on salmon", face = "bold") +
    ggthemes::theme_base()  +
    ggplot2::ylim(-thiscutoff, thiscutoff) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color="none")


  ggplot2::ggsave("cumsc_dietcheck_marinemamm.png", plot = pred.boxplot.mamm, device = "png", width= 12, height = 11, scale = 1, dpi = 600)


  return(pred.boxplot)


}
