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
plot_dietcheckcum <- function(dietcheckcum, salmoneffect, groupguilds) {

  pred.sc <- dietcheckcum %>%
    dplyr::filter(scenario_var != 1) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "0_8", "-20%", "+20%")) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "bottom-top", "Bottom-up & Top-down", scenario_name)) %>%
    dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
    dplyr::left_join(salmoneffect, by = c("scenario_name", "scenario_var")) %>%
    dplyr::mutate(year = Time/365)

  pred.base <- dietcheckcum %>%
    dplyr::filter(scenario_var == 1) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "bottom-top", "Bottom-up & Top-down", scenario_name)) %>%
    dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
    dplyr::rename(ini_tot_mort = tot_mort) %>%
    dplyr::select(-scenario_var)


  pred.plot.salmon <- pred.sc %>%
    dplyr::left_join(pred.base, by = c("Time","Predator","model_ver","scenario_name")) %>%
    dplyr::mutate(excess_mort = (tot_mort/ini_tot_mort - 1) * 100) %>%
    #excess mortality is proportional change in mortality
    dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Bottom-up", "Top-down", "Bottom-up & Top-down")) %>%
    dplyr::rename(code=Predator) %>%
    dplyr::left_join(groupguilds, by="code") %>%
    dplyr::mutate(excess_mort=dplyr::if_else(is.nan(excess_mort),0,excess_mort))

  pred.guild.base <- pred.plot.salmon %>%
    dplyr::group_by(year, scenario_name, salmon_effect, model_ver, guild) %>%
    dplyr::summarise(guild_tot_mort_base = sum(ini_tot_mort))

  pred.guild <- pred.plot.salmon %>%
    dplyr::group_by(year, scenario_name, salmon_effect, model_ver, guild) %>%
    dplyr::summarise(guild_tot_mort = sum(tot_mort)) %>%
    dplyr::left_join(pred.guild.base, by = c("year","scenario_name","salmon_effect","model_ver","guild")) %>%
    dplyr::mutate(excess_mort = (guild_tot_mort/guild_tot_mort_base - 1) * 100) %>%
    dplyr::mutate(excess_mort=dplyr::if_else(is.nan(excess_mort),0,
                                             dplyr::if_else(is.infinite(excess_mort),0,excess_mort)))


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

  salmon.colors <- salmon_colors()

  pred.boxplot <- pred.guild %>%
    dplyr::mutate(year = as.factor(year)) %>%
    dplyr::filter(!guild %in% c("Invertebrates","Zooplankton")) %>%
    ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = year, color = salmon_effect)) +
    ggplot2::geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.6, alpha = 0.6) +
    #ggplot2::geom_point(ggplot2::aes(color=guild), position = ggplot2::position_jitterdodge(), alpha=0.5) +
    #ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(scenario_name ~ guild, ncol=2, scales = "free_y") +
    #  ggplot2::scale_fill_manual(values = salmon.colors, name = "Guild") +
    #  ggplot2::scale_color_manual(values = salmon.colors) +
    ggplot2::labs(title = "Salmon predation mortality in cumulative scenarios",
                  y = "Proportional change in predation mortality (scenario/base)", x = "Year", face = "bold") +
    ggthemes::theme_base() +
    ggplot2::theme(legend.position = "right") #+
  #  ggplot2::geom_text(data=salmon.gen, ggplot2::aes(salmon_effect, excess_mort, label = label))
  #   ggrepel::geom_text_repel(
  #     data          = salmon.gen,
  #     mapping       = ggplot2::aes(salmon_effect, excess_mort, label = label),
  #     force = 0.9,
  #     force_pull = 0.9,
  #     size          = 3.5,
  #     colour = "black"
  #   ) +
  #   ggplot2::guides(color="none", fill=ggplot2::guide_legend(ncol =1))
  # #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))
  #

  ggplot2::ggsave("timeseries_dietcheck.png", plot = pred.boxplot, device = "png", width= 11.5, height = 12.00, scale = 1, dpi = 600)

  #figure predation by basin

  # salmon.basin <- pred.plot.salmon %>%
  #   dplyr::filter(excess_mort >= 100) %>%
  #   dplyr::arrange(salmon_genus, longname) %>%
  #   dplyr::select(basin, scenario_name, salmon_effect, excess_mort) %>%
  #   dplyr::mutate(label = round(excess_mort,0))%>%
  #   dplyr::group_by(basin, scenario_name, salmon_effect) %>%
  #   dplyr::slice(which.max(label)) %>% # leaves only the maximum value
  #   dplyr::mutate(label = paste(basin, as.character(label)), excess_mort = 95)
  #
  # basin.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
  #                 `Hood Canal` = "#032F5C")
  #
  # pred.basin.boxplot <- pred.plot.salmon %>%
  #   dplyr::filter(excess_mort<= 100) %>%
  #   dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
  #   ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = salmon_effect, fill = basin)) +
  #   ggplot2::geom_point(ggplot2::aes(color=basin), position = ggplot2::position_jitterdodge(), alpha=0.5) +
  #   ggplot2::geom_boxplot(outlier.shape = NA) +
  #   ggplot2::geom_hline(yintercept = 0) +
  #   ggplot2::facet_wrap(.~ scenario_name, ncol=1) +
  #   ggplot2::scale_fill_manual(values = basin.fill, name = "Basin") +
  #   ggplot2::scale_color_manual(values = basin.fill) +
  #   ggplot2::labs(title = "Predation mortality in cumulative scenarios",
  #                 y = "Proportional change in predation mortality (scenario-base)", x = "Expected salmon impact", face = "bold") +
  #   ggthemes::theme_base() +
  #   ggplot2::theme(legend.position = "right") +
  #   ggrepel::geom_text_repel(
  #     data          = salmon.basin,
  #     mapping       = ggplot2::aes(salmon_effect, excess_mort, label = label),
  #     force = 0.7,
  #     force_pull = 1.2,
  #     size          = 3.5,
  #     colour = "black"
  #   ) +
  #   ggplot2::guides(color="none", fill=ggplot2::guide_legend(ncol =1))+
  #   ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))
  # #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))
  #
  #
  #
  # ggplot2::ggsave("boxplot_predation_basin_scale.png", plot = pred.basin.boxplot, device = "png", width= 11.5, height = 12.00, scale = 1, dpi = 600)

  return(pred.boxplot)


}
