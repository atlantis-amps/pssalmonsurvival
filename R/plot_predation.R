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
        dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "0_8", "-20%", "+20%")) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "bottom-top", "Bottom-up & Top-down", scenario_name)) %>%
        dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
        dplyr::left_join(salmoneffect, by = c("scenario_name", "scenario_var")) %>%
        dplyr::group_by(code, longname, model_ver, scenario_name, salmon_effect) %>%
        dplyr::summarise(pred_mortality = sum(mortality)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(salmon.basin, by = c("code"))

    pred.base <- ensemblepredationcum %>%
        dplyr::filter(time == 28) %>%
        dplyr::filter(scenario_var == 1) %>%
        dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "bottom-top", "Bottom-up & Top-down", scenario_name)) %>%
        dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
        dplyr::group_by(code, longname, model_ver, scenario_name) %>%
        dplyr::summarise(base_mortality = sum(mortality)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(salmon.basin, by = c("code"))

    pred.plot.salmon <- pred.sc %>%
      dplyr::left_join(pred.base, by = c("code", "longname", "model_ver", "scenario_name", "name", "basin", "geo_order", "salmon_genus")) %>%
       dplyr::mutate(excess_mort = (pred_mortality/base_mortality - 1) * 100) %>%
      #excess mortality is proportional change in mortality
        dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Bottom-up", "Top-down", "Bottom-up & Top-down")) %>%
        dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound", "Hood Canal")) %>%
        dplyr::mutate(longname = gsub("Subyrlng","SY", longname)) %>%
        dplyr::mutate(longname = gsub("Yrlng","Y", longname)) %>%
        dplyr::mutate(longname = dplyr::if_else(longname == "Strait of Georgia salmonids", "St. of Georgia salmonids",
                                                dplyr::if_else(longname == "Chum Hood Canal summer run SY", "Chum Hood Canal SY", longname))) %>%
        dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye"))


    salmon.order <- salmonbybasin %>%
      # dplyr::filter(salmon_genus != 'Chum')
      dplyr::arrange(salmon_genus, geo_order, longname) %>%
      dplyr::pull(longname)

    # HEATMAP, not used because it can't incorporate variation in the model ensemble pred.plot.salmon %>% ggplot(aes(salmon_effect, longname)) +
    # geom_tile(aes(fill = log(excess_mort))) + scale_fill_gradient(low = 'lightyellow', high = 'firebrick4') + facet_wrap(.~scenario_name)


    salmon.gen <- pred.plot.salmon %>%
      dplyr::filter(excess_mort >= 100) %>%
      dplyr::arrange(salmon_genus, longname) %>%
      dplyr::select(scenario_name, salmon_effect, longname, excess_mort, code) %>%
      dplyr::mutate(label = round(excess_mort,0))%>%
      dplyr::group_by(scenario_name, salmon_effect, longname) %>%
      dplyr::slice(which.max(label)) %>% # leaves only the maximum value
      #dplyr::select(-prop_survival) %>%
      #dplyr::bind_cols(prop.survival) %>%
      dplyr::mutate(label = paste(as.character(label), code), excess_mort = 95) %>%
      dplyr::select(-code)

    salmon.colors <- salmon_colors()

    pred.boxplot <- pred.plot.salmon %>%
      dplyr::filter(excess_mort<= 100) %>%
      dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
      ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = salmon_effect, fill = longname)) +
      ggplot2::geom_point(ggplot2::aes(color=longname), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(.~ scenario_name, ncol=1) +
      ggplot2::scale_fill_manual(values = salmon.colors, name = "Salmon group") +
      ggplot2::scale_color_manual(values = salmon.colors) +
      ggplot2::labs(title = "Salmon predation mortality in cumulative scenarios",
                    y = "Proportional change in predation mortality (scenario/base)", x = "Expected salmon impact", face = "bold") +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position = "right") +
    #  ggplot2::geom_text(data=salmon.gen, ggplot2::aes(salmon_effect, excess_mort, label = label))
      ggrepel::geom_text_repel(
        data          = salmon.gen,
        mapping       = ggplot2::aes(salmon_effect, excess_mort, label = label),
        force = 0.9,
        force_pull = 0.9,
        size          = 3.5,
        colour = "black"
      ) +
      ggplot2::guides(color="none", fill=ggplot2::guide_legend(ncol =1))
    #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))


    ggplot2::ggsave("boxplot_predation_salmon_effect.png", plot = pred.boxplot, device = "png", width= 11.5, height = 12.00, scale = 1, dpi = 600)

    #figure predation by basin

    salmon.basin <- pred.plot.salmon %>%
      dplyr::filter(excess_mort >= 100) %>%
      dplyr::arrange(salmon_genus, longname) %>%
      dplyr::select(basin, scenario_name, salmon_effect, excess_mort) %>%
      dplyr::mutate(label = round(excess_mort,0))%>%
      dplyr::group_by(basin, scenario_name, salmon_effect) %>%
      dplyr::slice(which.max(label)) %>% # leaves only the maximum value
      dplyr::mutate(label = paste(basin, as.character(label)), excess_mort = 95)

    basin.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
                    `Hood Canal` = "#032F5C")

    pred.basin.boxplot <- pred.plot.salmon %>%
      dplyr::filter(excess_mort<= 100) %>%
      dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
      ggplot2::ggplot(ggplot2::aes(y = excess_mort, x = salmon_effect, fill = basin)) +
      ggplot2::geom_point(ggplot2::aes(color=basin), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(.~ scenario_name, ncol=1) +
      ggplot2::scale_fill_manual(values = basin.fill, name = "Basin") +
      ggplot2::scale_color_manual(values = basin.fill) +
      ggplot2::labs(title = "Predation mortality in cumulative scenarios",
                    y = "Proportional change in predation mortality (scenario-base)", x = "Expected salmon impact", face = "bold") +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position = "right") +
      ggrepel::geom_text_repel(
        data          = salmon.basin,
        mapping       = ggplot2::aes(salmon_effect, excess_mort, label = label),
        force = 0.7,
        force_pull = 1.2,
        size          = 3.5,
        colour = "black"
      ) +
      ggplot2::guides(color="none", fill=ggplot2::guide_legend(ncol =1))+
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))
    #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))



    ggplot2::ggsave("boxplot_predation_basin_scale.png", plot = pred.basin.boxplot, device = "png", width= 11.5, height = 12.00, scale = 1, dpi = 600)

    return(pred.boxplot)


}
