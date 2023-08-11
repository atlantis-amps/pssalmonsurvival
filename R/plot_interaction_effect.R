
#' Plot interaction effect
#'
#' @param ensemble.survival
#' @param ensemble.cum.survival
#' @param scenariocategories
#' @param salmnonbybasin
#' @param salmon.colors
#'
#' @return
#' @export
#'
#' @examples
#'
plot_interaction_effect <- function(ensemble.survival, ensemble.cum.survival, scenariocategories, salmnonbybasin, thiscutoff) {

    ind.survival <- ensemble.survival %>%
        dplyr::select(scenario_name, model_ver, longname, Code, salmon_genus, salmon_effect, prop_survival) %>%
        dplyr::left_join(scenariocategories, by = c("scenario_name")) %>%
        dplyr::group_by(scenario_category, model_ver, longname, Code, salmon_genus, salmon_effect) %>%
        dplyr::summarise(expec_cum = sum(prop_survival)) %>%
        dplyr::mutate(scenario_category = dplyr::if_else(scenario_category == "bottom-up", "Bottom-up", "Top-down")) %>%
        dplyr::ungroup()

    ind.cum.survival <- ensemble.survival %>%
        dplyr::select(scenario_name, model_ver, longname, Code, salmon_genus, salmon_effect, prop_survival) %>%
        dplyr::left_join(scenariocategories, by = c("scenario_name")) %>%
        dplyr::mutate(scenario_category = "Bottom-up & Top-down") %>%
        dplyr::group_by(scenario_category, model_ver, longname, Code, salmon_genus, salmon_effect) %>%
        dplyr::summarise(expec_cum = sum(prop_survival)) %>%
        dplyr::ungroup() %>%
        dplyr::bind_rows(ind.survival)

    #check categories
    #ind.cum.survival %>% dplyr::distinct(scenario_category)

    salmon.order <- salmonbybasin %>%
      # dplyr::filter(salmon_genus != 'Chum')
      dplyr::arrange(salmon_genus, geo_order, longname) %>%
      dplyr::pull(longname)

    effect.size <- ensemble.cum.survival %>%
        dplyr::select(scenario_name, model_ver, longname, Code, salmon_genus, salmon_effect, prop_survival) %>%
        dplyr::rename(scenario_category = scenario_name, obs_cum = prop_survival) %>%
        dplyr::right_join(ind.cum.survival, by = c("scenario_category", "model_ver", "Code", "longname", "salmon_genus", "salmon_effect")) %>%
        dplyr::mutate(effect_size = (obs_cum - expec_cum)) %>%
        dplyr::mutate(scenario_category = forcats::fct_relevel(as.factor(scenario_category), "Bottom-up", "Top-down", "Bottom-up & Top-down")) %>%
 #       dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound", "Hood Canal")) %>%
        dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
        dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye"))

    #check categories
    #effect.size %>% dplyr::distinct(scenario_category)
    #check for NA values
    #colSums(is.na(effect.size))

    col.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
        `Hood Canal` = "#032F5C")


    salmon.codes <- salmonbybasin %>%
      dplyr::select(Code, longname)

    salmon.eff.text <- effect.size %>%
      dplyr::filter(effect_size >= thiscutoff | effect_size <(-thiscutoff)) %>%
      dplyr::arrange(salmon_effect, longname) %>%
      dplyr::select(scenario_category, salmon_effect, longname, effect_size) %>%
      dplyr::mutate(label = round(effect_size,0)) %>%
      dplyr::group_by(scenario_category, salmon_effect, longname) %>%
      dplyr::slice(which.max(label)) %>% # leaves only the maximum value
      #dplyr::select(-prop_survival) %>%
      #dplyr::bind_cols(prop.survival) %>%
      dplyr::left_join(salmon.codes, by = "longname") %>%
      dplyr::mutate(label = paste(as.character(label), Code)) %>%
      dplyr::mutate(effect_size = dplyr::if_else(effect_size<thiscutoff, -(thiscutoff-5),
                                                 dplyr::if_else(effect_size>thiscutoff, (thiscutoff-5), effect_size))) %>%
      dplyr::select(-Code)

    #get salmon color palette
    salmon.colors <- salmon_colors()

    box.plot.effect <- effect.size %>%
      dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
      ggplot2::ggplot(ggplot2::aes(y = effect_size, x = salmon_effect, fill = longname)) +
      ggplot2::geom_point(ggplot2::aes(color=longname), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_hline(yintercept = 5, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = -5, linetype = "dashed") +
      ggplot2::facet_wrap(. ~ scenario_category, ncol=1) +
      ggplot2::ylim(-thiscutoff, thiscutoff) +
      ggplot2::scale_fill_manual(values = salmon.colors, name = "Salmon groups") +
      ggplot2::labs(title = "Effect size in cumulative salmon survival scenarios", y = "Effect size", x = "Expected salmon impact", face = "bold") +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position = "right") +
      ggrepel::geom_text_repel(
        data          = salmon.eff.text,
        mapping       = ggplot2::aes(salmon_effect, effect_size, label = label),
        force = 0.7,
        force_pull = 0.7,
        size          = 3,
        colour        = "black"
      ) +
      ggplot2::guides(color="none") +
      # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
      ggplot2::scale_color_manual(values = salmon.colors) +
      ggplot2::guides(color="none", fill=ggplot2::guide_legend(ncol =1)) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20))


    ggplot2::ggsave("boxplot_effect_size.png", plot = box.plot.effect, device = "png", width= 11.0, height = 9.50, scale = 1, dpi = 600)

    return(box.plot.effect)
}
