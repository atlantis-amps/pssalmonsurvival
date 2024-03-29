#' @title plot biomass of ensemble models
#' @param ensemblebiomass, a data frame
#' @param plotmodels, a list of models to plot
#'
#' @return models.used, data frame of final models plotted
#' @export
#'
#' @description Code to plot biomass of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com, February 2022
#'

plot_ensemblebiomass <- function(ensemblebiomass) {
    # , plotmodels

    plot.biomass <- ensemblebiomass %>%
        dplyr::filter(longname != "Carrion") %>%
        # dplyr::filter(!model_ver%in% plotmodels) %>%
    dplyr::mutate(model_ver = as.factor(model_ver))

    thisvariabletype <- "Biomass"
    # library(paletteer) paletteer_d('ggthemes::qMSOSlp')


    # Calculate the number of pages with 12 panels per page
    n_pages <- ceiling(length(levels(as.factor(plot.biomass$longname)))/16)

    print(n_pages)

    plot.list <- list()

    for (i in seq_len(n_pages)) {

        print(i)

        biomass.plot <- ggplot2::ggplot(plot.biomass, ggplot2::aes(x = Year, y = biomass, group = model_ver, colour = model_ver)) +
          ggplot2::geom_line() + ggplot2::labs(y = thisvariabletype, x = "Year") + #   ggplot2::scale_color_manual(values=col.pal, name = 'Model version')+ name = 'Model version')+
        ggthemes::theme_few() + ggthemes::scale_colour_few(name = "Model version") + ggplot2::scale_y_continuous(limits = c(0, NA)) + ggforce::facet_wrap_paginate(~longname,
            ncol = 4, nrow = 4, page = i, shrink = FALSE, scales = "free") + ggplot2::theme(strip.text = ggplot2::element_text(size = 7), legend.position = "bottom",
            axis.text.x = ggplot2::element_text(size = c(8)), axis.text.y = ggplot2::element_text(size = c(8)))

        plot.list[[i]] <- biomass.plot


        thisplotname <- paste(thisvariabletype, i, "model_comparison_plot.png", sep = "_")

        ggplot2::ggsave(thisplotname, plot = biomass.plot, device = "png", width = 10.86, height = 9.81, scale = 1, dpi= 600)
    }

    return(plot.list)
}

