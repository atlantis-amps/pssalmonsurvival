#' @title plot salmon abundance from Lossee et al
#' @param salmon.abundance, a data frame
#'
#' @return salmon.plot
#' @param salmonabundance, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon_cum_return_nums.csv, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002


plot_salmon_abundance <- function(salmon.abundance) {

    pal.col <- wesanderson::wes_palette("Cavalcanti1", 5)

    salmon.plot <- salmon.abundance %>%
        ggplot2::ggplot(ggplot2::aes(x = Year, y = biomass_mt, group = salmon_group)) + ggplot2::geom_line(ggplot2::aes(color = salmon_group, group = salmon_group,
        linetype = salmon_group %in% c("Pink")), size = 0.7) + ggplot2::scale_linetype_manual(values = c(`TRUE` = "twodash", `FALSE` = "solid")) + ggplot2::scale_color_manual("Salmon group",
        values = pal.col) + ggplot2::theme_minimal() + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 1)) + ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold")) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold")) + ggplot2::scale_y_continuous(name = "Biomass (metric tons)") + ggplot2::scale_x_continuous(name = "Year") +
        ggplot2::guides(color = ggplot2::guide_legend("Salmon group"), linetype = FALSE)


    ggplot2::ggsave("reconstructed_salmon.png", salmon.plot, width = 8, height = 6, device = "png")

    return(salmon.plot)

}
