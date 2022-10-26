#' @title plot salmon abundance from Lossee et al
#' @param salmonabundance, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon_cum_return_nums.csv, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002


plot_salmon_abundance <- function(salmon.abundance){

pal.col <- wesanderson::wes_palette("Cavalcanti1",5)

  salmon.plot <- salmon.abundance %>%
    dplyr::ggplot(dplyr::aes(x=Year, y=biomass_mt, group=`salmon_group`)) +
    dplyr::geom_line(dplyr::aes(color = salmon_group, group = salmon_group, linetype = salmon_group %in% c("Pink")), size = 0.7) +
    dplyr::scale_linetype_manual(values = c("TRUE" = "twodash", "FALSE" = "solid")) +
    dplyr::scale_color_manual("Salmon group", values=pal.col) +
    dplyr::theme_minimal()+
    dplyr::theme(axis.text.y=element_text(angle=90,hjust=1)) +
    dplyr::theme(axis.title.y=element_text(face="bold")) +
    dplyr::theme(axis.title.x=element_text(face="bold")) +
    dplyr::scale_y_continuous(name ="Biomass (metric tons)") +
    dplyr::scale_x_continuous(name ="Year") +
    dplyr::guides(color=guide_legend("Salmon group"), linetype = FALSE)


  dplyr::ggsave("reconstructed_salmon.png",salmon.plot, width = 8, height = 6, device='png')

}
