#' Hem Nalini Morzaria-Luna
#' hmorzarialuna@gmail.com
#' Code to create Atlantis food web from prey matrix that show linkages with 0 & 1
#' uses package ggnet2 https://briatte.github.io/ggnet/

#' @title plot Atlantis food web
#' @param ensemblebiomass, a data frame
#' @param plotmodels, a list of models to plot
#'
#' @return models.used, data frame of final models plotted
#' @export
#' @description Code to create Atlantis food web from prey matrix that show linkages with 0 & 1
#' @description reads square matrix of predators (rows) consuming prey (columns)
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com, February 2022
#' @example
#' example using random graph, useful to see the format needed
#' from https://briatte.github.io/ggnet/
#' random graph
#' net.ex = rgraph(10, mode = 'graph', tprob = 0.5)
#' net = network(net.ex, directed = FALSE)
#' vertex names
#' network.vertex.names(net) = letters[1:10]
#' ggnet2(net, size = 6, color = rep(c('tomato', 'steelblue'), 5))
#' ggnet2(net, mode = 'target', layout.par = list(niter = 100))


plot_foodweb <- function(ppreymatrix, plot.name) {

    ps.matrix <- ppreymatrix %>%
        dplyr::select(-Predator) %>%
        as.matrix()
    colnames(ps.matrix) <- NULL

    # creates network with nodes and edges
    net.prey <- network::network(ps.matrix, directed = FALSE)

    # vertex names
    network::network.vertex.names(net.prey) <- ppreymatrix$Predator

    # order of colors is bacteria primary producers zooplankton invertebrates forage fish salmon demersal fish elasmobranchs birds marine mammals detritus

    pred.labels <- c("Detritus", "Bacteria", "Primary producers", "Zooplankton", "Invertebrates", "Forage fish", "Salmon", "Demersal fish", "Elasmobranchs", "Birds",
        "Marine mammals")

    pred.names <- c(rep(pred.labels[2], 2), rep(pred.labels[3], 4), rep(pred.labels[4], 4), rep(pred.labels[5], 12), rep(pred.labels[6], 4), rep(pred.labels[7],
        21), rep(pred.labels[8], 9), rep(pred.labels[9], 4), rep(pred.labels[10], 3), rep(pred.labels[11], 7), rep(pred.labels[1], 3))

    pred.factors <- factor(pred.names, levels = pred.labels)

    net.graph <- GGally::ggnet2(net.prey, color = pred.factors, color.legend = "Guild", size = 6, edge.alpha = 0.5) + ggsci::scale_color_simpsons(name = "Guild")


    ggplot2::ggsave(plot.name, plot = net.graph, device = "png", width = 12.75, height = 9.78, scale = 1, dpi= 600)


    return(net.graph)

}





