#' @title plot biomass of ensemble models
#' @param ensemblebiomass, a data frame
#' @param plotmodels, a list of models to plot
#'
#' @return models.used, data frame of final models plotted
#' @export
#'
#' @title Code to plot multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date February 2022
#'

plot_ensemblebiomass <- function(ensemblebiomass, plotmodels){

  plot.biomass <- ensemblebiomass %>%
    filter(longname!="Carrion") %>%
    filter(!model_ver%in% plotmodels)

  thisvariabletype <- "Biomass"
  #library(paletteer)
  #paletteer_d("Redmonder::qMSOSlp")

  col.pal <- redmonder.pal(8, "qMSOSlp")


  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    length(levels(as.factor(model.biomass.frame$longname)))/ 16
  )

  print(n_pages)
  for (i in seq_len(n_pages)) {

    print(i)

    pplot <-  ggplot(plot.biomass, aes(x=Year,y=biomass, group = model_ver, colour = model_ver))+
      geom_line()+
      labs(y= thisvariabletype, x = "Year") +
      scale_color_manual(values=col.pal, name = "Model version")+
      scale_y_continuous(limits = c(0,NA))+
      facet_wrap_paginate(~longname, ncol = 4, nrow = 4, page = i, shrink = FALSE, scales = "free")+
      theme_minimal()+
      theme(strip.text = element_text(size = 7),
            legend.position="bottom",
            axis.text.x = element_text(size =c(8)),
            axis.text.y = element_text(size =c(8)))




    thisplotname <- paste(thisvariabletype,i,"model_comparison_plot.png",sep="_")

     ggsave(thisplotname,plot = pplot, device="png",width = 21, height = 29, units = "cm")
  }

}

