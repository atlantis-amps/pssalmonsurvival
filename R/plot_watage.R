#' Plot weight-at-age for individual scenarios
#'
#' @param ensemblewageind
#' @param salmonbybasin
#' @param salmongroups
#'
#' @return wage.boxplot
#' @export
#'
#' @examples
#' plot_predation(ensemblewageind, salmonbybasin, salmongroups, thiscutoff=100)
plot_watage <- function(ensemblewageind, salmonbybasin, salmongroups, thiscutoff) {

  salmon.basin <- salmonbybasin %>%
    dplyr::rename(code = Code) %>%
    dplyr::select(-longname)

  salmon.order <- salmonbybasin %>%
    # dplyr::filter(salmon_genus != 'Chum')
    dplyr::arrange(salmon_genus, geo_order, longname) %>%
    dplyr::pull(longname)

  salmon.cohort <- salmongroups %>%
    dplyr::rename(code = Code, name = Name, longname= Long.Name) %>%
    dplyr::mutate(longname =dplyr::if_else(longname == "Chum Hood Canal summer run Subyearling", "Chum Hood Canal Subyearling", longname))

  #check groups in original data
  # ensemblewageind %>% dplyr::distinct(name)

  pred.sc <- ensemblewageind %>%
    dplyr::filter(time > 25) %>%
    dplyr::mutate(salmon_effect = dplyr::if_else(scenario_var == "1", "base",
                                                 dplyr::if_else(scenario_var =="1_2", "Negative", "Positive"))) %>%
    dplyr::filter(variable !=0) %>%
    dplyr::rename(longname = name) %>%
    dplyr::group_by(longname, age, scenario_name, model_ver, salmon_effect) %>%
    dplyr::summarise(av_wage = mean(variable)) %>%
    dplyr::ungroup()

  #check groups in original data
  # pred.sc %>% dplyr::distinct(longname) %>% dplyr::pull(longname)

  pred.base <- pred.sc %>%
    dplyr::filter(salmon_effect == "base") %>%
    dplyr::rename(av_wage_base=av_wage, base_effect = salmon_effect)

  pred.sc.base <- pred.sc %>%
    dplyr::filter(salmon_effect != "base") %>%
    dplyr::left_join(pred.base, by=c("longname", "age","scenario_name", "model_ver")) %>%
    dplyr::mutate(rel_wage = ((av_wage / av_wage_base)-1)*100) %>%
    dplyr::mutate(rel_wage = dplyr::if_else(is.nan(rel_wage), 0, rel_wage))


   pred.plot.salmon <- pred.sc.base %>%
    dplyr::mutate(scenario_name = gsub("-"," ", scenario_name)) %>%
    dplyr::mutate(scenario_name =dplyr::if_else(scenario_name == "salmon competition", "wild pink & chum salmon competition",
                                                dplyr::if_else(scenario_name =="Gel Zoo", "gelatinous zooplankton abundance",
                                                               dplyr::if_else(scenario_name =="mammal predation", "pinniped predation",
                                                                              dplyr::if_else(scenario_name == "seabirds predation", "seabird predation",
                                                                                             dplyr::if_else(scenario_name =="Spinydog Fish", "Spiny dogfish predation",
                                                                                                            dplyr::if_else(scenario_name == "herring trend", "herring abundance", scenario_name))))))) %>%
    dplyr::mutate(scenario_name = stringr::str_to_sentence(scenario_name)) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="Chinookhatchery competition","Hatchery Chinook competition", scenario_name)) %>%
    dplyr::left_join(salmon.cohort, by =c("longname")) %>%
    dplyr::left_join(salmon.basin, by=c("code", "name")) %>%
    dplyr::mutate(longname = dplyr::if_else(longname == "Strait of Georgia salmonids", "St. of Georgia salmonids",
                                            dplyr::if_else(longname == "Chum Hood Canal summer run SY", "Chum Hood Canal SY", longname))) %>%
    dplyr::mutate(longname = gsub("Subyearling","SY", longname)) %>%
    dplyr::mutate(longname = gsub("Yearling","Y", longname)) %>%
    dplyr::mutate(scenario_type = dplyr::if_else(grepl("predation", scenario_name), "Top-down", "Bottom-up")) %>%
    dplyr::mutate(scenario_type = forcats::fct_relevel(scenario_type, "Bottom-up", "Top-down")) %>%
    dplyr::filter(age==NumCohorts)



#check scenario names
#pred.plot.salmon %>% dplyr::distinct(scenario_name)
  #check groups
  # pred.plot.salmon %>% dplyr::distinct(longname) %>% dplyr::pull(longname)


  salmon.colors <- salmon_colors()


  for(eachtype in c("Bottom-up", "Top-down")){

    thistype <- tolower(eachtype)

    salmon.gen <- pred.plot.salmon %>%
      dplyr::filter(scenario_type == eachtype) %>%
      dplyr::filter(rel_wage >= thiscutoff) %>%
      dplyr::arrange(geo_order, longname) %>%
      dplyr::select(scenario_name, salmon_effect, longname, rel_wage, code) %>%
      dplyr::mutate(label = round(rel_wage,0))%>%
      dplyr::group_by(scenario_name, salmon_effect, longname) %>%
      dplyr::slice(which.max(label)) %>% # leaves only the maximum value
      #dplyr::select(-prop_survival) %>%
      #dplyr::bind_cols(prop.survival) %>%
      dplyr::mutate(label = paste(as.character(label), code), rel_wage = 95) %>%
      dplyr::select(-code)

    wage.boxplot <- pred.plot.salmon %>%
      dplyr::filter(scenario_type == eachtype) %>%
      dplyr::filter(rel_wage<= thiscutoff) %>%
      dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
      ggplot2::ggplot(ggplot2::aes(y = rel_wage, x = salmon_effect, fill = longname)) +
      ggplot2::geom_point(ggplot2::aes(color=longname), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(. ~ scenario_name, ncol = 2) + #, scales = "free_y"
      ggplot2::scale_fill_manual(values = salmon.colors, name = "Salmon group") +
      ggplot2::scale_color_manual(values = salmon.colors) +
      ggplot2::labs(title = paste0("Salmon weight-at-age in ", thistype, " scenarios"),
                    y = "Proportional change in returning adult weight-at-age (scenario/base)", x = "Expected salmon impact", face = "bold") +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position = "right") +
      ggplot2::guides(color="none", fill=ggplot2::guide_legend(ncol =1)) +
      ggplot2::ylim(-thiscutoff, thiscutoff)

    if(nrow(salmon.gen)>0) {

      wage.boxplot <- wage.boxplot +
        #  ggplot2::geom_text(data=salmon.gen, ggplot2::aes(salmon_effect, excess_mort, label = label))
        ggrepel::geom_text_repel(
          data          = salmon.gen,
          mapping       = ggplot2::aes(salmon_effect, rel_wage, label = label),
          force = 0.9,
          force_pull = 0.9,
          size          = 3.5,
          colour = "black"
        )

    }
    #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95))

thisplotname <- paste0("boxplot_wage_salmon_effect","_", thistype,".png")

if(thistype == "top-down") ggplot2::ggsave(thisplotname, plot = wage.boxplot, device = "png", width= 12, height = 9.5, scale = 1, dpi = 600)
if(thistype == "bottom-up") ggplot2::ggsave(thisplotname, plot = wage.boxplot, device = "png", width= 10, height = 11, scale = 1, dpi = 600)


  }

  return(wage.boxplot)

}
