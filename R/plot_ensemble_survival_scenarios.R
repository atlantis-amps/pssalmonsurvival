#' @title plot salmon survival of ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002



plot_ensemble_survival_scenarios <- function(ensemblenumbersagescenarios, salmongroups, plotmodels, base.survival){

  salmon.max.nums <- ensemblenumbersagescenarios %>%
    dplyr::group_by(scenario_name, scenario_var, model_ver, Code, age, year_no) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by="Code") %>%
    dplyr::ungroup() %>%
  #  dplyr::filter(!model_ver%in% plotmodels) %>%
    dplyr::mutate(model_ver = as.double(model_ver)) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="salmon competition","wild pink and chum salmon competition",
                                                 dplyr::if_else(scenario_name=="mammal predation","pinniped predation",
                                                                dplyr::if_else(scenario_name=="seabirds predation","seabird predation", scenario_name)))) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var=="0_8", "-20%","+20%"))

  salmon.juv.nums <- salmon.max.nums %>%
    dplyr::filter(age == 1) %>%
    dplyr::rename(juv_nums = max_nums)

  salmon.return.nums <- salmon.max.nums %>%
    dplyr::filter(age == (years_away + 1)) %>%
    dplyr::mutate(cohort_yr = year_no - age) %>%
    dplyr::select(-years_away) %>%
    dplyr::rename(age_return = age, return_nums=max_nums, year_sim = year_no, year_no= cohort_yr) %>%
    dplyr::left_join(salmon.juv.nums, by=c("scenario_name","scenario_var","migiobox","model_ver","Code","year_no","Long.Name","NumCohorts","Name")) %>%
    dplyr::mutate(survival = return_nums/juv_nums) %>%
    dplyr::mutate(Year = year_sim - 2010) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(max_year = max(year_no)) %>%
    dplyr::mutate(ret_year = max_year-3)

  salmon.return.nums.yr <- salmon.return.nums %>%
    dplyr::filter(year_no<=ret_year) %>%
    dplyr::filter(year_no==max(year_no)) %>%
    dplyr::select("scenario_name","scenario_var","model_ver","Code","Long.Name","survival")

  readr::write_csv(salmon.return.nums.yr, "survival_rates.csv")

salmon.rel.survival <- base.survival %>%
    dplyr::select("scenario_name","model_ver","Code","Long.Name","survival") %>%
    dplyr::rename(base_survival = survival) %>%
    dplyr::left_join(salmon.return.nums.yr, by = c("scenario_name","model_ver","Code","Long.Name")) %>%
    dplyr::mutate(rel_survival = (((survival / base_survival)-1) * 100))

salmon.lollipop.data <- salmon.rel.survival %>%
  dplyr::mutate(long_name = as.factor(Long.Name)) %>%
  dplyr::mutate(model_ver = as.factor(model_ver)) %>%
  dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
  dplyr::group_by(scenario_name,scenario_var,Code,Long.Name) %>%
  dplyr::mutate(max_model = max(rel_survival)) %>%
  dplyr::mutate(min_model = min(rel_survival)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(long_name = gsub("Subyearling", "SY", long_name), long_name = gsub("Yearling","Y", long_name),
                long_name = dplyr::if_else(long_name=="Chum Hood Canal summer run SY", "Chum Hood Canal SY",
                                           dplyr::if_else(long_name=="Strait of Georgia salmonids", "St. of Georgia salmonids", long_name))) %>%
  dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
  dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Hatchery Chinook competition", "Hatchery competition", "Wild pink and chum salmon competition", "Gelatinous zooplankton increase", "Herring decrease", "Pinniped predation",
                                                    "Porpoise predation", "Seabird predation" , "Spiny dogfish predation"))


#bottom.up.sc <- c("Hatchery competition", "Wild pink and chum salmon competition", "Gelatinous zooplankton increase", "Herring decrease")
bottom.up.sc <- c("Hatchery Chinook competition", "Hatchery competition", "Wild pink and chum salmon competition", "Gelatinous zooplankton increase", "Herring decrease")
top.down.sc <- c("Pinniped predation", "Porpoise predation", "Seabird predation" , "Spiny dogfish predation")

scenario.list <- list("Bottom up hypotheses" = bottom.up.sc, "Top down hypotheses" = top.down.sc)

for(eachscenario in 1:length(scenario.list)){

  thislist <- scenario.list[eachscenario]

  thesescenarios <- thislist %>% unlist()
  thisname <- names(thislist)

  col.pal <- c("#ffbe0b","#0a9396")
  col.fill <- c("#ffbe0b","#0a9396", "#3fd2c7", "#99ddff", "#00458b", "#549896", "#83bb90", "#f1dd88", "#f7a482", "#81aa2c", "#f293b1", "#ed5181")


  plot.data <- salmon.lollipop.data %>%
    dplyr::filter(scenario_name %in% thesescenarios) %>%
    dplyr::filter(rel_survival < 50) %>%
    droplevels()

  # Calculate the number of pages with 12 panels per page
  n_pages <- plot.data %>%
    dplyr::distinct(long_name) %>%
    nrow(.)/9
  pages.num <- ceiling(n_pages)

  #  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")

  max.violin <- plot.data %>%
    dplyr::pull(rel_survival) %>%
    max() %>%
    ceiling()

  min.violin <- plot.data %>%
    dplyr::pull(rel_survival) %>%
    min() %>%
    floor()

  for (i in seq_len(pages.num)) {


    range.plot <-  plot.data %>%
    ggplot2::ggplot() +
    ggplot2::geom_segment(ggplot2::aes(x=min_model, xend=max_model, y=scenario_name, yend=scenario_name, color = scenario_var), size = 5, alpha = 0.1) +
  #  ggplot2::scale_color_manual(values = col.fill, name = "Change in key group abundance") +
   # ggplot2::geom_point(ggplot2::aes(y = scenario_name, x = rel_survival, fill = model_ver, color = model_ver, shape = scenario_var)) +
    ggplot2::geom_jitter(ggplot2::aes(y = scenario_name, x = rel_survival, shape = scenario_var, fill = model_ver), width = 0.25, height = 0.25, size = 1.5) +
    ggplot2::scale_shape_manual(values = c(21,24), name = "Change in key group abundance") +
    ggplot2::scale_fill_manual(values = col.fill, guide="none") +
    ggplot2::scale_color_manual(values = col.fill, guide="none") +
#    ggsci::scale_fill_d3("category20", name = "Model version") +
    #ggplot2::facet_wrap(ggplot2::vars(long_name), scales = "free_y") +
    ggforce::facet_wrap_paginate(. ~ long_name, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value') +
    ggplot2::coord_flip() +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::xlab("% change in survival rel. to base case") +
    ggplot2::ylab("Scenario") +
    ggplot2::labs(title = thisname) +
    ggthemes::theme_base() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5)) +
    ggplot2::guides(fill = "none") +
    ggplot2::xlim(min.violin, max.violin)


    # if(thisname == "Top down hypotheses"){
    #
    #   # A data frame with labels for each facet
    #   f_labels <- salmon.lollipop.data %>%
    #     dplyr::filter(rel_survival < 50) %>%
    #     droplevels() %>%
    #     dplyr::distinct(long_name) %>%
    #     data.frame(long_name =., label = c("", "", "", "", "", "", "", "","", "", "","", "", "","", "", "", "Green is \n scenario overlap", "Points are \n model variants", ""))
    #
    #   range.plot.txt <- range.plot +
    #     ggplot2::geom_text(x = -2, y = 2.5, ggplot2::aes(label = label), data = f_labels)
    #
    # }
    #
    #
    # if(thisname == "Bottom up hypotheses") {
    #
    #   f_labels <- salmon.lollipop.data %>%
    #     dplyr::filter(rel_survival < 50) %>%
    #     droplevels() %>%
    #     dplyr::distinct(long_name) %>%
    #     data.frame(long_name =., label = c("", "", "","", "", "", "", "","", "", "","", "", "","", "", "", "Green is \n scenario overlap", "Points are \n model variants", ""))
    #
    #   range.plot.txt <-range.plot +
    #     ggplot2::geom_text(x = -30, y = 2, ggplot2::aes(label = label), data = f_labels)
    #
    #
    # }


  ggplot2::ggsave(paste0(thisname,"_",i,"_survival.png"),plot = range.plot, device = "png", width = 40, height = 35, units = "cm", dpi = 400)


   violin.plot <- plot.data %>%
      ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
      ggplot2::geom_violin(trim = FALSE, position=ggplot2::position_dodge(),draw_quantiles=c(0.5)) +
      ggforce::facet_wrap_paginate(. ~ long_name, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y") +
      ggplot2::geom_boxplot(ggplot2::aes(fill = scenario_var), width=0.1, color="black",position = ggplot2::position_dodge(width =0.9)) +
      ggplot2::scale_fill_manual(values = col.pal, name = "Change in key group abundance") +
      ggplot2::ylab("% change in survival rel. to base case") +
      ggplot2::xlab("Scenario") +
      ggplot2::labs(title = thisname) +
      ggplot2::geom_hline(yintercept = 0) +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position="bottom") +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5)) +
      ggplot2::ylim(min.violin, max.violin)


   ggplot2::ggsave(paste0(thisname,"_",i,"_violin_survival.png"),plot = violin.plot, device = "png", width = 40, height = 35, units = "cm", dpi = 400)


   violin.plot.scale <- plot.data %>%
     ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
     ggplot2::geom_violin(trim = FALSE, position= ggplot2::position_dodge(),draw_quantiles=c(0.5)) +
     ggplot2::geom_hline(yintercept = 0) +
     ggforce::facet_wrap_paginate(. ~ long_name, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y") +
     ggplot2::geom_boxplot(ggplot2::aes(fill = scenario_var), width=0.1, color="black", position =  ggplot2::position_dodge(width =0.9)) +
     ggplot2::scale_fill_manual(values = col.pal, name = "Change in key group abundance") +
     ggplot2::ylab("% change in survival rel. to base case") +
     ggplot2::xlab("Scenario") +
     ggplot2::labs(title = thisname) +
     ggthemes::theme_base() +
     ggplot2::theme(legend.position="bottom") +
     ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5)) #+
   # ylim(min.violin, max.violin)


   ggplot2::ggsave(paste0(thisname,"_",i,"_violin_survival_scale.png"),plot = violin.plot.scale, device = "png", width = 40, height = 35, units = "cm", dpi = 400)


   box.plot.scale <- plot.data %>%
     ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
     ggplot2::geom_boxplot() +
     ggplot2::geom_hline(yintercept = 0) +
     ggforce::facet_wrap_paginate(. ~ long_name, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y") +
     ggplot2::scale_fill_manual(values = col.pal, name = "Change in key group abundance") +
     ggplot2::ylab("% change in survival rel. to base case") +
     ggplot2::xlab("Scenario") +
     ggplot2::labs(title = thisname) +
     ggthemes::theme_base() +
     ggplot2::theme(legend.position="bottom") +
     ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5)) +
     ggplot2::ylim(min.violin, max.violin)


   ggplot2::ggsave(paste0(thisname,"_",i,"_boxplot_survival_scale.png"),plot = box.plot.scale, device = "png", width = 40, height = 35, units = "cm", dpi = 400)

}


}
}


# salmon.rel.survival %>%
#   dplyr::mutate(long_name = as.factor(Long.Name)) %>%
#   dplyr::mutate(model_ver = as.factor(model_ver)) %>%
#   dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
#   dplyr::filter(scenario_name == "gelatinous zooplankton increase") %>%
#   ggplot2::ggplot(ggplot2::aes(x = scenario_var, y = rel_survival, colour=model_ver))+
#   ggplot2::geom_jitter(width = 0.25, height = 0.25) +
#   ggplot2::facet_wrap(vars(long_name), scales = "free_y")
#
# salmon.rel.survival %>%
#   dplyr::mutate(long_name = as.factor(Long.Name)) %>%
#   dplyr::mutate(model_ver = as.factor(model_ver)) %>%
#   dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
#   dplyr::filter(scenario_name == "gelatinous zooplankton increase") %>%
#   ggplot2::ggplot(ggplot2::aes(x = scenario_var, y = rel_survival, colour=model_ver))+
#   ggplot2::geom_jitter(width = 0.25, height = 0.25) +
#   ggplot2::facet_wrap(vars(long_name), scales = "free_y")
#
# salmon.rel.survival %>%
#   dplyr::mutate(long_name = as.factor(Long.Name)) %>%
#   dplyr::mutate(model_ver = as.factor(model_ver)) %>%
#   dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
#   dplyr::filter(scenario_name == "gelatinous zooplankton increase") %>%
#   ggblanket::gg_jitter(
#   x = scenario_var,
#   y = rel_survival,
#   col = model_ver,
#   col_intervals = ~model_ver::chop_quantiles(.x, probs = seq(0, 1, 0.25)),
#   position = ggplot2::position_jitter(width = 0.25, height = 0.25, seed = 123),
#   y_zero = TRUE,
#   facet = long_name,
#   facet_scales = "free_y")
#
# salmon.lollipop.data <- salmon.rel.survival %>%
#   dplyr::mutate(long_name = as.factor(Long.Name)) %>%
#   dplyr::mutate(model_ver = as.factor(model_ver)) %>%
#   dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
#   dplyr::filter(scenario_name == "gelatinous zooplankton increase") %>%
#   dplyr::group_by(scenario_name,scenario_var,Code,Long.Name) %>%
#   dplyr::mutate(max_model = max(rel_survival)) %>%
#   dplyr::mutate(min_model = min(rel_survival)) %>%
#   dplyr::ungroup()
#
# ggplot2::ggplot(salmon.lollipop.data) +
#   ggplot2::geom_segment(ggplot2::aes(x=min_model, xend=max_model, y=scenario_var, yend=scenario_var, color = scenario_var), size = 5, alpha = 0.2) +
#   ggplot2::geom_point(ggplot2::aes(x=min_model, y=scenario_var), size=3) +
#   ggplot2::geom_point(ggplot2::aes(x=max_model, y=scenario_var), size=3) +
#   ggplot2::facet_wrap(ggplot2::vars(long_name), scales = "free_y") +
#   ggplot2::geom_vline(xintercept = 1) +
#   ggplot2::xlab("Scenario") +
#   ggplot2::ylab("Relative survival")+
#   ggplot2::coord_flip()
# ggtheme::theme_pander() +

#   # Calculate the number of pages with 12 panels per page
#   n_pages <- ceiling(
#     nrow(salmongroups)/ 12
#   )
#
# #  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")
#
#   print(n_pages)
#
#   plot.list <- list()
#
#   for (i in seq_len(n_pages)) {
#
#
#     plot.list[[i]] <- survival.plot
#
#     thisplotname <- paste0("salmon_survival_plot_",i,".png")
#
#     ggplot2::ggsave(thisplotname,plot = survival.plot, device = "png", width = 21, height = 24, units = "cm")
#
#   }
#
#
#   return(plot.list)

