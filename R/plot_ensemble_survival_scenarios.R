#' @title plot salmon survival of ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002



plot_ensemble_survival_scenarios <- function(ensemblenumbersagescenarios, salmongroups, plotmodels, base.survival, salmonbybasin, scenario.effect){

  salmon.max.nums <- ensemblenumbersagescenarios %>%
    dplyr::group_by(scenario_name, scenario_var, model_ver, Code, age, year_no) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by="Code") %>%
    dplyr::ungroup() %>%
  #  dplyr::filter(!model_ver%in% plotmodels) %>%
    dplyr::mutate(model_ver = as.double(model_ver)) %>%
    dplyr::mutate(scenario_name = tolower(scenario_name)) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="salmon competition","wild pink & chum salmon competition",
                                                 dplyr::if_else(scenario_name=="mammal predation","pinniped predation",
                                                                dplyr::if_else(scenario_name=="seabirds predation","seabird predation",
                                                                               dplyr::if_else(scenario_name=="gelatinous zooplankton increase","gelatinous zooplankton abundance",
                                                                                              dplyr::if_else(scenario_name=="herring decrease","herring abundance",scenario_name)))))) %>%
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
    dplyr::select("scenario_name","scenario_var","model_ver","Code","Long.Name","survival") %>%
    dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="Hatchery chinook competition", "Hatchery Chinook competition", scenario_name))


salmon.rel.survival <- base.survival %>%
    dplyr::select("scenario_name","model_ver","Code","Long.Name","survival") %>%
    dplyr::rename(base_survival = survival) %>%
    dplyr::left_join(salmon.return.nums.yr, by = c("scenario_name","model_ver","Code","Long.Name")) %>%
    dplyr::mutate(rel_survival = (((survival / base_survival)-1) * 100))

salmon.return.nums %>%
  dplyr::filter(year_no<=ret_year) %>%
  dplyr::filter(year_no==max(year_no)) %>%
  readr::write_csv(., here::here("modelfiles","survival_rates.csv"))


salmon.basin <- salmonbybasin %>%
  dplyr::select(Code, basin, geo_order, salmon_genus)

salmon.lollipop.data <- salmon.rel.survival %>%
  dplyr::mutate(longname = as.factor(Long.Name)) %>%
  dplyr::mutate(model_ver = as.factor(model_ver)) %>%
  dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
  dplyr::group_by(scenario_name,scenario_var,Code,longname) %>%
  dplyr::mutate(max_model = max(rel_survival)) %>%
  dplyr::mutate(min_model = min(rel_survival)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(longname = gsub("Subyearling", "SY", longname), longname = gsub("Yearling","Y", longname),
                longname = dplyr::if_else(longname=="Chum Hood Canal summer run SY", "Chum Hood Canal SY",
                                           dplyr::if_else(longname=="Strait of Georgia salmonids", "St. of Georgia salmonids", longname))) %>%
  dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition", "Gelatinous zooplankton abundance", "Herring abundance", "Pinniped predation",
                                                     "Porpoise predation", "Seabird predation" , "Spiny dogfish predation")) %>%
  dplyr::left_join(salmon.basin, by=c("Code")) %>%
  dplyr::left_join(scenario.effect, by = c("scenario_name","scenario_var"))

readr::write_csv(salmon.lollipop.data, here::here("modelfiles","ensemble_survival.csv"))

#bottom.up.sc <- c("Hatchery competition", "Wild pink and chum salmon competition", "Gelatinous zooplankton increase", "Herring decrease")
bottom.up.sc <- c("Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition", "Gelatinous zooplankton abundance", "Herring abundance")
top.down.sc <- c("Pinniped predation", "Porpoise predation", "Seabird predation" , "Spiny dogfish predation")

scenario.list <- list("Bottom up hypotheses" = bottom.up.sc, "Top down hypotheses" = top.down.sc)

for(eachscenario in 1:length(scenario.list)){

  thislist <- scenario.list[eachscenario]

  thesescenarios <- thislist %>% unlist()
  thisname <- names(thislist)

  col.pal <- c("#ffbe0b","#0a9396")
  col.fill <- c("#ffbe0b","#0a9396", "#3fd2c7", "#99ddff", "#00458b", "#549896", "#83bb90", "#f1dd88", "#f7a482", "#81aa2c", "#f293b1", "#ed5181")

  salmon.order <- salmon.lollipop.data %>%
    dplyr::distinct(longname,basin, geo_order, salmon_genus) %>%
    dplyr::arrange(salmon_genus, geo_order) %>%
    dplyr::pull(longname)

  plot.data <- salmon.lollipop.data %>%
    dplyr::filter(scenario_name %in% thesescenarios) %>%
    dplyr::filter(rel_survival < 50) %>%
    droplevels() %>%
    dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound", "Hood Canal")) %>%
    dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname),salmon.order)) %>%
    dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus),"Chinook","Chum","Coho","Pink","Sockeye"))



  # Calculate the number of pages with 12 panels per page

  n_pages <- plot.data %>%
    dplyr::distinct(longname) %>%
    nrow(.)/9
  pages.num <- ceiling(n_pages)

  #  col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), "qMSOSlp")

  max.violin <- plot.data %>%
    dplyr::pull(rel_survival) %>%
    max() %>%
    ceiling(.)

  min.violin <- plot.data %>%
    dplyr::pull(rel_survival) %>%
    min() %>%
    floor(.)

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
    ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value') +
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



  ggplot2::ggsave(paste0(thisname,"_",i,"_survival.png"),plot = range.plot, device = "png", width = 40, height = 35, units = "cm", dpi = 400)


   violin.plot <- plot.data %>%
      ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
      ggplot2::geom_violin(trim = FALSE, position=ggplot2::position_dodge(),draw_quantiles=c(0.5)) +
      ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y") +
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
     ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y") +
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
     ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = 'label_value', scales = "free_y") +
     ggplot2::scale_fill_manual(values = col.pal, name = "Change in key group abundance") +
     ggplot2::labs(title = thisname, y = "% change in survival rel. to base case", x = "Scenario", face = "bold") +
     ggthemes::theme_base() +
     ggplot2::theme(legend.position="bottom") +
     ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.95)) +
     ggplot2::ylim(min.violin, max.violin)


   ggplot2::ggsave(paste0(thisname,"_",i,"_boxplot_survival_scale.png"),plot = box.plot.scale, device = "png", width = 40, height = 35, units = "cm", dpi = 600)

}


  scenario.level <- plot.data %>%
    dplyr::distinct(scenario_var) %>%
    dplyr::pull(scenario_var) %>%
    as.character()

  salmon.impacts <- c("Positive impacts on salmon","Negative impacts on salmon")
  sc.multipliers <- c("Positive","Negative")


  for(eachscenariovar in 1:length(sc.multipliers)) {

    sl.impact <- salmon.impacts[eachscenariovar]
    this.multiplier <- sc.multipliers[eachscenariovar]


    range.change <- plot.data %>%
      dplyr::filter(Code %in% c("CMH","CNY","CNS")) %>%
      dplyr::filter(salmon_effect==this.multiplier) %>%
      dplyr::pull(rel_survival) %>%
      range() %>% as.character

    file.range <- file(paste(this.multiplier, eachscenario, "change_range.txt", sep="_"))

    cat(paste(this.multiplier,eachscenario,range.change), file=paste(this.multiplier, eachscenario, "change_range.txt", sep="_"), sep="\n", append = TRUE)


    col.fill <- c("#ffbe0b","#ed5181", "#3fd2c7", "#00458b", "#f7a482", "#83bb90", "#f1dd88", "#81aa2c", "#f293b1")

    these.rows <- ceiling(length(thesescenarios) / 2)

    text.label <- plot.data %>%
      dplyr::filter(salmon_effect==this.multiplier) %>%
      dplyr::distinct(scenario_name, scenario_var) %>%
      dplyr::mutate(longname = "Pink Salmon SY", basin = "Puget Sound")

    box.plot.scale.basin <- plot.data %>%
      dplyr::filter(salmon_effect==this.multiplier) %>%
      ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = longname, fill = basin)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(yintercept = 0) +
      ggforce::facet_wrap_paginate(. ~ scenario_name, ncol = 2, nrow = these.rows, page = 1, shrink = FALSE, labeller = 'label_value', scales = "free_y") +
      ggplot2::scale_fill_manual(values = col.fill, name = "Basin of origin") +
      ggplot2::labs(title = thisname, y = "% change in survival rel. to base case", x = "Functional group", face = "bold", subtitle = sl.impact) +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position="bottom") +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.95)) +
      ggplot2::ylim(min.violin, max.violin) +
      ggplot2::geom_text(
        data    = text.label,
        mapping =  ggplot2::aes(x = -Inf, y = -Inf, label = scenario_var),
        hjust   = -0.1,
        vjust   = -1
      )


  text.label <- plot.data %>%
      dplyr::filter(salmon_effect==this.multiplier) %>%
      dplyr::distinct(scenario_name, scenario_var)

    ggplot2::ggsave(paste0(this.multiplier, "_,", thisname,"_",i,"_boxplot_survival_basin_scale_.png"), plot = box.plot.scale.basin, device = "png", width = 40, height = 35, units = "cm", dpi = 600)

  }





}
}


