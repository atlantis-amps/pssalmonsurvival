#' @title plot salmon survival of ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2022



plot_ensemble_survival_scenarios <- function(ensemblenumbersagescenarios, salmongroups, plotmodels, base.survival, salmonbybasin, indsalmoneffect) {

  salmon.max.nums <- ensemblenumbersagescenarios %>%
    dplyr::group_by(scenario_name, scenario_var, model_ver, Code, age, year_no) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by = "Code") %>%
    dplyr::ungroup() %>%
    # dplyr::filter(!model_ver%in% plotmodels) %>%
    dplyr::mutate(model_ver = as.double(model_ver)) %>%
    dplyr::mutate(scenario_name = tolower(scenario_name)) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "salmon competition", "wild pink & chum salmon competition", dplyr::if_else(scenario_name ==
                                                                                                                                                "mammal predation", "pinniped predation", dplyr::if_else(scenario_name == "seabirds predation", "seabird predation", dplyr::if_else(scenario_name ==
                                                                                                                                                                                                                                                                                      "gelatinous zooplankton increase", "gelatinous zooplankton abundance", dplyr::if_else(scenario_name == "herring decrease", "herring abundance", scenario_name)))))) %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var == "0_8", "-20%", "+20%"))

  salmon.juv.nums <- salmon.max.nums %>%
    dplyr::filter(age == 1) %>%
    dplyr::rename(juv_nums = max_nums)

  salmon.return.nums <- salmon.max.nums %>%
    dplyr::filter(age == (years_away + 1)) %>%
    dplyr::mutate(cohort_yr = year_no - age) %>%
    dplyr::select(-years_away) %>%
    dplyr::rename(age_return = age, return_nums = max_nums, year_sim = year_no, year_no = cohort_yr) %>%
    dplyr::left_join(salmon.juv.nums, by = c("scenario_name", "scenario_var", "migiobox", "model_ver", "Code", "year_no", "Long.Name", "NumCohorts", "Name")) %>%
    dplyr::mutate(survival = (return_nums/juv_nums) * 100) %>%
    dplyr::mutate(survival = dplyr::if_else(survival > 100, 100, survival)) %>%
    dplyr::mutate(Year = year_sim - 2010) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(max_year = max(year_no)) %>%
    dplyr::mutate(ret_year = max_year - 3)


  salmon.return.nums.yr <- salmon.return.nums %>%
    dplyr::filter(year_no <= ret_year) %>%
    dplyr::filter(year_no == max(year_no)) %>%
    dplyr::select(scenario_name, scenario_var, model_ver, Code, Long.Name, survival, juv_nums, return_nums) %>%
    dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name == "Hatchery chinook competition", "Hatchery Chinook competition", scenario_name))

  salmon.rel.survival <- base.survival %>%
    dplyr::select(scenario_name, model_ver, Code, Long.Name, survival, juv_nums, return_nums) %>%
    dplyr::rename(base_survival = survival) %>%
    dplyr::left_join(salmon.return.nums.yr, by = c("scenario_name", "model_ver", "Code", "Long.Name")) %>%
    dplyr::mutate(rel_survival = (survival - base_survival)) %>%
    dplyr::mutate(prop_survival = ((survival / base_survival)-1) *100) #make it proportional

  salmon.return.nums %>%
    dplyr::filter(year_no <= ret_year) %>%
    dplyr::filter(year_no == max(year_no)) %>%
    readr::write_csv(., here::here("modelfiles", "survival_rates.csv"))

  salmon.basin <- salmonbybasin %>%
    dplyr::select(Code, basin, geo_order, salmon_genus)

  salmon.return.nums %>%
    dplyr::filter(year_no <= ret_year) %>%
    dplyr::filter(year_no == max(year_no)) %>%
    readr::write_csv(., here::here("modelfiles", "survival_rates.csv"))

  salmon.basin.data <- salmon.rel.survival %>%
    dplyr::mutate(model_ver = as.factor(model_ver)) %>%
    dplyr::rename(longname = Long.Name) %>%
    dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
    dplyr::group_by(scenario_name, scenario_var, Code, longname) %>%
    dplyr::mutate(max_model = max(rel_survival)) %>%
    dplyr::mutate(min_model = min(rel_survival)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(longname = gsub("Subyearling", "SY", longname), longname = gsub("Yearling", "Y", longname), longname = dplyr::if_else(longname == "Chum Hood Canal summer run SY",
                                                                                                                                        "Chum Hood Canal SY", dplyr::if_else(longname == "Strait of Georgia salmonids", "St. of Georgia salmonids", longname))) %>%
    dplyr::mutate(scenario_name = forcats::fct_relevel(as.factor(scenario_name), "Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition",
                                                       "Gelatinous zooplankton abundance", "Herring abundance", "Pinniped predation", "Porpoise predation", "Seabird predation", "Spiny dogfish predation")) %>%
    dplyr::left_join(salmon.basin, by = c("Code")) %>%
    dplyr::left_join(indsalmoneffect, by = c("scenario_name", "scenario_var"))

  salmon.names <- salmon.basin.data %>%
    dplyr::distinct(salmon_genus, longname) %>%
    dplyr::arrange(salmon_genus, longname) %>%
    dplyr::pull(longname)

  salmon.lollipop.data <- salmon.basin.data %>%
    dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.names))


  readr::write_csv(salmon.lollipop.data, here::here("modelfiles", "ensemble_survival.csv"))

  # bottom.up.sc <- c('Hatchery competition', 'Wild pink and chum salmon competition', 'Gelatinous zooplankton increase', 'Herring decrease')
  bottom.up.sc <- c("Hatchery Chinook competition", "Hatchery competition", "Wild pink & chum salmon competition", "Gelatinous zooplankton abundance", "Herring abundance")
  top.down.sc <- c("Pinniped predation", "Porpoise predation", "Seabird predation", "Spiny dogfish predation")

  scenario.list <- list(`Bottom-up drivers` = bottom.up.sc, `Top-down drivers` = top.down.sc)

  print(scenario.list)

  salmon.order <- salmonbybasin %>%
    # dplyr::filter(salmon_genus != 'Chum')
    dplyr::arrange(salmon_genus, geo_order, longname) %>%
    dplyr::pull(longname)

 salmon.plot.data <- salmon.lollipop.data %>%
    dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound",
                                               "Hood Canal")) %>%
    dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye")) %>%
    dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order)) %>%
    dplyr::mutate(scenario_driver = dplyr::if_else(scenario_name %in% bottom.up.sc, "Bottom-up drivers", "Top-down drivers")) %>%
    dplyr::mutate(scenario_driver = forcats::fct_relevel(as.factor(scenario_driver), "Bottom-up drivers", "Top-down drivers"))


aggregated.survival <- salmon.lollipop.data %>%
  dplyr::group_by(basin, scenario_name, salmon_effect, salmon_genus) %>%
  dplyr::summarise(tot_base_juv = sum(juv_nums.x), tot_base_ret= sum(return_nums.x) , tot_sc_juv = sum(juv_nums.y), tot_sc_ret=sum(return_nums.y)) %>%
  dplyr::mutate(tot_base_survival = (tot_base_ret/tot_base_juv) * 100) %>%
  dplyr::mutate(tot_sc_survival = (tot_sc_ret/tot_sc_juv) * 100) %>%
  dplyr::mutate(tot_sc_survival = dplyr::if_else(tot_sc_survival > 100, 100, tot_sc_survival)) %>%
  dplyr::mutate(tot_base_survival = dplyr::if_else(tot_base_survival > 100, 100, tot_base_survival)) %>%
  dplyr::mutate(rel_survival = (tot_sc_survival - tot_base_survival)) %>%
  dplyr::mutate(prop_survival = ((tot_sc_survival / tot_base_survival)-1) *100) %>% #make it proportional
  dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye")) %>%
  dplyr::mutate(scenario_driver = dplyr::if_else(scenario_name %in% bottom.up.sc, "Bottom-up drivers", "Top-down drivers")) %>%
  dplyr::mutate(scenario_driver = forcats::fct_relevel(as.factor(scenario_driver), "Bottom-up drivers", "Top-down drivers")) %>%
  dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound",
                                             "Hood Canal"))


 basin.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
                 `Hood Canal` = "#032F5C")


 salmon.eff.basin <- aggregated.survival %>%
   dplyr::ungroup() %>%
   dplyr::filter(prop_survival >= 25) %>%
   dplyr::arrange(salmon_effect, salmon_genus) %>%
   dplyr::mutate(label = round(prop_survival,0))%>%
   dplyr::group_by(salmon_genus, scenario_driver, salmon_effect) %>%
   dplyr::slice(which.max(label)) %>% # leaves only the maximum value
   #dplyr::select(-prop_survival) %>%
   #dplyr::bind_cols(prop.survival) %>%
   #dplyr::left_join(salmon.codes, by = "longname") %>%
  # dplyr::mutate(basin_code = dplyr::if_else(basin == "South Puget Sound","SPS",
  #                                           "HC")) %>%
   dplyr::mutate(label = paste(as.character(label), salmon_genus), prop_survival = 24)

 genus.fill.all <- paletteer::paletteer_d("ggthemes::stata_economist",12)
 genus.fill <- genus.fill.all[c(5,10,8,6,9,11)]


 box.plot.scale.basin <- aggregated.survival %>%
    ggplot2::ggplot(ggplot2::aes(y = prop_survival, x = salmon_effect, fill = salmon_genus)) +
   ggplot2::geom_boxplot(outlier.shape = NA) +
   ggplot2::geom_point(ggplot2::aes(color=salmon_genus), position = ggplot2::position_jitterdodge(), alpha=0.5) +
   ggplot2::geom_hline(yintercept = 0) +
   ggplot2::facet_wrap(. ~ scenario_driver, scales = "free_y") +
 #  ggplot2::facet_grid(ggplot2::vars(salmon_effect), ggplot2::vars(scenario_driver)) +
   ggplot2::labs(title = "Salmon survival in bottom-up and top-down scenarios",
                 y = "Proportional change in survival (scenario/base)", x = "Expected impact on salmon", face = "bold") +
   ggthemes::theme_base() +
   ggplot2::theme(legend.position = "bottom") +
   #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.95, hjust = 0.95)) +
   ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
   ggplot2::ylim(-25, 25) +
   ggplot2::scale_fill_manual(values = genus.fill, name = "Salmon genus") +
   ggplot2::scale_color_manual(values = genus.fill) +
   #  ggplot2::geom_text(data = salmon.fg.text, label=salmon.fg.text$label, size = 3.5) +
   ggrepel::geom_text_repel(
     data          = salmon.eff.basin,
     mapping       = ggplot2::aes(salmon_effect, prop_survival, label = label),
     force = 0.7,
     force_pull = 0.7,
     size          = 4,
     colour        = "black",
     max.overlaps = 30,
     min.segment.length = 0
   ) +
   ggplot2::guides(color="none")

 box.plot.scale.basin.title <- gridExtra::grid.arrange(box.plot.scale.basin, right='Expected salmon impact')

 ggplot2::ggsave("boxplot_survival_aggregated.png", plot = box.plot.scale.basin, device = "png", width= 9, height = 7, scale = 1, dpi = 600)



  for (eachscenario in 1:length(scenario.list)) {

    thislist <- scenario.list[eachscenario]

    print(thislist)

    thesescenarios <- thislist %>%
      unlist()
    thisname <- names(thislist)

    these.rows <- ceiling(length(thesescenarios)/2)


    salmon.codes <- salmon.lollipop.data %>%
      dplyr::distinct(longname,Code)

    plot.data <- salmon.lollipop.data %>%
      dplyr::filter(scenario_name %in% thesescenarios) %>%
      droplevels() %>%
      dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound",
                                                 "Hood Canal")) %>%
      dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus), "Chinook", "Chum", "Coho", "Pink", "Sockeye")) %>%
      dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname), salmon.order))

    salmon.fg.fill <- salmon_colors()

    if(thisname == "Top-down drivers") {
      thiscuttoff <- 20
     thistitle <- "Salmon survival in top-down driver scenarios"
  }

    if(thisname == "Bottom-up drivers") {
      thiscuttoff <- 100
      thistitle <- "Salmon survival in bottom-up driver scenarios"
    }

    salmon.eff.text <- plot.data %>%
      dplyr::filter(prop_survival >= thiscuttoff) %>%
      dplyr::arrange(salmon_effect, longname) %>%
      dplyr::select(scenario_name, salmon_effect, longname, prop_survival) %>%
      dplyr::mutate(label = round(prop_survival,0))%>%
      dplyr::group_by(scenario_name, salmon_effect, longname) %>%
      dplyr::slice(which.max(label)) %>% # leaves only the maximum value
      #dplyr::select(-prop_survival) %>%
      #dplyr::bind_cols(prop.survival) %>%
      dplyr::left_join(salmon.codes, by = "longname") %>%
      dplyr::mutate(label = paste(as.character(label), Code), prop_survival = 95) %>%
      dplyr::select(-Code)

    #eliminate forced groups from plots
    #
    plot.data.wild <- plot.data %>%
      dplyr::filter(scenario_name=="Wild pink & chum salmon competition") %>%
      dplyr::filter(!Code %in% c("CMF","PIS"))

    plot.data.wild <- plot.data %>%
      dplyr::filter(scenario_name=="Wild pink & chum salmon competition") %>%
      dplyr::filter(!Code %in% c("CMF","PIS"))

    box.plot.effect <- plot.data %>%
      dplyr::filter(prop_survival <= thiscuttoff) %>%
      ggplot2::ggplot(ggplot2::aes(y = prop_survival, x = salmon_effect, fill = longname)) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_point(ggplot2::aes(color=longname), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(. ~ scenario_name, ncol = 2, nrow = these.rows, scales = "free_y") +
      ggplot2::labs(title = thistitle, y = "Proportional change in survival (scenario/base)", x = "Expected salmon impact", face = "bold") +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position = "right") +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 16)) +
      ggplot2::ylim(-thiscuttoff,+thiscuttoff) +
      ggplot2::scale_fill_manual(values = salmon.fg.fill, name = "Salmon group") +
      ggplot2::scale_color_manual(values = salmon.fg.fill) +
      #  ggplot2::geom_text(data = salmon.fg.text, label=salmon.fg.text$label, size = 3.5) +
      ggrepel::geom_text_repel(
        data          = salmon.eff.text,
        mapping       = ggplot2::aes(salmon_effect, prop_survival, label = label),
        force = 0.7,
        force_pull = 0.7,
        size          = 3,
        colour        = "black"
      ) +
      ggplot2::guides(color="none") +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol =1))
    #    ggplot2::theme(legend.position = "none") +

    # text.label <- plot.data %>% # dplyr::filter(salmon_effect==this.multiplier) %>% dplyr::distinct(scenario_name, scenario_var) %>%
    # dplyr::mutate(longname = 'Pink Salmon SY', basin = 'Puget Sound') ggplot2::geom_text( data = text.label, mapping = ggplot2::aes(x = -Inf, y = -Inf,
    # label = scenario_var), hjust = -0.5, vjust = -12 )http://13.85.57.116:8787/graphics/plot_zoom_png?width=1005&height=663

    if(thisname == "Top-down drivers") {

      ggplot2::ggsave(paste0(thisname, "_boxplot_survival_effect_fg.png"), plot = box.plot.effect, device = "png", width= 11, height = 8.3, scale = 1, dpi = 600)

    }

    if(thisname == "Bottom-up drivers") {

      ggplot2::ggsave(paste0(thisname, "_boxplot_survival_effect_fg.png"), plot = box.plot.effect, device = "png", width= 11.5, height = 11.3, scale = 1, dpi = 600)

    }

   #proportional survival plot by salmon effect and genus
    genus.fill.all <- paletteer::paletteer_d("ggthemes::stata_economist",12)
    genus.fill <- genus.fill.all[c(5,10,8,6,9)]

    salmon.gen <- plot.data %>%
      dplyr::filter(prop_survival >= thiscuttoff) %>%
      dplyr::arrange(salmon_genus, longname) %>%
      dplyr::select(scenario_name, salmon_effect, salmon_genus, prop_survival) %>%
      dplyr::mutate(label = round(prop_survival,0))%>%
      dplyr::group_by(scenario_name, salmon_effect, salmon_genus) %>%
      dplyr::slice(which.max(label)) %>% # leaves only the maximum value
      #dplyr::select(-prop_survival) %>%
      #dplyr::bind_cols(prop.survival) %>%
      dplyr::mutate(label = paste(as.character(label), salmon_genus), prop_survival = 95)

    box.plot.effect.genus <- plot.data %>%
      dplyr::filter(prop_survival <= thiscuttoff) %>%
      ggplot2::ggplot(ggplot2::aes(y = prop_survival, x = salmon_effect, color = salmon_genus)) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_point(ggplot2::aes(color=salmon_genus), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(. ~ scenario_name, ncol = 2, nrow = these.rows, scales = "free_y") +
      ggplot2::labs(title = thistitle, y = "Proportional change in survival (scenario/base)", x = "Expected salmon impact", face = "bold") + #subtitle = this.text,
      ggthemes::theme_base() +
      ggplot2::theme(legend.position = "bottom", legend.justification = "center") +
      # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.95, hjust = 0.5))+
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
      ggplot2::ylim(-thiscuttoff,+thiscuttoff) +
      ggplot2::scale_color_manual(values = genus.fill, name = "Salmon genus") +
      ggrepel::geom_text_repel(
        data          = salmon.gen,
        mapping       = ggplot2::aes(salmon_effect, prop_survival, label = label),
        force = 0.9,
        force_pull = 1,
        size          = 3.5,
        colour = "black"
      )
    #    ggplot2::theme(legend.position = "none") +

    if(thisname == "Top-down drivers") {
      ggplot2::ggsave(paste0(thisname, "_boxplot_survival_effect_genus.png"), plot = box.plot.effect.genus, device = "png", width= 8.8, height = 7.5, scale = 1, dpi = 600)

    }

    if(thisname == "Bottom-up drivers") {
      ggplot2::ggsave(paste0(thisname, "_boxplot_survival_effect_genus.png"), plot = box.plot.effect.genus, device = "png", width= 9.15, height = 11, scale = 1, dpi = 600)

    }


    # Calculate the number of pages with 12 panels per page

    n_pages <- plot.data %>%
      dplyr::distinct(longname) %>%
      nrow(.)/9
    pages.num <- ceiling(n_pages)

    # col.pal <- Redmonder::redmonder.pal(length(levels(salmon.return.nums$model_ver)), 'qMSOSlp')

    max.violin <- plot.data %>%
      dplyr::pull(rel_survival) %>%
      max() %>%
      ceiling(.)

    min.violin <- plot.data %>%
      dplyr::pull(rel_survival) %>%
      min() %>%
      floor(.)


    col.fill <- paletteer::paletteer_dynamic("cartography::turquoise.pal",8)

    for (i in seq_len(pages.num)) {

      print("creating range plot")

      range.plot <- plot.data %>%
        ggplot2::ggplot() + ggplot2::geom_segment(ggplot2::aes(x = min_model, xend = max_model, y = scenario_name, yend = scenario_name, color = scenario_var),
                                                  size = 5, alpha = 0.1) + #  ggplot2::scale_color_manual(values = col.fill, name = 'Change in key group abundance') +  ggplot2::geom_point(ggplot2::aes(y = scenario_name, x = rel_survival, fill = model_ver, color = model_ver, shape = scenario_var)) + size
        ggplot2::geom_jitter(ggplot2::aes(y = scenario_name, x = rel_survival, shape = scenario_var, fill = model_ver), width = 0.25, height = 0.25, size = 1.5) +
        ggplot2::scale_shape_manual(values = c(21, 24), name = "Change in key group abundance") +
        ggplot2::scale_fill_manual(values = col.fill, guide = "none") +
        ggplot2::scale_color_manual(values = col.fill, guide = "none") + #    ggsci::scale_fill_d3('category20', name = 'Model version') + ggplot2::facet_wrap(ggplot2::vars(long_name), scales = 'free_y') + ggplot2::scale_color_manual(values
        ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = "label_value") + ggplot2::coord_flip() + ggplot2::geom_vline(xintercept = 0) +
        ggplot2::xlab("% change in survival (scenario-base)") +
        ggplot2::ylab("Scenario") +
        ggplot2::labs(title = thisname) +
        ggthemes::theme_base() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
        ggplot2::guides(fill = "none") +
        ggplot2::xlim(min.violin, max.violin)



      ggplot2::ggsave(paste0(thisname, "_", i, "_survival.png"), plot = range.plot, device = "png", width= 14.75, height = 13.58, scale = 1, dpi = 600)

      print("creating violin plot")

      violin.plot <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
        ggplot2::geom_violin(trim = FALSE, position = ggplot2::position_dodge(),
                             draw_quantiles = c(0.5)) +
        ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = "label_value", scales = "free_y") +
        ggplot2::geom_boxplot(ggplot2::aes(fill = scenario_var), width = 0.1, color = "black", position = ggplot2::position_dodge(width = 0.9)) +
        ggplot2::scale_fill_manual(values = col.fill, name = "Change in key group abundance") +
        ggplot2::ylab("% change in survival (scenario-base)") + ggplot2::xlab("Scenario") +
        ggplot2::labs(title = thisname) +
        ggplot2::geom_hline(yintercept = 0) +
        ggthemes::theme_base() +
        ggplot2::theme(legend.position = "bottom") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
        ggplot2::ylim(min.violin, max.violin)


      ggplot2::ggsave(paste0(thisname, "_", i, "_violin_survival.png"), plot = violin.plot, device = "png", width= 11.55, height = 13.58, scale = 1, dpi = 600)

      print("creating violin plot scale")

      violin.plot.scale <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
        ggplot2::geom_violin(trim = FALSE, position = ggplot2::position_dodge(),
                             draw_quantiles = c(0.5)) +
        ggplot2::geom_hline(yintercept = 0) +
        ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE,
                                     labeller = "label_value", scales = "free_y") +
        ggplot2::geom_boxplot(ggplot2::aes(fill = scenario_var), width = 0.1, color = "black", position = ggplot2::position_dodge(width = 0.9)) +
        ggplot2::scale_fill_manual(values = col.fill, name = "Change in key group abundance") +
        ggplot2::ylab("Proportional change in survival (scenario/base)") +
        ggplot2::xlab("Scenario") +
        ggplot2::labs(title = thisname) +
        ggthemes::theme_base() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           vjust = 0.5))  #+
      # ylim(min.violin, max.violin)


      ggplot2::ggsave(paste0(thisname, "_", i, "_violin_survival_scale.png"), plot = violin.plot.scale, device = "png", width= 11.55, height = 13.58, scale = 1, dpi = 600)

      print("creating box plot scale")

      col.pal <- c("#ffbe0b", "#0a9396")

      box.plot.scale <- plot.data %>%
        ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = scenario_name, fill = scenario_var)) +
        ggplot2::geom_boxplot() +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::scale_fill_manual(values = col.pal, name = "Change in key group abundance") +
        ggforce::facet_wrap_paginate(. ~ longname, ncol = 3, nrow = 3, page = i, shrink = FALSE, labeller = "label_value", scales = "free_y") +
        ggplot2::labs(title = thisname, y = "% change in survival (scenario-base)", x = "Scenario", face = "bold") +
        ggthemes::theme_base() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5,
                                                           hjust = 0.95)) +
        ggplot2::ylim(min.violin, max.violin)


      ggplot2::ggsave(paste0(thisname, "_", i, "_boxplot_survival_scale.png"), plot = box.plot.scale, device = "png", width= 11.55, height = 13.58, scale = 1, dpi = 600)

    }

    print("creating plots for scenario effects on salmon")

    salmon.impacts <- indsalmoneffect %>%
      dplyr::distinct(salmon_effect) %>%
      dplyr::pull(salmon_effect)

    for(thisimpact in salmon.impacts) {

      basin.fill <- c(`Puget Sound` = "#7EADAA", `Strait of Georgia` = "#2F5A54", Whidbey = "#F3A800", `Central Puget Sound` = "#DE7A00", `South Puget Sound` = "#0B77E8",
                      `Hood Canal` = "#032F5C")


      salmon.fg.text <- plot.data %>%
        dplyr::filter(salmon_effect==thisimpact) %>%
        dplyr::filter(prop_survival >= 100) %>%
        dplyr::arrange(basin, longname) %>%
        dplyr::select(scenario_name, basin, longname, prop_survival) %>%
        dplyr::mutate(label = round(prop_survival,0))%>%
        dplyr::group_by(scenario_name, basin, longname) %>%
        dplyr::slice(which.max(label)) %>% # leaves only the maximum value
        #dplyr::select(-prop_survival) %>%
        #dplyr::bind_cols(prop.survival) %>%
        dplyr::left_join(salmon.codes, by = "longname") %>%
        dplyr::mutate(label = paste(as.character(label), Code), prop_survival = 95) %>%
        dplyr::select(-Code)

      box.plot.basin <- plot.data %>%
        dplyr::filter(salmon_effect==thisimpact) %>%
        dplyr::filter(prop_survival <= 100) %>%
        ggplot2::ggplot(ggplot2::aes(y = prop_survival, x = basin, fill = longname, group = longname)) +
        ggplot2::geom_boxplot(outlier.shape = NA) +
        ggplot2::geom_point(ggplot2::aes(color=longname), position = ggplot2::position_jitterdodge(), alpha=0.5) +
      #  ggplot2::geom_jitter(alpha =0.5) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::facet_wrap(. ~ scenario_name, ncol = 2, nrow = these.rows) +
        ggplot2::labs(title = thisname, subtitle = paste("Expected", tolower(thisimpact), "impact on salmon"), y = "Proportional change in survival (scenario/base)", x = "Puget Sound basin", face = "bold") +
        ggthemes::theme_base() +
        ggplot2::theme(legend.position = "bottom") +
       # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
        ggplot2::ylim(-100,+100) +
      # ggplot2::geom_text(data = salmon.fg.text, label=salmon.fg.text$label, size = 3.5) +
      #    ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_manual(values = salmon.fg.fill, name = "Salmon group") +
        ggplot2::scale_color_manual(values = salmon.fg.fill) +
        ggrepel::geom_text_repel(
          data          = salmon.fg.text,
          mapping       = ggplot2::aes(basin, prop_survival, label = label),
          size          = 3.5,
          colour        = "black"
        ) +
      ggplot2::guides(color="none")


      # text.label <- plot.data %>% # dplyr::filter(salmon_effect==this.multiplier) %>% dplyr::distinct(scenario_name, scenario_var) %>%
      # dplyr::mutate(longname = 'Pink Salmon SY', basin = 'Puget Sound') ggplot2::geom_text( data = text.label, mapping = ggplot2::aes(x = -Inf, y = -Inf,
      # label = scenario_var), hjust = -0.5, vjust = -12 )

      short.name <- thisimpact %>%
        stringr::str_split(" ") %>%
        unlist %>% .[1]

      ggplot2::ggsave(paste0(thisname,"_",short.name, "_boxplot_survival_basin_.png"), plot = box.plot.basin, device = "png", width= 14.45, height = 12.00, scale = 1, dpi = 600)

    }

    # col.fill <- c('#ffbe0b','#ed5181', '#3fd2c7', '#00458b', '#f7a482', '#83bb90', '#f1dd88', '#81aa2c', '#f293b1')

    effect.fill <- c(`Positive` = "#002db3", `Negative` = "#ffd11a")

    these.rows <- ceiling(length(thesescenarios)/2)


  }

  return(box.plot.effect)
}


