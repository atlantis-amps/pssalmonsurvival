#' @title plot salmon survival of cumulative impact scenarios
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon_cum_return_nums.csv, survival over time
#' @export
#'
#' @description Code to plot survival of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com February 2002



plot_ensemble_cum_survival_scenarios <- function(ensemblenumberscum, salmongroups, base.cum.survival, salmonbybasin, scenario.effect){

  salmon.max.nums <- ensemblenumberscum %>%
    dplyr::group_by(scenario_name, scenario_var, model_ver, Code, age, year_no) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by="Code") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(model_ver = as.double(model_ver)) %>%
    dplyr::filter(scenario_var != 1) %>%
    dplyr::filter(scenario_name == "bottom top") %>%
    dplyr::mutate(scenario_var = dplyr::if_else(scenario_var=="0_8", "-20%","+20%")) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="bottom top","Cumulative effects", scenario_name))

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

#Individual and interaction effects for cumulative impact scenarios.
#To estimate the individual effect we used the natural log response ratio (lnRR), calculated as:
#    lnRR = ln(Xs / Xb).
#  where Xs and Xb are mean survival under each scenario and the base run respectively.

salmon.rel.survival <- base.cum.survival %>%
  dplyr::select("scenario_name","model_ver","Code","Long.Name","survival") %>%
  dplyr::rename(base_survival = survival) %>%
  dplyr::left_join(salmon.return.nums.yr, by = c("scenario_name","model_ver","Code","Long.Name")) %>%
  dplyr::mutate(rel_survival = (((survival / base_survival)-1) * 100))

salmon.return.nums %>%
  dplyr::filter(year_no<=ret_year) %>%
  dplyr::filter(year_no==max(year_no)) %>%
  readr::write_csv(., here::here("modelfiles","survival_cum_rates.csv"))

salmon.basin <- salmonbybasin %>%
  dplyr::select(Code, basin, geo_order, salmon_genus)

salmon.lollipop.data <- salmon.rel.survival %>%
  dplyr::mutate(longname = as.factor(Long.Name)) %>%
  dplyr::mutate(model_ver = as.factor(model_ver)) %>%
  dplyr::mutate(scenario_var = as.factor(scenario_var)) %>%
  dplyr::group_by(scenario_var,Code,longname) %>%
  dplyr::mutate(max_model = max(rel_survival)) %>%
  dplyr::mutate(min_model = min(rel_survival)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(longname = gsub("Subyearling", "SY", longname), longname = gsub("Yearling","Y", longname),
                longname = dplyr::if_else(longname=="Chum Hood Canal summer run SY", "Chum Hood Canal SY",
                                           dplyr::if_else(longname=="Strait of Georgia salmonids", "St. of Georgia salmonids", longname))) %>%
  dplyr::mutate(scenario_name = Hmisc::capitalize(scenario_name)) %>%
  dplyr::left_join(salmon.basin, by=c("Code")) %>%
  dplyr::left_join(scenario.effect, by = c("scenario_name","scenario_var"))

readr::write_csv(salmon.lollipop.data, here::here("modelfiles","ensemble_cum_survival.csv"))


  thisscenario <- salmon.lollipop.data %>%
    dplyr::distinct(scenario_name) %>%
    dplyr::pull(scenario_name)

  col.pal <- c("#ffbe0b","#0a9396")
  col.fill <- c("#ffbe0b","#0a9396", "#3fd2c7", "#99ddff", "#00458b", "#549896", "#83bb90", "#f1dd88", "#f7a482", "#81aa2c", "#f293b1", "#ed5181")

  salmon.order <- salmon.lollipop.data %>%
    dplyr::distinct(longname, basin, geo_order, salmon_genus) %>%
    dplyr::arrange(salmon_genus, geo_order) %>%
    dplyr::pull(longname)

  plot.data <- salmon.lollipop.data %>%
    dplyr::filter(rel_survival < 50) %>%
    droplevels() %>%
    dplyr::mutate(basin = forcats::fct_relevel(as.factor(basin), "Puget Sound", "Strait of Georgia", "Whidbey", "Central Puget Sound", "South Puget Sound", "Hood Canal")) %>%
    dplyr::mutate(longname = forcats::fct_relevel(as.factor(longname),salmon.order)) %>%
    dplyr::mutate(salmon_genus = forcats::fct_relevel(as.factor(salmon_genus),"Chinook","Chum","Coho","Pink","Sockeye"))


 max.violin <- plot.data %>%
    dplyr::pull(rel_survival) %>%
    max() %>%
    ceiling(.)

  min.violin <- plot.data %>%
    dplyr::pull(rel_survival) %>%
    min() %>%
    floor(.)

    col.fill <- c("#ffbe0b","#ed5181", "#3fd2c7", "#00458b", "#f7a482", "#83bb90", "#f1dd88", "#81aa2c", "#f293b1")

    box.plot.scale.basin <- plot.data %>%
      ggplot2::ggplot(ggplot2::aes(y = rel_survival, x = longname, fill = basin)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(. ~ salmon_effect, scales = "free_y") +
      ggplot2::scale_fill_manual(values = col.fill, name = "Basin of origin") +
      ggplot2::labs(title = "Cumulative scenario", y = "% change in survival rel. to base case", x = "Functional group", face = "bold") +
      ggthemes::theme_base() +
      ggplot2::theme(legend.position="bottom") +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5, hjust=0.95)) +
      ggplot2::ylim(min.violin, max.violin)

    ggplot2::ggsave("boxplot_cum_survival_basin_scale.png", plot = box.plot.scale.basin, device = "png", width = 40, height = 35, units = "cm", dpi = 600)


  }

