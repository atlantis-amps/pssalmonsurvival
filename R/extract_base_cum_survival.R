#' @title plot salmon survival of cumulative scenarios of ensemble models
#' @param ensemblenumbersage, a data frame
#' @param func.groups, a data frame of salmon groups in the model
#'
#' @return salmon.return.nums, survival over time
#' @export
#'
#' @description Code to plot survival in cumulative scenarios of multiple AMPS versions
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna_gmail.com October 2022




extract_base_cum_survival <- function(ensemblenumberscum, salmongroups){

  salmon.max.nums <- ensemblenumberscum %>%
    dplyr::group_by(scenario_name, scenario_var, model_ver, Code, age, year_no) %>%
    dplyr::summarise(max_nums = max(nums)) %>%
    dplyr::left_join(salmongroups, by="Code") %>%
    dplyr::ungroup()  %>%
    dplyr::filter(scenario_var == 1) %>%
    dplyr::filter(scenario_name == "bottom top")

  salmon.juv.nums <- salmon.max.nums %>%
    dplyr::filter(age == 1) %>%
    dplyr::rename(juv_nums = max_nums)

  salmon.return.nums <- salmon.max.nums %>%
    dplyr::filter(age == (years_away + 1)) %>%
    dplyr::mutate(cohort_yr = year_no - age) %>%
    dplyr::select(-years_away) %>%
    dplyr::rename(age_return = age, return_nums=max_nums, year_sim = year_no, year_no= cohort_yr) %>%
    dplyr::left_join(salmon.juv.nums, by=c("scenario_name","scenario_var", "model_ver","Code","year_no","Long.Name","NumCohorts","Name")) %>%
    dplyr::mutate(survival = return_nums/juv_nums) %>%
    dplyr::mutate(model_ver = as.factor(model_ver)) %>%
    dplyr::mutate(Year = year_sim - 2010) %>%
    dplyr::mutate(max_year = max(year_no)) %>%
    dplyr::mutate(ret_year = max_year-3)

  salmon.plot.data <- salmon.return.nums %>%
    dplyr::filter(year_no<=ret_year) %>%
    dplyr::mutate(scenario_name = dplyr::if_else(scenario_name=="bottom top","Cumulative effects", scenario_name)) %>%
    dplyr::mutate(Long.Name = as.factor(Long.Name), Year = as.factor(Year), scenario_name = as.factor(scenario_name)) %>%
    droplevels() %>%
    tidyr::drop_na() %>%
    dplyr::select(-migiobox.x, -migiobox.y) %>%
    dplyr::mutate(Year = as.factor(Year))


  readr::write_csv(salmon.plot.data, here::here("modelfiles","base_cum_survival.csv"))


}
