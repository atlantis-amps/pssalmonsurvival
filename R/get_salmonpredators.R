#' Get salmon predators
#'
#' @param ppreymatrix
#' @param functionalgroups
#'
#' @return predgroups
#' @export
#'
#' @examples
get_salmonpredators <- function(ppreymatrix, functionalgroups){

  func.group <- functionalgroups %>%
    dplyr::rename(Predator = longname)


  salmon.patterns <- c("Chinook", "Chum", "Coho", "salmonids")

  predgroups <- ppreymatrix[,c(1,28:48)] %>%
    dplyr::mutate(tot_prey = rowSums(dplyr::across(2:ncol(.)))) %>%
    dplyr::filter(tot_prey > 0) %>%
    dplyr::select(Predator, tot_prey) %>%
    dplyr::left_join(func.group, by = "Predator") %>%
   # dplyr::filter(isExternal==0) %>%
    dplyr::select(Predator, Code, name, GroupType, isExternal) %>%
    dplyr::mutate(guild = dplyr::if_else(name=="Sm_Plank_Fish","Small planktivorous fish",
                                         dplyr::if_else(GroupType=="SHARK","Elasmobranchs",
                                                 dplyr::if_else(GroupType=="BIRD", "Seabirds",
                                                                dplyr::if_else(GroupType=="MAMMAL","Marine mammals",
                                                                               dplyr::if_else(grepl(paste(salmon.patterns, collapse="|"), Predator),"Salmon","Demersal fish"))))))


  usethis::use_data(predgroups, overwrite = TRUE)
  #creates an R file with that name
  usethis::use_r('data-predgroups')
  #creates template describing the data
  data.description <-sinew::makeOxygen(predgroups)
  cat(data.description, file=here::here("R",paste0("data-","predgroups.R")))

  return(predgroups)

}
