#' Title
#'
#' @return
#' @export
#'
#' @examples

function(){

  salmon.max.nums <- salmon.nums %>%
    bind_rows() %>%
    group_by(Code, age, year_no) %>%
    summarise(max_nums = max(nums)) %>%
    left_join(func.groups, by="Code") %>%
    ungroup()

  salmon.juv.nums <- salmon.max.nums %>%
    filter(age == 1)

  salmon.return.nums <- salmon.max.nums %>%
    filter(age == (years_away + 1)) %>%
    dplyr::select(-years_away) %>%
    dplyr::rename(age_return = age, return_nums=max_nums) %>%
    left_join(salmon.juv.nums, by=c("Code","year_no")) %>%
    mutate(survival = return_nums/ max_nums)

  # Calculate the number of pages with 12 panels per page
  n_pages <- ceiling(
    nrow(func.groups)/ 12
  )

  print(n_pages)
  for (i in seq_len(n_pages)) {

    survival.plot <- salmon.return.nums %>%
      mutate(Code = as.factor(Code)) %>%
      filter(!year_no %in% c(2041)) %>%
      ggplot(aes(x = year_no, y = survival))+
      geom_line() +
      #facet_wrap(~ Code, scales = "free_y")
      facet_wrap_paginate(~ Code, ncol = 3, nrow = 4, page = 1, shrink = FALSE, scales = "free", labeller = 'label_value')

    thisplotname <- paste("salmon_survival_plot_",i,".png",sep="_")

    #ggsave(thisplotname,plot = pplot, device = "png", width = 10, height = 6)
    ggsave(thisplotname,plot = survival.plot, device = "png", width = 21, height = 24, units = "cm")

  }
}
