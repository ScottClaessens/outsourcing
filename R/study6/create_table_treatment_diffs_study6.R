# function to create a table of treatment differences in study 6
create_table_treatment_diffs_study6 <- function(treatment_means_study6) {
  treatment_means_study6 %>%
    dplyr::select(Response, Treatment, post) %>%
    pivot_wider(
      names_from = Treatment,
      values_from = post
    ) %>%
    rowwise() %>%
    transmute(
      Response = Response,
      `Tool - Control` = list(Tool - Control),
      `Full - Control` = list(Full - Control),
      `Full - Tool`    = list(Full - Tool)
    ) %>%
    mutate(
      across(
        !Response,
        function(x) paste0(
          format(round(median(x), digits = 2), nsmall = 2),
          " [",
          format(round(quantile(x, 0.025), digits = 2), nsmall = 2),
          " ",
          format(round(quantile(x, 0.975), digits = 2), nsmall = 2),
          "]"
        )
      )
    ) %>%
    ungroup() %>%
    pivot_longer(cols = !Response, names_to = " ") %>%
    pivot_wider(names_from = Response, values_from = value)
}
