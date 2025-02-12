# function to create a table of treatment differences in study 1
create_table_treatment_diffs_study1 <- function(treatment_means_study1) {
  treatment_means_study1 %>%
    dplyr::select(Response, Treatment, post) %>%
    pivot_wider(
      names_from = Treatment,
      values_from = post
    ) %>%
    rowwise() %>%
    transmute(
      Response          = Response,
      `Control - AI`    = list(`AI outsourcing` - Control),
      `Control - Human` = list(`Human outsourcing` - Control),
      `AI - Human`      = list(`Human outsourcing` - `AI outsourcing`)
    ) %>%
    mutate(
      across(
        !Response,
        function(x) paste0(
          ifelse(median(x) > 0, " ", ""),
          format(round(median(x), digits = 2), nsmall = 2),
          " [",
          ifelse(quantile(x, 0.025) > 0, " ", ""),
          format(round(quantile(x, 0.025), digits = 2), nsmall = 2),
          " ",
          ifelse(quantile(x, 0.975) > 0, " ", ""),
          format(round(quantile(x, 0.975), digits = 2), nsmall = 2),
          "]"
        )
      )
    ) %>%
    ungroup() %>%
    pivot_longer(cols = !Response, names_to = "Difference") %>%
    pivot_wider(names_from = Response, values_from = value)
}
