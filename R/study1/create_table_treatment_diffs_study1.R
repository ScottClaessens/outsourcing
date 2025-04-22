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
      `AI - Control`    = list(`AI outsourcing` - Control),
      `Human - Control` = list(`Human outsourcing` - Control),
      `Human - AI`      = list(`Human outsourcing` - `AI outsourcing`)
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
