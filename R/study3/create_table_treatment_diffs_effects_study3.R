# function to create a table of treatment differences and effects in study 3
create_table_treatment_diffs_effects_study3 <- function(treatment_means_study3) {
  treatment_means_study3 %>%
    dplyr::select(Response, Treatment, Task, post) %>%
    unite(Condition, Treatment:Task) %>%
    pivot_wider(
      names_from = Condition,
      values_from = post
    ) %>%
    rowwise() %>%
    transmute(
      Response = str_to_sentence(str_replace(Response, "_", " ")),
      # effects of outsourcing
      `Tool Social - Control Social` = list(Tool_Social - Control_Social),
      `Full Social - Control Social` = list(Full_Social - Control_Social),
      `Full Social - Tool Social` = list(Full_Social - Tool_Social),
      `Tool Non-social - Control Non-social` = list(Tool_NonSocial - Control_NonSocial),
      `Full Non-social - Control Non-social` = list(Full_NonSocial - Control_NonSocial),
      `Full Non-social - Tool Non-social` = list(Full_NonSocial - Tool_NonSocial),
      # effects of social task
      `Control Social - Control Non-social` = list(Control_Social - Control_NonSocial),
      `Tool Social - Tool Non-social` = list(Tool_Social - Tool_NonSocial),
      `Full Social - Full Non-social` = list(Full_Social - Full_NonSocial),
      # interaction effects
      `Interaction: Tool - Control` = list(`Tool Social - Control Social` -
                                             `Tool Non-social - Control Non-social`),
      `Interaction: Full - Control` = list(`Full Social - Control Social` -
                                             `Full Non-social - Control Non-social`),
      `Interaction: Full - Tool` = list(`Full Social - Tool Social` -
                                             `Full Non-social - Tool Non-social`)
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
