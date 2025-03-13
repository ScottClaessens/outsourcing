# function to create a table of treatment differences and effects in study 4
create_table_treatment_diffs_effects_study4 <- function(treatment_means_study4) {
  treatment_means_study4 %>%
    dplyr::select(Response, Treatment, post) %>%
    pivot_wider(
      names_from = Treatment,
      values_from = post
    ) %>%
    rowwise() %>%
    transmute(
      Response = Response,
      # comparison to control
      `Standard Low Effort - Control`      = list(AI_Standard_LowEffort      - Control),
      `Standard High Effort - Control`     = list(AI_Standard_HighEffort     - Control),
      `Personalised Low Effort - Control`  = list(AI_Personalised_LowEffort  - Control),
      `Personalised High Effort - Control` = list(AI_Personalised_HighEffort - Control),
      # effect of AI type
      `Standard Low Effort - Personalised Low Effort` = 
        list(AI_Standard_LowEffort - AI_Personalised_LowEffort),
      `Standard High Effort - Personalised High Effort` = 
        list(AI_Standard_HighEffort - AI_Personalised_HighEffort),
      # effect of effort
      `Standard Low Effort - Standard High Effort` = 
        list(AI_Standard_LowEffort - AI_Standard_HighEffort),
      `Personalised Low Effort - Personalised High Effort` = 
        list(AI_Personalised_LowEffort - AI_Personalised_HighEffort),
      # interaction
      `Interaction effect` = 
        list(`Standard Low Effort - Standard High Effort` - 
               `Personalised Low Effort - Personalised High Effort`)
    ) %>%
    mutate(
      across(
        !Response,
        function(x) paste0(
          ifelse(round(median(x), digits = 2) >= 0, " ", ""),
          format(round(median(x), digits = 2), nsmall = 2),
          " [",
          ifelse(round(quantile(x, 0.025), digits = 2) >= 0, " ", ""),
          format(round(quantile(x, 0.025), digits = 2), nsmall = 2),
          " ",
          ifelse(round(quantile(x, 0.975), digits = 2) >= 0, " ", ""),
          format(round(quantile(x, 0.975), digits = 2), nsmall = 2),
          "]"
        )
      )
    ) %>%
    ungroup() %>%
    pivot_longer(cols = !Response, names_to = " ") %>%
    pivot_wider(names_from = Response, values_from = value)
}
