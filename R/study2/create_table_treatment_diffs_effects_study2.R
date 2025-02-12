# function to create a table of treatment differences and effects in study 2
create_table_treatment_diffs_effects_study2 <- function(treatment_means_study2) {
  treatment_means_study2 %>%
    dplyr::select(Response, Treatment, post) %>%
    pivot_wider(
      names_from = Treatment,
      values_from = post
    ) %>%
    rowwise() %>%
    transmute(
      Response          = Response,
      # comparisons to control
      `Control - Tool Honest`     = list(AI_Tool_Honest    - Control),
      `Control - Tool Deception`  = list(AI_Tool_Deception - Control),
      `Control - Full Honest`     = list(AI_Full_Honest    - Control),
      `Control - Full Deception`  = list(AI_Full_Deception - Control),
      # honesty effects
      `Tool Honest - Tool Deception` = list(AI_Tool_Honest - AI_Tool_Deception),
      `Full Honest - Full Deception` = list(AI_Full_Honest - AI_Full_Deception),
      # outsourcing type effects
      `Tool Honest - Full Honest` = list(AI_Tool_Honest - AI_Full_Honest),
      `Tool Deception - Full Deception` = 
        list(AI_Tool_Deception - AI_Full_Deception),
      # interaction
      `Interaction effect` = list(`Tool Honest - Tool Deception` - 
                                    `Full Honest - Full Deception`)
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
    pivot_longer(cols = !Response, names_to = " ") %>%
    pivot_wider(names_from = Response, values_from = value)
}
