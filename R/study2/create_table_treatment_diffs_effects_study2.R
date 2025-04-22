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
      Response = Response,
      # comparison to control
      `Tool Honest - Control`     = list(AI_Tool_Honest    - Control),
      `Tool Deception - Control`  = list(AI_Tool_Deception - Control),
      `Full Honest - Control`     = list(AI_Full_Honest    - Control),
      `Full Deception - Control`  = list(AI_Full_Deception - Control),
      # effect of full outsourcing
      `Full Honest - Tool Honest` = list(AI_Full_Honest - AI_Tool_Honest),
      `Full Deception - Tool Deception` = 
        list(AI_Full_Deception - AI_Tool_Deception),
      # effect of deception
      `Tool Deception - Tool Honest` = list(AI_Tool_Deception - AI_Tool_Honest),
      `Full Deception - Full Honest` = list(AI_Full_Deception - AI_Full_Honest),
      # interaction
      `Interaction effect` = list(`Full Deception - Full Honest` - 
                                    `Tool Deception - Tool Honest`)
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
