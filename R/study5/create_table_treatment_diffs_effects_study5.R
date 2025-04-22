# function to create a table of treatment differences and effects in study 5
create_table_treatment_diffs_effects_study5 <- function(treatment_means_study5) {
  treatment_means_study5 %>%
    dplyr::select(Response, Treatment, post) %>%
    pivot_wider(
      names_from = Treatment,
      values_from = post
    ) %>%
    rowwise() %>%
    transmute(
      Response = Response,
      # comparison to control
      `Tool Bad Reason - Control`  = list(AI_Tool_Bad  - Control),
      `Tool Good Reason - Control` = list(AI_Tool_Good - Control),
      `Full Bad Reason - Control`  = list(AI_Full_Bad  - Control),
      `Full Good Reason - Control` = list(AI_Full_Good - Control),
      # effect of full outsourcing
      `Full Bad Reason - Tool Bad Reason`   = list(AI_Full_Bad - AI_Tool_Bad),
      `Full Good Reason - Tool Good Reason` = list(AI_Full_Good - AI_Tool_Good),
      # effect of bad reasons
      `Tool Bad Reason - Tool Good Reason` = list(AI_Tool_Bad - AI_Tool_Good),
      `Full Bad Reason - Full Good Reason` = list(AI_Full_Bad - AI_Full_Good),
      # interaction
      `Interaction effect` = 
        list(`Full Bad Reason - Tool Bad Reason` - 
               `Full Good Reason - Tool Good Reason`)
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
