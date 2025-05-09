# function to combine task means for linking to study 1 data
combine_task_means <- function(task_means_social, task_means_socialskills, 
                               task_means_impactothers, task_means_consequences,
                               task_means_intrinsiceffort, 
                               task_means_extrinsiceffort) {
  bind_rows(
    task_means_social, task_means_socialskills, task_means_impactothers, 
    task_means_consequences, task_means_intrinsiceffort, 
    task_means_extrinsiceffort
  ) %>%
    ungroup() %>%
    mutate(
      Response = rep(c("social", "socialskills", "impactothers",
                       "consequences", "intrinsiceffort", "extrinsiceffort"),
                     each = 20)
      ) %>%
    dplyr::select(!c(Q2.5, Q97.5)) %>%
    pivot_wider(
      names_from = Response,
      values_from = c(Estimate, Est.Error),
      names_glue = "{Response}_{.value}"
    ) %>%
    rename_with(
      ~ str_remove(.x, "_Estimate"),
      cols = ends_with("_Estimate")
    ) %>%
    rename_with(
      ~ str_replace(.x, "_Est.Error", "_SE"),
      cols = ends_with("_Est.Error")
    )
}