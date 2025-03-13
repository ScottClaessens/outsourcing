# function to load and clean study 4 data
load_study4_data <- function(study4_data_file, combined_task_means) {
  # read csv file
  read_csv(
    file = study4_data_file,
    show_col_types = FALSE
    ) %>%
    # remove participants who did not correctly answer the attention check
    filter(attention == "TikTok") %>%
    dplyr::select(!attention) %>%
    # treatment as factor
    mutate(
      treatment = factor(
        ifelse(treatment == "Control", treatment, paste0("AI_", treatment)),
        levels = c("Control", "AI_Standard_LowEffort", "AI_Standard_HighEffort",
                   "AI_Personalised_LowEffort", "AI_Personalised_HighEffort")
        )
      ) %>%
    # join task estimates from pilot study
    left_join(
      combined_task_means,
      by = c("task" = "Task")
    )
}
