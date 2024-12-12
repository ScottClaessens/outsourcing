# function to load and clean study 2 data
load_study2_data <- function(study2_data_file, combined_task_means) {
  # read csv file
  read_csv(
    file = study2_data_file,
    show_col_types = FALSE
    ) %>%
    # remove participants who did not correctly answer the attention check
    filter(attention == "TikTok") %>%
    dplyr::select(!attention) %>%
    # remove missing data (only a few instances)
    drop_na() %>%
    # treatment as factor
    mutate(
      treatment = factor(
        treatment, levels = c("Control", "AI_Tool_Honest", "AI_Tool_Deception",
                              "AI_Full_Honest", "AI_Full_Deception")
        )
      ) %>%
    # join task estimates from pilot study
    left_join(
      combined_task_means,
      by = c("task" = "Task")
    )
}
