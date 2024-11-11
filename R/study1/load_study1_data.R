# function to load and clean study 1 data
load_study1_data <- function(study1_data_file, combined_task_means) {
  # read csv file
  read_csv(
    file = study1_data_file,
    show_col_types = FALSE
    ) %>%
    # remove participants who did not correctly answer the attention check
    filter(attention == "TikTok") %>%
    dplyr::select(!attention) %>%
    # remove missing data (only one task for one participant)
    drop_na() %>%
    # treatment as factor
    mutate(
      treatment = factor(
        treatment, levels = c("Control", "AI outsourcing", "Human outsourcing")
        )
      ) %>%
    # join task estimates from pilot study
    left_join(
      combined_task_means,
      by = c("task" = "Task")
    )
}
