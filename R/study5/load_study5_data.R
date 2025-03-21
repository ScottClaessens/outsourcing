# function to load and clean study 5 data
load_study5_data <- function(study5_data_file, combined_task_means) {
  # read csv file
  read_csv(
    file = study5_data_file,
    show_col_types = FALSE
    ) %>%
    # remove participants with low captcha scores
    filter(captcha >= 0.5) %>%
    dplyr::select(!captcha) %>%
    # remove participants who did not correctly answer the attention check
    filter(attention == "TikTok") %>%
    dplyr::select(!attention) %>%
    # treatment as factor
    mutate(
      treatment = factor(
        treatment,
        levels = c("Control", "AI_Tool_Bad", "AI_Tool_Good",
                   "AI_Full_Bad", "AI_Full_Good")
        )
      ) %>%
    # join task estimates from pilot study
    left_join(
      combined_task_means,
      by = c("task" = "Task")
    )
}
