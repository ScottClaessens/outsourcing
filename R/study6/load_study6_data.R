# function to load and clean study 6 data
load_study6_data <- function(study6_data_file) {
  # read csv file
  read_csv(
    file = study6_data_file,
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
      treatment = factor(treatment, levels = c("Control", "Tool", "Full"))
      )
}
