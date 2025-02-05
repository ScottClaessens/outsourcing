# function to load and clean data from study 3
load_study3_data <- function(study3_data_file) {
  # read csv file
  read_csv(
    file = study3_data_file,
    show_col_types = FALSE
    ) %>%
    # remove participants with captcha < 0.5
    filter(captcha >= 0.5) %>%
    dplyr::select(!captcha) %>%
    # remove participants who did not correctly answer the attention check
    filter(attention == "TikTok") %>%
    dplyr::select(!attention) %>%
    # variables as factors
    mutate(across(treatment:answer, factor)) %>%
    # grade as ordered factor
    mutate(grade = ordered(grade, levels = LETTERS[5:1]))
}
