# function to load and clean pilot data
load_pilot_data <- function(pilot_data_file) {
  # read csv file
  read_csv(
    file = pilot_data_file,
    show_col_types = FALSE
    ) %>%
    # remove participants who did not correctly answer the attention check
    filter(attention == "TikTok") %>%
    dplyr::select(!attention) %>%
    # convert characters to factors
    mutate(
      gender = factor(gender),
      task = factor(task)
    )
}
