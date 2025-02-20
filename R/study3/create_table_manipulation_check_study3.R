# function to create a manipulation check table in study 3
create_table_manipulation_check_study3 <- function(study3_data) {
  # create manipulation check vector
  manip_check <- c(
    "They wrote it themselves, without any help from ChatGPT" = 
      "Control",
    "They wrote it themselves, using ChatGPT to provide ideas and feedback" = 
      "Tool",
    "They copied it word-for-word from ChatGPT" = 
      "Full"
  )
  # create table
  study3_data %>%
    rowwise() %>%
    mutate(
      manipulation_check = manip_check[manipulation_check] == treatment
    ) %>%
    ungroup() %>%
    group_by(treatment) %>%
    summarise(
      `Pass manipulation check (%)` = 
        mean(manipulation_check, na.rm = TRUE) * 100,
      `Believe manipulation (%)` = 
        mean(believe_check == "Yes", na.rm = TRUE) * 100
    ) %>%
    rename(Condition = treatment) %>%
    mutate(
      Condition = ifelse(Condition == "Control", as.character(Condition), 
                         paste0(Condition, " outsourcing"))
    )
}
