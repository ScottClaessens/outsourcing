# function to create a table of essay comprehension in study 3
create_table_essay_comprehension_study3 <- function(study3_data) {
  # create initial table
  initial_table <-
    tibble(
      Prompt = c("Social", "", "", "Non-social", "", ""),
      answer = c("Father", "Friend", "Sister", "Buffy", "Hobbit", "Titanic"),
      `Number of words` = as.integer(c(234, 211, 218, 251, 278, 239))
    ) %>%
    # calculate expected reading time at 275 words per minute
    mutate(
      `Expected reading time (secs)` = round((`Number of words` / 275) * 60, 2)
      )
  # create vector for comprehension answers
  comp_answers <- c(
    "Their father" = "Father",
    "Their best friend" = "Friend",
    "Their sister" = "Sister",
    "The TV show 'Buffy the Vampire Slayer'" = "Buffy",
    "The book 'The Hobbit'" = "Hobbit",
    "The film 'Titanic'" = "Titanic"
  )
  # calculate and append actual reading time and comprehension proportion
  study3_data %>%
    rowwise() %>%
    mutate(comprehension = comp_answers[comprehension] == answer) %>%
    ungroup() %>%
    group_by(answer) %>%
    summarise(
      `Average reading time (secs)` = round(median(essay_seconds), 2),
      `Comprehension (%)` = round(mean(comprehension, na.rm = TRUE) * 100, 2)
    ) %>%
    left_join(initial_table, by = "answer") %>%
    rename(Answer = answer) %>%
    dplyr::select(
      Prompt, Answer, `Number of words`, 
      `Expected reading time (secs)`, `Average reading time (secs)`, 
      `Comprehension (%)`
      )
}
