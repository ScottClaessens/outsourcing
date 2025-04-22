# function to create a table of tasks across studies
create_table_tasks <- function() {
  tibble(
    Task = c(
      "Writing wedding vows",
      "Writing a love letter",
      "Writing a marriage proposal",
      "Choosing an engagement ring",
      "Finding a gift for a family member",
      "Deciding how to discipline a child",
      "Writing a bereavement card to a family member",
      "Writing an apology letter to a friend",
      "Planning a city tour for a friend",
      "Planning a surprise birthday party for a friend",
      "Writing a cover letter for a job application",
      "Writing computer code",
      "Solving a mathematical equation",
      "Planning a syllabus",
      "Writing a short story",
      "Writing a poem",
      "Creating visual art",
      "Creating a daily schedule",
      "Generating a shopping list",
      "Writing a dinner recipe"
    ),
    `Pilot Study 1` = rep("$\\checkmark$", 20),
    `Pilot Study 2` = rep("$\\checkmark$", 20),
    `Study 1` = rep("$\\checkmark$", 20),
    `Study 2` = c(rep("$\\checkmark$", 3), rep("", 3), 
                  rep("$\\checkmark$", 10), "",
                  rep("$\\checkmark$", 3)),
    `Study 4` = c(rep("$\\checkmark$", 2), rep("", 4), 
                  rep("$\\checkmark$", 2), rep("", 2),
                  rep("$\\checkmark$", 4), rep("", 6))
  )
}
