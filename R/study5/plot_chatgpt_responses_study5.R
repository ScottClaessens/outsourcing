# function to plot the raw data for chatgpt questions
plot_chatgpt_responses_study5 <- function(study5_data) {
  # wrangle data for plotting
  study5_data <-
    study5_data %>%
    group_by(id) %>%
    summarise(across(starts_with("chatgpt_"), unique))
  # plot a
  pA <-
    ggplot(
      data = study5_data,
      mapping = aes(x = chatgpt_familiarity)
      ) +
    geom_bar(fill = "#05702A") +
    scale_x_continuous(
      name = "How familiar are you with\nthe AI tool ChatGPT?",
      breaks = 1:7
    ) +
    scale_y_continuous(
      name = "Count",
      expand = expansion(mult = c(0, .1))
    ) +
    theme_classic()
  # plot b
  pB <-
    ggplot(
      data = study5_data,
      mapping = aes(x = chatgpt_used)
      ) +
    geom_bar(fill = "#2BA106") +
    xlab("Have you ever used\nChatGPT before?") +
    scale_y_continuous(
      name = "Count",
      expand = expansion(mult = c(0, .1))
    ) +
    theme_classic()
  # plot c
  pC <-
    ggplot(
      data = study5_data,
      mapping = aes(x = chatgpt_frequency)
    ) +
    geom_bar(fill = "#0BA669") +
    scale_x_continuous(
      name = "How frequently do you\nuse ChatGPT?",
      breaks = 1:5
    ) +
    scale_y_continuous(
      name = "Count",
      expand = expansion(mult = c(0, .1))
    ) +
    theme_classic()
  # plot d
  pD <-
    ggplot(
      data = study5_data,
      mapping = aes(x = chatgpt_trust)
    ) +
    geom_bar(fill = "#2F866A") +
    scale_x_continuous(
      name = "How trustworthy do you\nthink ChatGPT is?",
      breaks = 1:7
    ) +
    scale_y_continuous(
      name = "Count",
      expand = expansion(mult = c(0, .1))
    ) +
    theme_classic()
  # put together
  top <- pA + pB + plot_layout(widths = c(1, 0.7))
  bot <- pC + pD + plot_layout(widths = c(0.7, 1))
  out <- (top / bot) + plot_annotation(tag_levels = "a")
  # save
  ggsave(
    filename = "plots/study5_chatgpt_responses.pdf",
    plot = out,
    width = 6,
    height = 5
  )
  return(out)
}
