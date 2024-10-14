# function to plot task-specific correlations 
plot_correlations <- function(fit) {
  # labels for plot
  labels <- c(
    "social" = "Social",
    "socialskills" = "Social skills",
    "impactothers" = "Impact others",
    "consequences" = "Consequences",
    "intrinsiceffort" = "Intrinsic effort",
    "extrinsiceffort" = "Extrinsic effort"
  )
  # extract task-specific correlations from fitted model
  p <-
    summary(fit)$random$task %>%
    # tidy resulting data frame
    rownames_to_column("parameter") %>%
    filter(str_starts(parameter, "cor")) %>%
    mutate(parameter = str_sub(parameter, 5, -2)) %>%
    separate(parameter, into = c("var1", "var2"), sep = ",") %>%
    mutate(
      var1 = str_sub(var1, 0, -11),
      var2 = str_sub(var2, 0, -11),
      var1 = factor(labels[var1], levels = labels),
      var2 = factor(labels[var2], levels = labels)
    ) %>%
    # plot correlations
    ggplot(
      aes(x = var1, y = fct_rev(var2), fill = Estimate,
          label = round(Estimate, 2))
      ) +
    geom_tile() +
    geom_text() +
    scale_fill_gradient2(high = "red", low = "blue") +
    ggtitle("Task-specific correlations") +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank()
    )
  # save
  ggsave(
    plot = p,
    filename = "plots/task_correlations.pdf",
    height = 4,
    width = 5
  )
  return(p)
}
