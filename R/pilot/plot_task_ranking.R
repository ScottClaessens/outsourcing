# function to plot ranking for a specific response variable
plot_task_ranking <- function(pilot_data, task_means, resp = "social") {
  # y-axis titles for plot
  yaxes <- c(
    "social" = "\nIs this a social task?",
    "socialskills" = "\nDoes this task require social skills?",
    "impactothers" = "\nDoes this task impact other people?",
    "consequences" = "How important are the\nconsequences of this task?",
    "intrinsiceffort" = "How important is it that\neffort goes into this task?",
    "extrinsiceffort" = paste0(
      "How important is it\n",
      "that others see the effort\n",
      "that went into this task?"
      )
  )
  # response col name in data
  resp_col <- c(
    "social" = "social",
    "socialskills" = "social_skills",
    "impactothers" = "impact_others",
    "consequences" = "consequences",
    "intrinsiceffort" = "intrinsic_effort",
    "extrinsiceffort" = "extrinsic_effort"
  )
  col <- resp_col[resp]
  # plot
  p <-
    ggplot() +
    geom_jitter(
      data = pilot_data,
      mapping = aes(
        x = fct_relevel(task, rev(unique(task_means$Task))),
        y = !!sym(col)
      ),
      width = 0.15,
      size = 0.5,
      colour = "lightgrey"
    ) +
    geom_pointrange(
      data = task_means,
      aes(
        x = Task,
        y = Estimate,
        ymin = Q2.5,
        ymax = Q97.5
      )
    ) +
    scale_y_continuous(
      breaks = 1:7,
      limits = c(1, 7),
      oob = scales::squish
    ) +
    labs(y = yaxes[resp]) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 5.5, angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 35)
    )
  # save
  ggsave(
    plot = p,
    filename = paste0("plots/task_ranking_", resp, ".pdf"),
    width = 6,
    height = 4
  )
  return(p)
}
