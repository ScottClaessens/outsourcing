# function to plot interaction effects for study 4
plot_interaction_effects_study4 <- function(combined_interaction_effects_study4) {
  # prepare data for plotting
  responses <- c(
    "competent"   = "Competent",
    "warm"        = "Warm",
    "moral"       = "Moral",
    "lazy"        = "Lazy",
    "trustworthy" = "Trustworthy"
  )
  predictors <- c(
    "social"          = "Social",
    "socialskills"    = "Social skills",
    "impactothers"    = "Impact others",
    "consequences"    = "Consequences",
    "intrinsiceffort" = "Intrinsic effort",
    "extrinsiceffort" = "Extrinsic effort"
  )
  # plot
  out <-
    combined_interaction_effects_study4 %>%
    mutate(
      resp = factor(responses[resp], levels = responses),
      var = factor(predictors[var], levels = predictors)
    ) %>%
    ggplot(
      mapping = aes(
        x = pred,
        y = estimate,
        ymin = lower,
        ymax = upper,
        fill = fct_rev(effect)
        )
      ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
      ) +
    geom_ribbon(alpha = 0.4) +
    geom_line(aes(colour = fct_rev(effect))) +
    facet_grid(
      resp ~ var,
      switch = "both"
      ) +
    scale_x_continuous(
      name = "Features of tasks",
      limits = c(1, 7),
      breaks = 1:7,
      expand = c(0, 0)
      ) +
    scale_y_continuous(
      name = "Estimated difference from control condition",
      breaks = c(-5, -2.5, 0, 2.5, 5),
      limits = c(-5.5, 5.5)
      ) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      axis.text = element_text(size = 6.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(0.7, "lines"),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
        ),
      strip.placement = "outside",
      legend.position = "top"
      )
  # save
  ggsave(
    filename = "plots/study4_interactions.pdf",
    plot = out,
    height = 6,
    width = 7
  )
  return(out)
}
