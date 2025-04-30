# function to plot overall treatment effects in study 4
plot_treatments_overall_study4 <- function(study4_data,
                                           treatment_means_study4) {
  # treatment variable for plotting
  study4_data <-
    study4_data %>%
    mutate(
      treatment = str_replace_all(as.character(treatment), "_", "\n"),
      treatment = str_remove(treatment, fixed("AI\n")),
      treatment = str_replace(treatment, "Low", "Low "),
      treatment = str_replace(treatment, "High", "High "),
      treatment = factor(
        treatment, levels = c("Control", "Standard\nLow Effort",
                              "Standard\nHigh Effort",
                              "Personalised\nLow Effort",
                              "Personalised\nHigh Effort")
        )
    )
  treatment_means <-
    treatment_means_study4 %>%
    mutate(
      Treatment = str_replace_all(as.character(Treatment), "_", "\n"),
      Treatment = str_remove(Treatment, fixed("AI\n")),
      Treatment = str_replace(Treatment, "Low", "Low "),
      Treatment = str_replace(Treatment, "High", "High "),
      Treatment = factor(
        Treatment, levels = c("Control", "Standard\nLow Effort",
                              "Standard\nHigh Effort",
                              "Personalised\nLow Effort",
                              "Personalised\nHigh Effort")
      )
    )
  # generic plotting function
  plot_fun <- function(resp, colour) {
    ggplot() +
      geom_jitter(
        data = study4_data,
        mapping = aes(
          x = treatment,
          y = !!sym(resp)
        ),
        width = 0.3,
        height = 0.45,
        colour = colour,
        alpha = 0.1,
        size = 0.2
      ) +
      geom_pointrange(
        data = filter(
          treatment_means,
          Response == str_to_title(resp)
          ),
        mapping = aes(
          x = Treatment,
          y = Estimate,
          ymin = Q2.5,
          ymax = Q97.5
        ),
        size = 0.3
      ) +
      scale_y_continuous(
        name = str_to_title(resp),
        breaks = 1:7,
        limits = c(1, 7),
        oob = scales::squish
      ) +
      xlab(NULL) +
      theme_minimal() +
      theme(
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 6.5)
      )
  }
  # get plots
  pA <- plot_fun(resp = "competent",   colour = "#E69F00")
  pB <- plot_fun(resp = "warm",        colour = "#56B4E9")
  pC <- plot_fun(resp = "moral",       colour = "#009E73")
  pD <- plot_fun(resp = "lazy",        colour = "#F0E442")
  pE <- plot_fun(resp = "trustworthy", colour = "#0072B2")
  # put together
  top <- pA + pB
  mid <- pC + pD
  bot <- 
    plot_spacer() + pE + plot_spacer() + 
    plot_layout(widths = c(0.4, 1, 0.6))
  out <- (top / mid / bot) + plot_annotation(tag_levels = "a")
  # add margin
  out <- out & theme(plot.margin = unit(c(0, 0, 15, 0), "pt"))
  # save
  ggsave(
    filename = "plots/study4_treatments.pdf",
    plot = out,
    width = 7,
    height = 6
  )
  return(out)
}
