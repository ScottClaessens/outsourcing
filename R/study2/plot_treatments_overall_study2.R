# function to plot overall treatment effects in study 2
plot_treatments_overall_study2 <- function(study2_data,
                                           treatment_means_study2) {
  # treatment variable for plotting
  study2_data <-
    study2_data %>%
    mutate(
      treatment = str_remove(
        str_replace_all(as.character(treatment), "_", "\n"),
        "AI\n"
      ),
      treatment = factor(treatment, levels = c("Control", "Tool\nHonest",
                                               "Tool\nDeception",
                                               "Full\nHonest",
                                               "Full\nDeception"))
    )
  treatment_means <-
    treatment_means_study2 %>%
    mutate(
      Treatment = str_remove(
        str_replace_all(as.character(Treatment), "_", "\n"),
        "AI\n"
        ),
      Treatment = factor(Treatment, levels = c("Control", "Tool\nHonest",
                                               "Tool\nDeception",
                                               "Full\nHonest",
                                               "Full\nDeception"))
    )
  # generic plotting function
  plot_fun <- function(resp, colour) {
    ggplot() +
      geom_jitter(
        data = study2_data,
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
        axis.text.x = element_text(size = 7)
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
  # save
  ggsave(
    filename = "plots/study2_treatments.pdf",
    plot = out,
    width = 6,
    height = 6
  )
  return(out)
}