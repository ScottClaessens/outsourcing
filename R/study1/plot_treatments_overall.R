# function to plot overall treatment effects
plot_treatments_overall <- function(study1_data, treatment_means) {
  # treatment variable for plotting
  study1_data <-
    study1_data %>%
    mutate(
      treatment = as.character(treatment),
      treatment = ifelse(treatment == "Human outsourcing", "Human", treatment),
      treatment = ifelse(treatment == "AI outsourcing", "AI", treatment),
      treatment = factor(treatment, levels = c("Control", "AI", "Human"))
    )
  treatment_means <-
    treatment_means %>%
    mutate(
      Treatment = ifelse(Treatment == "Human outsourcing", "Human", Treatment),
      Treatment = ifelse(Treatment == "AI outsourcing", "AI", Treatment),
      Treatment = factor(Treatment, levels = c("Control", "AI", "Human"))
    )
  # generic plotting function
  plot_fun <- function(resp, colour) {
    ggplot() +
      geom_jitter(
        data = study1_data,
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
        panel.grid.minor.y = element_blank()
      )
  }
  # get plots
  pA <- plot_fun(resp = "competent",   colour = "#E69F00")
  pB <- plot_fun(resp = "warm",        colour = "#56B4E9")
  pC <- plot_fun(resp = "moral",       colour = "#009E73")
  pD <- plot_fun(resp = "lazy",        colour = "#F0E442")
  pE <- plot_fun(resp = "trustworthy", colour = "#0072B2")
  # put together
  top <- pA + pB + pC
  bot <- 
    plot_spacer() + pD + pE + plot_spacer() + 
    plot_layout(widths = c(0.35, 1, 1, 0.65))
  out <- (top / bot) + plot_annotation(tag_levels = "a")
  # save
  ggsave(
    filename = "plots/study1_treatments.pdf",
    plot = out,
    width = 6,
    height = 4
  )
  return(out)
}