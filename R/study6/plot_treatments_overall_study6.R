# function to plot overall treatment effects in study 6
plot_treatments_overall_study6 <- function(study6_data,
                                           treatment_means_study6) {
  # generic plotting function
  plot_fun <- function(resp, colour) {
    ggplot() +
      geom_jitter(
        data = study6_data,
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
          treatment_means_study6,
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
        axis.text.x = element_text(size = 6.4)
      )
  }
  # get plots
  pA <- plot_fun(resp = "effort",      colour = "#999999")
  pB <- plot_fun(resp = "authentic",   colour = "#D55E00")
  pC <- plot_fun(resp = "care",        colour = "#CC79A7")
  pD <- plot_fun(resp = "competent",   colour = "#E69F00")
  pE <- plot_fun(resp = "warm",        colour = "#56B4E9")
  pF <- plot_fun(resp = "moral",       colour = "#009E73")
  pG <- plot_fun(resp = "lazy",        colour = "#F0E442")
  pH <- plot_fun(resp = "trustworthy", colour = "#0072B2")
  # put together
  top <- (pA + pB + pC + pD) + plot_layout(nrow = 1)
  bot <- (pE + pF + pG + pH) + plot_layout(nrow = 1)
  out <- (top / bot) + plot_annotation(tag_levels = "a")
  # save
  ggsave(
    filename = "plots/study6_treatments.pdf",
    plot = out,
    width = 6,
    height = 4
  )
  return(out)
}
