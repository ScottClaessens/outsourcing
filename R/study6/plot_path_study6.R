# function to plot path model fitted to study 6 data
plot_path_study6 <- function(study6_fit2) {
  # create arrow function
  place_arrow <- function(x, y, xend, yend, ns = FALSE, curved = FALSE) {
    if (curved) {
      annotate(
        geom = "curve",
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        lineend = "butt",
        curvature = 0.3,
        linewidth = 1.2,
        arrow = arrow(length = unit(0.34, "cm")),
        colour = ifelse(ns, "#EDEDED", "black")
      )
    } else {
      annotate(
        geom = "segment",
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        lineend = "butt",
        linejoin = "mitre",
        linewidth = 1.2,
        arrow = arrow(length = unit(0.34, "cm")),
        colour = ifelse(ns, "#EDEDED", "black")
      )
    }
  }
  # get parameters of interest
  pars <- 
    summary(study6_fit2)$fixed %>%
    as_tibble(rownames = "Parameter") %>%
    filter(
      Parameter %in% c("effort_motreatment", "authentic_motreatment",
                       "authentic_moeffort", "care_motreatment",
                       "care_moeffort", "evaluation_motreatment",
                       "evaluation_moeffort", "evaluation_moauthentic",
                       "evaluation_mocare")
      ) %>%
    # parse at text for plot
    mutate(
      Label = paste0(
        "b = ",
        trimws(format(round(Estimate, 2), nsmall = 2), "left"),
        ifelse(Parameter %in% c("effort_motreatment", "evaluation_moauthentic"),
               "\n[", " ["),
        trimws(format(round(`l-95% CI`, 2), nsmall = 2), "left"),
        " ",
        trimws(format(round(`u-95% CI`, 2), nsmall = 2), "left"),
        "]"
        )
      ) %>%
    # add parameter label coordinates and angles
    mutate(
      x = c(-0.01, 0.29, 0.48, 0.23, 0.3, 0.5, 0.68, 0.99, 0.75),
      y = c(0.5, 0.75, -0.3, 0.6, 0.05, 1.05, 0.76, 0.5, 0.6),
      angle = c(0, -23, 0, -36, 0, 0, 25, 0, 39),
      colour = c(rep("1", times = 6), "2", "1", "1")
    )
  # dag coordinates
  coord <-
    tribble(
      ~x, ~y, ~label,
      0.00, 1.00, "Outsourcing",
      1.00, 1.00, "Evaluations",
      0.10, 0.00, "Effort",
      0.50, 0.00, "Care",
      0.88, 0.00, "Authentic"
    )
  # plot path model
  p <-
    ggplot() +
    # labels
    geom_text(data = coord, aes(x = x, y = y, label = label), size = 4.5) +
    # arrows
    place_arrow(x = 0.17, y =  0.09, xend = 0.86, yend =  0.92, ns = TRUE) +
    place_arrow(x = 0.15, y =  1.00, xend = 0.85, yend =  1.00) +
    place_arrow(x = 0.00, y =  0.90, xend = 0.10, yend =  0.10) +
    place_arrow(x = 0.05, y =  0.90, xend = 0.45, yend =  0.10) +
    place_arrow(x = 0.12, y =  0.90, xend = 0.80, yend =  0.10) +
    place_arrow(x = 0.18, y =  0.00, xend = 0.43, yend =  0.00) +
    place_arrow(x = 0.55, y =  0.10, xend = 0.92, yend =  0.90) +
    place_arrow(x = 0.88, y =  0.10, xend = 1.00, yend =  0.90) +
    place_arrow(x = 0.15, y = -0.10, xend = 0.82, yend = -0.10, curved = TRUE) +
    # parameter labels
    geom_text(
      data = pars,
      aes(x = x, y = y, angle = angle, label = Label, colour = colour), 
      size = 1.8
      ) +
    scale_colour_manual(values = c("black", "grey")) +
    # margins and theme
    coord_cartesian(
      xlim = c(-0.1, 1.1),
      ylim = c(-0.6, 1.3)
      ) +
    theme_void() +
    theme(legend.position = "none")
  # save plot
  ggsave(
    plot = p,
    filename = "plots/study6_path.pdf",
    height = 3,
    width = 5
  )
  return(p)
}
