# function to plot overall treatment effects for person perceptions in study 3
plot_treatments_study3_person <- function(study3_data, treatment_means_study3) {
  # task labelling
  study3_data$task <- 
    ifelse(
      study3_data$task == "NonSocial",
      "Non-social",
      "Social"
    )
  treatment_means_study3$Task <- 
    ifelse(
      treatment_means_study3$Task == "NonSocial",
      "Non-social",
      "Social"
      )
  # plotting function
  fun <- function(resp) {
    ggplot() +
      geom_point(
        data = study3_data,
        mapping = aes(
          x = treatment,
          y = !!sym(resp),
          colour = task
        ),
        position = position_jitterdodge(
          dodge.width = 0.7,
          jitter.height = 0.4,
          jitter.width = 0.2
        ),
        alpha = 0.1,
        size = 0.2
      ) +
      geom_pointrange(
        data = filter(treatment_means_study3, Response == resp),
        mapping = aes(
          x = Treatment,
          y = Estimate,
          ymax = `Q97.5`,
          ymin = `Q2.5`,
          colour = Task
        ),
        position = position_dodge(width = 0.7),
        size = 0.4
      ) +
      scale_y_continuous(
        name = str_to_sentence(resp),
        breaks = 1:7,
        limits = c(1, 7),
        oob = scales::squish
      ) +
      xlab(NULL) +
      theme_minimal() +
      theme(legend.title = element_blank())
  }
  # get plots
  pA <- fun("competent")
  pB <- fun("warm")
  pC <- fun("moral")
  pD <- fun("lazy")
  pE <- fun("trustworthy")
  # put together
  design <- "
    123
    456
  "
  out <- 
    pA + pB + pC + pD + pE + guide_area() + 
    plot_layout(design = design, guides = "collect") +
    plot_annotation(tag_levels = "a")
  # save
  ggsave(
    filename = "plots/study3_treatments_person.pdf",
    plot = out,
    width = 6,
    height = 4
  )
  return(out)
}
