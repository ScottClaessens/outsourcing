# function to plot overall treatment effects for perceptions of work in study 3
plot_treatments_study3_work <- function(study3_data, treatment_means_study3) {
  # task labelling
  study3_data$task <- 
    ifelse(
      study3_data$task == "NonSocial",
      "Non-social",
      as.character(study3_data$task)
    )
  treatment_means_study3$Task <- 
    ifelse(
      treatment_means_study3$Task == "NonSocial",
      "Non-social",
      treatment_means_study3$Task
      )
  # grades as numeric
  study3_data$grade <- as.numeric(study3_data$grade)
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
          jitter.height = ifelse(resp == "reward", 0.05, 0.4),
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
        name = str_to_sentence(str_replace(resp, "_", " ")),
        breaks = 
          if (resp == "reward") {
            seq(0, 1, by = 0.2)
          } else if (resp == "grade") {
            1:5
          } else {
            1:7
          },
        limits = 
          if (resp == "reward") {
            c(0, 1)
          } else if (resp == "grade") {
            c(1, 5)
          } else {
            c(1, 7)
          },
        labels = 
          if (resp == "reward") {
            scales::dollar_format(prefix = "£")
          } else if (resp == "grade") {
            LETTERS[5:1]
          } else {
            waiver()
          },
        oob = scales::squish
      ) +
      xlab(NULL) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        axis.text.y = element_text(
          size = ifelse(resp == "reward", 6, 9)
          )
        )
  }
  # get plots
  pA <- fun("well_written")
  pB <- fun("meaningful")
  pC <- fun("authentic")
  pD <- fun("grade")
  pE <- fun("reward")
  # put together
  design <- "
    123
    456
  "
  out <- 
    pA + pB + pC + pD + pE + guide_area() + 
    plot_layout(design = design, guides = "collect") +
    plot_annotation(tag_levels = "a")
  # add margin
  out <- out & theme(plot.margin = unit(c(0, 0, 15, 0), "pt"))
  # save
  ggsave(
    filename = "plots/study3_treatments_work.pdf",
    plot = out,
    width = 6,
    height = 4
  )
  return(out)
}
