# function to extract and plot interaction parameters
plot_interaction_pars_study1 <- function(study1_fit2_social,
                                         study1_fit2_socialskills,
                                         study1_fit2_impactothers,
                                         study1_fit2_consequences,
                                         study1_fit2_intrinsiceffort,
                                         study1_fit2_extrinsiceffort) {
  # function to extract interaction parameters
  extract_fun <- function(study1_fit2) {
    # get model summary
    s <- summary(study1_fit2)
    # interaction parameters only
    s$fixed[str_detect(rownames(s$fixed), ":"),] %>%
      rownames_to_column() %>%
      separate(
        rowname,
        into = c("Response", "Predictor", "Effect"),
        sep = "_"
        ) %>%
      mutate(
        Predictor = str_remove(Predictor, "me"),
        Predictor = str_sub(Predictor, 1, nchar(Predictor) / 2),
        Effect = str_remove(Effect, "SEgrEQtask:treatment"),
        Effect = str_remove(Effect, "outsourcing")
      )
  }
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
    bind_rows(
      extract_fun(study1_fit2_social),
      extract_fun(study1_fit2_socialskills),
      extract_fun(study1_fit2_impactothers),
      extract_fun(study1_fit2_consequences),
      extract_fun(study1_fit2_intrinsiceffort),
      extract_fun(study1_fit2_extrinsiceffort)
      ) %>%
    mutate(
      Response = factor(responses[Response], levels = responses),
      Predictor = factor(predictors[Predictor], levels = predictors)
    ) %>%
    ggplot(
      aes(
        x = Estimate,
        xmin = `l-95% CI`,
        xmax = `u-95% CI`,
        y = fct_rev(Predictor),
        colour = Effect
        )
      ) +
    geom_pointrange(
      position = position_dodge(width = 0.3)
    ) +
    ylab(NULL) +
    facet_wrap(
      Response ~ .,
      ncol = 1
      ) +
    theme_minimal() +
    theme(legend.title = element_blank())
  # save
  ggsave(
    filename = "plots/study1_interaction_pars.pdf",
    plot = out,
    height = 6,
    width = 6
  )
  return(out)
}