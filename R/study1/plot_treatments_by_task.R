# function to plot treatment effects by task
plot_treatments_by_task <- function(study1_data, study1_fit1) {
  # function to extract differences
  extract_diffs <- function(resp) {
    # new data
    d <- expand_grid(
      treatment = c("Control", "AI outsourcing", "Human outsourcing"),
      task = sort(unique(study1_data$task))
    )
    # get fitted values
    f <- fitted(
      object = study1_fit1,
      newdata = d,
      resp = resp,
      re_formula = ~ 1 + treatment + (1 + treatment | task),
      summary = FALSE
    )
    # calculate posterior means
    post <- matrix(0, nrow = nrow(f), ncol = ncol(f))
    for (i in 1:7) post <- post + (f[, , i] * i)
    # add posterior means to data
    d %>%
      mutate(
        post = lapply(seq_len(ncol(post)), function(i) as.numeric(post[,i]))
      ) %>%
      pivot_wider(
        names_from = treatment,
        values_from = post
      ) %>%
      rowwise() %>%
      transmute(
        Response = str_to_title(resp),
        Task = task,
        AI = list(`AI outsourcing` - Control),
        Human = list(`Human outsourcing` - Control)
      ) %>%
      pivot_longer(
        cols = c(AI, Human),
        names_to = "Effect",
        values_to = "post"
      ) %>%
      rowwise() %>%
      mutate(
        Estimate = mean(post),
        Est.Error = sd(post),
        Q2.5 = quantile(post, 0.025),
        Q97.5 = quantile(post, 0.975)
      ) %>%
      dplyr::select(!post)
  }
  # put together all effects
  diffs <-
    bind_rows(
      extract_diffs("competent"),
      extract_diffs("warm"),
      extract_diffs("moral"),
      extract_diffs("lazy"),
      extract_diffs("trustworthy")
    ) %>%
    mutate(
      Response = factor(Response, level = c("Competent", "Warm", "Moral", 
                                            "Lazy", "Trustworthy"))
      )
  # plot treatment effects split by task
  p <-
    ggplot(
      data = diffs,
      mapping = aes(
        x = Estimate,
        xmin = Q2.5,
        xmax = Q97.5,
        y = fct_rev(Task),
        colour = Effect
        )
      ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
      ) +
    geom_pointrange(fatten = 3) +
    facet_grid(. ~ Response) +
    labs(
      x = "Estimated difference from control condition",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0.8, "lines"),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
        ),
      axis.text.y = element_text(size = 7.5),
      legend.title = element_blank(),
      legend.position = "top"
    )
  # save
  ggsave(
    filename = "plots/study1_treatments_by_task.pdf",
    plot = p,
    width = 8.5,
    height = 5
  )
  return(p)
}