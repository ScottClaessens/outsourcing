# function to plot treatment effects by task in study 4
plot_treatments_by_task_study4 <- function(study4_data, study4_fit1) {
  # function to extract differences
  extract_diffs <- function(resp) {
    # new data
    d <- expand_grid(
      treatment = c("Control", "AI_Standard_LowEffort", "AI_Standard_HighEffort",
                    "AI_Personalised_LowEffort", "AI_Personalised_HighEffort"),
      task = sort(unique(study4_data$task))
    )
    # get fitted values
    f <- fitted(
      object = study4_fit1,
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
        Standard_LowEffort      = list(AI_Standard_LowEffort      - Control),
        Standard_HighEffort     = list(AI_Standard_HighEffort     - Control),
        Personalised_LowEffort  = list(AI_Personalised_LowEffort  - Control),
        Personalised_HighEffort = list(AI_Personalised_HighEffort - Control)
      ) %>%
      pivot_longer(
        cols = Standard_LowEffort:Personalised_HighEffort,
        names_to = "Effect",
        values_to = "post"
      ) %>%
      separate(
        col = Effect,
        into = c("AI Type", "Effort"),
        sep = "_"
      ) %>%
      rowwise() %>%
      mutate(
        Effort = ifelse(Effort == "LowEffort", "Low effort", "High effort"),
        Estimate = mean(post),
        Est.Error = sd(post),
        Q2.5 = quantile(post, 0.025),
        Q97.5 = quantile(post, 0.975)
      ) %>%
      dplyr::select(!post)
  }
  # get task ratings
  task_ratings <- 
    study4_data %>%
    group_by(task) %>%
    summarise(social = unique(social))
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
                                            "Lazy", "Trustworthy")),
      Effort = factor(Effort, levels = c("Low effort", "High effort")),
      `AI Type` = ifelse(`AI Type` == "Standard", "Standard LLM",
                     "Personalised LLM"),
      `AI Type` = factor(`AI Type`, levels = c("Standard LLM",
                                               "Personalised LLM"))
      ) %>%
    # link data on task ratings
    left_join(task_ratings, by = c("Task" = "task"))
  # plot treatment effects split by task
  p <-
    ggplot(
      data = diffs,
      mapping = aes(
        x = Estimate,
        xmin = Q2.5,
        xmax = Q97.5,
        y = fct_reorder(Task, social),
        colour = Effort
        )
      ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
      ) +
    geom_pointrange(fatten = 2) +
    facet_grid(`AI Type` ~ Response) +
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
      panel.grid.minor.x = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3),
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 6.5),
      legend.title = element_blank(),
      legend.position = "top"
    )
  # save
  ggsave(
    filename = "plots/study4_treatments_by_task.pdf",
    plot = p,
    width = 8.5,
    height = 5
  )
  return(p)
}