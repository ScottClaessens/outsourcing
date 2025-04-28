# function to plot reasons effects by task in study 5
plot_reasons_by_task_study5 <- function(study5_data, study5_fit1) {
  # function to extract differences
  extract_diffs <- function(resp) {
    # new data
    d <- expand_grid(
      treatment = c("Control", "AI_Tool_Bad", "AI_Tool_Good",
                    "AI_Full_Bad", "AI_Full_Good"),
      task = sort(unique(study5_data$task))
    )
    # get fitted values
    f <- fitted(
      object = study5_fit1,
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
        Tool = list(AI_Tool_Bad - AI_Tool_Good),
        Full = list(AI_Full_Bad - AI_Full_Good)
      ) %>%
      pivot_longer(
        cols = Tool:Full,
        names_to = "Usage",
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
  # get task ratings
  task_ratings <- 
    study5_data %>%
    group_by(task) %>%
    summarise(social = unique(social))
  # put together all effects
  diffs <-
    bind_rows(
      extract_diffs("competent"),
      extract_diffs("warm"),
      extract_diffs("moral"),
      extract_diffs("lazy"),
      extract_diffs("trustworthy"),
      extract_diffs("care"),
    ) %>%
    mutate(
      Response = factor(Response, level = c("Competent", "Warm", "Moral", 
                                            "Lazy", "Trustworthy", "Care")),
      Usage = ifelse(Usage == "Tool", "Using AI as a tool",
                     "Fully outsourcing to AI"),
      Usage = factor(Usage, levels = c("Using AI as a tool",
                                       "Fully outsourcing to AI"))
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
        colour = Usage
        )
      ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.4
      ) +
    geom_pointrange(
      fatten = 1.5,
      position = position_dodge(width = 0.4)
      ) +
    facet_grid(. ~ Response) +
    labs(
      x = "Estimated difference between bad and good reason conditions",
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
      axis.text.y = element_text(size = 8.5),
      axis.text.x = element_text(size = 7),
      legend.title = element_blank(),
      legend.position = "top",
      plot.margin = unit(c(0, 0, 15, 0), "pt")
    )
  # save
  ggsave(
    filename = "plots/study5_reasons_by_task.pdf",
    plot = p,
    width = 8.5,
    height = 3.5
  )
  return(p)
}