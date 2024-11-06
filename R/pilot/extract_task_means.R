# function to extract task means for a specific response variable
extract_task_means <- function(pilot_data, pilot_fit, resp = "social") {
  # new data
  d <- tibble(task = levels(pilot_data$task))
  # get fitted values
  f <- fitted(
    object = pilot_fit,
    newdata = d,
    resp = resp,
    re_formula = ~ 1 + (1 | task),
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
    rowwise() %>%
    mutate(
      Estimate = mean(post),
      Est.Error = sd(post),
      Q2.5 = quantile(post, 0.025),
      Q97.5 = quantile(post, 0.975)
    ) %>%
    rename(Task = task) %>%
    dplyr::select(!post) %>%
    arrange(Estimate)
}
