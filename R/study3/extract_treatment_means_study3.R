# function to extract treatment means in study 3
extract_treatment_means_study3 <- function(study3_fit1, study3_fit2) {
  # function to extract treatment means for specific response variable
  extract_fun <- function(resp, model) {
    # new data
    d <- expand_grid(
      treatment = c("Control", "Tool", "Full"),
      task = c("Social", "NonSocial")
      )
    # get fitted values
    f <- fitted(
      object = model,
      newdata = d,
      resp = str_remove(resp, "_"),
      re_formula = NA,
      summary = FALSE
    )
    # calculate posterior means
    post <- matrix(0, nrow = nrow(f), ncol = ncol(f))
    if (resp == "grade") {
      for (i in 1:5) post <- post + (f[, , i] * i)
    } else if (resp != "reward") {
      for (i in 1:7) post <- post + (f[, , i] * i)
    } else {
      post <- f
    }
    # add posterior means to data
    d %>%
      mutate(
        post = lapply(seq_len(ncol(post)), function(i) as.numeric(post[,i]))
      ) %>%
      rowwise() %>%
      transmute(
        Response = resp,
        Treatment = treatment,
        Task = task,
        Estimate = mean(post),
        Est.Error = sd(post),
        Q2.5 = quantile(post, 0.025),
        Q97.5 = quantile(post, 0.975)
      )
  }
  # bind rows
  bind_rows(
    extract_fun("well_written", study3_fit1),
    extract_fun("meaningful",   study3_fit1),
    extract_fun("authentic",    study3_fit1),
    extract_fun("grade",        study3_fit1),
    extract_fun("reward",       study3_fit2),
    extract_fun("competent",    study3_fit1),
    extract_fun("warm",         study3_fit1),
    extract_fun("moral",        study3_fit1),
    extract_fun("lazy",         study3_fit1),
    extract_fun("trustworthy",  study3_fit1)
  )
}
