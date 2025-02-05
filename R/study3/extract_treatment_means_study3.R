# function to extract treatment means from model 1 in study 3
extract_treatment_means_study3 <- function(study3_fit1) {
  # function to extract treatment means for specific response variable
  extract_fun <- function(resp) {
    # new data
    d <- expand_grid(
      treatment = c("Control", "Tool", "Full"),
      task = c("NonSocial", "Social")
      )
    # get fitted values
    f <- fitted(
      object = study3_fit1,
      newdata = d,
      resp = resp,
      re_formula = NA,
      summary = FALSE
    )
    # calculate posterior means
    post <- matrix(0, nrow = nrow(f), ncol = ncol(f))
    if (resp == "grade") {
      for (i in 1:5) post <- post + (f[, , i] * i)
    } else {
      for (i in 1:7) post <- post + (f[, , i] * i)
    }
    # add posterior means to data
    d %>%
      mutate(
        post = lapply(seq_len(ncol(post)), function(i) as.numeric(post[,i]))
      ) %>%
      rowwise() %>%
      transmute(
        Response = str_to_title(resp),
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
    extract_fun("wellwritten"),
    extract_fun("meaningful"),
    extract_fun("authentic"),
    extract_fun("grade"),
    extract_fun("warm"),
    extract_fun("moral"),
    extract_fun("lazy"),
    extract_fun("trustworthy")
  )
}
