# function to extract effect sizes at each level of a given moderator
extract_interaction_effects_study4 <- function(study4_fit2, var) {
  # new data
  d <-
    expand_grid(
      treatment = c("Control", "AI_Standard_LowEffort", "AI_Standard_HighEffort",
                    "AI_Personalised_LowEffort", "AI_Personalised_HighEffort"),
      pred = seq(1, 7, length.out = 100),
      pred_SE = 0.000001 # essentially no measurement error
    ) %>%
    rename_with(~ var, pred) %>%
    rename_with(~ paste0(var, "_SE"), pred_SE)
  # function to extract treatment means for specific response variable
  extract_fun <- function(resp) {
    # get fitted values
    f <- fitted(
      object = study4_fit2,
      newdata = d,
      resp = resp,
      re_formula = NA,
      allow_new_levels = TRUE,
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
        pred = !!sym(var),
        Standard_LowEffort      = list(`AI_Standard_LowEffort`      - Control),
        Standard_HighEffort     = list(`AI_Standard_HighEffort`     - Control),
        Personalised_LowEffort  = list(`AI_Personalised_LowEffort`  - Control),
        Personalised_HighEffort = list(`AI_Personalised_HighEffort` - Control)
      ) %>%
      pivot_longer(
        cols = Standard_LowEffort:Personalised_HighEffort,
        names_to = "effect",
        values_to = "post"
        ) %>%
      rowwise() %>%
      transmute(
        resp = resp,
        var = var,
        pred = pred,
        effect = str_replace(effect, "_", "-"),
        estimate = mean(post),
        lower = quantile(post, 0.025),
        upper = quantile(post, 0.975)
      ) %>%
      ungroup()
  }
  bind_rows(
    extract_fun("competent"),
    extract_fun("warm"),
    extract_fun("moral"),
    extract_fun("lazy"),
    extract_fun("trustworthy")
  )
}
