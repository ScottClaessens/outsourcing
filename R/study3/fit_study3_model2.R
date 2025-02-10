# fit model 2 to study 3 data
fit_study3_model2 <- function(study3_data) {
  # generate formula for brms
  f <- bf(
    reward ~ 1 + treatment*task + (1 + treatment | answer),
    phi ~ 1 + treatment*task + (1 + treatment | answer),
    zoi ~ 1 + treatment*task + (1 + treatment | answer),
    coi ~ 1 + treatment*task + (1 + treatment | answer),
    family = zero_one_inflated_beta
  )
  # fit model
  brm(
    formula = f,
    data = study3_data,
    prior = c(
      prior(normal(0, 1), class = Intercept),
      prior(normal(0, 1), class = Intercept, dpar = phi),
      prior(normal(0, 1), class = Intercept, dpar = zoi),
      prior(normal(0, 1), class = Intercept, dpar = coi),
      prior(normal(0, 1), class = b),
      prior(normal(0, 1), class = b, dpar = phi),
      prior(normal(0, 1), class = b, dpar = zoi),
      prior(normal(0, 1), class = b, dpar = coi),
      prior(exponential(2), class = sd),
      prior(exponential(2), class = sd, dpar = phi),
      prior(exponential(2), class = sd, dpar = zoi),
      prior(exponential(2), class = sd, dpar = coi),
      prior(lkj(3), class = cor, group = answer)
    ),
    chains = 4,
    cores = 4,
    control = list(adapt_delta = 0.99),
    seed = 2113
  )
}
