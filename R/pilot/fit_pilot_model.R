# function to fit model to pilot data
fit_pilot_model <- function(pilot_data) {
  # generate formulas for brms
  f <- " ~ 1 + (1 |i| id) + (1 |j| task)"
  bf1 <- bf(as.formula(paste0("social", f)), family = cumulative)
  bf2 <- bf(as.formula(paste0("social_skills", f)), family = cumulative)
  bf3 <- bf(as.formula(paste0("impact_others", f)), family = cumulative)
  bf4 <- bf(as.formula(paste0("consequences", f)), family = cumulative)
  bf5 <- bf(as.formula(paste0("intrinsic_effort", f)), family = cumulative)
  bf6 <- bf(as.formula(paste0("extrinsic_effort", f)), family = cumulative)
  # fit model
  brm(
    formula = bf1 + bf2 + bf3 + bf4 + bf5 + bf6 + set_rescor(FALSE),
    data = pilot_data,
    prior = c(
      prior(normal(0, 2), class = Intercept, resp = social),
      prior(normal(0, 2), class = Intercept, resp = socialskills),
      prior(normal(0, 2), class = Intercept, resp = impactothers),
      prior(normal(0, 2), class = Intercept, resp = consequences),
      prior(normal(0, 2), class = Intercept, resp = intrinsiceffort),
      prior(normal(0, 2), class = Intercept, resp = extrinsiceffort),
      prior(exponential(3), class = sd, resp = social),
      prior(exponential(3), class = sd, resp = socialskills),
      prior(exponential(3), class = sd, resp = impactothers),
      prior(exponential(3), class = sd, resp = consequences),
      prior(exponential(3), class = sd, resp = intrinsiceffort),
      prior(exponential(3), class = sd, resp = extrinsiceffort),
      prior(lkj(4), class = cor, group = id),
      prior(lkj(4), class = cor, group = task)
    ),
    chains = 4,
    cores = 4,
    seed = 2113
  )
}
