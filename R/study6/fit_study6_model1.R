# function to fit model 1 to study 6 data
fit_study6_model1 <- function(study6_data) {
  # generate formulas for brms
  f <- " ~ 1 + treatment"
  bf1 <- bf(as.formula(paste0("effort",      f)))
  bf2 <- bf(as.formula(paste0("authentic",   f)))
  bf3 <- bf(as.formula(paste0("care",        f)))
  bf4 <- bf(as.formula(paste0("competent",   f)))
  bf5 <- bf(as.formula(paste0("warm",        f)))
  bf6 <- bf(as.formula(paste0("moral",       f)))
  bf7 <- bf(as.formula(paste0("lazy",        f)))
  bf8 <- bf(as.formula(paste0("trustworthy", f)))
  # fit model
  brm(
    formula = bf1 + bf2 + bf3 + bf4 + bf5 + bf6 + bf7 + bf8 + set_rescor(FALSE),
    data = study6_data,
    family = cumulative,
    prior = c(
      prior(normal(0, 1.5), class = Intercept, resp = effort),
      prior(normal(0, 1.5), class = Intercept, resp = authentic),
      prior(normal(0, 1.5), class = Intercept, resp = care),
      prior(normal(0, 1.5), class = Intercept, resp = competent),
      prior(normal(0, 1.5), class = Intercept, resp = warm),
      prior(normal(0, 1.5), class = Intercept, resp = moral),
      prior(normal(0, 1.5), class = Intercept, resp = lazy),
      prior(normal(0, 1.5), class = Intercept, resp = trustworthy),
      prior(normal(0, 0.5), class = b, resp = effort),
      prior(normal(0, 0.5), class = b, resp = authentic),
      prior(normal(0, 0.5), class = b, resp = care),
      prior(normal(0, 0.5), class = b, resp = competent),
      prior(normal(0, 0.5), class = b, resp = warm),
      prior(normal(0, 0.5), class = b, resp = moral),
      prior(normal(0, 0.5), class = b, resp = lazy),
      prior(normal(0, 0.5), class = b, resp = trustworthy)
    ),
    chains = 4,
    cores = 4,
    seed = 2113
  )
}
