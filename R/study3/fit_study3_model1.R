# fit model 1 to study 3 data
fit_study3_model1 <- function(study3_data) {
  # generate formulas for brms
  f <- " ~ 1 + treatment*task + (1 + treatment |i| answer)"
  bf1 <- bf(as.formula(paste0("well_written", f)), family = cumulative)
  bf2 <- bf(as.formula(paste0("meaningful",   f)), family = cumulative)
  bf3 <- bf(as.formula(paste0("authentic",    f)), family = cumulative)
  bf4 <- bf(as.formula(paste0("grade",        f)), family = cumulative)
  bf5 <- bf(as.formula(paste0("competent",    f)), family = cumulative)
  bf6 <- bf(as.formula(paste0("warm",         f)), family = cumulative)
  bf7 <- bf(as.formula(paste0("moral",        f)), family = cumulative)
  bf8 <- bf(as.formula(paste0("lazy",         f)), family = cumulative)
  bf9 <- bf(as.formula(paste0("trustworthy",  f)), family = cumulative)
  # fit model
  brm(
    formula = bf1 + bf2 + bf3 + bf4 + bf5 + 
      bf6 + bf7 + bf8 + bf9 + set_rescor(FALSE),
    data = study3_data,
    prior = c(
      prior(normal(0, 1.5), class = Intercept, resp = wellwritten),
      prior(normal(0, 1.5), class = Intercept, resp = meaningful),
      prior(normal(0, 1.5), class = Intercept, resp = authentic),
      prior(normal(0, 1.5), class = Intercept, resp = grade),
      prior(normal(0, 1.5), class = Intercept, resp = competent),
      prior(normal(0, 1.5), class = Intercept, resp = warm),
      prior(normal(0, 1.5), class = Intercept, resp = moral),
      prior(normal(0, 1.5), class = Intercept, resp = lazy),
      prior(normal(0, 1.5), class = Intercept, resp = trustworthy),
      prior(normal(0, 0.5), class = b, resp = wellwritten),
      prior(normal(0, 0.5), class = b, resp = meaningful),
      prior(normal(0, 0.5), class = b, resp = authentic),
      prior(normal(0, 0.5), class = b, resp = grade),
      prior(normal(0, 0.5), class = b, resp = competent),
      prior(normal(0, 0.5), class = b, resp = warm),
      prior(normal(0, 0.5), class = b, resp = moral),
      prior(normal(0, 0.5), class = b, resp = lazy),
      prior(normal(0, 0.5), class = b, resp = trustworthy),
      prior(exponential(2), class = sd, resp = wellwritten),
      prior(exponential(2), class = sd, resp = meaningful),
      prior(exponential(2), class = sd, resp = authentic),
      prior(exponential(2), class = sd, resp = grade),
      prior(exponential(2), class = sd, resp = competent),
      prior(exponential(2), class = sd, resp = warm),
      prior(exponential(2), class = sd, resp = moral),
      prior(exponential(2), class = sd, resp = lazy),
      prior(exponential(2), class = sd, resp = trustworthy),
      prior(lkj(2), class = cor, group = answer)
    ),
    chains = 4,
    cores = 4,
    init = 0,
    control = list(adapt_delta = 0.9),
    seed = 2113
  )
}
