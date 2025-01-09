# function to fit model 2 to study 2 data
fit_study2_model2 <- function(study2_data, var = "social") {
  # generate formulas for brms
  # use me() to include meaasurement error
  f <- paste0(
    " ~ 1 + treatment*me(", var, ", ", var, "_SE, gr = task)",
    " + (1 |i| id) + (1 + treatment |j| task)"
    )
  bf1 <- bf(as.formula(paste0("competent",   f)), family = cumulative)
  bf2 <- bf(as.formula(paste0("warm",        f)), family = cumulative)
  bf3 <- bf(as.formula(paste0("moral",       f)), family = cumulative)
  bf4 <- bf(as.formula(paste0("lazy",        f)), family = cumulative)
  bf5 <- bf(as.formula(paste0("trustworthy", f)), family = cumulative)
  # fit model
  brm(
    formula = bf1 + bf2 + bf3 + bf4 + bf5 + 
      set_rescor(FALSE) + set_mecor(FALSE),
    data = study2_data,
    prior = c(
      prior(normal(0, 1.5), class = Intercept, resp = competent),
      prior(normal(0, 1.5), class = Intercept, resp = warm),
      prior(normal(0, 1.5), class = Intercept, resp = moral),
      prior(normal(0, 1.5), class = Intercept, resp = lazy),
      prior(normal(0, 1.5), class = Intercept, resp = trustworthy),
      prior(normal(0, 0.5), class = b, resp = competent),
      prior(normal(0, 0.5), class = b, resp = warm),
      prior(normal(0, 0.5), class = b, resp = moral),
      prior(normal(0, 0.5), class = b, resp = lazy),
      prior(normal(0, 0.5), class = b, resp = trustworthy),
      prior(exponential(4), class = sd, resp = competent),
      prior(exponential(4), class = sd, resp = warm),
      prior(exponential(4), class = sd, resp = moral),
      prior(exponential(4), class = sd, resp = lazy),
      prior(exponential(4), class = sd, resp = trustworthy),
      prior(lkj(5), class = cor, group = id),
      prior(lkj(5), class = cor, group = task),
      prior(normal(0, 0.5), class = meanme),
      prior(exponential(4), class = sdme)
    ),
    chains = 4,
    cores = 4,
    init = 0,
    seed = 2113
  )
}