# function to fit model 2 to study 6 data
fit_study6_model2 <- function(study6_data) {
  # wrangle data
  study6_data <-
    study6_data %>%
    # ensure all variables are ordered factors
    mutate(across(c(treatment, effort:lazy), ordered)) %>%
    # add empty evaluation variable
    mutate(evaluation = as.numeric(NA))
  # generate formulas for brms
  bf1 <- bf(effort      ~ 1 + mo(treatment),              family = cumulative)
  bf2 <- bf(authentic   ~ 1 + mo(treatment) + mo(effort), family = cumulative)
  bf3 <- bf(care        ~ 1 + mo(treatment) + mo(effort), family = cumulative)
  bf4 <- bf(competent   ~ 1 + mi(evaluation),             family = cumulative)
  bf5 <- bf(warm        ~ 1 + mi(evaluation),             family = cumulative)
  bf6 <- bf(moral       ~ 1 + mi(evaluation),             family = cumulative)
  bf7 <- bf(lazy        ~ 1 + mi(evaluation),             family = cumulative)
  bf8 <- bf(trustworthy ~ 1 + mi(evaluation),             family = cumulative)
  bf9 <- bf(evaluation | mi() ~ 0 + mo(treatment) + mo(effort) + 
              mo(authentic) + mo(care),
            family = gaussian)
  # fit model
  brm(
    formula = bf1 + bf2 + bf3 + bf4 + bf5 + 
      bf6 + bf7 + bf8 + bf9 + set_rescor(FALSE),
    data = study6_data,
    prior = c(
      prior(normal(0, 1.0), class = Intercept, resp = effort),
      prior(normal(0, 1.0), class = Intercept, resp = authentic),
      prior(normal(0, 1.0), class = Intercept, resp = care),
      prior(normal(0, 1.0), class = Intercept, resp = competent),
      prior(normal(0, 1.0), class = Intercept, resp = warm),
      prior(normal(0, 1.0), class = Intercept, resp = moral),
      prior(normal(0, 1.0), class = Intercept, resp = lazy),
      prior(normal(0, 1.0), class = Intercept, resp = trustworthy),
      prior(normal(0, 0.5), class = b, resp = effort),
      prior(normal(0, 0.5), class = b, resp = authentic),
      prior(normal(0, 0.5), class = b, resp = care),
      prior(constant(1),    class = b, resp = competent),
      prior(normal(0, 0.5), class = b, resp = warm),
      prior(normal(0, 0.5), class = b, resp = moral),
      prior(normal(0, 0.5), class = b, resp = lazy),
      prior(normal(0, 0.5), class = b, resp = trustworthy),
      prior(normal(0, 0.5), class = b, resp = evaluation),
      prior(exponential(4), class = sigma, resp = evaluation)
    ),
    chains = 4,
    cores = 4,
    iter = 6000,
    seed = 2113
  )
}
