# function to run simulated power analysis for study 3
calculate_power_study3 <- function(n = 100, effect_size = -0.5, nsims = 100) {
  # function to simulate data
  simulate_data <- function(n) {
    tibble(
      id = 1:n,
      condition = sample(
        c("control", "treatment"), 
        size = n,
        replace = TRUE
        ),
      answer = sample(
        paste0("answer", 1:3),
        size = n,
        replace = TRUE
      ),
      rating = sample(1:7, size = n, replace = TRUE)
    )
  }
  # get dummy dataset
  d <- simulate_data(n)
  # get model priors
  prior_fit <-
    brm(
      formula = rating ~ 1 + condition + (1 + condition | answer),
      data = d,
      family = cumulative,
      prior = c(
        # this is the effect size
        prior_string(paste0("normal(", effect_size, ", 0.02)"), 
                     class = "b", coef = "conditiontreatment"),
        # other priors
        prior(normal(-2.0, 0.02), class = Intercept, coef = 1),
        prior(normal(-0.6, 0.02), class = Intercept, coef = 2),
        prior(normal( 0.2, 0.02), class = Intercept, coef = 3),
        prior(normal( 1.0, 0.02), class = Intercept, coef = 4),
        prior(normal( 2.1, 0.02), class = Intercept, coef = 5),
        prior(normal( 3.0, 0.02), class = Intercept, coef = 6),
        prior(normal( 0.1, 0.02), class = sd),
        prior(lkj(4), class = cor)
      ),
      sample_prior = "only",
      cores = 4
    )
  # set up power results data frame
  power <- data.frame(
    mean_difference = rep(NA, nsims),
    cohens_d = rep(NA, nsims),
    power = rep(NA, nsims)
  )
  # simulate multiple datasets from model
  for (i in 1:nsims) {
    dd <- 
      simulate_data(n) %>% 
      dplyr::select(-rating)
    y <- posterior_predict(prior_fit, newdata = as.data.frame(dd))[1,]
    dd <- mutate(dd, rating = as.ordered(y))
    # fit clmm (frequentist for speed)
    m <- clmm(
      formula = rating ~ 1 + condition + (1 + condition | answer),
      data = dd
    )
    # record empirical mean difference on original scale
    power$mean_difference[i] <- 
      mean(as.numeric(dd$rating[dd$condition == "treatment"])) -
      mean(as.numeric(dd$rating[dd$condition == "control"]))
    # record cohen's d
    power$cohens_d[i] <- power$mean_difference[i] / sqrt(
      (sd(as.numeric(dd$rating[dd$condition == "treatment"]))^2 +
        sd(as.numeric(dd$rating[dd$condition == "control"]))^2) / 2
    )
    # extract coefficient
    power$power[i] <- 
      summary(m)$coefficients["conditiontreatment","Pr(>|z|)"] < 0.05 &
      summary(m)$coefficients["conditiontreatment","Estimate"] < 0
  }
  return(power)
}
