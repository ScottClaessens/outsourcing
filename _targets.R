library(targets)
library(tarchetypes)
library(tidyverse)

# set options for targets and source R functions
tar_option_set(packages = c("brms", "tidyverse"))
tar_source()

# targets pipeline
list(
  # data file
  tar_target(data_file, "data/pilot/pilot_data_clean.csv", format = "file"),
  # load data
  tar_target(data, load_data(data_file)),
  # fit model
  tar_target(fit, fit_model(data)),
  # plot correlations
  tar_target(plot_cors, plot_correlations(fit)),
  tar_map(
    values = tibble(
      resp = c("social", "socialskills", "impactothers", "consequences", 
               "intrinsiceffort", "extrinsiceffort")
      ),
    # extract task means
    tar_target(means, extract_means(data, fit, resp)),
    # plot ranking
    tar_target(plot_rank, plot_ranking(data, means, resp))
  )
)
