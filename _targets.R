library(targets)
library(tarchetypes)

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
  # plot results
  tar_target(plot_cors, plot_correlations(fit))
)
