library(targets)
library(tarchetypes)
library(tidyverse)

# set options for targets and source R functions
tar_option_set(packages = c("brms", "ordinal", "patchwork", "tidyverse"))
tar_source()

# targets pipeline
list(
  
  #### Pilot study ####
  
  ## pilot data file
  #tar_target(pilot_data_file, "data/pilot/pilot_data_clean.csv", format = "file"),
  ## load pilot data
  #tar_target(pilot_data, load_pilot_data(pilot_data_file)),
  ## fit model
  #tar_target(pilot_fit, fit_pilot_model(pilot_data)),
  ## plot task correlations
  #tar_target(plot_task_cors, plot_task_correlations(pilot_fit)),
  #tar_map(
  #  values = tibble(
  #    resp = c("social", "socialskills", "impactothers", "consequences", 
  #             "intrinsiceffort", "extrinsiceffort")
  #    ),
  #  # extract task means
  #  tar_target(task_means, extract_task_means(pilot_data, pilot_fit, resp)),
  #  # plot task ranking
  #  tar_target(plot_task_rank, plot_task_ranking(pilot_data, task_means, resp))
  #),
  
  #### Study 1 ####
  
  # calculate power
  #tar_target(study1_power, calculate_power_study1(n = 300, effect_size = -0.4)),
  # study 1 data file
  tar_target(study1_data_file, "data/study1/study1_data_clean.csv", format = "file"),
  # load study 1 data
  tar_target(study1_data, load_study1_data(study1_data_file)),
  # plot chatgpt responses
  tar_target(plot_chatgpt, plot_chatgpt_responses(study1_data)),
  # fit model 1 to study 1 data
  tar_target(study1_fit1, fit_study1_model1(study1_data)),
  # extract treatment means
  tar_target(treatment_means, extract_treatment_means_study1(study1_fit1)),
  # plot treatments overall
  tar_target(plot_treatments, plot_treatments_overall(study1_data, treatment_means))
  
)
