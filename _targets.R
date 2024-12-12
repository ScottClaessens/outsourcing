library(targets)
library(tarchetypes)
library(tidyverse)

# set options for targets and source R functions
tar_option_set(packages = c("brms", "ordinal", "patchwork", "tidyverse"))
tar_source()

# targets pipeline
list(
  
  #### Pilot study ####
  
  # pilot data file
  tar_target(pilot_data_file, "data/pilot/pilot_data_clean.csv", 
             format = "file"),
  # load pilot data
  tar_target(pilot_data, load_pilot_data(pilot_data_file)),
  # fit model
  tar_target(pilot_fit, fit_pilot_model(pilot_data)),
  # plot task correlations
  tar_target(plot_task_cors, plot_task_correlations(pilot_fit)),
  tar_map(
    values = tibble(
      resp = c("social", "socialskills", "impactothers", "consequences", 
               "intrinsiceffort", "extrinsiceffort")
      ),
    # extract task means
    tar_target(task_means, extract_task_means(pilot_data, pilot_fit, resp)),
    # plot task ranking
    tar_target(plot_task_rank, plot_task_ranking(pilot_data, task_means, resp))
  ),
  # combined task means for linking to study 1 data
  tar_target(
    combined_task_means,
    combine_task_means(
      task_means_social, task_means_socialskills, task_means_impactothers,
      task_means_consequences, task_means_intrinsiceffort,
      task_means_extrinsiceffort
      )
    ),
  
  #### Study 1 ####
  
  # calculate power
  tar_target(study1_power, calculate_power_study1(n = 300, effect_size = -0.4)),
  # study 1 data file
  tar_target(study1_data_file, "data/study1/study1_data_clean.csv", 
             format = "file"),
  # load study 1 data
  tar_target(study1_data, load_study1_data(study1_data_file, 
                                           combined_task_means)),
  # plot chatgpt responses
  tar_target(plot_chatgpt_study1, plot_chatgpt_responses_study1(study1_data)),
  # fit model 1 to study 1 data
  tar_target(study1_fit1, fit_study1_model1(study1_data)),
  # extract treatment means
  tar_target(
    treatment_means_study1,
    extract_treatment_means_study1(study1_fit1)
    ),
  # plot treatments overall
  tar_target(
    plot_treatments_study1,
    plot_treatments_overall_study1(study1_data, treatment_means_study1)),
  # plot treatment effects by task
  tar_target(
    plot_treatments_tasks_study1,
    plot_treatments_by_task_study1(study1_data, study1_fit1)),
  tar_map(
    values = tibble(
      var = c("social", "socialskills", "impactothers", "consequences", 
              "intrinsiceffort", "extrinsiceffort")
      ),
    # fit model 2 to study 1 data
    tar_target(study1_fit2, fit_study1_model2(study1_data, var)),
    # extract interaction effects for each model
    tar_target(
      interaction_effects_study1,
      extract_interaction_effects_study1(study1_fit2, var)
      )
  ),
  # combined interaction effects
  tar_target(
    combined_interaction_effects_study1,
    bind_rows(
      interaction_effects_study1_social,
      interaction_effects_study1_socialskills,
      interaction_effects_study1_impactothers,
      interaction_effects_study1_consequences,
      interaction_effects_study1_intrinsiceffort,
      interaction_effects_study1_extrinsiceffort
      )
    ),
  # plot interaction effects
  tar_target(
    plot_interactions_study1,
    plot_interaction_effects_study1(combined_interaction_effects_study1)
    ),
  # plot interaction parameters
  tar_target(
    plot_interaction_parameters_study1,
    plot_interaction_pars_study1(
      study1_fit2_social,
      study1_fit2_socialskills,
      study1_fit2_impactothers,
      study1_fit2_consequences,
      study1_fit2_intrinsiceffort,
      study1_fit2_extrinsiceffort
    )
  ),
  
  #### Study 2 ####
  
  # study 2 data file
  tar_target(study2_data_file, "data/study2/study2_data_clean.csv", 
             format = "file"),
  # load study 2 data
  tar_target(study2_data, load_study2_data(study2_data_file, 
                                           combined_task_means)),
  # fit model 1 to study 2 data
  tar_target(study2_fit1, fit_study2_model1(study2_data)),
  # extract treatment means
  tar_target(
    treatment_means_study2,
    extract_treatment_means_study2(study2_fit1)
    )
)
