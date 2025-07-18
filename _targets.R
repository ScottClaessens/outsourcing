library(crew)
library(targets)
library(tarchetypes)
library(tidyverse)

# set options for targets and source R functions
tar_option_set(
  packages = c("brms", "flextable", "ordinal", "patchwork", "tidyverse")
  )
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
  # create table of tasks
  tar_target(table_tasks, create_table_tasks()),
  
  #### Study 1 ####
  
  # calculate power
  #tar_target(study1_power, calculate_power_study1(n = 300, effect_size = -0.4)),
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
    plot_treatments_overall_study1(study1_data, treatment_means_study1)
    ),
  # plot treatment effects by task
  tar_target(
    plot_treatments_tasks_study1,
    plot_treatments_by_task_study1(study1_data, study1_fit1)
    ),
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
  # create table of treatment differences
  tar_target(
    table_treatment_diffs_study1,
    create_table_treatment_diffs_study1(treatment_means_study1)
  ),
  
  #### Study 2 ####
  
  # study 2 data file
  tar_target(study2_data_file, "data/study2/study2_data_clean.csv", 
             format = "file"),
  # load study 2 data
  tar_target(study2_data, load_study2_data(study2_data_file, 
                                           combined_task_means)),
  # plot chatgpt responses
  tar_target(plot_chatgpt_study2, plot_chatgpt_responses_study2(study2_data)),
  # fit model 1 to study 2 data
  tar_target(study2_fit1, fit_study2_model1(study2_data)),
  # extract treatment means
  tar_target(
    treatment_means_study2,
    extract_treatment_means_study2(study2_fit1)
    ),
  # plot treatments overall
  tar_target(
    plot_treatments_study2,
    plot_treatments_overall_study2(study2_data, treatment_means_study2)
    ),
  # plot treatments by task
  tar_target(
    plot_treatments_tasks_study2,
    plot_treatments_by_task_study2(study2_data, study2_fit1)
  ),
  tar_map(
    values = tibble(
      var = c("social", "socialskills", "impactothers", "consequences", 
              "intrinsiceffort", "extrinsiceffort")
    ),
    # fit model 2 to study 2 data
    tar_target(study2_fit2, fit_study2_model2(study2_data, var)),
    # extract interaction effects for each model
    tar_target(
      interaction_effects_study2,
      extract_interaction_effects_study2(study2_fit2, var)
      )
    ),
  # combined interaction effects
  tar_target(
    combined_interaction_effects_study2,
    bind_rows(
      interaction_effects_study2_social,
      interaction_effects_study2_socialskills,
      interaction_effects_study2_impactothers,
      interaction_effects_study2_consequences,
      interaction_effects_study2_intrinsiceffort,
      interaction_effects_study2_extrinsiceffort
    )
  ),
  # plot interaction effects
  tar_target(
    plot_interactions_study2,
    plot_interaction_effects_study2(combined_interaction_effects_study2)
  ),
  # plot interaction parameters
  tar_target(
    plot_interaction_parameters_study2,
    plot_interaction_pars_study2(
      study2_fit2_social,
      study2_fit2_socialskills,
      study2_fit2_impactothers,
      study2_fit2_consequences,
      study2_fit2_intrinsiceffort,
      study2_fit2_extrinsiceffort
    )
  ),
  # create table of treatment differences and effects
  tar_target(
    table_treatment_diffs_effects_study2,
    create_table_treatment_diffs_effects_study2(treatment_means_study2)
  ),
  
  #### Study 3 ####
  
  # calculate power for study 3 
  #tar_target(study3_power, calculate_power_study3(n = 250, effect_size = -0.7)),
  # study 3 data file
  tar_target(study3_data_file, "data/study3/study3_data_clean.csv",
             format = "file"),
  # load study 3 data
  tar_target(study3_data, load_study3_data(study3_data_file)),
  # plot chatgpt responses
  tar_target(plot_chatgpt_study3, plot_chatgpt_responses_study3(study3_data)),
  # fit models to study 3 data
  tar_target(study3_fit1, fit_study3_model1(study3_data)),
  tar_target(study3_fit2, fit_study3_model2(study3_data)),
  # extract means
  tar_target(
    treatment_means_study3,
    extract_treatment_means_study3(study3_fit1, study3_fit2)
  ),
  # plot results
  tar_target(
    plot_treatments_work_study3,
    plot_treatments_study3_work(study3_data, treatment_means_study3)
  ),
  tar_target(
    plot_treatments_person_study3,
    plot_treatments_study3_person(study3_data, treatment_means_study3)
  ),
  # create table of essay comprehension
  tar_target(
    table_essay_comprehension_study3,
    create_table_essay_comprehension_study3(study3_data)
  ),
  # create table of essay answers
  tar_target(
    table_essay_answers_study3,
    create_table_essay_answers_study3()
  ),
  # create manipulation check table
  tar_target(
    table_manipulation_check_study3,
    create_table_manipulation_check_study3(study3_data)
  ),
  # create table of treatment differences and effects
  tar_target(
    table_treatment_diffs_effects_study3,
    create_table_treatment_diffs_effects_study3(treatment_means_study3)
  ),
  
  #### Study 4 ####
  
  # study 4 data file
  tar_target(study4_data_file, "data/study4/study4_data_clean.csv", 
             format = "file"),
  # load study 4 data
  tar_target(study4_data, load_study4_data(study4_data_file, 
                                           combined_task_means)),
  # plot chatgpt responses
  tar_target(plot_chatgpt_study4, plot_chatgpt_responses_study4(study4_data)),
  # fit model 1 to study 4 data
  tar_target(study4_fit1, fit_study4_model1(study4_data)),
  # extract treatment means
  tar_target(
    treatment_means_study4,
    extract_treatment_means_study4(study4_fit1)
  ),
  # plot treatments overall
  tar_target(
    plot_treatments_study4,
    plot_treatments_overall_study4(study4_data, treatment_means_study4)
  ),
  # plot treatments by task
  tar_target(
    plot_treatments_tasks_study4,
    plot_treatments_by_task_study4(study4_data, study4_fit1)
  ),
  tar_map(
    values = tibble(
      var = c("social", "socialskills", "impactothers", "consequences", 
              "intrinsiceffort", "extrinsiceffort")
    ),
    # fit model 2 to study 4 data
    tar_target(study4_fit2, fit_study4_model2(study4_data, var)),
    # extract interaction effects for each model
    tar_target(
      interaction_effects_study4,
      extract_interaction_effects_study4(study4_fit2, var)
    )
  ),
  # combined interaction effects
  tar_target(
    combined_interaction_effects_study4,
    bind_rows(
      interaction_effects_study4_social,
      interaction_effects_study4_socialskills,
      interaction_effects_study4_impactothers,
      interaction_effects_study4_consequences,
      interaction_effects_study4_intrinsiceffort,
      interaction_effects_study4_extrinsiceffort
    )
  ),
  # plot interaction effects
  tar_target(
    plot_interactions_study4,
    plot_interaction_effects_study4(combined_interaction_effects_study4)
  ),
  # plot interaction parameters
  tar_target(
    plot_interaction_parameters_study4,
    plot_interaction_pars_study4(
      study4_fit2_social,
      study4_fit2_socialskills,
      study4_fit2_impactothers,
      study4_fit2_consequences,
      study4_fit2_intrinsiceffort,
      study4_fit2_extrinsiceffort
    )
  ),
  # create table of treatment differences and effects
  tar_target(
    table_treatment_diffs_effects_study4,
    create_table_treatment_diffs_effects_study4(treatment_means_study4)
  ),
  
  #### Study 5 ####
  
  # study 5 data file
  tar_target(study5_data_file, "data/study5/study5_data_clean.csv", 
             format = "file"),
  # load study 5 data
  tar_target(study5_data, load_study5_data(study5_data_file, 
                                           combined_task_means)),
  # plot chatgpt responses
  tar_target(plot_chatgpt_study5, plot_chatgpt_responses_study5(study5_data)),
  # fit model 1 to study 5 data
  tar_target(study5_fit1, fit_study5_model1(study5_data)),
  # extract treatment means
  tar_target(
    treatment_means_study5,
    extract_treatment_means_study5(study5_fit1)
  ),
  # plot treatments overall
  tar_target(
    plot_treatments_study5,
    plot_treatments_overall_study5(study5_data, treatment_means_study5)
  ),
  # plot treatments by task
  tar_target(
    plot_treatments_tasks_study5,
    plot_treatments_by_task_study5(study5_data, study5_fit1)
  ),
  # plot effect of reasons by task
  tar_target(
    plot_reasons_tasks_study5,
    plot_reasons_by_task_study5(study5_data, study5_fit1)
  ),
  tar_map(
    values = tibble(
      var = c("social", "socialskills", "impactothers", "consequences", 
              "intrinsiceffort", "extrinsiceffort")
    ),
    # fit model 2 to study 5 data
    tar_target(study5_fit2, fit_study5_model2(study5_data, var)),
    # extract interaction effects for each model
    tar_target(
      interaction_effects_study5,
      extract_interaction_effects_study5(study5_fit2, var)
    )
  ),
  # combined interaction effects
  tar_target(
    combined_interaction_effects_study5,
    bind_rows(
      interaction_effects_study5_social,
      interaction_effects_study5_socialskills,
      interaction_effects_study5_impactothers,
      interaction_effects_study5_consequences,
      interaction_effects_study5_intrinsiceffort,
      interaction_effects_study5_extrinsiceffort
    )
  ),
  # plot interaction effects
  tar_target(
    plot_interactions_study5,
    plot_interaction_effects_study5(combined_interaction_effects_study5)
  ),
  # plot interaction parameters
  tar_target(
    plot_interaction_parameters_study5,
    plot_interaction_pars_study5(
      study5_fit2_social,
      study5_fit2_socialskills,
      study5_fit2_impactothers,
      study5_fit2_consequences,
      study5_fit2_intrinsiceffort,
      study5_fit2_extrinsiceffort
    )
  ),
  # create table of treatment differences and effects
  tar_target(
    table_treatment_diffs_effects_study5,
    create_table_treatment_diffs_effects_study5(treatment_means_study5)
  ),
  
  #### Study 6 ####
  
  # calculate power for study 6
  #tar_target(study6_power, calculate_power_study6(n = 400, effect_size = -0.5)),
  # study 6 data file
  tar_target(study6_data_file, "data/study6/study6_data_clean.csv", 
             format = "file"),
  # load study 6 data
  tar_target(study6_data, load_study6_data(study6_data_file)),
  # plot chatgpt responses
  tar_target(plot_chatgpt_study6, plot_chatgpt_responses_study6(study6_data)),
  # fit model 1 to study 6 data
  tar_target(study6_fit1, fit_study6_model1(study6_data)),
  # extract treatment means
  tar_target(
    treatment_means_study6,
    extract_treatment_means_study6(study6_fit1)
  ),
  # plot treatment means
  tar_target(
    plot_treatments_study6,
    plot_treatments_overall_study6(study6_data, treatment_means_study6)
  ),
  # create table of treatment differences
  tar_target(
    table_treatment_diffs_study6,
    create_table_treatment_diffs_study6(treatment_means_study6),
  ),
  # fit model 2 to study 6 data
  tar_target(study6_fit2, fit_study6_model2(study6_data)),
  # plot path model
  tar_target(plot_path_model_study6, plot_path_study6(study6_fit2)),
  # text analysis data file
  tar_target(
    butter_data_file,
    "data/butter/BUTTER-Frequency_ComparisonAllComparisions.csv",
    format = "file"
    ),
  # text analysis data
  tar_target(butter_data, read_csv(butter_data_file)),
  # text analysis results table
  tar_target(
    table_text_analysis_study6,
    create_table_text_analysis_study6(butter_data)
    ),
  
  #### Manuscript ####
  
  # write manuscript file
  tar_quarto(manuscript, "manuscript.qmd", quiet = FALSE)

)
