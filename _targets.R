# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "rstan", "brms", "base", "bayesplot",
                            "tidybayes", "ggh4x", "broom.mixed", "quarto", "marginaleffects",
                            "kableExtra", "patchwork"))

list(
  # Load in data
  tar_target(file, "data/data forced rep study.csv", format = "file"),
  tar_target(data, get_data(file)),

  # Fit model
  tar_target(model_brms, fit_brms_model(data)),

  # Make and save plots
  tar_target(individual_data_plot, plot_individual_data(data)),
  tar_target(model_data_plot, plot_model_data(model_brms, data)),
  tar_target(marg_effs_act_assist_plot, plot_marg_effs_act_assist(model_brms, data)),
  tar_target(marg_effs_role_plot, plot_marg_effs_role(model_brms, data)),
  tar_target(marg_effs_rep_plot, plot_marg_effs_rep(model_brms, data)),
  tar_target(cond_effs_plot, plot_cond_effs(model_brms, data)),

  tar_target(rhat_plot, make_rhat_plot(model_brms)),
  tar_target(trace_plots, make_trace_plots(model_brms)),
  tar_target(pp_check_plot, make_pp_check(model_brms)),

  # Get tidy model summary
  tar_target(tidy_model_brms, get_tidy_model(model_brms)),

  # Render the report
  tar_quarto(report, "report.qmd"),

  # Render the supplementary material
  tar_quarto(diagnostic_plots, "diagnostic_plots.qmd"),

  # Render plots as tiff files for submission
  tar_target(individual_data_plot_tiff, make_individual_data_plot_tiff(individual_data_plot)),
  tar_target(model_data_plot_tiff, make_model_data_plot_tiff(model_data_plot)),
  tar_target(marg_effs_plot_tiff, make_marg_effs_plot_tiff(marg_effs_act_assist_plot, marg_effs_rep_plot, marg_effs_role_plot)),
  tar_target(cond_effs_plot_tiff, make_cond_effs_plot_tiff(cond_effs_plot))


)
