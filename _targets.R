# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("here", "readxl", "janitor", "tidyverse", "base", "scales", "ggtext", "zoo",
                            "performance", "lme4", "marginaleffects", "patchwork", "kableExtra", "bayestestR",
                            "quarto"))

list(
  # Load in data
  tar_target(file, here("data","Zelemiq Data analysis - JW - all data.xlsx"), format = "file"),
  tar_target(data, read_prepare_data(file)),

  # Fit model
  tar_target(model, fit_model(data)),

  # Model checks
  tar_target(model_checks, make_model_checks(model)),

  # Make and save plots
  tar_target(individual_data_plot, plot_individual_data(data)),
  tar_target(individual_data_plot_tiff, make_individual_data_plot_tiff(individual_data_plot)),

  tar_target(model_plot, plot_model(data, model)),
  tar_target(model_plot_tiff, make_model_plot_tiff(model_plot)),

  tar_target(individual_preds_plot, plot_individual_preds(data, model)),
  tar_target(individual_preds_plot_tiff, make_individual_preds_plot_tiff(individual_preds_plot)),

  tar_target(main_plot, combine_plots(individual_data_plot, individual_preds_plot, model_plot)),

  # Add baseline adjustment to data
  tar_target(data_adj, add_adj_data(data)),

  # Fit new model adjusting for each participants baseline
  tar_target(model_adj, fit_model_adj(data_adj)),

  # Model checks
  tar_target(model_adj_checks, make_model_adj_checks(model_adj)),

  # Make and save plots

  tar_target(model_adj_plot, plot_model_adj(data_adj, model_adj)),
  tar_target(model_adj_plot_tiff, make_model_adj_plot_tiff(model_adj_plot)),

  tar_target(individual_adj_preds_plot, plot_individual_adj_preds(data_adj, model_adj)),
  tar_target(individual_adj_preds_plot_tiff, make_individual_adj_preds_plot_tiff(individual_adj_preds_plot)),

  tar_target(main_adj_plot, combine_adj_plots(individual_data_plot, individual_adj_preds_plot, model_adj_plot)),

  # Compare models
  tar_target(model_comparison_2logBF, compare_models(model, model_adj)),

  # Render the report
  tar_quarto(report, "report.qmd")

)
