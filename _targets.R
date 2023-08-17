# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("here", "readxl", "janitor", "tidyverse", "base", "scales", "ggtext", "zoo",
                            "performance", "lme4"))

list(
  # Load in data
  tar_target(file, here("data","Zelemiq Data analysis - JW - all data.xlsx"), format = "file"),
  tar_target(data, read_prepare_data(file)),

  # Fit model
  tar_target(model, fit_model(data)),


  # Make and save plots
  tar_target(individual_data_plot, plot_individual_data(data)),
  tar_target(individual_data_plot_tiff, make_individual_data_plot_tiff(individual_data_plot))


)
