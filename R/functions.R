# Functions for targets
normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

read_prepare_data <- function(file) {
  # file including path if needed
  file <- here("data","Zelemiq Data analysis - JW - all data.xlsx")
  # read the sheets and only keep the Survey sheets
  sheets <- excel_sheets(file)
  sheets <- sheets[grep("P", sheets)]

  # read the data, only first 10 columns (A:J)
  data <- lapply(sheets, read_excel, path = file, range = cell_cols(c("B","C","E"))) %>%
    bind_rows(.id = "id") %>%
    rowid_to_column() %>%
    clean_names() %>%
    group_by(id) %>%
    mutate(max_lactate = max(lactate, na.rm=TRUE)) %>%
    slice(1:max(which(lactate == max_lactate))) %>%
    mutate(time_norm = normalise(rowid),
           zelemiq_avg = rollmeanr(zelemiq_raw, 10, fill=NA)) %>%
    mutate(zelemiq_raw_z = (zelemiq_raw - mean(zelemiq_raw, na.rm=TRUE))/sd(zelemiq_raw, na.rm=TRUE),
           zelemiq_avg_z = (zelemiq_avg - mean(zelemiq_avg, na.rm=TRUE))/sd(zelemiq_avg, na.rm=TRUE),
           lactate_z = (lactate - mean(lactate, na.rm=TRUE))/sd(lactate, na.rm=TRUE)) |>
    ungroup() |>
    select(id, time_norm, zelemiq_raw, zelemiq_raw_z, zelemiq_avg, zelemiq_avg_z, lactate, lactate_z)

}

plot_individual_data <- function(data) {
  # rescale and plot time series
  # ylim.prim <- c(0,8)
  # ylim.sec <- c(0.4,0.6)
  # b <- diff(ylim.prim)/diff(ylim.sec)
  # a <- ylim.prim[1] - b*ylim.sec[1]

  data |>
    ggplot(aes(x=time_norm, y=lactate_z)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(y = zelemiq_raw_z), color = "#56B4E9",  alpha = 0.1) + # zelemiq
    geom_smooth(aes(y = zelemiq_raw_z), color = "#56B4E9", se=FALSE) + # zelemiq
    geom_point(color = "#D55E00", alpha = 0.5) + # lactate
    geom_smooth(se=FALSE, color = "#D55E00") + # lactate
    scale_x_continuous("Time (normalised percentage)", labels = percent) +
    facet_wrap("id", nrow = 2) +
    labs(y = "Standardised Value",
         title = "Standardised values for <span style = 'color: #56B4E9;'>Zelemiq Ltd Output</span> and <span style = 'color: #D55E00;'>Blood Lactate from Biosen C-Line</span> during the incremental test",
         caption = "Note: Curves are LOESS smooths") +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title = element_markdown())
}

make_individual_data_plot_tiff <- function(individual_data_plot) {

  ggsave("plots/individual_data_plot.tiff", individual_data_plot, width = 10, height = 5, device = "tiff", dpi = 300)

}

fit_model <- function(data) {
  model <- brm(lactate_z ~ zelemiq_avg_z + I(zelemiq_avg_z^2) + (zelemiq_avg_z | id),
               data = data)
}

make_model_checks_tiff <- function(model) {
  checks <- check_model(model)

  plot(checks)

  ggsave("plots/model_checks.tiff", width = 10, height = 10, device = "tiff", dpi = 300)

}

plot_model <- function(data, model) {
  model_epred <- predictions(model,
                             newdata = datagrid(id = NA,
                                                zelemiq_avg_z = seq(min(data$zelemiq_avg_z, na.rm = TRUE),
                                                                    max(data$zelemiq_avg_z, na.rm = TRUE),
                                                                    by=0.01)),
                             re_formula = NA,
                             type = "response")

  model_cond_epred <- predictions(model,
                                  newdata = datagrid(id = NA,
                                                     zelemiq_avg_z = seq(min(data$zelemiq_avg_z, na.rm = TRUE),
                                                                         max(data$zelemiq_avg_z, na.rm = TRUE),
                                                                         by=0.01)),
                                  re_formula = NULL,
                                  type = "response")

  model_preds <- predictions(model,
                             newdata = datagrid(id = NA,
                                                zelemiq_avg_z = seq(min(data$zelemiq_avg_z, na.rm = TRUE),
                                                                    max(data$zelemiq_avg_z, na.rm = TRUE),
                                                                    by=0.01)),
                             type = "prediction")

  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(data=model_preds, aes(x = zelemiq_avg_z, ymin=conf.low, ymax=conf.high), fill = "#CCCCCC", alpha = 0.5) +
    geom_ribbon(data=model_cond_epred, aes(x = zelemiq_avg_z, ymin=conf.low, ymax=conf.high), fill = "#999999", alpha = 0.5) +
    geom_ribbon(data=model_epred, aes(x = zelemiq_avg_z, ymin=conf.low, ymax=conf.high), fill = "#333333", alpha = 0.5) +
    geom_line(data=model_epred, aes(x = zelemiq_avg_z, y = estimate), linewidth=1) +
    geom_point(data=data, aes(x = zelemiq_avg_z, y=lactate_z), alpha=1) +
    labs(x = "Zelemiq Ltd Output (standardised)",
         y = bquote("Blood Lactate (standardised)"),
         title = "Model predictions",
         subtitle = "Interval estimates are, from widest to narrowest: posterior predictions, conditional effects for participants, and global grand mean"
    ) +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title = element_markdown())
}

make_model_plot_tiff <- function(model_plot) {

  ggsave("plots/model_plot.tiff", model_plot, width = 10, height = 10, device = "tiff", dpi = 300)

}

plot_individual_preds <- function(data, model) {

  ind_preds <- predictions(model,
                           newdata = datagrid(id = data$id,
                                              zelemiq_avg_z = seq(min(data$zelemiq_avg_z, na.rm = TRUE),
                                                                  max(data$zelemiq_avg_z, na.rm = TRUE),
                                                                  by=0.01)),
                           re_formula = NULL)

  ggplot(ind_preds, aes(x = zelemiq_avg_z, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
    geom_line(size=1) +
    geom_point(data=data, aes(y=lactate_z), alpha=0.25) +
    labs(x = "Zelemiq Ltd Output (standardised)",
         y = bquote("Blood Lactate (standardised)"),
         title = "Individual participant level predictions") +
    facet_wrap("id", nrow = 2) +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title = element_markdown())
}

make_individual_preds_plot_tiff <- function(individual_preds_plot) {

  ggsave("plots/individual_preds_plot.tiff", individual_preds_plot, width = 10, height = 5, device = "tiff", dpi = 300)

}

combine_plots <- function(individual_data_plot, individual_preds_plot, model_plot) {

  ((individual_data_plot / individual_preds_plot) | model_plot) +
    plot_annotation(tag_level = "A",
                    tag_prefix = "(", tag_suffix = ")")

  ggsave("plots/main_plot.tiff", width = 20, height = 10, device = "tiff", dpi = 300)

}

get_tidy_model <- function(model) {
  tidy(model)
}

calculate_thresholds <- function(data) {
  data_thresholds <- data |>
    group_by(id) |>
    filter(!is.na(lactate)) |>
    mutate(intensity = seq_len(n()),
           intensity = case_when(
             intensity == 1 ~ 50,
             intensity == 2 ~ 100,
             intensity == 3 ~ 125,
             intensity == 4 ~ 150,
             intensity == 5 ~ 175,
             intensity == 6 ~ 200,
             intensity == 7 ~ 225,
             intensity == 8 ~ 250,
             intensity == 9 ~ 275,
             intensity == 10 ~ 300
           )) |>
    ungroup() |>
    select(id, intensity, lactate_z, zelemiq_avg_z)

  library(lactater)

  thresholds <- data.frame(
    id = character(),
    method_category = factor(),
    method = factor(),
    fitting = character(),
    intensity_lactate = as.numeric(),
    lactate = as.numeric(),
    intensity_zelemiq = as.numeric(),
    zelemiq = as.numeric()
  )

  for(i in unique(data_thresholds$id)) {
    lactate_thresholds <- data_thresholds |>
      filter(id == i) |>
      lactate_threshold(
        intensity_column = "intensity",
        lactate_column = "lactate_z",
        fit = "3rd degree polynomial",
        method = c("Log-log", "Dmax", "LTP"),
        include_baseline = TRUE,
        sport = "cycling",
        loglog_restrainer = 1,
        plot = FALSE
      )

    zelemiq_thresholds <- data_thresholds |>
      filter(id == i) |>
      # select(-lactate) |>
      lactate_threshold(
        intensity_column = "intensity",
        lactate_column = "zelemiq_avg_z",
        fit = "3rd degree polynomial",
        method = c("Log-log", "Dmax", "LTP"),
        include_baseline = TRUE,
        sport = "cycling",
        loglog_restrainer = 1,
        plot = FALSE
      )

    thresholds <- rbind(thresholds,
                        data.frame(id = i,
                                   method_category = lactate_thresholds$method_category,
                                   method = lactate_thresholds$method,
                                   fitting = lactate_thresholds$fitting,
                                   intensity_lactate = lactate_thresholds$intensity,
                                   lactate = lactate_thresholds$lactate,
                                   intensity_zelemiq = zelemiq_thresholds$intensity,
                                   zelemiq = zelemiq_thresholds$lactate)
    )
  }
}

