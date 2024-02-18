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

# Diagnostic plots
make_rhat_plot <- function(model) {
  mod_rhat <- enframe(brms::rhat(model))

  rhat_main_params <- mod_rhat$value

  mcmc_rhat(rhat_main_params) +
    scale_x_continuous(breaks = c(1,1.01,1.02,1.03,1.04,1.05)) +
    geom_vline(xintercept = 1.01, linetype="dashed", alpha = 0.25)
}

make_trace_plots <- function(model) {
  plot(model)
}

make_pp_check <- function(model) {
  pp_check(model)
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

combine_plots <- function(individual_data_plot, individual_preds_plot, model_plot, thresholds_agree_plot) {

  (((individual_data_plot / individual_preds_plot) | model_plot) / thresholds_agree_plot) +
    plot_annotation(tag_level = "A",
                    tag_prefix = "(", tag_suffix = ")") +
    plot_layout(heights = c(10,4)) &
    theme(axis.title = element_text(size = 13))

  ggsave("plots/main_plot.tiff", width = 20, height = 14, device = "tiff", dpi = 300)

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
  return(thresholds)
}

calculate_thresholds_agree <- function(thresholds) {
  thresholds_agree <- data.frame(
    method = factor(),
    bias = as.numeric(),
    bias_lower = as.numeric(),
    bias_upper = as.numeric(),
    lower_loa = as.numeric(),
    lower_loa_lower = as.numeric(),
    lower_loa_upper = as.numeric(),
    upper_loa = as.numeric(),
    upper_loa_lower = as.numeric(),
    upper_loa_upper = as.numeric(),
    ccc = as.numeric(),
    ccc_lower = as.numeric(),
    ccc_upper = as.numeric()
  )

  method <- tibble(method = unique(thresholds$method)) |>
    filter(str_detect(method, pattern = "Log", negate = TRUE))


  for(i in method$method) {
    method_data <- thresholds |>
      filter(str_detect(method, pattern = "Log", negate = TRUE)) |>
      filter(method == i)

    agree <- SimplyAgree::agree_test(x = method_data$intensity_zelemiq,
                                     y = method_data$intensity_lactate)

    thresholds_agree <- rbind(thresholds_agree,
                              data.frame(
                                method = i,
                                bias = agree$loa$estimate[1],
                                bias_lower = agree$loa$lower.ci[1],
                                bias_upper = agree$loa$upper.ci[1],
                                lower_loa = agree$loa$estimate[2],
                                lower_loa_lower = agree$loa$lower.ci[2],
                                lower_loa_upper = agree$loa$upper.ci[2],
                                upper_loa = agree$loa$estimate[3],
                                upper_loa_lower = agree$loa$lower.ci[3],
                                upper_loa_upper = agree$loa$upper.ci[3],
                                ccc = agree$ccc.xy$est.ccc,
                                ccc_lower = agree$ccc.xy$lower.ci,
                                ccc_upper = agree$ccc.xy$upper.ci

                              )
    )
  }
  return(thresholds_agree)
}

plot_thresholds_agree <- function(thresholds, thresholds_agree) {
  thresholds |>
    filter(str_detect(method, pattern = "Log", negate = TRUE)) |>
    ggplot(aes(x = intensity_lactate, y = intensity_zelemiq)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_point() +
    geom_text(data = thresholds_agree,
              aes(label = glue::glue("Bias = {round(bias,2)} [95%CI: {round(bias_lower,2)}, {round(bias_upper,2)}]")),
              x = 140,
              y = 250,
              size = 2.5
    ) +
    geom_text(data = thresholds_agree,
              aes(label = glue::glue("Lower LOA = {round(lower_loa,2)} [90%CI: {round(lower_loa_lower,2)}, {round(lower_loa_upper,2)}]")),
              x = 140,
              y = 235,
              size = 2.5
    ) +
    geom_text(data = thresholds_agree,
              aes(label = glue::glue("Upper LOA = {round(upper_loa,2)} [90%CI: {round(upper_loa_lower,2)}, {round(upper_loa_upper,2)}]")),
              x = 140,
              y = 220,
              size = 2.5
    ) +
    geom_text(data = thresholds_agree,
              aes(label = paste(glue::glue("rho[CCC] == {round(ccc,2)}"))),
              x = 235,
              y = 125,
              size = 2.5,
              parse = TRUE
    ) +
    geom_text(data = thresholds_agree,
              aes(label = glue::glue("[95%CI: {round(ccc_lower,2)}, {round(ccc_upper,2)}]")),
              x = 235,
              y = 110,
              size = 2.5
    ) +
    facet_grid(.~method) +
    labs(x = "Threshold determined by blood lactate (Watts)",
         y = "Threshold determined by Zelemiq Ltd (Watts)",
         title = "Agreement of thresholds") +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title = element_markdown())
}

make_thresholds_agree_plot_tiff <- function(thresholds_agree_plot) {

  ggsave("plots/thresholds_agree_plot.tiff", thresholds_agree_plot, width = 20, height = 4, device = "tiff", dpi = 300)

}
