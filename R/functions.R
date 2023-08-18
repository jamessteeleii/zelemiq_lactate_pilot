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
    select(time_norm, zelemiq_raw, zelemiq_avg, lactate)
}

plot_individual_data <- function(data) {
  # rescale and plot time series
  ylim.prim <- c(0,8)
  ylim.sec <- c(0.4,0.6)
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1]

  ggplot(data, aes(x=time_norm, y=lactate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(y = a + zelemiq_raw*b), color = "#56B4E9",  alpha = 0.1) + # zelemiq
    geom_smooth(aes(y = a + zelemiq_raw*b), color = "#56B4E9", se=FALSE) + # zelemiq
    geom_point(color = "#D55E00", alpha = 0.5) + # lactate
    geom_smooth(se=FALSE, color = "#D55E00") + # lactate
    scale_y_continuous(bquote("Blood Lactate (mmol\U00B7"~L^-1~")"), sec.axis = sec_axis(~ (. - a)/b, name = "Zelemiq Ltd Output (raw)")) +
    scale_x_continuous("Time (normalised percentage)", labels = percent) +
    facet_wrap("id", nrow = 2) +
    labs(title = "<span style = 'color: #56B4E9;'>Zelemiq Ltd Ouput</span> and <span style = 'color: #D55E00;'>Blood Lactate from Biosen C-Line</span> during the incremental test",
         caption = "Note: Curves are LOESS smooths") +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title = element_markdown())
}

make_individual_data_plot_tiff <- function(individual_data_plot) {

  ggsave("plots/individual_data_plot.tiff", individual_data_plot, width = 10, height = 5, device = "tiff", dpi = 300)

}

fit_model <- function(data) {
  model <- lmer(lactate ~ zelemiq_avg + (zelemiq_avg | id),
                data = data,
                REML = TRUE)
}

make_model_checks_tiff <- function(model) {
  checks <- check_model(model)

  plot(checks)

  ggsave("plots/model_checks.tiff", width = 10, height = 10, device = "tiff", dpi = 300)

}

plot_model <- function(data, model) {
  model_preds <- predictions(model,
                             newdata = datagrid(id = NA,
                                                zelemiq_avg = seq(min(data$zelemiq_avg, na.rm = TRUE),
                                                                  max(data$zelemiq_avg, na.rm = TRUE),
                                                                  by=0.01)),
                             re.form = NA)

  ggplot(model_preds, aes(x = zelemiq_avg, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
    geom_line(size=1) +
    geom_point(data=data, aes(y=lactate), alpha=0.25) +
    labs(x = "Zelemiq Ltd Output (10 sample moving average)",
         y = bquote("Blood Lactate (mmol\U00B7"~L^-1~")"),
         title = "Conditional model predictions") +
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
                                              zelemiq_avg = seq(min(data$zelemiq_avg, na.rm = TRUE),
                                                                    max(data$zelemiq_avg, na.rm = TRUE),
                                                                    by=0.01)))

  ggplot(ind_preds, aes(x = zelemiq_avg, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
    geom_line(size=1) +
    geom_point(data=data, aes(y=lactate), alpha=0.25) +
    labs(x = "Zelemiq Ltd Output (10 sample moving average)",
         y = bquote("Blood Lactate (mmol\U00B7"~L^-1~")"),
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


add_adj_data <- function(data) {
  data %>%
    group_by(id) %>%
    mutate(zelemiq_avg_adj = zelemiq_avg - first(zelemiq_avg, na_rm = TRUE))
}



fit_model_adj <- function(data_adj) {
  model_adj <- lmer(lactate ~ zelemiq_avg_adj + (zelemiq_avg_adj | id),
                data = data_adj,
                REML = TRUE)
}

get_tidy_model <- function(model_adj) {
  tidy(model_adj, conf.int=TRUE, conf.method="profile")
}

make_model_adj_checks_tiff <- function(model_adj) {
  checks_adj <- check_model(model_adj)

  plot(checks_adj)

  ggsave("plots/model_adj_checks.tiff", width = 10, height = 10, device = "tiff", dpi = 300)

}

plot_model_adj <- function(data_adj, model_adj) {
  model_adj_preds <- predictions(model_adj,
                             newdata = datagrid(id = NA,
                                                     zelemiq_avg_adj = seq(min(data_adj$zelemiq_avg_adj, na.rm = TRUE),
                                                                           max(data_adj$zelemiq_avg_adj, na.rm = TRUE),
                                                                           by=0.01)),
                             re.form = NA)

  ggplot(model_adj_preds, aes(x = zelemiq_avg_adj, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
    geom_line(size=1) +
    geom_point(data=data_adj, aes(y=lactate), alpha=0.25) +
    labs(x = "Baseline Adjusted Zelemiq Ltd Output (10 sample moving average)",
         y = bquote("Blood Lactate (mmol\U00B7"~L^-1~")"),
         title = "Conditional model predictions") +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title = element_markdown())
}

make_model_adj_plot_tiff <- function(model_adj_plot) {

  ggsave("plots/model_adj_plot.tiff", model_adj_plot, width = 10, height = 10, device = "tiff", dpi = 300)

}

plot_individual_adj_preds <- function(data_adj, model_adj) {

  ind_preds <- predictions(model_adj,
                           newdata = datagrid(id = data_adj$id,
                                                      zelemiq_avg_adj = seq(min(data_adj$zelemiq_avg_adj, na.rm = TRUE),
                                                                            max(data_adj$zelemiq_avg_adj, na.rm = TRUE),
                                                                            by=0.01)))

  ggplot(ind_preds, aes(x = zelemiq_avg_adj, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
    geom_line(size=1) +
    geom_point(data=data_adj, aes(y=lactate), alpha=0.25) +
    labs(x = "Baseline Adjusted Zelemiq Ltd Output (10 sample moving average)",
         y = bquote("Blood Lactate (mmol\U00B7"~L^-1~")"),
         title = "Individual participant level predictions") +
    facet_wrap("id", nrow = 2) +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title = element_markdown())
}

make_individual_adj_preds_plot_tiff <- function(individual_adj_preds_plot) {

  ggsave("plots/individual_adj_preds_plot.tiff", individual_adj_preds_plot, width = 10, height = 5, device = "tiff", dpi = 300)

}

combine_adj_plots <- function(individual_data_plot, individual_adj_preds_plot, model_adj_plot) {

  ((individual_data_plot / individual_adj_preds_plot) | model_adj_plot) +
    plot_annotation(tag_level = "A",
                    tag_prefix = "(", tag_suffix = ")")

  ggsave("plots/main_adj_plot.tiff", width = 20, height = 10, device = "tiff", dpi = 300)

}

compare_models <- function(model, model_adj) {
  bf_test <- bayesfactor_models(model, model_adj)
}


