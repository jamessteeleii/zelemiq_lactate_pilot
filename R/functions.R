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
    geom_point(aes(y = a + zelemiq_raw*b), color = "#56B4E9",  alpha = 0.1) + # zelemiq
    geom_smooth(aes(y = a + zelemiq_raw*b), color = "#56B4E9", se=FALSE) + # zelemiq
    geom_point(color = "#D55E00", alpha = 0.5) + # lactate
    geom_smooth(se=FALSE, color = "#D55E00") + # lactate
    scale_y_continuous(bquote("Blood Lactate (mmol."~L^-1~")"), sec.axis = sec_axis(~ (. - a)/b, name = "Zelemiq Ltd Output (raw)")) +
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
