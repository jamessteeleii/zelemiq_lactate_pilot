# renv::install("tidyverse")

library(tidyverse)

# renv::install("readxl")

library(readxl)

# renv::install("here")

library(here)

# renv::install("janitor")

library(janitor)

# renv::install("zoo")

library(zoo)

normalise <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

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


# rescale and plot time series
ylim.prim <- c(0,8)
ylim.sec <- c(0.4,0.6)
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

# renv::install("scales")

library(scales)

# renv::install("ggtext")

library(ggtext)

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

# mixed model
# renv::install("lme4")

library(lme4)

# renv::install("performance")

model <- lmer(lactate ~ zelemiq_avg + (zelemiq_avg | id),
              data = data,
              REML = TRUE)

# renv::install("marginaleffects")

library(marginaleffects)

model_preds <- predictions(model,
                           newdata = datagrid(id = NA,
                                              zelemiq_avg = seq(0.4,0.6, by=0.01)),
                           re.form = NA,
                           type = "response")

ggplot(model_preds, aes(x = zelemiq_avg, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
  geom_line(size=1) +
  geom_point(data=data, aes(y=lactate), alpha=0.25) +
  labs(x = "Zelemiq Ltd Output",
       y = bquote("Blood Lactate (mmol."~L^-1~")"),
       title = "Conditional model predictions") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        plot.title = element_markdown())


ind_preds <- predictions(model,
                         newdata = datagrid(id = data$id,
                                            zelemiq_avg = seq(0.4,0.6, by=0.01)))

ggplot(ind_preds, aes(x = zelemiq_avg, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
  geom_line(size=1) +
  geom_point(data=data, aes(y=lactate), alpha=0.25) +
  labs(x = "Zelemiq Ltd Output",
       y = bquote("Blood Lactate (mmol."~L^-1~")"),
       title = "Individual participant level predictions") +
  facet_wrap("id", nrow = 2) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        plot.title = element_markdown())


# Adjust at unit level for baseline
data <- data %>%
  group_by(id) %>%
  mutate(zelemiq_avg_adj = zelemiq_avg - first(zelemiq_avg, na_rm = TRUE))

model_adj <- lmer(lactate ~ zelemiq_avg_adj + (zelemiq_avg_adj | id),
              data = data,
              REML = TRUE)

performance::check_model(model_adj)

model_adj_preds <- predictions(model_adj,
                           newdata = datagrid(id = NA,
                                              zelemiq_avg_adj = seq(min(data$zelemiq_avg_adj, na.rm = TRUE),
                                                                    max(data$zelemiq_avg_adj, na.rm = TRUE),
                                                                    by=0.01)),
                           re.form = NA,
                           type = "response")

ggplot(model_adj_preds, aes(x = zelemiq_avg_adj, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
  geom_line(size=1) +
  geom_point(data=data, aes(y=lactate), alpha=0.25) +
  labs(x = "Zelemiq Ltd Output",
       y = bquote("Blood Lactate (mmol."~L^-1~")"),
       title = "Conditional model_adj predictions") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        plot.title = element_markdown())


ind_preds <- predictions(model_adj,
                         newdata = datagrid(id = data$id,
                                            zelemiq_avg_adj = seq(min(data$zelemiq_avg_adj, na.rm = TRUE),
                                                                  max(data$zelemiq_avg_adj, na.rm = TRUE),
                                                                  by=0.01))
                         )
  (
ggplot(ind_preds, aes(x = zelemiq_avg_adj, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
  geom_line(size=1) +
  geom_point(data=data, aes(y=lactate), alpha=0.25) +
  labs(x = "Zelemiq Ltd Output",
       y = bquote("Blood Lactate (mmol."~L^-1~")"),
       title = "Individual participant level predictions") +
  facet_wrap("id", nrow = 2) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        plot.title = element_markdown())) +

  (
    ggplot(model_adj_preds, aes(x = zelemiq_avg_adj, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.25) +
      geom_line(size=1) +
      geom_point(data=data, aes(y=lactate), alpha=0.25) +
      labs(x = "Zelemiq Ltd Output",
           y = bquote("Blood Lactate (mmol."~L^-1~")"),
           title = "Conditional model_adj predictions") +
      theme_bw() +
      theme(panel.grid=element_blank(),
            plot.title = element_markdown()) )
# compare

performance::compare_performance(model, model_adj, rank = TRUE)


# renv::install("sjPlot")
library(sjPlot)

tab_model(model_adj)

# renv::install("bayestestR")
library(bayestestR)

bf_test <- bayesfactor_models(model, model_adj)

2*round(bf_test$log_BF, 1)[2]


R.version
renv::install("remotes")

remotes::install_github("datalorax/equatiomatic")

renv::install("texPreview")

targets::tar_load(model_checks)



p.sd_1001 <- profile(model,which="theta_",
                     signames=FALSE)
ran_cor <- as.data.frame(confint(p.sd_1001))

p.sd_3001 <- profile(LMM_3001,which="theta_",
                     signames=FALSE)
confint(p.sd_3001)

p.sd_5001 <- profile(LMM_5001,which="theta_",
                     signames=FALSE)
confint(p.sd_5001)

confint(model_adj)

renv::install("quarto")
