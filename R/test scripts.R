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
  mutate(zelemiq_raw_z = (zelemiq_raw - mean(zelemiq_raw, na.rm=TRUE))/sd(zelemiq_raw, na.rm=TRUE),
         zelemiq_avg_z = (zelemiq_avg - mean(zelemiq_avg, na.rm=TRUE))/sd(zelemiq_avg, na.rm=TRUE),
         lactate_z = (lactate - mean(lactate, na.rm=TRUE))/sd(lactate, na.rm=TRUE)) |>
  ungroup() |>
  select(id, time_norm, zelemiq_raw, zelemiq_raw_z, zelemiq_avg, zelemiq_avg_z, lactate, lactate_z)



# rescale and plot time series
ylim.prim <- c(0,8)
ylim.sec <- c(0.4,0.6)
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

# renv::install("scales")

library(scales)

# renv::install("ggtext")

library(ggtext)

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
       title = "<span style = 'color: #56B4E9;'>Zelemiq Ltd Ouput</span> and <span style = 'color: #D55E00;'>Blood Lactate from Biosen C-Line</span> during the incremental test",
       caption = "Note: Curves are LOESS smooths") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        plot.title = element_markdown())


data |>
  ggplot(aes(y = log(lactate), x = zelemiq_avg)) +
  geom_point() +
  geom_smooth(se=FALSE, method = "lm") +
  facet_wrap("id")

# mixed model
# renv::install("lme4")

library(lme4)

# renv::install("performance")

model <- lmer(lactate_z ~ zelemiq_avg_z + I(zelemiq_avg_z^2) + (zelemiq_avg_z | id),
              data = data)


model <- brms::brm(lactate_z ~ zelemiq_avg_z + I(zelemiq_avg_z^2) + (zelemiq_avg_z | id),
               data = data
)

plot(model)

pp_check(model)

performance::check_model(model)

# renv::install("marginaleffects")

library(marginaleffects)

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


plot_predictions(model, condition = "zelemiq_avg_z",
                 points = 1,
                 type = "response")

ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(data=model_preds, aes(x = zelemiq_avg_z, ymin=conf.low, ymax=conf.high), fill = "#CCCCCC", alpha = 0.5) +
  geom_ribbon(data=model_cond_epred, aes(x = zelemiq_avg_z, ymin=conf.low, ymax=conf.high), fill = "#999999", alpha = 0.5) +
  geom_ribbon(data=model_epred, aes(x = zelemiq_avg_z, ymin=conf.low, ymax=conf.high), fill = "#333333", alpha = 0.5) +
  geom_line(data=model_epred, aes(x = zelemiq_avg_z, y = estimate), size=1) +
  geom_point(data=data, aes(x = zelemiq_avg_z, y=lactate_z), alpha=1) +
  labs(x = "Zelemiq Ltd Output (standardised)",
       y = bquote("Blood Lactate (standardised)"),
       title = "Model predictions",
       subtitle = "Interval estimates are, from widest to narrowest: posterior predictions, conditional effects for participants, and global grand mean"
       ) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        plot.title = element_markdown(),
        plot.subtitle = element_text(size=8))


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


# Adjust at unit level for baseline
data <- data %>%
  group_by(id) %>%
  mutate(zelemiq_avg_adj = zelemiq_avg - first(zelemiq_avg, na_rm = TRUE))

model_adj <- brm(lactate ~ zelemiq_avg_adj + (zelemiq_avg_adj | id),
             data = data,
             family = exponential(link = "log")
)

plot(model_adj)

pp_check(model_adj)

bayestestR::bayesfactor_models(model, model_adj)

# model_adj <- lmer(lactate ~ zelemiq_avg_adj + (zelemiq_avg_adj | id),
#               data = data,
#               REML = TRUE)

performance::check_model(model_adj)

model_adj_preds <- predictions(model_adj,
                           newdata = datagrid(id = NA,
                                              zelemiq_avg_adj = seq(min(data$zelemiq_avg_adj, na.rm = TRUE),
                                                                    max(data$zelemiq_avg_adj, na.rm = TRUE),
                                                                    by=0.01)),
                           re_formula = NA,
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

renv::install("see")



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

checks <- performance::check_model(model)

plot(checks)


renv::install("broom.mixed")

tidy_model <- broom.mixed::tidy(model_adj, conf.int=TRUE, conf.method="profile")

R2_adj <- performance::r2(model)

R2_adj$R2_marginal

tidy_model_adj <- tidy_model[c(1,2,3,5,4,6),c(1,3,4,7,8)]

ICC_adj <- performance::variance_decomposition(model)


ICC_adj[1]

visNetwork::visSave(targets::tar_visnetwork(targets_only = TRUE),"visnetwork.html")
