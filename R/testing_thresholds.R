data |>
  ggplot(aes(lactate)) +
  geom_density() +
  facet_wrap("id")

install.packages("lactater")

max(data_1$intensity, na.rm = TRUE)

data_thresholds <- data |>
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
  ungroup()

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
      lactate_column = "lactate",
      fit = "3rd degree polynomial",
      method = c("Log-log", "Dmax", "LTP", "LTratio"),
      include_baseline = TRUE,
      sport = "cycling",
      loglog_restrainer = 1,
      plot = TRUE
    )

  zelemiq_thresholds <- data_thresholds |>
    filter(id == i) |>
    select(-lactate) |>
    lactate_threshold(
      intensity_column = "intensity",
      lactate_column = "zelemiq_avg",
      fit = "3rd degree polynomial",
      method = c("Log-log", "Dmax", "LTP", "LTratio"),
      include_baseline = TRUE,
      sport = "cycling",
      loglog_restrainer = 1,
      plot = TRUE
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


thresholds |>
  filter(method != "ModDmax") |>
  ggplot(aes(x = intensity_lactate, y = intensity_zelemiq)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap("method")



