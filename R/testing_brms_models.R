get_prior(lactate ~ zelemiq_avg + (zelemiq_avg | id),
          data = data,
          family = exponential(link = "log"))

model_exp <- brm(lactate ~ zelemiq_avg + (zelemiq_avg | id),
             data = data,
             family = exponential(link = "log"),
             save_pars = save_pars(all = TRUE)
)

plot(model_exp)

pp_check(model_exp)

model_preds_exp <- predictions(model_exp,
                          newdata = datagrid(id = NA,
                                             zelemiq_avg = seq(0.4,0.6, by=0.01)),
                          re_formula = NA,
                          type = "response")

# plot_predictions(model_exp, condition = "zelemiq_avg",
#                  points = 1,
#                  type = "response")

ggplot(model_preds_exp, aes(x = zelemiq_avg, y = estimate)) +
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


ind_preds <- predictions(model_exp,
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

model_gamma <- brm(lactate ~ zelemiq_avg + (zelemiq_avg | id),
                 data = data,
                 family = Gamma(link = "log"),
                 save_pars = save_pars(all = TRUE)
)

plot(model_gamma)

pp_check(model_gamma)

model_preds_exp <- predictions(model_gamma,
                               newdata = datagrid(id = NA,
                                                  zelemiq_avg = seq(0.4,0.6, by=0.01)),
                               re_formula = NA,
                               type = "response")

# plot_predictions(model_gamma, condition = "zelemiq_avg",
#                  points = 1,
#                  type = "response")

ggplot(model_preds_exp, aes(x = zelemiq_avg, y = estimate)) +
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


ind_preds <- predictions(model_gamma,
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

model_exgauss <- brm(lactate ~ zelemiq_avg + (zelemiq_avg | id),
                   data = data,
                   family = exgaussian(link = "log"),
                   save_pars = save_pars(all = TRUE)
)

plot(model_exgauss)

pp_check(model_exgauss)

model_preds_exp <- predictions(model_exgauss,
                               newdata = datagrid(id = NA,
                                                  zelemiq_avg = seq(0.4,0.6, by=0.01)),
                               re_formula = NA,
                               type = "response")

# plot_predictions(model_exgauss, condition = "zelemiq_avg",
#                  points = 1,
#                  type = "response")

ggplot(model_preds_exp, aes(x = zelemiq_avg, y = estimate)) +
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


ind_preds <- predictions(model_exgauss,
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

bf_mods <- bayestestR::bayesfactor_models(model_exp, model_gamma, model_exgauss, denominator = model_exp)

round(as.matrix(bf_mods),3)

log(1/3)
