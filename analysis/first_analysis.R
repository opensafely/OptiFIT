# load libraries
library(data.table)
library("ggplot2")

fit <- data.table::fread(here::here("output", "dataset.csv.gz"))
setkey(fit, patient_id)

#manage fit value
fit[, .N, keyby=index_fit_value]
fit[index_fit_value == 0, .N]
fit[index_fit_value == 10, .N]
fit[index_fit_value > 400, .N]

fit[, fit10 := ifelse(index_fit_value > 10, TRUE, FALSE)]
fit[, .N, keyby=fit10]

fit[, fit_cat := fcase(
  index_fit_value <= 4, "<4",
  index_fit_value > 4 & index_fit_value <= 10, "4-9.9",
  index_fit_value > 10 & index_fit_value < 20, "10-19.9",
  index_fit_value >= 20 & index_fit_value < 40, "20-39.9",
  index_fit_value >= 40 & index_fit_value < 100, "40-99.9",
  index_fit_value >= 100, ">=100"
)]

fit[, fit_cat := factor(
  fit_cat,
  levels = c("<4", "4-9.9", "10-19.9", "20-39.9", "40-99.9", ">=100"),
  ordered = TRUE
)]
fit[, .N, keyby=fit_cat]

#look at fit values
plot.n <- fit[index_fit_value <=10, .N, keyby=index_fit_value]
## For value FIT <=10
fitn <- ggplot(plot.n, aes(x = index_fit_value, y = N)) +
  geom_line() +
  geom_point() +
  geom_text(
    data = plot.n[index_fit_value <= 10 & index_fit_value %% 1 == 0],  # only integers
    aes(label = N),
    vjust = -0.5
  ) +
  scale_x_continuous(
    breaks = -1:10,
    limits = c(-1, 10)
  ) +
  labs(
    x = "FIT value (µgHb/g)",
    y = "Number of FIT"
  ) +
  theme_bw()
  ggsave(
  filename = "output/fitn.png",
  plot = fitn,
  width = 8,
  height = 6,
  dpi = 300
)
print(fitn)

plot.p <- fit[index_fit_value >= 10, .(
  N = .N
), by = .(
  fit_bin = pmin(floor(index_fit_value / 10) * 10, 400)
)][order(fit_bin)]

fitp <- ggplot(plot.p, aes(x = fit_bin, y = N)) +
  geom_col() +
  scale_x_continuous(
    breaks = seq(10, 400, by = 10),
    limits = c(10, 400),
    expand = c(0, 0)
  ) +
  labs(
    x = "FIT value (µgHb/g)",
    y = "Number of FIT"
  ) +
  theme_bw()
ggsave(
  filename = "output/fitp.png",
  plot = fitp,
  width = 10,
  height = 6,
  dpi = 300
)
print(fitp)

#comparator
fit[, .N, keyby=fit_comparator] 
fit[fit_comparator == "<", .N, keyby=index_fit_value]
fit[fit_comparator == "<=", .N, keyby=index_fit_value]
fit[fit_comparator == "=", .N, keyby=index_fit_value]
fit[fit_comparator == ">", .N, keyby=index_fit_value]
fit[fit_comparator == ">=", .N, keyby=index_fit_value]
fit[fit_comparator == "~", .N, keyby=index_fit_value]


#manage age and gender
fit[, age_cat := cut(
  age_at_index_fit,
  breaks = c(0, 40, 50, 60, 70, Inf),
  labels = c("<40", "40-49", "50-59", "60-69", ">=70"),
  right = FALSE
)]
fit[, .N, keyby=age_cat]

fit[, gender := fifelse(
  sex == "female", "F",
  fifelse(sex == "male", "M", "I")
)]
fit[, .N, keyby=gender]

#look at numbers
fit[, .N, keyby=index_fit_year]
fit[, .N, keyby=.(fit10, index_fit_year)]
fit[, .N, keyby=gender]
fit[, .N, keyby=.(fit10, gender)]
fit[, .N, keyby=age_cat]
fit[, .N, keyby=.(fit10, age_cat)]
fit[, .N, keyby=region]
fit[, .N, keyby=.(fit10, region)]
