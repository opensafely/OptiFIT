# load libraries
library(data.table)

fit <- data.table::fread(here::here("output", "dataset.csv"))
setkey(fit, patient_id)

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

#manage fit value
fit[, fit10 := ifelse(index_fit_value > 10, TRUE, FALSE)]

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