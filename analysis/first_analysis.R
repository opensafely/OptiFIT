# load libraries
library(data.table)
library("ggplot2")

# Help functions (could be in another file)
median.iqr <- function(x, digits = 0) {
  paste0(round(median(x, na.rm = T), digits = digits),
         " (",
         paste(round(quantile(
           x, c(0.25, 0.75), na.rm = T
         ), digits = digits), collapse = ","),
         ")")
}

mean.ci <- function(x, dig = 0) {
  paste0(round(mean(x, na.rm = T), digits = dig),
         " (",
         paste(
           round(mean(x, na.rm = T) - 1.96 * sd(x, na.rm = T), digits = dig),
           round(mean(x, na.rm = T) + 1.96 * sd(x, na.rm = T), digits = dig),
           collapse = ","
         )
         ,
         ")")
}

n.perc <- function(x, dig = 0) {
  paste0((sum(x == T, na.rm = T)),
         " (", round(mean(x, na.rm = T), digits = dig) * 100, "%)")
}

fit <- data.table::fread(here::here("output", "dataset.csv.gz"))
setkey(fit, patient_id)


fit[, .(
  n = .N,
  n_missing = sum(is.na(index_fit_value)),
  mean = mean(index_fit_value, na.rm = TRUE),
  sd = sd(index_fit_value, na.rm = TRUE),
  median = median(index_fit_value, na.rm = TRUE),
  q1 = quantile(index_fit_value, 0.25, na.rm = TRUE),
  q3 = quantile(index_fit_value, 0.75, na.rm = TRUE),
  iqr = IQR(index_fit_value, na.rm = TRUE),
  min = min(index_fit_value, na.rm = TRUE),
  max = max(index_fit_value, na.rm = TRUE)
)]
fit[index_fit_value == 0, .N]
fit[index_fit_value == 10, .N]
fit[index_fit_value > 400, .N]

#manage fit value
fit[, fit10 := ifelse(index_fit_value > 10, TRUE, FALSE)]
fit[, .(
  n = .N,
  percentage = round(.N / nrow(fit) * 100, 1)
), keyby = fit10]

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
fit[, .(
  n = .N,
  percentage = round(.N / nrow(fit) * 100, 1)
), keyby = fit_cat]


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
fit[, .(
  n = .N,
  percentage = round(.N / nrow(fit) * 100, 1)
), keyby = fit_comparator]
fit[index_fit_value == 10 & fit_comparator == "<", .N, keyby=index_fit_value]
fit[index_fit_value == 10 & fit_comparator == "<=", .N, keyby=index_fit_value]
fit[index_fit_value == 10 & fit_comparator == "=", .N, keyby=index_fit_value]
fit[index_fit_value == 10 & fit_comparator == ">", .N, keyby=index_fit_value]
fit[index_fit_value == 10 & fit_comparator == ">=", .N, keyby=index_fit_value]
fit[index_fit_value == 10 & fit_comparator == "~", .N, keyby=index_fit_value]

fit[index_fit_value <10 & fit_comparator == "<", .N, keyby=index_fit_value]
fit[index_fit_value <10 & fit_comparator == "<=", .N, keyby=index_fit_value]
fit[index_fit_value <10 & fit_comparator == "=", .N, keyby=index_fit_value]
fit[index_fit_value <10 & fit_comparator == ">", .N, keyby=index_fit_value]
fit[index_fit_value <10 & fit_comparator == ">=", .N, keyby=index_fit_value]
fit[index_fit_value <10 & fit_comparator == "~", .N, keyby=index_fit_value]

fit[index_fit_value >10 & fit_comparator == "<", .N, keyby=index_fit_value]
fit[index_fit_value >10 & fit_comparator == "<=", .N, keyby=index_fit_value]
fit[index_fit_value >10 & fit_comparator == "=", .N, keyby=index_fit_value]
fit[index_fit_value >10 & fit_comparator == ">", .N, keyby=index_fit_value]
fit[index_fit_value >10 & fit_comparator == ">=", .N, keyby=index_fit_value]
fit[index_fit_value >10 & fit_comparator == "~", .N, keyby=index_fit_value]


#manage age and gender
fit[, age_cat := cut(
  age_at_index_fit,
  breaks = c(0, 40, 50, 60, 70, Inf),
  labels = c("<40", "40-49", "50-59", "60-69", ">=70"),
  right = FALSE
)]
fit[, .(
  n = .N,
  percentage = round(.N / nrow(fit) * 100, 1)
), keyby = age_cat]

fit[, gender := fifelse(
  sex == "female", "F",
  fifelse(sex == "male", "M", "I")
)]
fit[, .(
  n = .N,
  percentage = round(.N / nrow(fit) * 100, 1)
), keyby = gender]

#look at numbers
##year
fit[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = index_fit_year]

##gender
fit[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = gender]

##age_cat
fit[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = age_cat]

##region
fit[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = region]

