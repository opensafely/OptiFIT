# load libraries
library(data.table)
library("ggplot2")
library(xtable)

### Help functions
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
fit[index_fit_value <10, .N]
fit[index_fit_value == 10, .N]
fit[index_fit_value > 400, .N]

#manage fit value
## for now, consider value at 10 as 10, but value at 10 with < or <= sign as 9.9
fit[, adj_value := index_fit_value]
fit[index_fit_value == 10 & fit_comparator == "<", adj_value := 9.99]
fit[index_fit_value == 10 & fit_comparator == "<=", adj_value := 9.99]
fit[index_fit_value == 10, .N, keyby=adj_value]

fit[, fit10 := adj_value >= 10]

fit[, .(
  n = .N,
  percentage = round(.N / nrow(fit) * 100, 1)
), keyby = fit10]

fit[, fit_cat := fcase(
  index_fit_value == 0, "0.00",
  index_fit_value > 0 & index_fit_value < 1, "0.01-0.99",
  index_fit_value == 1, "1.00",
  index_fit_value > 1 & index_fit_value < 2, "1.01-1.99",
  index_fit_value == 2, "2.00",
  index_fit_value > 2 & index_fit_value < 3, "2.01-2.99",
  index_fit_value == 3, "3.00",
  index_fit_value > 3 & index_fit_value < 4, "3.01-3.99",
  index_fit_value == 4, "4.00",
  index_fit_value > 4 & index_fit_value < 5, "4.01-4.99",
  index_fit_value == 5, "5.00",
  index_fit_value > 5 & index_fit_value < 6, "5.01-5.99",
  index_fit_value == 6, "6.00",
  index_fit_value > 6 & index_fit_value < 7, "6.01-6.99",
  index_fit_value == 7, "7.00",
  index_fit_value > 7 & index_fit_value < 8, "7.01-7.99",
  index_fit_value == 8, "8.00",
  index_fit_value > 8 & index_fit_value < 9, "8.01-8.99",
  index_fit_value == 9, "9.00",
  index_fit_value > 9 & index_fit_value < 10, "9.01-9.99",
  index_fit_value == 10, "10.00",
  index_fit_value > 10 & index_fit_value < 11, "10.01-10.99",
  index_fit_value >= 11 & index_fit_value < 20, "11.00-19.99",
  index_fit_value >= 20 & index_fit_value < 40, "20.00-39.99",
  index_fit_value >= 40 & index_fit_value < 100, "40.00-99.99",
  index_fit_value >= 100 & index_fit_value < 200, "100.00-199.99",
  index_fit_value >= 200 & index_fit_value < 400, "200.00-399.99",
  index_fit_value >= 400, ">=400"
)]

fit[, fit_cat := factor(
  fit_cat,
  levels = c(
    "0.00",
    "0.01-0.99",
    "1.00",
    "1.01-1.99",
    "2.00",
    "2.01-2.99",
    "3.00",
    "3.01-3.99",
    "4.00",
    "4.01-4.99",
    "5.00",
    "5.01-5.99",
    "6.00",
    "6.01-6.99",
    "7.00",
    "7.01-7.99",
    "8.00",
    "8.01-8.99",
    "9.00",
    "9.01-9.99",
    "10.00",
    "10.01-10.99",
    "11.00-19.99",
    "20.00-39.99",
    "40.00-99.99",
    "100.00-199.99",
    "200.00-399.99",
    ">=400"
  ),
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


#manage age, gender and ethnicity
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

fit[, eth_grp := fcase(
  ethnicity %in% c("A", "B", "C"), "White",
  ethnicity %in% c("D", "E", "F", "G"), "Mixed",
  ethnicity %in% c("H", "J", "K", "L"), "Asian",
  ethnicity %in% c("M", "N", "P"), "Black",
  ethnicity %in% c("R", "S", "Z"), "Others",
  default = NA_character_
)]

fit[gender == "I", .N]

fit2 <- fit[gender != "I"]

#look at numbers
## code
#fit[, {
#  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
#  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
#  
#  .(
#    overall = .N,
#    overall_colpct = round(.N / nrow(fit) * 100, 1),
#    
#    fitn = n_false,
#    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
#    
#    fitp = n_true,
#    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
#  )
#}, keyby = index_fit_code]

##year
fit2_year <- fit2[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit2) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = index_fit_year]

##gender
fit2_gender <- fit2[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit2) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = gender]

##age_cat
fit2_age <- fit2[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit2) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = age_cat]

##region
fit2_region <- fit2[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit2) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = region]

fit2_eth <- fit2[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit2) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = eth_grp]

fit2_imd <- fit2[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE, na.rm = TRUE)
  
  .(
    overall = .N,
    overall_colpct = round(.N / nrow(fit2) * 100, 1),
    
    fitn = n_false,
    fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    
    fitp = n_true,
    fitp_rowpct = round(n_true / (n_false + n_true) * 100, 1)
  )
}, keyby = imd_quintile]


make_tab <- function(dt, var) {
  dt[, {
    n_false <- sum(fit10 == FALSE, na.rm = TRUE)
    n_true  <- sum(fit10 == TRUE,  na.rm = TRUE)
    .(
      overall        = .N,
      overall_colpct = round(.N / nrow(dt) * 100, 1),
      fitn           = n_false,
      fitn_rowpct    = round(n_false / (n_false + n_true) * 100, 1),
      fitp           = n_true,
      fitp_rowpct    = round(n_true / (n_false + n_true) * 100, 1)
    )
  }, keyby = var][
    , `:=`(variable = var, level = as.character(get(var)))
  ][
    , c("variable", "level", "overall", "overall_colpct",
        "fitn", "fitn_rowpct", "fitp", "fitp_rowpct"), with = FALSE
  ]
}

vars <- c("index_fit_year", "gender", "age_cat", "region", "eth_grp", "imd_quintile")

fit2_all <- rbindlist(lapply(vars, make_tab, dt = fit2))
total <- fit2[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE,  na.rm = TRUE)
  .(variable = "Total", level = "", overall = .N, overall_colpct = 100,
    fitn = n_false, fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    fitp = n_true,  fitp_rowpct = round(n_true  / (n_false + n_true) * 100, 1))
}]

fit2_all <- rbindlist(list(total, fit2_all))
fit2_all
print(
  xtable(fit2_all),
  type = "html",
  include.rownames = FALSE,
  file = "output/tbl1_no_indeterminate_gender.html"
)

#look at zeroes
fit2_zero <- fit2[index_fit_value == 0]
fit2_zero_all <- rbindlist(lapply(vars, make_tab, dt = fit2_zero))
total_zero <- fit2_zero[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE,  na.rm = TRUE)
  .(variable = "Total", level = "", overall = .N, overall_colpct = 100,
    fitn = n_false, fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    fitp = n_true,  fitp_rowpct = round(n_true  / (n_false + n_true) * 100, 1))
}]

fit2_zero_all <- rbindlist(list(total_zero, fit2_zero_all))
fit2_zero_all
print(
  xtable(fit2_zero_all),
  type = "html",
  include.rownames = FALSE,
  file = "output/tbl1_no_indeterminate_gender_value_zero.html"
)


#no zeroes
fit2_nozero <- fit2[index_fit_value != 0]
fit2_nozero_all <- rbindlist(lapply(vars, make_tab, dt = fit2_nozero))
total_nozero <- fit2_nozero[, {
  n_false <- sum(fit10 == FALSE, na.rm = TRUE)
  n_true  <- sum(fit10 == TRUE,  na.rm = TRUE)
  .(variable = "Total", level = "", overall = .N, overall_colpct = 100,
    fitn = n_false, fitn_rowpct = round(n_false / (n_false + n_true) * 100, 1),
    fitp = n_true,  fitp_rowpct = round(n_true  / (n_false + n_true) * 100, 1))
}]

fit2_nozero_all <- rbindlist(list(total_nozero, fit2_nozero_all))
fit2_nozero_all
print(
  xtable(fit2_nozero_all),
  type = "html",
  include.rownames = FALSE,
  file = "output/tbl1_no_indeterminate_gender_value_nozero.html"
)

fit2[index_fit_value < 0, .N]
