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
fit_comp <- fit[fit_comparator != ""]
fit_comp <- fit_comp[sex == "male" | sex == "female"]
rm(fit)

fit_comp[, .(
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

fit_comp[, fit_cat := fcase(
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

fit_comp[, fit_cat := factor(
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

fit_comp[, .(
  overall = .N,
  overall_colpct = round(.N / nrow(fit_comp) * 100, 1)
), keyby = index_fit_year]

fit_comp[, .(
  overall = .N,
  overall_colpct = round(.N / nrow(fit_comp) * 100, 1)
), keyby = region]


#Create a table 1 without indeterminate gender
tbl_comp <- transpose(fit_comp[, .(
  "0.00 (%)" = n.perc(fit_cat == "0.00", 2),
  "0.01-0.99 (%)" = n.perc(fit_cat == "0.01-0.99", 2),
  "1.00 (%)" = n.perc(fit_cat == "1.00", 2),
  "1.01-1.99 (%)" = n.perc(fit_cat == "1.01-1.99", 2),
  "2.00 (%)" = n.perc(fit_cat == "2.00", 2),
  "2.01-2.99 (%)" = n.perc(fit_cat == "2.01-2.99", 2),
  "3.00 (%)" = n.perc(fit_cat == "3.00", 2),
  "3.01-3.99 (%)" = n.perc(fit_cat == "3.01-3.99", 2),
  "4.00 (%)" = n.perc(fit_cat == "4.00", 2),
  "4.01-4.99 (%)" = n.perc(fit_cat == "4.01-4.99", 2),
  "5.00 (%)" = n.perc(fit_cat == "5.00", 2),
  "5.01-5.99 (%)" = n.perc(fit_cat == "5.01-5.99", 2),
  "6.00 (%)" = n.perc(fit_cat == "6.00", 2),
  "6.01-6.99 (%)" = n.perc(fit_cat == "6.01-6.99", 2),
  "7.00 (%)" = n.perc(fit_cat == "7.00", 2),
  "7.01-7.99 (%)" = n.perc(fit_cat == "7.01-7.99", 2),
  "8.00 (%)" = n.perc(fit_cat == "8.00", 2),
  "8.01-8.99 (%)" = n.perc(fit_cat == "8.01-8.99", 2),
  "9.00 (%)" = n.perc(fit_cat == "9.00", 2),
  "9.01-9.99 (%)" = n.perc(fit_cat == "9.01-9.99", 2),
  "10.00 (%)" = n.perc(fit_cat == "10.00", 2),
  "10.01-10.99 (%)" = n.perc(fit_cat == "10.01-10.99", 2),
  "11.00-19.99 (%)" = n.perc(fit_cat == "11.00-19.99", 2),
  "20.00-39.99 (%)" = n.perc(fit_cat == "20.00-39.99", 2),
  "40.00-99.99 (%)" = n.perc(fit_cat == "40.00-99.99", 2),
  "100.00-199.99 (%)" = n.perc(fit_cat == "100.00-199.99", 2),
  "200.00-399.99 (%)" = n.perc(fit_cat == "200.00-399.99", 2),
  ">=400 (%)" = n.perc(fit_cat == ">=400", 2),
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "No region (%)" = n.perc(region == "", 2),
  "East (%)" = n.perc(region == "East", 2),
  "East Midlands (%)" = n.perc(region == "East Midlands", 2),
  "London (%)" = n.perc(region == "London", 2),
  "North East (%)" = n.perc(region == "North East", 2),
  "North West (%)" = n.perc(region == "North West", 2),
  "South East (%)" = n.perc(region == "South East", 2),
  "South West (%)" = n.perc(region == "South West", 2),
  "West Midlands (%)" = n.perc(region == "West Midlands", 2),
  "Yorkshire and The Humber (%)" = n.perc(region == "Yorkshire and The Humber", 2)
)], keep.names = "Category")

tbla_comp <- transpose(fit_comp[fit_comparator == "<", .(
  "0.00 (%)" = n.perc(fit_cat == "0.00", 2),
  "0.01-0.99 (%)" = n.perc(fit_cat == "0.01-0.99", 2),
  "1.00 (%)" = n.perc(fit_cat == "1.00", 2),
  "1.01-1.99 (%)" = n.perc(fit_cat == "1.01-1.99", 2),
  "2.00 (%)" = n.perc(fit_cat == "2.00", 2),
  "2.01-2.99 (%)" = n.perc(fit_cat == "2.01-2.99", 2),
  "3.00 (%)" = n.perc(fit_cat == "3.00", 2),
  "3.01-3.99 (%)" = n.perc(fit_cat == "3.01-3.99", 2),
  "4.00 (%)" = n.perc(fit_cat == "4.00", 2),
  "4.01-4.99 (%)" = n.perc(fit_cat == "4.01-4.99", 2),
  "5.00 (%)" = n.perc(fit_cat == "5.00", 2),
  "5.01-5.99 (%)" = n.perc(fit_cat == "5.01-5.99", 2),
  "6.00 (%)" = n.perc(fit_cat == "6.00", 2),
  "6.01-6.99 (%)" = n.perc(fit_cat == "6.01-6.99", 2),
  "7.00 (%)" = n.perc(fit_cat == "7.00", 2),
  "7.01-7.99 (%)" = n.perc(fit_cat == "7.01-7.99", 2),
  "8.00 (%)" = n.perc(fit_cat == "8.00", 2),
  "8.01-8.99 (%)" = n.perc(fit_cat == "8.01-8.99", 2),
  "9.00 (%)" = n.perc(fit_cat == "9.00", 2),
  "9.01-9.99 (%)" = n.perc(fit_cat == "9.01-9.99", 2),
  "10.00 (%)" = n.perc(fit_cat == "10.00", 2),
  "10.01-10.99 (%)" = n.perc(fit_cat == "10.01-10.99", 2),
  "11.00-19.99 (%)" = n.perc(fit_cat == "11.00-19.99", 2),
  "20.00-39.99 (%)" = n.perc(fit_cat == "20.00-39.99", 2),
  "40.00-99.99 (%)" = n.perc(fit_cat == "40.00-99.99", 2),
  "100.00-199.99 (%)" = n.perc(fit_cat == "100.00-199.99", 2),
  "200.00-399.99 (%)" = n.perc(fit_cat == "200.00-399.99", 2),
  ">=400 (%)" = n.perc(fit_cat == ">=400", 2),
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "No region (%)" = n.perc(region == "", 2),
  "East (%)" = n.perc(region == "East", 2),
  "East Midlands (%)" = n.perc(region == "East Midlands", 2),
  "London (%)" = n.perc(region == "London", 2),
  "North East (%)" = n.perc(region == "North East", 2),
  "North West (%)" = n.perc(region == "North West", 2),
  "South East (%)" = n.perc(region == "South East", 2),
  "South West (%)" = n.perc(region == "South West", 2),
  "West Midlands (%)" = n.perc(region == "West Midlands", 2),
  "Yorkshire and The Humber (%)" = n.perc(region == "Yorkshire and The Humber", 2)
)], keep.names = "Category")

tblb_comp <- transpose(fit_comp[fit_comparator == "<=", .(
  "0.00 (%)" = n.perc(fit_cat == "0.00", 2),
  "0.01-0.99 (%)" = n.perc(fit_cat == "0.01-0.99", 2),
  "1.00 (%)" = n.perc(fit_cat == "1.00", 2),
  "1.01-1.99 (%)" = n.perc(fit_cat == "1.01-1.99", 2),
  "2.00 (%)" = n.perc(fit_cat == "2.00", 2),
  "2.01-2.99 (%)" = n.perc(fit_cat == "2.01-2.99", 2),
  "3.00 (%)" = n.perc(fit_cat == "3.00", 2),
  "3.01-3.99 (%)" = n.perc(fit_cat == "3.01-3.99", 2),
  "4.00 (%)" = n.perc(fit_cat == "4.00", 2),
  "4.01-4.99 (%)" = n.perc(fit_cat == "4.01-4.99", 2),
  "5.00 (%)" = n.perc(fit_cat == "5.00", 2),
  "5.01-5.99 (%)" = n.perc(fit_cat == "5.01-5.99", 2),
  "6.00 (%)" = n.perc(fit_cat == "6.00", 2),
  "6.01-6.99 (%)" = n.perc(fit_cat == "6.01-6.99", 2),
  "7.00 (%)" = n.perc(fit_cat == "7.00", 2),
  "7.01-7.99 (%)" = n.perc(fit_cat == "7.01-7.99", 2),
  "8.00 (%)" = n.perc(fit_cat == "8.00", 2),
  "8.01-8.99 (%)" = n.perc(fit_cat == "8.01-8.99", 2),
  "9.00 (%)" = n.perc(fit_cat == "9.00", 2),
  "9.01-9.99 (%)" = n.perc(fit_cat == "9.01-9.99", 2),
  "10.00 (%)" = n.perc(fit_cat == "10.00", 2),
  "10.01-10.99 (%)" = n.perc(fit_cat == "10.01-10.99", 2),
  "11.00-19.99 (%)" = n.perc(fit_cat == "11.00-19.99", 2),
  "20.00-39.99 (%)" = n.perc(fit_cat == "20.00-39.99", 2),
  "40.00-99.99 (%)" = n.perc(fit_cat == "40.00-99.99", 2),
  "100.00-199.99 (%)" = n.perc(fit_cat == "100.00-199.99", 2),
  "200.00-399.99 (%)" = n.perc(fit_cat == "200.00-399.99", 2),
  ">=400 (%)" = n.perc(fit_cat == ">=400", 2),
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "No region (%)" = n.perc(region == "", 2),
  "East (%)" = n.perc(region == "East", 2),
  "East Midlands (%)" = n.perc(region == "East Midlands", 2),
  "London (%)" = n.perc(region == "London", 2),
  "North East (%)" = n.perc(region == "North East", 2),
  "North West (%)" = n.perc(region == "North West", 2),
  "South East (%)" = n.perc(region == "South East", 2),
  "South West (%)" = n.perc(region == "South West", 2),
  "West Midlands (%)" = n.perc(region == "West Midlands", 2),
  "Yorkshire and The Humber (%)" = n.perc(region == "Yorkshire and The Humber", 2)
)], keep.names = "Category")

tblc_comp <- transpose(fit_comp[fit_comparator == ">", .(
  "0.00 (%)" = n.perc(fit_cat == "0.00", 2),
  "0.01-0.99 (%)" = n.perc(fit_cat == "0.01-0.99", 2),
  "1.00 (%)" = n.perc(fit_cat == "1.00", 2),
  "1.01-1.99 (%)" = n.perc(fit_cat == "1.01-1.99", 2),
  "2.00 (%)" = n.perc(fit_cat == "2.00", 2),
  "2.01-2.99 (%)" = n.perc(fit_cat == "2.01-2.99", 2),
  "3.00 (%)" = n.perc(fit_cat == "3.00", 2),
  "3.01-3.99 (%)" = n.perc(fit_cat == "3.01-3.99", 2),
  "4.00 (%)" = n.perc(fit_cat == "4.00", 2),
  "4.01-4.99 (%)" = n.perc(fit_cat == "4.01-4.99", 2),
  "5.00 (%)" = n.perc(fit_cat == "5.00", 2),
  "5.01-5.99 (%)" = n.perc(fit_cat == "5.01-5.99", 2),
  "6.00 (%)" = n.perc(fit_cat == "6.00", 2),
  "6.01-6.99 (%)" = n.perc(fit_cat == "6.01-6.99", 2),
  "7.00 (%)" = n.perc(fit_cat == "7.00", 2),
  "7.01-7.99 (%)" = n.perc(fit_cat == "7.01-7.99", 2),
  "8.00 (%)" = n.perc(fit_cat == "8.00", 2),
  "8.01-8.99 (%)" = n.perc(fit_cat == "8.01-8.99", 2),
  "9.00 (%)" = n.perc(fit_cat == "9.00", 2),
  "9.01-9.99 (%)" = n.perc(fit_cat == "9.01-9.99", 2),
  "10.00 (%)" = n.perc(fit_cat == "10.00", 2),
  "10.01-10.99 (%)" = n.perc(fit_cat == "10.01-10.99", 2),
  "11.00-19.99 (%)" = n.perc(fit_cat == "11.00-19.99", 2),
  "20.00-39.99 (%)" = n.perc(fit_cat == "20.00-39.99", 2),
  "40.00-99.99 (%)" = n.perc(fit_cat == "40.00-99.99", 2),
  "100.00-199.99 (%)" = n.perc(fit_cat == "100.00-199.99", 2),
  "200.00-399.99 (%)" = n.perc(fit_cat == "200.00-399.99", 2),
  ">=400 (%)" = n.perc(fit_cat == ">=400", 2),
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "No region (%)" = n.perc(region == "", 2),
  "East (%)" = n.perc(region == "East", 2),
  "East Midlands (%)" = n.perc(region == "East Midlands", 2),
  "London (%)" = n.perc(region == "London", 2),
  "North East (%)" = n.perc(region == "North East", 2),
  "North West (%)" = n.perc(region == "North West", 2),
  "South East (%)" = n.perc(region == "South East", 2),
  "South West (%)" = n.perc(region == "South West", 2),
  "West Midlands (%)" = n.perc(region == "West Midlands", 2),
  "Yorkshire and The Humber (%)" = n.perc(region == "Yorkshire and The Humber", 2)
)], keep.names = "Category")

tbld_comp <- transpose(fit_comp[fit_comparator == ">=", .(
  "0.00 (%)" = n.perc(fit_cat == "0.00", 2),
  "0.01-0.99 (%)" = n.perc(fit_cat == "0.01-0.99", 2),
  "1.00 (%)" = n.perc(fit_cat == "1.00", 2),
  "1.01-1.99 (%)" = n.perc(fit_cat == "1.01-1.99", 2),
  "2.00 (%)" = n.perc(fit_cat == "2.00", 2),
  "2.01-2.99 (%)" = n.perc(fit_cat == "2.01-2.99", 2),
  "3.00 (%)" = n.perc(fit_cat == "3.00", 2),
  "3.01-3.99 (%)" = n.perc(fit_cat == "3.01-3.99", 2),
  "4.00 (%)" = n.perc(fit_cat == "4.00", 2),
  "4.01-4.99 (%)" = n.perc(fit_cat == "4.01-4.99", 2),
  "5.00 (%)" = n.perc(fit_cat == "5.00", 2),
  "5.01-5.99 (%)" = n.perc(fit_cat == "5.01-5.99", 2),
  "6.00 (%)" = n.perc(fit_cat == "6.00", 2),
  "6.01-6.99 (%)" = n.perc(fit_cat == "6.01-6.99", 2),
  "7.00 (%)" = n.perc(fit_cat == "7.00", 2),
  "7.01-7.99 (%)" = n.perc(fit_cat == "7.01-7.99", 2),
  "8.00 (%)" = n.perc(fit_cat == "8.00", 2),
  "8.01-8.99 (%)" = n.perc(fit_cat == "8.01-8.99", 2),
  "9.00 (%)" = n.perc(fit_cat == "9.00", 2),
  "9.01-9.99 (%)" = n.perc(fit_cat == "9.01-9.99", 2),
  "10.00 (%)" = n.perc(fit_cat == "10.00", 2),
  "10.01-10.99 (%)" = n.perc(fit_cat == "10.01-10.99", 2),
  "11.00-19.99 (%)" = n.perc(fit_cat == "11.00-19.99", 2),
  "20.00-39.99 (%)" = n.perc(fit_cat == "20.00-39.99", 2),
  "40.00-99.99 (%)" = n.perc(fit_cat == "40.00-99.99", 2),
  "100.00-199.99 (%)" = n.perc(fit_cat == "100.00-199.99", 2),
  "200.00-399.99 (%)" = n.perc(fit_cat == "200.00-399.99", 2),
  ">=400 (%)" = n.perc(fit_cat == ">=400", 2),
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "No region (%)" = n.perc(region == "", 2),
  "East (%)" = n.perc(region == "East", 2),
  "East Midlands (%)" = n.perc(region == "East Midlands", 2),
  "London (%)" = n.perc(region == "London", 2),
  "North East (%)" = n.perc(region == "North East", 2),
  "North West (%)" = n.perc(region == "North West", 2),
  "South East (%)" = n.perc(region == "South East", 2),
  "South West (%)" = n.perc(region == "South West", 2),
  "West Midlands (%)" = n.perc(region == "West Midlands", 2),
  "Yorkshire and The Humber (%)" = n.perc(region == "Yorkshire and The Humber", 2)
)], keep.names = "Category")

tble_comp <- transpose(fit_comp[fit_comparator == "=", .(
  "0.00 (%)" = n.perc(fit_cat == "0.00", 2),
  "0.01-0.99 (%)" = n.perc(fit_cat == "0.01-0.99", 2),
  "1.00 (%)" = n.perc(fit_cat == "1.00", 2),
  "1.01-1.99 (%)" = n.perc(fit_cat == "1.01-1.99", 2),
  "2.00 (%)" = n.perc(fit_cat == "2.00", 2),
  "2.01-2.99 (%)" = n.perc(fit_cat == "2.01-2.99", 2),
  "3.00 (%)" = n.perc(fit_cat == "3.00", 2),
  "3.01-3.99 (%)" = n.perc(fit_cat == "3.01-3.99", 2),
  "4.00 (%)" = n.perc(fit_cat == "4.00", 2),
  "4.01-4.99 (%)" = n.perc(fit_cat == "4.01-4.99", 2),
  "5.00 (%)" = n.perc(fit_cat == "5.00", 2),
  "5.01-5.99 (%)" = n.perc(fit_cat == "5.01-5.99", 2),
  "6.00 (%)" = n.perc(fit_cat == "6.00", 2),
  "6.01-6.99 (%)" = n.perc(fit_cat == "6.01-6.99", 2),
  "7.00 (%)" = n.perc(fit_cat == "7.00", 2),
  "7.01-7.99 (%)" = n.perc(fit_cat == "7.01-7.99", 2),
  "8.00 (%)" = n.perc(fit_cat == "8.00", 2),
  "8.01-8.99 (%)" = n.perc(fit_cat == "8.01-8.99", 2),
  "9.00 (%)" = n.perc(fit_cat == "9.00", 2),
  "9.01-9.99 (%)" = n.perc(fit_cat == "9.01-9.99", 2),
  "10.00 (%)" = n.perc(fit_cat == "10.00", 2),
  "10.01-10.99 (%)" = n.perc(fit_cat == "10.01-10.99", 2),
  "11.00-19.99 (%)" = n.perc(fit_cat == "11.00-19.99", 2),
  "20.00-39.99 (%)" = n.perc(fit_cat == "20.00-39.99", 2),
  "40.00-99.99 (%)" = n.perc(fit_cat == "40.00-99.99", 2),
  "100.00-199.99 (%)" = n.perc(fit_cat == "100.00-199.99", 2),
  "200.00-399.99 (%)" = n.perc(fit_cat == "200.00-399.99", 2),
  ">=400 (%)" = n.perc(fit_cat == ">=400", 2),
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "No region (%)" = n.perc(region == "", 2),
  "East (%)" = n.perc(region == "East", 2),
  "East Midlands (%)" = n.perc(region == "East Midlands", 2),
  "London (%)" = n.perc(region == "London", 2),
  "North East (%)" = n.perc(region == "North East", 2),
  "North West (%)" = n.perc(region == "North West", 2),
  "South East (%)" = n.perc(region == "South East", 2),
  "South West (%)" = n.perc(region == "South West", 2),
  "West Midlands (%)" = n.perc(region == "West Midlands", 2),
  "Yorkshire and The Humber (%)" = n.perc(region == "Yorkshire and The Humber", 2)
)], keep.names = "Category")

tblf_comp <- transpose(fit_comp[fit_comparator == "~", .(
  "0.00 (%)" = n.perc(fit_cat == "0.00", 2),
  "0.01-0.99 (%)" = n.perc(fit_cat == "0.01-0.99", 2),
  "1.00 (%)" = n.perc(fit_cat == "1.00", 2),
  "1.01-1.99 (%)" = n.perc(fit_cat == "1.01-1.99", 2),
  "2.00 (%)" = n.perc(fit_cat == "2.00", 2),
  "2.01-2.99 (%)" = n.perc(fit_cat == "2.01-2.99", 2),
  "3.00 (%)" = n.perc(fit_cat == "3.00", 2),
  "3.01-3.99 (%)" = n.perc(fit_cat == "3.01-3.99", 2),
  "4.00 (%)" = n.perc(fit_cat == "4.00", 2),
  "4.01-4.99 (%)" = n.perc(fit_cat == "4.01-4.99", 2),
  "5.00 (%)" = n.perc(fit_cat == "5.00", 2),
  "5.01-5.99 (%)" = n.perc(fit_cat == "5.01-5.99", 2),
  "6.00 (%)" = n.perc(fit_cat == "6.00", 2),
  "6.01-6.99 (%)" = n.perc(fit_cat == "6.01-6.99", 2),
  "7.00 (%)" = n.perc(fit_cat == "7.00", 2),
  "7.01-7.99 (%)" = n.perc(fit_cat == "7.01-7.99", 2),
  "8.00 (%)" = n.perc(fit_cat == "8.00", 2),
  "8.01-8.99 (%)" = n.perc(fit_cat == "8.01-8.99", 2),
  "9.00 (%)" = n.perc(fit_cat == "9.00", 2),
  "9.01-9.99 (%)" = n.perc(fit_cat == "9.01-9.99", 2),
  "10.00 (%)" = n.perc(fit_cat == "10.00", 2),
  "10.01-10.99 (%)" = n.perc(fit_cat == "10.01-10.99", 2),
  "11.00-19.99 (%)" = n.perc(fit_cat == "11.00-19.99", 2),
  "20.00-39.99 (%)" = n.perc(fit_cat == "20.00-39.99", 2),
  "40.00-99.99 (%)" = n.perc(fit_cat == "40.00-99.99", 2),
  "100.00-199.99 (%)" = n.perc(fit_cat == "100.00-199.99", 2),
  "200.00-399.99 (%)" = n.perc(fit_cat == "200.00-399.99", 2),
  ">=400 (%)" = n.perc(fit_cat == ">=400", 2),
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "No region (%)" = n.perc(region == "", 2),
  "East (%)" = n.perc(region == "East", 2),
  "East Midlands (%)" = n.perc(region == "East Midlands", 2),
  "London (%)" = n.perc(region == "London", 2),
  "North East (%)" = n.perc(region == "North East", 2),
  "North West (%)" = n.perc(region == "North West", 2),
  "South East (%)" = n.perc(region == "South East", 2),
  "South West (%)" = n.perc(region == "South West", 2),
  "West Midlands (%)" = n.perc(region == "West Midlands", 2),
  "Yorkshire and The Humber (%)" = n.perc(region == "Yorkshire and The Humber", 2)
)], keep.names = "Category")

setnames(tbl_comp, "V1", "All patients")
setnames(tbla_comp, "V1", "<")
setnames(tblb_comp, "V1", "<=")
setnames(tblc_comp, "V1", ">")
setnames(tbld_comp, "V1", ">=")
setnames(tble_comp, "V1", "=")
setnames(tblf_comp, "V1", "~")

tbl_comp_combined <- merge(tbl_comp, tbla_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp_combined <- merge(tbl_comp_combined, tblb_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp_combined <- merge(tbl_comp_combined, tblc_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp_combined <- merge(tbl_comp_combined, tbld_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp_combined <- merge(tbl_comp_combined, tble_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp_combined <- merge(tbl_comp_combined, tblf_comp, by = "Category", all = TRUE, sort = FALSE)

print(
  xtable(tbl_comp_combined),
  type = "html",
  include.rownames = FALSE,
  file = "output/tbl_comp_combined.html"
)

rm(tbl_comp, tbla_comp, tblb_comp, tblc_comp, tbld_comp, tble_comp, tblf_comp)
