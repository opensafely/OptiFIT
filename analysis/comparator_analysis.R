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
  index_fit_value >= 0 & index_fit_value < 1, "0-0.99",
  index_fit_value >= 1 & index_fit_value < 2, "1-1.99",
  index_fit_value >= 2 & index_fit_value < 3, "2-2.99",
  index_fit_value >= 3 & index_fit_value < 4, "3-3.99",
  index_fit_value >= 4 & index_fit_value < 5, "4-4.99",
  index_fit_value >= 5 & index_fit_value < 6, "5-5.99",
  index_fit_value >= 6 & index_fit_value < 7, "6-6.99",
  index_fit_value >= 7 & index_fit_value < 8, "7-7.99",
  index_fit_value >= 8 & index_fit_value < 9, "8-8.99",
  index_fit_value >= 9 & index_fit_value < 10, "9-9.99",
  index_fit_value == 10, "10.00",
  index_fit_value > 10 & index_fit_value < 11, "10.01-10.99",
  index_fit_value >= 11 & index_fit_value < 20, "11.00-19.99",
  index_fit_value >= 20 & index_fit_value < 40, "20.00-39.99",
  index_fit_value >= 40 & index_fit_value < 100, "40.00-99.99",
  index_fit_value >= 100 & index_fit_value < 200, "100.00-199.99",
  index_fit_value >= 200 & index_fit_value < 400, "200.00-399.99",
  index_fit_value >= 400, ">=400"
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
tbl1_comp <- transpose(fit_comp[, .(
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

tbl1a_comp <- transpose(fit_comp[fit_comparator == "<", .(
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

tbl1b_comp <- transpose(fit_comp[fit_comparator == "<=", .(
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

tbl1c_comp <- transpose(fit_comp[fit_comparator == ">", .(
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

tbl1d_comp <- transpose(fit_comp[fit_comparator == ">=", .(
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

tbl1e_comp <- transpose(fit_comp[fit_comparator == "=", .(
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

tbl1f_comp <- transpose(fit_comp[fit_comparator == "~", .(
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

setnames(tbl1_comp, "V1", "All patients")
setnames(tbl1a_comp, "V1", "<")
setnames(tbl1b_comp, "V1", "<=")
setnames(tbl1c_comp, "V1", ">")
setnames(tbl1d_comp, "V1", ">=")
setnames(tbl1e_comp, "V1", "=")
setnames(tbl1f_comp, "V1", "~")

tbl_comp <- merge(tbl1_comp, tbl1a_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp <- merge(tbl_comp, tbl1b_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp <- merge(tbl_comp, tbl1c_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp <- merge(tbl_comp, tbl1d_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp <- merge(tbl_comp, tbl1e_comp, by = "Category", all = TRUE, sort = FALSE)
tbl_comp <- merge(tbl_comp, tbl1f_comp, by = "Category", all = TRUE, sort = FALSE)

print(
  xtable(tbl_comp),
  type = "html",
  include.rownames = FALSE,
  file = "output/tbl_comp.html"
)
