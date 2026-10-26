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

null <- data.table::fread(here::here("output", "dataset_value_null.csv.gz"))

null[, .N, keyby=index_fit_code]
null[, .N, keyby=fit_comparator]

tbl_null <- transpose(null[, .(
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

print(
  xtable(tbl_null),
  type = "html",
  include.rownames = FALSE,
  file = "output/tbl_null.html"
)

