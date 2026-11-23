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
fit2 <- fit[sex == "male" | sex == "female"]
fit2_comp <- fit2[fit_comparator != ""]

fit2_comp[, comp_grp := factor(
  fcase(
    fit_comparator %in% c("<", "<="), "less",
    fit_comparator %in% c(">", ">="), "more",
    fit_comparator %in% c("=", "~"),  "equal"
  ),
  levels = c("less", "more", "equal")
)]
fit2_comp[, .N, keyby=comp_grp]


#table for comp
tot <- fit2[, .(fit2_n = .N), keyby = fit_cat]
cmp <- fit2_comp[, .(comp_n = .N), keyby = fit_cat]
grp <- fit2_comp[, .N, by = .(fit_cat, comp_grp)]
grp <- dcast(grp, fit_cat ~ comp_grp, value.var = "N", fill = 0)

res <- merge(tot, cmp, by = "fit_cat", all.x = TRUE)
res <- merge(res, grp, by = "fit_cat", all.x = TRUE)
grp_cols <- levels(fit2_comp$comp_grp)

# a fit_cat with no comparator rows gives NA; make those 0
for (j in c("comp_n", grp_cols)) set(res, which(is.na(res[[j]])), j, 0L)

# row % of fit2: how many in each fit_cat have a comparator
res[, comp_pct := round(comp_n / fit2_n * 100, 1)]

# row % of fit2_comp: split of each fit_cat across comparator groups
res[, (paste0(grp_cols, "_pct")) := lapply(.SD, function(x) round(x / comp_n * 100, 1)),
    .SDcols = grp_cols]

setcolorder(res, c("fit_cat", "fit2_n", "comp_n", "comp_pct",
                   as.vector(rbind(grp_cols, paste0(grp_cols, "_pct")))))

tbl_comp <- copy(res)
cnt_cols <- c("comp_n", grp_cols)

for (j in cnt_cols) {
  set(tbl_comp, j = j,
      value = ifelse(tbl_comp[[j]] < 10, "<10",
                     format(tbl_comp[[j]], big.mark = ",", trim = TRUE)))
}

print(xtable(tbl_comp), type = "html", include.rownames = FALSE,
      file = "output/tbl_fitcat_comparator.html")

fit[, .N, keyby=sex]
fit2[, .N, keyby=fit_comparator]
fit2_comp[, .(
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
