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
fit[index_fit_value == 10, .N]
fit[index_fit_value > 400, .N]

#manage fit value
## for now, consider value at 10 as 10, but value at 10 with < or <= sign as 9.9
fit[, adj_value := fifelse(
  index_fit_value == 10 & fit_comparator %in% c("<", "<="),
  9.9,
  index_fit_value
)]

fit[, fit10 := adj_value >= 10]

fit[, .(
  n = .N,
  percentage = round(.N / nrow(fit) * 100, 1)
), keyby = fit10]

fit[, fit_cat := fcase(
  adj_value <= 4, "<4",
  adj_value > 4 & adj_value <= 10, "4-9.9",
  adj_value > 10 & adj_value < 20, "10-19.9",
  adj_value >= 20 & adj_value < 40, "20-39.9",
  adj_value >= 40 & adj_value < 100, "40-99.9",
  adj_value >= 100, ">=100"
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
fit[index_fit_value == 10 & fit_comparator == "<", .N]
fit[index_fit_value == 10 & fit_comparator == "<=", .N]
fit[index_fit_value == 10 & fit_comparator == "=", .N]
fit[index_fit_value == 10 & fit_comparator == ">", .N]
fit[index_fit_value == 10 & fit_comparator == ">=", .N]
fit[index_fit_value == 10 & fit_comparator == "~", .N]


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
}, keyby = eth_grp]

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
}, keyby = imd_quintile]

#For now, remove gender not M or F
fit_gender <- fit[gender == "M" | gender == "F"]


#Create a table 1 without indeterminate gender
tbl1 <- transpose(fit_gender[, .(
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "Mean Age (95% CI)" = mean.ci(age_at_index_fit, 0),
  "Male (%)" = n.perc(gender == "M", 2),
  "Female (%)" = n.perc(gender == "F", 2),
  "Age <40 (%)" = n.perc(age_cat == "<40", 2),
  "Age 40-49 (%)" = n.perc(age_cat == "40-49", 2),
  "Age 50-59 (%)" = n.perc(age_cat == "50-59", 2),
  "Age 60-69 (%)" = n.perc(age_cat == "60-69", 2),
  "Age >=70 (%)" = n.perc(age_cat == ">=70", 2),
  "Ethnicity, White (%)" = n.perc(eth_grp == "White", 2),
  "Ethnicity, Asian (%)" = n.perc(eth_grp == "Asian", 2),
  "Ethnicity, Black (%)" = n.perc(eth_grp == "Black", 2),
  "Ethnicity, Mixed (%)" = n.perc(eth_grp == "Mixed", 2),
  "Ethnicity, Others (%)" = n.perc(eth_grp == "Others", 2),
  "Ethnicity, Unknown (%)" = n.perc(is.na(eth_grp), 2),
  "Most deprived (%)" = n.perc(imd_quintile == "1 - most deprived", 2),
  "2 (%)" = n.perc(imd_quintile == "2", 2),
  "3 (%)" = n.perc(imd_quintile == "3", 2),
  "4 (%)" = n.perc(imd_quintile == "4", 2),
  "Least deprived (%)" = n.perc(imd_quintile == "5 - least deprived", 2),
  "Unknown (%)" = n.perc(imd_quintile == "unknown", 2)
)], keep.names = "Category")

tbl1n <- transpose(fit_gender[fit10 == FALSE, .(
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "Mean Age (95% CI)" = mean.ci(age_at_index_fit, 0),
  "Male (%)" = n.perc(gender == "M", 2),
  "Female (%)" = n.perc(gender == "F", 2),
  "Age <40 (%)" = n.perc(age_cat == "<40", 2),
  "Age 40-49 (%)" = n.perc(age_cat == "40-49", 2),
  "Age 50-59 (%)" = n.perc(age_cat == "50-59", 2),
  "Age 60-69 (%)" = n.perc(age_cat == "60-69", 2),
  "Age >=70 (%)" = n.perc(age_cat == ">=70", 2),
  "Ethnicity, White (%)" = n.perc(eth_grp == "White", 2),
  "Ethnicity, Asian (%)" = n.perc(eth_grp == "Asian", 2),
  "Ethnicity, Black (%)" = n.perc(eth_grp == "Black", 2),
  "Ethnicity, Mixed (%)" = n.perc(eth_grp == "Mixed", 2),
  "Ethnicity, Others (%)" = n.perc(eth_grp == "Others", 2),
  "Ethnicity, Unknown (%)" = n.perc(is.na(eth_grp), 2),
  "Most deprived (%)" = n.perc(imd_quintile == "1 - most deprived", 2),
  "2 (%)" = n.perc(imd_quintile == "2", 2),
  "3 (%)" = n.perc(imd_quintile == "3", 2),
  "4 (%)" = n.perc(imd_quintile == "4", 2),
  "Least deprived (%)" = n.perc(imd_quintile == "5 - least deprived", 2),
  "Unknown (%)" = n.perc(imd_quintile == "unknown", 2)
)], keep.names = "Category")

tbl1p <- transpose(fit_gender[fit10 == TRUE, .(
  "2019 (%)" = n.perc(index_fit_year == "2019", 2),
  "2020 (%)" = n.perc(index_fit_year == "2020", 2),
  "2021 (%)" = n.perc(index_fit_year == "2021", 2),
  "2022 (%)" = n.perc(index_fit_year == "2022", 2),
  "2023 (%)" = n.perc(index_fit_year == "2023", 2),
  "2024 (%)" = n.perc(index_fit_year == "2024", 2),
  "Mean Age (95% CI)" = mean.ci(age_at_index_fit, 0),
  "Male (%)" = n.perc(gender == "M", 2),
  "Female (%)" = n.perc(gender == "F", 2),
  "Age <40 (%)" = n.perc(age_cat == "<40", 2),
  "Age 40-49 (%)" = n.perc(age_cat == "40-49", 2),
  "Age 50-59 (%)" = n.perc(age_cat == "50-59", 2),
  "Age 60-69 (%)" = n.perc(age_cat == "60-69", 2),
  "Age >=70 (%)" = n.perc(age_cat == ">=70", 2),
  "Ethnicity, White (%)" = n.perc(eth_grp == "White", 2),
  "Ethnicity, Asian (%)" = n.perc(eth_grp == "Asian", 2),
  "Ethnicity, Black (%)" = n.perc(eth_grp == "Black", 2),
  "Ethnicity, Mixed (%)" = n.perc(eth_grp == "Mixed", 2),
  "Ethnicity, Others (%)" = n.perc(eth_grp == "Others", 2),
  "Ethnicity, Unknown (%)" = n.perc(is.na(eth_grp), 2),
  "Most deprived (%)" = n.perc(imd_quintile == "1 - most deprived", 2),
  "2 (%)" = n.perc(imd_quintile == "2", 2),
  "3 (%)" = n.perc(imd_quintile == "3", 2),
  "4 (%)" = n.perc(imd_quintile == "4", 2),
  "Least deprived (%)" = n.perc(imd_quintile == "5 - least deprived", 2),
  "Unknown (%)" = n.perc(imd_quintile == "unknown", 2)
)], keep.names = "Category")

setnames(tbl1, "V1", "All patients")
setnames(tbl1n, "V1", "FIT <10")
setnames(tbl1p, "V1", "FIT >=10")

tbl1c <- merge(tbl1, tbl1n, by = "Category", all = TRUE, sort = FALSE)
tbl1c <- merge(tbl1c, tbl1p, by = "Category", all = TRUE, sort = FALSE)
print(
  xtable(tbl1c),
  type = "html",
  include.rownames = FALSE,
  file = "output/tbl1c.html"
)