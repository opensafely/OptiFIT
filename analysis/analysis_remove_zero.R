# load libraries
library(data.table)
library("ggplot2")
library(xtable)

fit <- data.table::fread(here::here("output", "dataset.csv.gz"))
setkey(fit, patient_id)

#first of all, count 