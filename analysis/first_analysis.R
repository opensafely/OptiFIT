# load libraries
library(data.table)

dt <- data.table::fread(here::here("output", "dataset.csv"))
data.table::setkey(dt,patient_id)
