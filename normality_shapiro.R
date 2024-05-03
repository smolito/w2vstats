library(tidyverse)
library(purrr)
library(writexl)

source("directory_work.R")

filecode2analyze = "B" # either B/PR/FB or PR for split words data

df0 = read.csv("data-input/15yhc_clean.csv") # either 15yhc or 22ehc; (clean is merged)

cg2analyze = df0 |>
  select(group) |>
  distinct() |>
  as.character()

finalpath = paste0("data-interim/normalita_merged00",
                   filecode2analyze, "_",
                   cg2analyze,
                   "_vs_ppd.csv")

df1 = read.csv("data-input/28ppd_clean.csv")

# shapiro-wilk normality test ----
# df0 is either yhc or ehc

shapiro0_raw = df0 |> filter(file_code == filecode2analyze) |>
  select(cer) |>
  unlist() |>
  as.numeric()

sw = shapiro.test(shapiro0_raw)
typ_dat = "raw"
vysledky0 = tibble(dataset, cg2analyze, typ_dat, filecode2analyze, sw$statistic, sw$p.value)
vysledky1 = vysledky0

# transform data
# transform back
# data_back = sin(transormed_data)^2

shapiro0_t = asin(sqrt(shapiro0_raw))
sw = shapiro.test(shapiro0_t)
typ_dat = "transformed"
vysledky0 = tibble(dataset, cg2analyze, typ_dat, filecode2analyze, sw$statistic, sw$p.value)
vysledky1 = bind_rows(vysledky1, vysledky0)

# df1 are always patiens with pd
shapiro1_raw = df1 |> filter(file_code == filecode2analyze) |>
  select(cer) |>
  unlist() |>
  as.numeric()

sw = shapiro.test(shapiro1_raw)
typ_dat = "raw"
cg2analyze = "28 People with Parkinson's disease"
vysledky0 = tibble(dataset, cg2analyze, typ_dat, filecode2analyze, sw$statistic, sw$p.value)
vysledky1 = bind_rows(vysledky1, vysledky0)

shapiro1_t = asin(sqrt(shapiro1_raw))
sw = shapiro.test(shapiro1_t)
typ_dat = "transformed"
vysledky0 = tibble(dataset, cg2analyze, typ_dat, filecode2analyze, sw$statistic, sw$p.value)
vysledky1 = bind_rows(vysledky1, vysledky0)

vysledky = vysledky1

write.csv(vysledky, finalpath)

# bind all csv's data when all combinations are done

# f = list.files("data-interim", full.names = TRUE)
# 
# alldf = map(f, function(x){
#   csvdf = read.csv(x) |> as_tibble()
#   
#   return(csvdf)
# }) |> bind_rows() |>
#   distinct(cg2analyze, typ_dat, filecode2analyze, .keep_all = TRUE) |>
#   select(-c(X)) |>
#   write_xlsx(path = "data-output/normality_all_mergedb.xlsx")