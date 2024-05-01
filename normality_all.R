library(tidyverse)
library(ggpubr)
library(purrr)
library(writexl)

# root file control

if (!dir.exists("data-input")){
  dir.create("data-input")
}

if (!dir.exists("data-output")){
  dir.create("data-output")
}

if (!dir.exists("data-interim")){
  dir.create("data-interim")
}

# control group all file types (in YHC only b and pr)
df0 = read.csv("data-input/22ehc_2804.csv", header = TRUE) |> as_tibble()
names(df0)

# patients group all file types
df1 = read.csv("data-input/28ppd_2804.csv", header = TRUE) |> as_tibble()
names(df1)

dataset = "celé nahrávky"
filecode2analyze = "FB" # either B/PR/FB or PR for split words data
cg2analyze = df0 |> select(group) |>
  distinct() |>
  as.character()

finalpath = paste0("data-interim/normalita", filecode2analyze,"_",cg2analyze,"_vs_ppd.csv")

# visual normality => hist, q-q ----
# histograms ----
h0 = df0 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(x = cer)) + 
  geom_histogram() + 
  ggtitle(paste0(cg2analyze, " ,typ ", filecode2analyze))

h1 = df1 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(x = cer)) + 
  geom_histogram() + 
  ggtitle(paste0("Pacienti s PD", " ,typ ", filecode2analyze))

figure  = ggarrange(h0, h1)

figure

ggsave(filename = paste0("data-output/hist_",cg2analyze,"_vs_ppd_", filecode2analyze, ".png"))

# q-q plots ----

h0 = df0 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(sample = cer)) + 
  stat_qq() +
  stat_qq_line() +
  ggtitle(paste0(cg2analyze, " ,typ ", filecode2analyze))

h1 = df1 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(sample = cer)) + 
  stat_qq() +
  stat_qq_line() + 
  ggtitle(paste0("Pacienti s PD", " ,typ ", filecode2analyze))

figure  = ggarrange(h0, h1)

figure

ggsave(filename = paste0("data-output/qq_",cg2analyze,"_vs_ppd_", filecode2analyze, ".png"))

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
#   write_xlsx(path = "data-output/normality_all.xlsx")
