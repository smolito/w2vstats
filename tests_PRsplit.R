library(tidyverse)

# means/names ----
df0 = read.csv("data-output/mean_cer_across_names_15 Young Healthy Control.csv", header = TRUE) |>
  as_tibble() |>
  select(-c(X))

df00 = read.csv("data-output/mean_cer_across_names_22 Elderly Healthy Control.csv", header = TRUE) |>
  as_tibble() |>
  select(-c(X))

df1 = read.csv("data-output/mean_cer_across_names_28 People with Parkinson's disease.csv", header = TRUE) |>
  as_tibble() |>
  select(-c(X))

shapiro.test(df0$prumer)
shapiro.test(df00$prumer)
shapiro.test(df1$prumer)

data2test = bind_rows(df0, df00, df1)

kruskal.test(prumer ~ group, data = data2test)
# mezi skupinami s prumery na jmeno je statisticky významný rozdíl

# means/word ----
df0 = read.csv("data-output/mean_cer_by_word_in_group_15 Young Healthy Control.csv", header = TRUE) |>
  as_tibble() |>
  select(-c(X))

df00 = read.csv("data-output/mean_cer_by_word_in_group_22 Elderly Healthy Control.csv", header = TRUE) |>
  as_tibble() |>
  select(-c(X))

df1 = read.csv("data-output/mean_cer_by_word_in_group_28 People with Parkinson's disease.csv", header = TRUE) |>
  as_tibble() |>
  select(-c(X))

shapiro.test(df0$prumer)
shapiro.test(df00$prumer)
shapiro.test(df1$prumer)

# variance yhc vs ppd
var.test(df0$prumer, df1$prumer)

# variance ehc vs ppd
var.test(df00$prumer, df1$prumer)

# t-test yhc vs ppd
t.test(df0$prumer, df1$prumer, var.equal = TRUE)

# t-test ehc vs ppd 
t.test(df00$prumer, df1$prumer, var.equal = TRUE)

# anova 
data2test = bind_rows(df0, df00, df1)
oneway.test(prumer ~ group, data = data2test, var.equal = TRUE)
