library(tidyverse)
library(ggpubr)
library(purrr)
library(writexl)

source("directory_work.R")

# COLORS: yhc = springgreen4, ehc = royalblue1, ppd = firebrick

# script made to create histograms and q-q plots, generates output with the help
# of some hardcoded values: "filecode2analyze", "df0" - which should always be data
# from a control group (yhc/ehc)

# control group all file types (in YHC only b and pr)
df0 = read.csv("data-input/15yhc_clean.csv", header = TRUE) |>
  as_tibble()

names(df0)

# patients group all file types
df1 = read.csv("data-input/28ppd_clean.csv", header = TRUE) |>
  as_tibble()

names(df1)

filecode2analyze = "PR" # either B/PR/FB or PR for split words data
cg2analyze = df0 |> select(group) |>
  distinct() |>
  as.character()

# visual normality => histograms ----
h0 = df0 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(x = cer)) + 
  geom_histogram(fill = "springgreen4") + 
  ggtitle(paste0(cg2analyze, ", typ ", filecode2analyze)) + 
  labs(x = "CER", y = "počet")

h1 = df1 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(x = cer)) + 
  geom_histogram(fill = "firebrick") + 
  ggtitle(paste0("Pacienti s PD", ", typ ", filecode2analyze)) + 
  labs(x = "CER", y = "počet")

figure = ggarrange(h0, h1)

figure

ggsave(filename = paste0("data-output/hist_", cg2analyze, "_vs_ppd_", filecode2analyze, ".png"))

# visual normality => 2 q-q plots ----

h0 = df0 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(sample = cer)) + 
  stat_qq(color = "springgreen4") +
  stat_qq_line() +
  ggtitle(paste0(cg2analyze, ", typ ", filecode2analyze)) +
  labs(x = "Teoretické kvantily", y = "CER")

h1 = df1 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(sample = cer)) + 
  stat_qq(color = "firebrick") +
  stat_qq_line() + 
  ggtitle(paste0("Pacienti s PD", ", typ ", filecode2analyze)) +
  labs(x = "Teoretické kvantily", y = "CER")

figure = ggarrange(h0, h1)

figure

ggsave(filename = paste0("data-output/qq_", cg2analyze,"_vs_ppd_", filecode2analyze, ".png"))

# visual normality => 1 q-q plot ? ----
