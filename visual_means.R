library(tidyverse)
library(ggpubr)
library(purrr)

source("directory_work.R")

# means/names
# means = "names"
# df0 = read.csv("data-output/mean_cer_across_names_15 Young Healthy Control.csv", header = TRUE) |>
#   as_tibble()
# 
# df00 = read.csv("data-output/mean_cer_across_names_22 Elderly Healthy Control.csv", header = TRUE) |>
#   as_tibble()
# 
# df1 = read.csv("data-output/mean_cer_across_names_28 People with Parkinson's disease.csv", header = TRUE) |>
#   as_tibble()

# means/word
means = "word"
df0 = read.csv("data-output/mean_cer_by_word_in_group_15 Young Healthy Control.csv", header = TRUE) |>
  as_tibble()

df00 = read.csv("data-output/mean_cer_by_word_in_group_22 Elderly Healthy Control.csv", header = TRUE) |>
  as_tibble()

df1 = read.csv("data-output/mean_cer_by_word_in_group_28 People with Parkinson's disease.csv", header = TRUE) |>
  as_tibble()

cg0 = "15 YHC"
cg00 = "22 EHC"
cg1 = "PPD"

# CHECK! ----
if (means == "word"){
  suffix = "_na_slova"
  ttl = " průměr přes slova"
}

if (means == "names"){
  suffix = "_na_jmeno"
  ttl = " průměr na jméno"
}

# histograms 
h0 = df0 |>
  ggplot(mapping = aes(x = prumer)) + 
  geom_histogram(fill = "springgreen4") + 
  ggtitle(paste0(cg0, ttl)) + 
  labs(x = "průměr CER", y = "počet")

figure = ggarrange(h0)
figure
ggsave(filename = paste0("plots/hist_", cg0, suffix, ".png"))

h00 = df00 |>
  ggplot(mapping = aes(x = prumer)) + 
  geom_histogram(fill = "royalblue1") + 
  ggtitle(paste0(cg00, ttl)) + 
  labs(x = "průměr CER", y = "počet")

figure = ggarrange(h00)
figure
ggsave(filename = paste0("plots/hist_", cg00, suffix, ".png"))

h1 = df1 |>
  ggplot(mapping = aes(x = prumer)) + 
  geom_histogram(fill = "firebrick") + 
  ggtitle(paste0(cg1, ttl)) + 
  labs(x = "průměr CER", y = "počet")

figure = ggarrange(h1)
figure
ggsave(filename = paste0("plots/hist_", cg1, suffix, ".png"))

# q-q plots

h0 = df0 |>
  ggplot(mapping = aes(sample = prumer)) + 
  stat_qq(color = "springgreen4") +
  stat_qq_line() +
  ggtitle(paste0(cg0, ttl)) +
  labs(x = "Teoretické kvantily", y = "CER")

figure = ggarrange(h0)
figure
ggsave(filename = paste0("plots/qq_", cg0, suffix, ".png"))

h00 = df00 |>
  ggplot(mapping = aes(sample = prumer)) + 
  stat_qq(color = "royalblue1") +
  stat_qq_line() +
  ggtitle(paste0(cg00, ttl)) +
  labs(x = "Teoretické kvantily", y = "CER")

figure = ggarrange(h00)
figure
ggsave(filename = paste0("plots/qq_", cg00, suffix, ".png"))

h1 = df1 |>
  ggplot(mapping = aes(sample = prumer)) + 
  stat_qq(color = "firebrick") +
  stat_qq_line() +
  ggtitle(paste0(cg1, ttl)) +
  labs(x = "Teoretické kvantily", y = "CER")

figure = ggarrange(h1)
figure
ggsave(filename = paste0("plots/qq_", cg1, suffix, ".png"))
