library(tidyverse)
library(ggpubr)

# root file control

if (!dir.exists("data-input")){
  dir.create("data-input")
}

if (!dir.exists("data-output")){
  dir.create("data-output")
}

# control group all file types (in YHC only b and pr)
df0 = read.csv("data-input/15yhc_2804.csv", header = TRUE) |> as_tibble()
names(df0)

# patients group all file types
df1 = read.csv("data-input/28ppd_2804.csv", header = TRUE) |> as_tibble()
names(df1)

filecode2analyze = "B" # either B/PR/FB or PR for split words data
cg2analyze = df0 |> select(group) |>
  distinct() |>
  as.character()

m = utils::menu(choices = cg2analyze,
                title = "vybrání kontrolní skupiny")

cg2analyze = cg2analyze[m]

# visual normality => hist, q-q ----
# histograms ----
h0 = df0 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(x = cer)) + 
  geom_histogram() + 
  ggtitle(paste0(cg2analyze, " typ ", filecode2analyze))

h1 = df1 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(x = cer)) + 
  geom_histogram() + 
  ggtitle(paste0("Pacienti s PD", " typ ", filecode2analyze))

figure  = ggarrange(h0, h1)

figure

ggsave(filename = paste0("data-output/hist_",cg2analyze,"_vs_ppd_", filecode2analyze, ".png"))

# q-q plots ----

h0 = df0 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(sample = cer)) + 
  stat_qq() +
  stat_qq_line() +
  ggtitle(paste0(cg2analyze, " typ ", filecode2analyze))

h1 = df1 |> filter(file_code == filecode2analyze) |>
  ggplot(mapping = aes(sample = cer)) + 
  stat_qq() +
  stat_qq_line() + 
  ggtitle(paste0("Pacienti s PD", " typ ", filecode2analyze))

figure  = ggarrange(h0, h1)

figure

ggsave(filename = paste0("data-output/qq_",cg2analyze,"_vs_ppd_", filecode2analyze, ".png"))

# shapiro-wilk normality test ----
