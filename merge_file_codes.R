library(tidyverse)
library(purrr)
library(writexl)

source("directory_work.R")

# script designed to clean data from wav2vec output across whole, uncut .wav files

# setup output file name
output_name = "28ppd_clean"

# load and select data
fs = list.files("data-input", full.names = TRUE)
m = utils::menu(choices = fs,
                graphics = TRUE,
                title = "choose data to merge by file_code")

path2data = fs[m]

df0 = read.csv(path2data)
names(df)

df = df0 |> select(-c(transcribed.text, t..s., wer))

# get unique types of files and names in this data-set
file_codes = df |> distinct(file_code) |> unlist()

uniquenms = df |> select(name) |> distinct() |> unlist()

# merge by name and file_code with mean()
mrgd = map(file_codes, function(fc){
  
  merge_by_code = map(uniquenms, function(nm){
    
    g = df |>
      filter(file_code == fc & name == nm) |>
      mutate(prumer = mean(cer))
    
    return(g)
    
  }) |> bind_rows()
  
  return(merge_by_code)
}, .progress = TRUE) |>
  bind_rows() |>
  distinct(name,file_code, prumer, .keep_all = TRUE) |>
  select(-c(cer)) |>
  rename(cer = prumer)

write.csv(mrgd, paste0("data-input/", output_name, ".csv"))
write_xlsx(mrgd, paste0("data-input/", output_name, ".xlsx"))
