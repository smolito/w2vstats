library(tidyverse)
library(purrr)
library(powerjoin)

source("directory_work.R")

df = read.csv("data-input/pr_split2804.csv")

# adjusting word ids for skipped words in: Saverio S, Nicolo C and Giustina M
words = df |> select(word) |> distinct() |> unlist()

ppl_who_skipped = df |> filter(name == "Saverio S" | name == "Nicolo C" | name == "Giustina M")

their_words = ppl_who_skipped |> select(word) |> unlist()

match_wordid2word = map(their_words, function(w){
  
  id = which(w == words)
  
  return(id)
}) |> unlist()

tojoin = tibble(word_id = match_wordid2word, word = their_words)

correct_ids = ppl_who_skipped |>
  power_left_join(tojoin, by = "word", conflict = coalesce_yx) |>
  distinct() |>
  mutate(word_id = word_id - 1) |>
  relocate(word_id, .before = word)

df1 = df |>
  power_left_join(correct_ids, by = c("group", "name", "word"), conflict = coalesce_yx)

write.csv(df1, "data-input/pr_split2804_clean.csv")
