library(tidyverse)
library(purrr)
library(writexl)
library(readxl)

source("directory_work.R")

df = read_xlsx("data-input/pr_splitwprsn_id.xlsx") |> select(-c(Column1)) |>
  mutate(cer = as.numeric(cer)) |>
  mutate(word_id = word_id + 1)

grps = df |> select(group) |> distinct() |> unlist()
words = df |> select(word) |> distinct() |> unlist()

# yhc = df |> filter(group == grps[1])
# ehc = df |> filter(group == grps[2])
# ppd = df |> filter(group == grps[3])
# nms = df |> filter(group == grps[3])|> select(name) |> distinct() |> unlist()

# group mean by words ----
prumer_skupin_po_slovu = map(grps, function(skupina){
  
  prumer_skupiny_po_slovu = map(words, function(slovo){
    
    # get df with all ppl in a group, by word
    dfmean = df |>
      filter(word == slovo & group == skupina)
    
    cer = dfmean |> select(cer) |> unlist()
    
    dfmean = dfmean |> select(group, word, word_id) |>
      distinct() |>
      mutate(prumer = mean(cer))
    
    return(dfmean)
  }) |> bind_rows()
  
  return(prumer_skupiny_po_slovu)
}, .progress = TRUE)

shapiro.test(prumer_skupin_po_slovu$group1$prumer)
shapiro.test(prumer_skupin_po_slovu$group2$prumer)
shapiro.test(prumer_skupin_po_slovu$group3$prumer)

map(prumer_skupin_po_slovu, function(df){
  
  nm = df |> select(group) |> distinct() |> unlist()
  
  write.csv(df, paste0("data-output/", "mean_cer_by_word_in_group_", nm, ".csv"))
  write_xlsx(df, paste0("data-output/", "mean_cer_by_word_in_group_", nm, ".xlsx"))
})

# means across all the words by names (person_id)----

skupiny_list = map(grps, function(x){
  y = df |> filter(group == x)
  
  return(y)
})

prumer_skupin_po_jmenu = map(skupiny_list, function(skupina){
  
  uniqueids = skupina |> select(person_id) |> distinct() |> unlist()
  
  prumer_skupiny_po_jmenu = map(uniqueids, function(id){
    
    grp = skupina |> select(group) |> distinct() |> unlist()
    
    # get df with all ppl in a group, by name
    dfmean = skupina |>
      filter(person_id == id & group == grp)
    
    cer = dfmean |> select(cer) |> unlist()
    
    dfmean = dfmean |> select(group, person_id, name) |>
      mutate(prumer = mean(cer)) |>
      distinct()
    
    return(dfmean)
  }) |> bind_rows()
  
  return(prumer_skupiny_po_jmenu)
}, .progress = TRUE)

shapiro.test(prumer_skupin_po_jmenu$group1$prumer)
shapiro.test(prumer_skupin_po_jmenu$group2$prumer)
shapiro.test(prumer_skupin_po_jmenu$group3$prumer)

map(prumer_skupin_po_jmenu, function(df){
  
  nm = df |> select(group) |> distinct() |> unlist()
  
  write.csv(df, paste0("data-output/", "mean_cer_across_names_", nm, ".csv"))
  write_xlsx(df, paste0("data-output/", "mean_cer_across_names_", nm, ".xlsx"))
})