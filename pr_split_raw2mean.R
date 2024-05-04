library(tidyverse)
library(purrr)
library(writexl)

source("directory_work.R")

df = read.csv("data-input/pr_split2804_clean.csv") |> select(-c(X))

grps = df |> select(group) |> distinct() |> unlist()
words = df |> select(word) |> distinct() |> unlist()

# yhc = df |> filter(group == grps[1])
# ehc = df |> filter(group == grps[2])
# ppd = df |> filter(group == grps[3])

# getting mean of dependent measurements ----

mergnute_zaznamy_na_slovo = map(grps, function(skupina){
  
  df0 = df |> filter(group == skupina)
  uniquenms0 = df0 |> select(name) |> distinct() |> unlist()
  
  mrgd = map(words, function(slovo){
    
    merge_by_code = map(uniquenms0, function(nm){
      
      g = df0 |>
        filter(word == slovo & name == nm) |>
        mutate(prumer = mean(cer))
      
      return(g)
      
    }) |> bind_rows()
    
    return(merge_by_code)
  }) |>
    bind_rows() |>
    distinct(name, word, word_id, prumer, .keep_all = TRUE) |>
    select(-c(cer, t, transcript, wer)) |>
    rename(cer = prumer)
  
}, .progress = TRUE)

# group mean by words ----
prumer_skupin_po_slovu = map(mergnute_zaznamy_na_slovo, function(skupina){
  
  prumer_skupiny_po_slovu = map(words, function(slovo){
    
    grp = skupina |> select(group) |> distinct() |> unlist()
    
    # get df with all ppl in a group, by word
    dfmean = skupina |>
      filter(word == slovo & group == grp)
    
    cer = dfmean |> select(cer) |> unlist()
    
    dfmean = dfmean |> select(-c(name, cer)) |>
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

# TODO: means across all the words by names ----

prumer_skupin_po_jmenu = map(mergnute_zaznamy_na_slovo, function(skupina){
  
  uniquenms = skupina |> select(name) |> distinct() |> unlist()
  
  prumer_skupiny_po_jmenu = map(uniquenms, function(jmeno){
    
    grp = skupina |> select(group) |> distinct() |> unlist()
    
    # get df with all ppl in a group, by name
    dfmean = skupina |>
      filter(name == jmeno & group == grp)
    
    cer = dfmean |> select(cer) |> unlist()
    
    dfmean = dfmean |> select(-c(word, word_id, cer)) |>
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
