library(tidyverse)
library(openxlsx)

files = list.files('./analyses/', pattern = '*_descriptives.xlsx')

descriptives = lapply(files, function(f) {
  read.xlsx(paste0('./analyses/', f)) %>%
    pivot_wider(names_from = 'Variable', values_from = 'Value') %>%
    mutate(text = f)
}) %>%
  bind_rows()

descriptives %>%
  # mutate(text = text %>% str_sub(8, 16) %>%
  #          str_replace_all('247_', '0') %>%
  #          str_replace_all('IP_', '1') %>%
  #          str_remove_all('_') %>%
  #          str_remove_all('d') %>%
  #          as.numeric()) %>%
  arrange(text) %>%
  write.xlsx('./analyses/descriptives.xlsx')

files = list.files('./analyses/', pattern = '*_cohesion_indices.xlsx')

indices = lapply(files, function(f) {
  read.xlsx(paste0('./analyses/', f)) %>%
    mutate(Text = f)
}) %>%
  bind_rows()

indices %>%
  pivot_longer(c(Vertex, Edge), names_to = 'Type', values_to = 'Value') %>%
  group_by(Text, Type, Index) %>%
  summarise(Mean = mean(Value)) %>%
  ggplot(aes(Type, Mean, fill = Index)) +
  geom_boxplot() +
  geom_jitter()
