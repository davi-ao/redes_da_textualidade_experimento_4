library(tidyverse)
library(openxlsx)

files_slate = list.files('./results/journal_slate/', 
                   pattern = '*_cohesion_indices.xlsx')

files_icic = list.files('./results/letters_icic/', 
                         pattern = '*_cohesion_indices.xlsx')

files_pg = list.files('./results/short_stories/', 
                        pattern = '*_cohesion_indices.xlsx')

files_biomed = list.files('./results/technical_biomed/', 
                        pattern = '*_cohesion_indices.xlsx')

files_gov = list.files('./results/technical_government_media/', 
                        pattern = '*_cohesion_indices.xlsx')

files_plos = list.files('./results/technical_plos/', 
                        pattern = '*_cohesion_indices.xlsx')

indices_slate = lapply(files_slate, function(f) {
  read.xlsx(paste0('./results/journal_slate/', f)) %>%
    mutate(Text = f,
           Genre = 'slate')
}) %>%
  bind_rows()

indices_icic = lapply(files_icic, function(f) {
  read.xlsx(paste0('./results/letters_icic/', f)) %>%
    mutate(Text = f,
           Genre = 'icic')
}) %>%
  bind_rows()

indices_pg = lapply(files_pg, function(f) {
  read.xlsx(paste0('./results/short_stories/', f)) %>%
    mutate(Text = f,
           Genre = 'pg')
}) %>%
  bind_rows()

indices_biomed = lapply(files_biomed, function(f) {
  read.xlsx(paste0('./results/technical_biomed/', f)) %>%
    mutate(Text = f,
           Genre = 'biomed')
}) %>%
  bind_rows()

indices_gov = lapply(files_gov, function(f) {
  read.xlsx(paste0('./results/technical_government_media/', f)) %>%
    mutate(Text = f,
           Genre = 'gov')
}) %>%
  bind_rows()

indices_plos = lapply(files_plos, function(f) {
  read.xlsx(paste0('./results/technical_plos/', f)) %>%
    mutate(Text = f,
           Genre = 'plos')
}) %>%
  bind_rows()

indices = list(indices_biomed,
               indices_gov,
               indices_icic,
               indices_pg,
               indices_plos,
               indices_slate) %>%
  bind_rows()

indices %>%
  write.xlsx('./results/experiment4_cohesion_indices.xlsx')
