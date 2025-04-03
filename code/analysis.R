library(tidyverse)
library(openxlsx)
library(sjPlot)
library(igraph)
library(tidygraph)

# Processed texts --------------------------------------------------------------
data = list('results/journal_slate/',
              'results/letters_icic/',
              'results/technical_biomed/',
              'results/technical_government_media/',
              'results/technical_plos/',
              'results/short_stories/') %>%
  lapply(function(path) {
    list.files(path, '_data.xlsx') %>%
      lapply(function(f) {
        read.xlsx(paste0(path, f)) %>%
          mutate(Text = f, Genre = path)
      }) %>%
      bind_rows()
  }) %>%
  bind_rows() %>%
  as_tibble()

data %>%
  write_csv('results/experiment4_data.csv')

tokens = data %>%
  filter(upos %in% c('ADJ', 'ADV', 'NOUN', 'NUM', 'PROPN', 'VERB')) %>%
  distinct(stem) %>%
  as_tibble()

# Descriptive statistics -------------------------------------------------------
descriptives = list('results/journal_slate/',
                    'results/letters_icic/',
                    'results/technical_biomed/',
                    'results/technical_government_media/',
                    'results/technical_plos/',
                    'results/short_stories/') %>%
  lapply(function(path) {
    list.files(path, '_descriptives.xlsx') %>%
      lapply(function(f) {
        read.xlsx(paste0(path, f)) %>%
          pivot_wider(names_from = 'Variable', values_from = 'Value') %>%
          mutate(Text = f)
      }) %>%
      bind_rows()
  }) %>%
  bind_rows()

# Indices (texts) --------------------------------------------------------------
indices_texts = list('results/journal_slate/',
               'results/letters_icic/',
               'results/technical_biomed/',
               'results/technical_government_media/',
               'results/technical_plos/',
               'results/short_stories/') %>%
  lapply(function(path) {
    list.files(path, '_cohesion_indices.xlsx') %>%
      lapply(function(f) {
        read.xlsx(paste0(path, f)) %>%
          mutate(Text = f, Genre = path)
      }) %>%
      bind_rows()
  }) %>%
  bind_rows() %>%
  mutate(Text = Text %>% str_remove_all('_cohesion_indices.xlsx')) %>%
  left_join(descriptives %>%
              mutate(Text = Text %>% str_remove_all('_descriptives.xlsx')),
            by = 'Text') %>%
  as_tibble()

# Non-texts (level 3) ----------------------------------------------------------

# Word length
word_len = tokens %>%
  mutate(len = stem %>% str_length())

word_len_stats = word_len %>%
  summarise(N = n(),
            mean = mean(len),
            sd = sd(len),
            mean_log = mean(log(len)),
            sd_log = sd(log(len)))

word_len %>%
  ggplot(aes(len)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1) +
  stat_function(fun = dlnorm, 
                args = list(meanlog = word_len_stats$mean_log, 
                            sdlog = word_len_stats$sd_log), 
                color = '#E41A1CFF') +
  stat_function(fun = dnorm,
                args = list(mean = word_len_stats$mean, 
                            sd = word_len_stats$sd), 
                color = '#377EB8FF')  +
  xlim(c(-5, NA))

nonwords = 1:(nrow(tokens)) %>%
  lapply(function(t) {
    nonword_len = sample(rlnorm(word_len_stats$N,
                                word_len_stats$mean_log,
                                word_len_stats$sd_log),
                         1) %>% round()
    
    nonword = tibble(letters) %>%
      slice_sample(n = 1) %>%
      .$letters
    
    repeat {
      if (str_length(nonword) < nonword_len) {
        next_character = tibble(letters) %>%
          slice_sample(n = 1) %>%
          .$letters
        
        nonword = str_c(nonword, next_character, collapse = '')
      } else {
        break
      }
    }
    
    tibble(nonword)
  }) %>%
  bind_rows()

nonwords %>%
  write.xlsx('results/nonwords.xlsx')

# Segment length
segment_len = data %>%
  group_by(Text, segment_id) %>%
  count() %>%
  ungroup()

segment_len_stats = segment_len %>%
  summarise(N = n(),
            mean = mean(n),
            sd = sd(n),
            mean_log = mean(log(n)),
            sd_log = sd(log(n)))

segment_len %>%
  ggplot(aes(n)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5) +
  stat_function(fun = dlnorm, 
                args = list(meanlog = segment_len_stats$mean_log, 
                            sdlog = segment_len_stats$sd_log), 
                color = '#E41A1CFF') +
  stat_function(fun = dnorm, 
                args = list(mean = segment_len_stats$mean, 
                            sd = segment_len_stats$sd), 
                color = '#377EB8FF') +
  xlim(c(-15, NA))

text_len = data %>%
  group_by(Text, segment_id) %>%
  count() %>%
  select(-n) %>%
  group_by(Text) %>%
  count() %>%
  ungroup()
  
text_len_stats = text_len %>%
  summarise(N = n(),
            mean = mean(n),
            sd = sd(n),
            mean_log = mean(log(n)),
            sd_log = sd(log(n)))

text_len %>%
  ggplot(aes(n)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 35) +
  stat_function(fun = dlnorm, 
                args = list(meanlog = text_len_stats$mean_log, 
                            sdlog = text_len_stats$sd_log), 
                color = '#E41A1CFF') +
  stat_function(fun = dnorm, 
                args = list(mean = text_len_stats$mean, 
                            sd = text_len_stats$sd), 
                color = '#377EB8FF')  +
  xlim(c(-5, NA))

nontexts = 1:145 %>% lapply(function(i) {
  t_len = rlnorm(text_len_stats$N, 
                 segment_len_stats$mean_log, 
                 segment_len_stats$sd_log) %>%
    sample(1) %>%
    round()
  1:t_len %>% lapply(function(j) {
    nonwords %>%
      slice_sample(n = rlnorm(segment_len_stats$N,
                              segment_len_stats$mean_log,
                              segment_len_stats$sd_log) %>% 
                     sample(1) %>% 
                     round()) %>%
      mutate(nontext = paste0('nontext', 
                              str_pad(i, 3, 'left', '0')),
             segment_id = j)
  }) %>%
    bind_rows()
}) %>%
  bind_rows() %>%
  group_by(nontext)

nontexts %>%
  write.xlsx('results/nontexts.xlsx')

# Networks
networks = list('results/journal_slate/',
                'results/letters_icic/',
                'results/technical_biomed/',
                'results/technical_government_media/',
                'results/technical_plos/',
                'results/short_stories/') %>%
  lapply(function(path) {
    list.files(path, '_network.xlsx') %>%
      lapply(function(f) {
        vertices = read.xlsx(paste0(path, f), sheet = 'Vertices')
        edges = read.xlsx(paste0(path, f), sheet = 'Edges')
        
        graph_from_data_frame(edges, directed = F, vertices = vertices)
      })
  })

networks = c(networks[[1]],
             networks[[2]],
             networks[[3]],
             networks[[4]],
             networks[[5]],
             networks[[6]])

segments = lapply(networks, function(G) {
  segment_ids = V(G)$Timestamp %>%
    str_remove_all('[<\\[\\]>]') %>%
    str_split(',') %>%
    unlist() %>%
    unique() %>%
    as.numeric() %>%
    sort()
  
  segments = lapply(segment_ids, function(s) {
    G %>%
      as_tbl_graph() %>%
      activate(nodes) %>%
      filter(Timestamp %>% str_detect(paste0('[\\[,]', s ,'[\\],]'))) %>%
      as.igraph()
  })
  
  segments
})

segments_edge_p = segments %>%
  lapply(function(G) {
    G %>% 
      lapply(function(s) {
        s_i = s %>% 
          delete_edges(E(s)[which(CohesionEdge == 1)])
        
        tibble(n_s_i = gorder(s_i), density_s_i = s_i %>% edge_density())
      }) %>%
      bind_rows()
  }) %>%
  bind_rows()

segments_edge_p %>%
  write_csv('results/experiment4_segments_edge_p.csv')

segments_edge_p_stats = segments_edge_p %>%
  summarise(N = n(),
            mean = mean(density_s_i),
            sd = sd(density_s_i),
            mean_log = mean(log(density_s_i)),
            sd_log = sd(log(density_s_i)))

segments_edge_p %>%
  ggplot(aes(density_s_i)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .1) +
  stat_function(fun = dlnorm, 
                args = list(meanlog = segments_edge_p_stats$mean_log, 
                            sdlog = segments_edge_p_stats$sd_log), 
                color = '#E41A1CFF') +
  stat_function(fun = dnorm, 
                args = list(mean = segments_edge_p_stats$mean, 
                            sd = segments_edge_p_stats$sd), 
                color = '#377EB8FF')

segments_edge_p %>%
  mutate(log_n = log(n_s_i), 
         log_d = log(density_s_i)) %>%
  ggplot(aes(log_n, log_d)) +
  geom_point() +
  geom_smooth(method = 'lm',
              colour = '#4DAF4A')

segments_edge_p %>%
  mutate(log_n = log(n_s_i), 
         log_d = log(density_s_i)) %>%
  lm(log_d ~ log_n, data = .) %>%
  summary()

# exp((-0.89 * log(x)) + 0.59)

n_s_i = nontexts %>%
  group_by(nontext, segment_id) %>%
  mutate(n_s_i = n()) %>%
  select(-nonword) %>%
  ungroup() %>%
  distinct()

nontexts_edges = nontexts %>%
  group_by(nontext, segment_id) %>%
  rename(Source = nonword) %>%
  mutate(Target = Source) %>%
  expand(Source, Target) %>%
  rowwise() %>%
  mutate(edge_id = paste(sort(c(Source, Target)), collapse = '-')) %>%
  ungroup() %>%
  filter(Source != Target) %>%
  distinct(nontext, segment_id, edge_id, .keep_all = T) %>%
  left_join(n_s_i, by = c('nontext', 'segment_id')) %>%
  mutate(p = exp((-0.89 * log(n_s_i)) + 0.59)) %>%
  mutate(keep = rbinom(n(), 1, p) == 1) %>%
  filter(keep) %>%
  select(-c(n_s_i, p, keep, edge_id))

nonnyms = nonwords$nonword %>%
  lapply(function(n) {
    tibble(lemma = n, nyms = nonwords$nonword) %>%
      filter(lemma != nyms) %>%
      mutate(keep = rbinom(n(), 1, .001) == 1) %>%
      filter(keep) %>%
      select(-keep)
  }) %>%
  bind_rows() %>%
  rowwise() %>%
  mutate(edge_id = paste(sort(c(lemma, nyms)), collapse = '-')) %>%
  ungroup() %>%
  distinct(edge_id, .keep_all = T) %>%
  select(-edge_id)

nonnyms_network = nonnyms %>%
  as.matrix() %>% 
  graph_from_edgelist(directed = F) %>%
  simplify()

source('code/create_network_from_edge_list.R')

nontexts_networks = nontexts_edges$nontext %>% 
  unique() %>%
  lapply(function(nt) {
    nontexts_edges %>%
      filter(nontext == nt) %>%
      create_network_from_edge_list(nonnyms_network)
  })

source('code/cohesion_indices_functions.R')

nontexts_global_cohesion = 1:length(nontexts_networks) %>%
  lapply(function(i) {
    calculate_global_cohesion(nontexts_networks[[i]]$segments, 
                              nontexts_networks[[i]]$G_next,
                              nonnyms_network) %>%
      mutate(nontext = i)
  }) %>%
  bind_rows()

nontexts_local_cohesion = 1:length(nontexts_networks) %>%
  lapply(function(i) {
    calculate_local_cohesion(nontexts_networks[[i]]$segments,
                             nonnyms_network) %>%
      mutate(nontext = i)
  }) %>%
  bind_rows()

nontexts_pairwise_cohesion = 1:length(nontexts_networks) %>%
  lapply(function(i) {
    calculate_pairwise_cohesion(nontexts_networks[[i]]$segments,
                                nonnyms_network) %>%
      mutate(nontext = i)
  }) %>%
  bind_rows()

indices_nontexts = nontexts_global_cohesion %>%
  mutate(index = 'global') %>%
  bind_rows(
    nontexts_local_cohesion %>%
      mutate(index = 'local')
  ) %>%
  bind_rows(
    nontexts_pairwise_cohesion %>%
      mutate(index = 'pairwise')
  )

nontexts_tokens_n = nontexts %>%
  group_by(nontext) %>%
  distinct(nonword) %>%
  count() %>%
  rename(Text = nontext,
         N_tokens = n)

indices = indices_texts %>%
  select(Genre, 
         Text, 
         Segment, 
         Index, 
         Vertex, 
         Edge, 
         `Number of segments`, 
         `Number of unique tokens (stems)`) %>%
  rename(N_segments = `Number of segments`,
         N_tokens = `Number of unique tokens (stems)`) %>%
  bind_rows(
    indices_nontexts %>%
      rename(Segment = segment,
             Index = index,
             Vertex = v,
             Edge = e,
             Text = nontext) %>%
      mutate(Genre = 'nontext',
             Text = Text %>% 
               str_pad(3, 'left', 0) %>%
               paste0('nontext', .)) %>%
      group_by(Text, Index) %>%
      mutate(N_segments = n()) %>% 
      left_join(nontexts_tokens_n, by = 'Text')
  )

indices %>% 
  write_csv('results/experiment4_cohesion_indices.csv')

# Mean indices -----------------------------------------------------------------
mean_indices = indices %>%
  group_by(Genre, Text, Index) %>%
  summarise(M_Vertex = mean(Vertex),
            M_Edge = mean(Edge),
            n_segments = mean(N_segments),
            n_seg_log = mean(log(N_segments)))

mean_indices %>%
  filter(Genre != 'nontext') %>%
  pivot_longer(c(M_Vertex, M_Edge)) %>%
  ggplot(aes(n_seg_log, value)) +
  facet_grid(rows = vars(name), cols = vars(Index), scales = 'free_y') +
  geom_point() +
  geom_smooth(method = 'lm')

mean_indices %>%
  ggplot(aes(Genre, n_seg_log)) +
  geom_boxplot()

# Outliers ---------------------------------------------------------------------
outliers = mean_indices %>%
  filter(Genre != 'nontext') %>%
  pivot_longer(c(M_Vertex, M_Edge)) %>%
  group_by(Genre, Index, name) %>%
  mutate(IQR = IQR(value),
         min = quantile(value, .25) - (1.5 * IQR(value)),
         max = quantile(value, .75) + (1.5 * IQR(value)),
         outlier = value < min | value > max) %>%
  filter(outlier)

outliers %>%
  write.xlsx('results/outliers.xlsx')

# Models -----------------------------------------------------------------------

# Vertex Global
model_vertex_global = mean_indices %>%
  filter(Index == 'global') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Vertex ~ Genre * n_seg_log, data = .)

model_vertex_global %>%
  summary()

plot_model(model_vertex_global, 
           type = 'eff', 
           terms = c('n_seg_log', 'Genre'),
           show.data = T)

# Vertex Local
model_vertex_local_interactions = mean_indices %>%
  filter(Index == 'local') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Vertex ~ Genre * n_seg_log, data = .)

model_vertex_local_interactions %>%
  summary()

plot_model(model_vertex_local_interactions, 
           type = 'eff', 
           terms = c('n_seg_log', 'Genre'),
           show.data = T)

model_vertex_local = mean_indices %>%
  filter(Index == 'local') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Vertex ~ Genre, data = .)

model_vertex_local %>%
  summary()

plot_model(model_vertex_local, 
           type = 'eff', 
           terms = c('Genre'))

# Vertex Pairwise
model_vertex_pairwise_interactions = mean_indices %>%
  filter(Index == 'pairwise') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Vertex ~ Genre * n_seg_log, data = .)

model_vertex_pairwise_interactions %>%
  summary()

plot_model(model_vertex_pairwise_interactions, 
           type = 'eff', 
           terms = c('n_seg_log', 'Genre'),
           show.data = T)

# Edge Global
model_edge_global = mean_indices %>%
  filter(Index == 'global') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Edge ~ Genre * n_seg_log, data = .)

model_edge_global %>%
  summary()

plot_model(model_edge_global, 
           type = 'eff', 
           terms = c('n_seg_log', 'Genre'),
           show.data = T)

# Edge Local
model_edge_local_interactions = mean_indices %>%
  filter(Index == 'local') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Edge ~ Genre * n_seg_log, data = .)

model_edge_local_interactions %>%
  summary()

plot_model(model_edge_local_interactions, 
           type = 'eff', 
           terms = c('n_seg_log', 'Genre'),
           show.data = T)

model_edge_local = mean_indices %>%
  filter(Index == 'local') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Edge ~ Genre, data = .)

model_edge_local %>%
  summary()

plot_model(model_edge_local, 
           type = 'eff', 
           terms = c('Genre'),
           show.data = T)

# Edge Pairwise
model_edge_pairwise_interactions = mean_indices %>%
  filter(Index == 'pairwise') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Edge ~ Genre * n_seg_log, data = .)

model_edge_pairwise_interactions %>%
  summary()

plot_model(model_edge_pairwise_interactions, 
           type = 'eff', 
           terms = c('n_seg_log', 'Genre'),
           show.data = T)

model_edge_pairwise = mean_indices %>%
  filter(Index == 'pairwise') %>%
  mutate(Genre = Genre %>% as_factor()) %>%
  lm(M_Edge ~ Genre, data = .)

model_edge_pairwise %>%
  summary()

plot_model(model_edge_pairwise, 
           type = 'eff', 
           terms = c('Genre'),
           show.data = T)

# Cummulative distribution of cohesion indices ---------------------------------
indices %>%
  pivot_longer(c(Vertex, Edge)) %>%
  group_by(Text, Index, name) %>%
  mutate(cum_value = cumsum(value),
         relative_position = Segment/max(Segment)) %>%
  ggplot(aes(relative_position, cum_value, group = Text, color = Genre)) +
  geom_line(alpha = .5) +
  facet_wrap(vars(name, Index), scales = 'free')

indices %>%
  pivot_longer(c(Vertex, Edge)) %>%
  group_by(Text, Index, name) %>%
  mutate(cum_value = cumsum(value),
         relative_position = Segment/max(Segment)) %>%
  ggplot(aes(relative_position, cum_value, color = Genre)) +
  geom_smooth() +
  facet_wrap(vars(name, Index), scales = 'free')
