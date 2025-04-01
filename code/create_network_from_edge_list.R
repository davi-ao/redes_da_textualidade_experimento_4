create_network_from_edge_list = function(edge_list, nyms) {
  G =  edge_list %>%
    select(Source, Target) %>%
    as.matrix() %>%
    graph_from_edgelist(directed = F) %>%
    simplify() %>%
    set_edge_attr('cohesion_edge', value = F)
  
  nyms_sub_edges = nyms %>%
    induced_subgraph(V(G)$name[V(G)$name %in% 
                                 V(nyms)$name]) %>%
    ends(., E(.))
  
  for (row in 1:nrow(nyms_sub_edges)) {
    if (!G %>% are_adjacent(nyms_sub_edges[row,1], nyms_sub_edges[row,2])) {
      G = G %>%
        add_edges(c(nyms_sub_edges[row,1], nyms_sub_edges[row,2]),
                  cohesion_edge = T)
    }
  }
  
  v = tibble(name = V(G)$name) %>%
    left_join(
      edge_list %>%
        arrange(segment_id) %>%
        pivot_longer(c(Source, Target),
                     names_to = 'type',
                     values_to = 'name') %>%
        unique() %>%
        group_by(name) %>%
        summarise(segments = paste0(
          '<[',
          paste(unique(segment_id[!is.na(segment_id)]), collapse = ','),
          ']>'))
      )
  
  V(G)$id = V(G)$name
  V(G)$segments = v$segments
  
  segments = V(G)$segments %>%
    str_remove_all('[<\\[\\]>]') %>%
    str_split(',') %>%
    unlist() %>%
    unique() %>%
    as.numeric() %>%
    sort() %>%
    lapply(function(segment) {
      induced_subgraph(G, V(G)[which(
        V(G)$segments %>%
          str_detect(paste0('[\\[,]', segment, '[,\\]]')))]) %>%
        delete_edge_attr('cohesion_edge')
    })
  
  G_next = 1:length(segments) %>%
    lapply(function(s) {
      V_G_next = segments[1:s] %>%
        reduce(union) %>%
        V() %>%
        .$name
      
      G %>%
        induced_subgraph(V_G_next)
    })
  
  list(network = G, segments = segments, G_next = G_next)
}