
# function for Figure 2d
# w = 1

Func_dif_rel__not_bygroup_w1 <- function(csv){
  
  approach <- substr(csv, 7, 7)
  
  score_x <- read.csv(file = csv, strip.white = T, stringsAsFactors = F)[, -1] %>% 
    as.data.frame()
  
  # by group
  # --- with trade scenario
  score_rel_bygroup <- score_x %>% 
    filter(scenario == paste0('rel_', approach)) %>%
    group_by(group) %>%
    summarise_at(.vars = names(.)[2:16],
                 .funs = c(mean="mean"))
  # --- no trade 
  score_not_bygroup <- score_x %>% 
    filter(scenario == paste0('not_', approach)) %>%
    group_by(group) %>%
    summarise_at(.vars = names(.)[2:16],
                 .funs = c(mean="mean"))
  
  # by globe
  # --- with trade
  score_rel_byglobe <- score_x %>% 
    filter(scenario == paste0('rel_', approach)) %>%
    summarise_at(.vars = names(.)[2:16],
                 .funs = c(mean="mean")) %>%
    mutate(group = 'glo') %>%
    dplyr::select(group, paste0('X', seq(1995, 2009), '_mean'))
  
  # --- no trade
  score_not_byglobe <- score_x %>% 
    filter(scenario == paste0('not_', approach)) %>%
    summarise_at(.vars = names(.)[2:16],
                 .funs = c(mean="mean")) %>%
    mutate(group = 'glo') %>%
    dplyr::select(group, paste0('X', seq(1995, 2009), '_mean'))
  
  # cal the diff between rel and not (trade) scenario
  dif_byglobe <- (score_rel_byglobe[, 2:16] - score_not_byglobe[, 2:16]) %>%
    mutate(group = 'glo')
  
  dif_bygroup <- (score_rel_bygroup[, 2:16] - score_not_bygroup[, 2:16]) %>%
    mutate(group = score_rel_bygroup$group)
  
  # rbind to one file
  dif_rel__not_bygroup <- rbind(dif_byglobe,
                                dif_bygroup)
  
  # assign(paste('sdg_', substr(csv, 7, 13), sep=''), dif_rel__not_bygroup)
  
  # assign(paste('dif_rel__not_bygroup_', gsub(".*?([0-9]+).*", "\\1", csv), sep=''), dif_rel__not_bygroup)
  # return(dif_rel__not_bygroup)
  
  
  # save to csv
  fname <- paste0('./dif_rel__not_bygroup_wequ_', approach, '_',
                  gsub(".*?([0-9]+).*", "\\1", csv), '.csv')
  # fname
  write.csv(x = dif_rel__not_bygroup, file = fname)
}
