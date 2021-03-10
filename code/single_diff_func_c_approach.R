

single_diff_func_c <- function(score_c){
  sum.global  <- score_c %>% 
    group_by(scenario) %>%
    summarise_at(.vars = names(.)[2:16], .funs = c(mean="mean")) %>%
    dplyr::mutate(group = 'glo') %>%
    dplyr::select(scenario, group, paste0(seq(1995, 2009), '_mean')) %>%
    as.data.frame()
  
  sum.bygroup <- score_c %>% 
    group_by(scenario, group) %>%
    summarise_at(.vars = names(.)[2:16], .funs = c(mean="mean")) %>%
    mutate(group = as.factor(group)) %>%
    as.data.frame()
  
  # return(sum.bygroup)
  
  class(sum.bygroup)
  
  sum <- rbind(sum.global, sum.bygroup) %>%
    melt(. , id = c('group', 'scenario')) %>%        # wide to long
    dplyr::mutate(group_scenario = paste0(group, '_', scenario),
                  year           = gsub('_mean', '', variable)) %>%
    dplyr::select(group_scenario, year, value) %>%
    spread(key = group_scenario, value = value) %>%  # long to wide
    dplyr::mutate(diff_glo = glo_dst_c - glo_ner_c,
                  diff_1   = `1_dst_c` - `1_ner_c`,
                  diff_0   = `0_dst_c` - `0_ner_c`) %>%
    dplyr::select(year, diff_glo, diff_1, diff_0) %>%
    gather(group_scenario, value, 2:4)  %>%         # wide to long
    dplyr::mutate(name = gsub('.xlsx', '', df.xlsx)) %>%
    spread(key = year, value = value)               # long to wide
  
  # write to xlsx
  out.dir <- paste0(dir, "/update_0503_SUM_dist/_output_score")
  f.name <- paste0(out.dir, '/diff_score_c_', gsub('.xlsx', '', df.xlsx), '.csv'); f.name
  write.csv(x = sum, file = f.name)
  
}




# test --------------------------------------------------------------------

# single_diff_func(score_c)
