

single_diff_func_b <- function(score_b){
  sum.global  <- score_b %>% 
    group_by(scenario) %>%
    summarise_at(.vars = names(.)[2:16], .funs = c(mean="mean")) %>%
    mutate(group = 'glo') %>%
    select(scenario, group, paste0(seq(1995, 2009), '_mean')) %>%
    as.data.frame()
  
  sum.bygroup <- score_b %>% 
    group_by(scenario, group) %>%
    summarise_at(.vars = names(.)[2:16], .funs = c(mean="mean")) %>%
    mutate(group = as.factor(group)) %>%
    as.data.frame()
  
  class(sum.bygroup)
  
  sum <- rbind(sum.global, sum.bygroup) %>%
    melt(. , id = c('group', 'scenario')) %>%        # wide to long
    mutate(group_scenario = paste0(group, '_', scenario),
           year = gsub('_mean', '', variable)) %>%
    dplyr::select(group_scenario, year, value) %>%
    spread(key = group_scenario, value = value) %>%  # long to wide
    mutate(diff_glo = glo_dst_b - glo_ner_b,
           diff_1   = `1_dst_b` - `1_ner_b`,
           diff_0   = `0_dst_b` - `0_ner_b`) %>%
    dplyr::select(year, diff_glo, diff_1, diff_0) %>%
    gather(group_scenario, value, 2:4)  %>%         # wide to long
    mutate(name = gsub('.xlsx', '', df.xlsx)) %>%
    spread(key = year, value = value)               # long to wide
  
  # write to xlsx
  out.dir <- paste0(dir, "/update_0503_SUM_dist/_output_score")
  f.name <- paste0(out.dir, '/diff_score_b_', gsub('.xlsx', '', df.xlsx), '.csv'); f.name
  write.csv(x = sum, file = f.name)
  
}




# test --------------------------------------------------------------------

# single_diff_func(score_b)
