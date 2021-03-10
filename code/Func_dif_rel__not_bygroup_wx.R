
# function for Figure 2d
# w = 1

Func_dif_rel__not_bygroup_wx <- function(csv, w){
  
  approach <- substr(csv, 7, 7)
  
  # weight
  w <- w %>% as.data.frame() %>% mutate_all(function(x) as.numeric(as.character(x)))
  
  w_col.sum.group <- w %>%
    group_by(group) %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))      # sum(wi)
  w_col.sum.globe <- w %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))      # sum(wi)
  
  # score read data
  score_x <- read.csv(file = csv, strip.white = T, stringsAsFactors = F)[, -1] %>% 
    as.data.frame()
  
  # by group 
  # -- with trade scenario
  score_rel_bygroup <- score_x %>% filter(scenario == paste0('rel_', approach)) %>% 
    dplyr::select(paste0('X', seq(1995, 2009)), group)
  score_rel_bygroup.w <- (score_rel_bygroup * w) %>%
    group_by(group) %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))                         # sum(sdgi * wi)
  score_rel_bygroup.ww <- score_rel_bygroup.w/w_col.sum.group
  score_rel_bygroup.ww[is.na(score_rel_bygroup.ww)]  <- 0      # the group name for developing country will turn NA (need to change to 0)
  # --- no trade
  score_not_bygroup <- score_x %>%  filter(scenario == paste0('not_', approach)) %>%
    dplyr::select(paste0('X', seq(1995, 2009)), group)
  score_not_bygroup.w  <- (score_not_bygroup * w) %>%
    group_by(group) %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))                         # sum(sdgi * wi)
  score_not_bygroup.ww <- score_not_bygroup.w/w_col.sum.group
  score_not_bygroup.ww[is.na(score_not_bygroup.ww)]  <- 0  
  
  
  # by globe
  # --- with trade
  score_rel_byglobe <- score_x %>% filter(scenario == paste0('rel_', approach)) %>%
    dplyr::select(paste0('X', seq(1995, 2009)), group)
  score_rel_byglobe.w <- (score_rel_byglobe * w) %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))                         # sum(sdgi * wi)
  score_rel_byglobe.ww <- (score_rel_byglobe.w/w_col.sum.globe) %>%
    mutate(group = 'glo') %>%
    dplyr::select(group, paste0('X', seq(1995, 2009), '_sum'))
  # no trade
  score_not_byglobe <- score_x %>% filter(scenario == paste0('not_', approach)) %>%
    dplyr::select(paste0('X', seq(1995, 2009)), group)
  score_not_byglobe.w <- (score_not_byglobe * w) %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))                         # sum(sdgi * wi)
  score_not_byglobe.ww <- (score_not_byglobe.w/w_col.sum.globe) %>%
    mutate(group = 'glo') %>%
    dplyr::select(group, paste0('X', seq(1995, 2009), '_sum'))
  
  
  # cal the diff between rel and not (trade) scenario
  dif_byglobe <- (score_rel_byglobe.ww[, 2:16] - score_not_byglobe.ww[, 2:16]) %>%
    mutate(group = 'glo')
  
  dif_bygroup <- (score_rel_bygroup.ww[, 2:16] - score_not_bygroup.ww[, 2:16]) %>%
    mutate(group = score_not_bygroup.ww$group)
  
  # rbind to one file
  dif_rel__not_bygroup <- rbind(dif_byglobe,
                                dif_bygroup)
  
  # assign(paste('sdg_', substr(csv, 7, 13), sep=''), dif_rel__not_bygroup)
  
  # assign(paste('dif_rel__not_bygroup_', gsub(".*?([0-9]+).*", "\\1", csv), sep=''), dif_rel__not_bygroup)
  # return(dif_rel__not_bygroup)
  
  
  # save to csv
  fname <- paste0('./dif_rel__not_bygroup_w', wname, '_', approach, '_',
                  gsub(".*?([0-9]+).*", "\\1", csv), '.csv'); fname
  write.csv(x = dif_rel__not_bygroup, file = fname)
}



####################################################

# getwd()
# data.path <- 'G:/My Drive/_paper/170923_trade_Global SDGs/results/update_0503_SUM_dist/_output_score'
# setwd(data.path)
# csvs <- list.files(path = data.path, pattern = '^score_c_*'); csvs


## test 
# w     = pop
# wname = 'pop'
# csv = "score_c_6_wat.csv"
# 
# Func_dif_rel__not_bygroup_wx(csv, w)









