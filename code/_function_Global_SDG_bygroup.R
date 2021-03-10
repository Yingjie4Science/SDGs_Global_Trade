


library(dplyr)

Global_SDG_bygroup_fun <- function(sdg, w){
  
  string.name <- deparse(substitute(sdg))           # convert the name of a dataframe to a string
  scenario.name  <- str_sub(string.name, start= -3) # Extracting the last n characters from a string
  
  
  sdg   <- sdg %>% as.data.frame() %>%
    dplyr::select(as.character(seq(1995, 2009)), group) %>%
    mutate_all(function(x) as.numeric(as.character(x)))
  
  w <- w %>% as.data.frame() %>%
    dplyr::select(paste0('X', seq(1995, 2009)), group) %>%
    mutate_all(function(x) as.numeric(as.character(x)))
  
  sdg_multiply_w <- (sdg * w) %>%           # (sdgi * wi)
    group_by(group) %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))      # sum(sdgi * wi)
  
  w_col.sum <- w %>% 
    group_by(group) %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))      # sum(wi)
  
  sdg.w <- sdg_multiply_w/w_col.sum         # sum(sdgi * wi)/sum(wi)
  sdg.w[is.na(sdg.w)] <- 0                  # the group name for developing country will turn NA (need to change to 0)
  
  # add scenario col
  sdg.w$scenario <- scenario.name
  
  sdg.w <- as.data.frame(sdg.w)
  
  return(sdg.w)
}


# test
# test <- Global_SDG_bygroup_fun(SDGc_rel, pop)









