
### calculate each SDG score at global level based on different weight across countries

Global_SDG_fun <- function(sdg, w){
  sdg <- sdg[, 1:15] %>% as.data.frame() %>%
    mutate_all(function(x) as.numeric(as.character(x))) %>% as.data.frame()
  
  w   <- w[, 1:15] %>% as.data.frame() %>%
    mutate_all(function(x) as.numeric(as.character(x))) %>% as.data.frame()
  
  sdg_multiply_w <- (sdg * w) %>%           # (sdgi * wi)
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))      # sum(sdgi * wi)
  
  w_col.sum <- w %>%
    summarise_at(.vars = names(.)[1:15],
                 .funs = c(sum="sum"))      # sum(wi)
  
  sdg.w <- sdg_multiply_w/w_col.sum         # sum(sdgi * wi)/sum(wi)
  
  return(sdg.w)
}