
## *A function for normalization*
# Put all four scenarios together and normalize to SDG score 0-100


Func_norm_sdg_score_good <- function(df_rel_c, df_not_c, df_ner_c, df_dst_c, bottom, top){
  
  # put all data together, and
  # find min and max for further normalization
  all_c <- rbind(df_rel_c, df_not_c, df_ner_c, df_dst_c) %>% as.data.frame()
  all_c.val <- gather(data = all_c, key = cols, value = value)
  # ggplot(all_c.val, aes(x = value)) + geom_histogram()
  
  # quantile(all_c.val$value, probs = c(0.025, 0.975))
  min <- quantile(all_c.val$value, probs = bottom); min
  max <- quantile(all_c.val$value, probs = top);    max
  
  
  ### Creat a function for cal (choose one)
  # 1) the larger the better
  norm <- function(x){
    ifelse(x < min, 0,
           ifelse(x<max, (x-min)/(max-min)*100, 100))
  }
  
  # ## 2) the larger the worse
  # norm <- function(x){
  #   ifelse(x > max, 0, 
  #          ifelse(x>min, (x-max)/(min-max)*100, 100))
  # }
  
  # copy data
  df_rel_c.norm <- df_rel_c
  df_not_c.norm <- df_not_c
  df_ner_c.norm <- df_ner_c
  df_dst_c.norm <- df_dst_c
  
  # apply the function to data
  df_rel_c.norm[] <- lapply(df_rel_c.norm, norm)
  df_not_c.norm[] <- lapply(df_not_c.norm, norm)
  df_ner_c.norm[] <- lapply(df_ner_c.norm, norm)
  df_dst_c.norm[] <- lapply(df_dst_c.norm, norm)
  
  
  # add group code for developed or developing
  # getwd()
  group.file <- './_input_data/_nearby_distant_developed_developing_20181110.xlsx'
  group.code <- read_excel(group.file, sheet = 'group', col_names = T, range = "A1:B42") %>% arrange(nation)
  
  Add_Nation <- function(f){
    df_norm          <- f %>% as.data.frame() %>% mutate(nation   = rownames(df_rel_c))
    df_norm$scenario <- paste0(substr(deparse(substitute(f)), 4, 8))
    df_norm_nation   <- merge(x = df_norm, y = group.code, by.x = 'nation', by.y = 'nation')
  }
  
  ## norm using function we created
  score_c_rel <- Add_Nation(df_rel_c.norm)
  score_c_not <- Add_Nation(df_not_c.norm)
  score_c_ner <- Add_Nation(df_ner_c.norm)
  score_c_dst <- Add_Nation(df_dst_c.norm)
  
  score_c     <- rbind(score_c_rel, score_c_not, score_c_ner, score_c_dst)
  names(score_c)
  names(score_c) <- c('nation', seq(1995, 2009), "scenario", "group")
  return(score_c)
}






Func_norm_sdg_score_bad <- function(df_rel_c, df_not_c, df_ner_c, df_dst_c, bottom, top){
  
  # put all data together, and
  # find min and max for further normalization
  all_c <- as.data.frame(rbind(df_rel_c, df_not_c, df_ner_c, df_dst_c))
  all_c.val <- gather(all_c, cols, value)
  # ggplot(all_c.val, aes(x = value)) + geom_histogram()
  
  # quantile(all_c.val$value, probs = c(0.025, 0.975))
  min <- quantile(all_c.val$value, probs = bottom); min
  max <- quantile(all_c.val$value, probs = top);    max
  
  
  ### Creat a function for cal (choose one)
  ## 1) the larger the better
  # norm <- function(x){
  #   ifelse(x < min, 0,
  #          ifelse(x<max, (x-min)/(max-min)*100, 100))
  # }
  
  ## 2) the larger the worse
  norm <- function(x){
    ifelse(x > max, 0, 
           ifelse(x>min, (x-max)/(min-max)*100, 100))
  }
  
  # copy data
  df_rel_c.norm <- df_rel_c
  df_not_c.norm <- df_not_c
  df_ner_c.norm <- df_ner_c
  df_dst_c.norm <- df_dst_c
  # apply the function to data
  df_rel_c.norm[] <- lapply(df_rel_c.norm, norm)
  df_not_c.norm[] <- lapply(df_not_c.norm, norm)
  df_ner_c.norm[] <- lapply(df_ner_c.norm, norm)
  df_dst_c.norm[] <- lapply(df_dst_c.norm, norm)
  
  
  # add group code for developed or developing
  # getwd()
  group.file <- './_input_data/_nearby_distant_developed_developing_20181110.xlsx'
  group.code <- read_excel(group.file, sheet = 'group', col_names = T, range = "A1:B42")
  group.code <- arrange(group.code, nation)
  
  Add_Nation <- function(f){
    df_norm          <- f %>% as.data.frame() %>% mutate(nation   = rownames(df_rel_c))
    df_norm$scenario <- paste0(substr(deparse(substitute(f)), 4, 8))
    df_norm_nation   <- merge(x = df_norm, y = group.code, by.x = 'nation', by.y = 'nation')
  }
  
  ## norm using function we created
  score_c_rel <- Add_Nation(df_rel_c.norm)
  score_c_not <- Add_Nation(df_not_c.norm)
  score_c_ner <- Add_Nation(df_ner_c.norm)
  score_c_dst <- Add_Nation(df_dst_c.norm)
  
  score_c     <- rbind(score_c_rel, score_c_not, score_c_ner, score_c_dst)
  names(score_c)
  names(score_c) <- c('nation', seq(1995, 2009), "scenario", "group")
  return(score_c)
}
