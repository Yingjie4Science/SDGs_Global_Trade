

library(psych)
library(readxl)
library(tidyverse)
library(data.table)
library(parallel)
library(stringi)


# ----------------------------------------------- #
#                                                 #
#                                                 #
#                                                 #
# ----------------------------------------------- #

# Function : Normize each Indics -----
normalize_SDG <-
  function(x) {
    X <- x[!is.na(x)]
    Xmax <-
      X[order(X, decreasing = T)][1:3] %>% mean(.) %>% rep(., length(x))
    Xmin <- X[order(X, decreasing = F)][2] %>% rep(., length(x))
    ans <- ifelse(Xmax == Xmin, x, (x - Xmin) / (Xmax - Xmin)) * 100
    ans <- winsor(ans, trim = 0.05, na.rm = TRUE)
    return(ans)
  }


# Function : Generate total SDG score in different combination of SDG indics---- 
combnSDG <- function(norm) {
  name_norm <- names(data.frame(norm))
  n <- ncol(norm)
  combn_list <- lapply(1:n, combn, x = n)
  g <- length(combn_list)
  chooseSDG <- function(i) {
    combn <- combn_list[[i]]
    m <- ncol(combn)
    ans_list <-
      lapply(1:m, function(x)
        norm[, combn[, x]] %>% rowMeans(., na.rm = TRUE))
    ans <- as.data.table(ans_list)
    names(ans) <-
      apply(combn, 2, function(x)
        paste(name_norm[x] , collapse = "-"))
    return(ans)
  }
  combnSDG <- lapply(2:g, chooseSDG)
  combnSDG <- c(list(norm), combnSDG)
  return(combnSDG)
}

combnSDG <- function(dt) {
  dt <- as_tibble(dt)
  num <- ncol(dt)
  combn_list <- lapply(1:num, combn, x = num)
  answer <-
    lapply(combn_list, function(x) {
      apply(x, 2, function(y)
        data.frame(
          Province = data[, 1],
          SDG_score = rowMeans(dt[, y],
                               na.rm = TRUE),
          inx_num = dim(dt[, y])[2],
          type = str_c(names(dt[, y]), collapse = "-")
          
        ))
    }) %>%
    lapply(., function(x)
      do.call(rbind, lapply(x, data.frame))) 
  answer <-  do.call(rbind, lapply(answer, data.frame)) %>% as_tibble()
  return(answer)
}
# ----------------------------------------------- #
#                                                 #
#                                                 #
#                                                 #
# ----------------------------------------------- #
# Import Data ----
data <- 
  read_excel("Data/SDG_index.xlsx") %>%
  filter(!grepl("[\u4e00-\u9fa5]", Provinces))

# data <-
#   cbind(data[, 1], data[, -1] %>% apply(., 2, as.numeric))

data <-
  within(data,{
    Provinces[1:31] <- 
      paste(Provinces[1:31],"2000",sep = "")
    Provinces[32:62] <- 
      paste(Provinces[32:62],"2015",sep = "")
  })
# seperate indics for each type ofnSDG
begin_num <- 
  seq(1, ncol(data))[grepl("SDG", names(data))]

end_num <- 
  c(begin_num[-1] - 1, ncol(data))

# separate SDG indics into List form 
SDG_List <-
  lapply(seq(1, length(begin_num)),
         function(i)
           data[, seq(begin_num[i], end_num[i])]) 
SDG_List[[14]]
SDG_normalize <-
  lapply(SDG_List,
         function(x)
           apply(x, 2, normalize_SDG))


# generate each SDG indics
SDG_indics <-
  do.call(cbind, lapply(lapply(SDG_normalize,
                               function(x)
                               {
                                 apply(x, 1, mean, na.rm = TRUE)
                               }) ,
                        data.frame))
names(SDG_indics) <- paste0("SDG",1:ncol(SDG_indics))

# generate total SDG score
Total_SDG <-
  cbind(data[, 1]
        ,
        Total_SDG = apply(SDG_indics, 1, mean, na.rm = TRUE))
# ----------------------------------------------- #
#                                                 #
#                                                 #
#                                                 #
# ----------------------------------------------- #




draw_boxplot <- function(i) {
  df <- combnSDG(SDG_normalize[[i]]) %>% filter( Provinces == "2015")
  file_name <- paste0("Results/","SDG",i,".tiff")
  p <- ggplot(df, aes(x=inx_num, y = SDG_score, group = inx_num))  + 
    stat_boxplot(geom ='errorbar') + 
    geom_boxplot() + 
    xlab("Number of Indicators") + 
    ylab(paste0("SDG",i, "_Score")) + 
    scale_x_continuous(breaks=df$inx_num, labels = as.character(df$inx_num))
  ggsave(filename = file_name, plot = p)
}

lapply(1:17, draw_boxplot)
