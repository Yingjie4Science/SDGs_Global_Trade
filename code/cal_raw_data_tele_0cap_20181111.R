
# ------------- cal dat same as SDG with trade 1995-2009+ROW-0401.xlsx --------------

# To clear your environment 
remove(list = ls())

# set work dir
setwd("c:")
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
getwd()

setwd(paste0(dir, "/update_0503_SUM_dist")); getwd()

library("readxl")

df.xlsx <- "0_capital.xlsx"
# df.xlsx <- "6_wat.xlsx";          SDG <- 'SDG6'
# df.xlsx <- "7_ene.xlsx";          SDG <- 'SDG7'
# df.xlsx <- "8_mat.xlsx";          SDG <- 'SDG8'
# df.xlsx <- "9_co2.xlsx";          SDG <- 'SDG9'
# df.xlsx <- "12_mat-perCap.xlsx";  SDG <- 'SDG12'
# df.xlsx <- "13_co2-forest.xlsx";  SDG <- 'SDG13'
# df.xlsx <- "15_for.xlsx";         SDG <- 'SDG15'


# functions 
Net_Import_Cal <- function(df.xlsx) {
  # trade matrix
  s1 = read_excel(df.xlsx, sheet = 1, col_names = T, range = "B1:AP616")
  s2 = matrix
  
  # nearby trade matrix
  s3 = s1 * s2
  
  # add country name as a new col
  countries <- colnames(s2[1:41])
  colnames(s3) <- countries; length(countries)
  
  # add year seq to data frame 
  year_seq = rep(seq(1995, 2009, by=1), each = 41)
  s3$year <- year_seq
  
  # total nearby imports
  s4 <- s3 %>% group_by(year) %>% summarise_all(funs(sum))
  
  # tanspose data frame and keep the first col as new header
  s5 = as.data.frame(t(s4[,-1]))
  colnames(s5) <- seq(1995, 2009, by=1)
  s5 <- s5[ order(row.names(s5)), ]
  
  # total nearby exports
  library("reshape2") # expand long table to wide one
  s6 <- s3 %>%
    mutate(row.sum   = rowSums(s3[,1:41]),
           year      = year_seq,
           countries = rep(countries, times = 15)) %>% # times=15, each = 15
    dcast(countries ~ year, value.var = 'row.sum') # year as new col names
  s6 <- data.frame(s6[,-1], row.names=s6[,1])
  s6 <- s6[ order(row.names(s6)), ]
  
  # net nearby imports
  s7 <- s5 - s6
}


ner_dst_matrix <- "./_input_data/_adjancet_distant_matrix.xlsx"
ner.matrix <- read_excel(path = ner_dst_matrix, sheet = 'S2.country_nearby',  col_names = T, range = "B1:AP616")
dst.matrix <- read_excel(path = ner_dst_matrix, sheet = 'S2.country_distant', col_names = T, range = "B1:AP616")
not.matrix <- read_excel(path = ner_dst_matrix, sheet = 'S2.country_not',     col_names = T, range = "B1:AP616")

# cal using function
matrix <- not.matrix
s7.not <- Net_Import_Cal(df.xlsx)
matrix <- ner.matrix
s7.ner <- Net_Import_Cal(df.xlsx)
matrix <- dst.matrix
s7.dst <- Net_Import_Cal(df.xlsx)


# value under reality
loc <- read_excel(df.xlsx,sheet = 7, col_names = T, range = ("T1:AI42")) # 'S7.net_import_nearby'
loc <- as.data.frame(loc)
# first col as row name
loc <- data.frame(loc[,-1], row.names=loc[,1])/10^6
loc <- loc[ order(row.names(loc)), ]


# ####################################################################### #
# ~~ 1) close approach -------------------------------------------------------
# ####################################################################### #

df_rel_c <- (loc + 0     ) 
df_not_c <- (loc + s7.not) # s7.not: net imported value
df_ner_c <- (loc + s7.dst) # ************** (loc + net distant imports) means SDG under only  nearby trade scenario
df_dst_c <- (loc + s7.ner) # ************** (loc + net nearby  imports) means SDG under only distant trade scenario

# revise data, change GDP < O to 700 to avoid error
min(df_rel_c)
min(df_not_c)
min(df_ner_c)
min(df_dst_c)

df_not_c[1:15] <- lapply(df_not_c[1:15], function(x) (ifelse(x < 0, 700, x)))
df_dst_c[1:15] <- lapply(df_dst_c[1:15], function(x) (ifelse(x < 0, 700, x)))


df_c <- cbind(rbind(df_rel_c, df_not_c, df_ner_c, df_dst_c), rep(c('rel_c', 'not_c', 'ner_c', 'dst_c'), each = 41))
# names(df_c)
names(df_c) <- c(seq(1995, 2009), 'scenario')

# save csv
out.dir <- './_input_data'
fname <- paste0(out.dir, '/_GDP_c_', gsub('.xlsx', '', df.xlsx), '.csv'); fname
write.csv(x = df_c, file = fname, row.names = T)

# ----------------------------------------------------------------------------- #
# check result
# s9 <- read_excel(df.xlsx,sheet = 9, col_names = T, range = "T44:AI85") # 'S9.net_import_distant'
# s9 <- as.data.frame(s9)
# s9 <- data.frame(s9[,-1], row.names=countries)
# s9 <- s9[ order(row.names(s9)), ]
# check_results <- round(s9 - df_dst_c, digits = 3)
# ----------------------------------------------------------------------------- #






# ####################################################################### #
# ~~ 2) baseline approach 1 --------------------------------------------------
# ####################################################################### #
rep.row  <-function(x,n){matrix(rep(x,each=n),nrow=n)}
rep.col  <-function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
loc_base <- as.data.frame(rep.col(loc[,1], 15)) # set the start year 1995 as base line


df_rel_b <- (loc      + 0     ) 
df_not_b <- (loc_base + s7.not) # s7.not: net imported value
df_ner_b <- (loc_base + s7.dst) # ************** (loc + net distant imports) means SDG under only nearby trade scenario
df_dst_b <- (loc_base + s7.ner) # ************** (loc + net nearby  imports) means SDG under only distant trade scenario

# revise data, change GDP < O to 700 to avoid error
min(df_rel_b)
min(df_not_b)
min(df_ner_b)
min(df_dst_b)

df_rel_b[1:15] <- lapply(df_rel_b[1:15], function(x) (ifelse(x < 0, 700, x)))
df_not_b[1:15] <- lapply(df_not_b[1:15], function(x) (ifelse(x < 0, 700, x)))
df_ner_b[1:15] <- lapply(df_ner_b[1:15], function(x) (ifelse(x < 0, 700, x)))
df_dst_b[1:15] <- lapply(df_dst_b[1:15], function(x) (ifelse(x < 0, 700, x)))

names(df_rel_b) <- names(df_not_b) # keep the same name

df_b <- cbind(rbind(df_rel_b, df_not_b, df_ner_b, df_dst_b), 
              rep(c('rel_b', 'not_b', 'ner_b', 'dst_b'), each = 41))
names(df_b)
names(df_b) <- c(seq(1995, 2009), 'scenario')

# save csv
out.dir <- './_input_data'
fname <- paste0(out.dir, '/_GDP_b_', gsub('.xlsx', '', df.xlsx), '.csv'); fname
write.csv(x = df_b, file = fname, row.names = T)


# ####################################################################### #
# ~~ 3) baseline approach 2 --------------------------------------------------
# ####################################################################### #

# trade volume in the first year
s7.not.base <- as.data.frame(rep.col(s7.not[,1], 15)) # set the start year 1995 as base line
s7.dst.base <- as.data.frame(rep.col(s7.dst[,1], 15))
s7.ner.base <- as.data.frame(rep.col(s7.ner[,1], 15))

df_rel_b <- (loc + 0     ) 
df_not_b <- (loc + s7.not.base) # s7.not.base: net imported value in the initial year
df_ner_b <- (loc + s7.dst.base)      # ************** (loc + net distant imports) means SDG under only nearby trade scenario
df_dst_b <- (loc + s7.ner.base)      # ************** (loc + net nearby  imports) means SDG under only distant trade scenario

# revise data, change GDP < O to 700 to avoid error
min(df_rel_b)
min(df_not_b)
min(df_ner_b)
min(df_dst_b)

df_rel_b[1:15] <- lapply(df_rel_b[1:15], function(x) (ifelse(x < 0, 700, x)))
df_not_b[1:15] <- lapply(df_not_b[1:15], function(x) (ifelse(x < 0, 700, x)))
df_ner_b[1:15] <- lapply(df_ner_b[1:15], function(x) (ifelse(x < 0, 700, x)))
df_dst_b[1:15] <- lapply(df_dst_b[1:15], function(x) (ifelse(x < 0, 700, x)))

names(df_rel_b) <- names(df_not_b) # keep the same name

df_b <- cbind(rbind(df_rel_b, df_not_b, df_ner_b, df_dst_b), 
              rep(c('rel_b', 'not_b', 'ner_b', 'dst_b'), each = 41))
names(df_b)
names(df_b) <- c(seq(1995, 2009), 'scenario')

# save csv
getwd()
out.dir <- './_input_data'
fname <- paste0(out.dir, '/_GDP_b2_', gsub('.xlsx', '', df.xlsx), '.csv'); fname
write.csv(x = df_b, file = fname, row.names = T)


