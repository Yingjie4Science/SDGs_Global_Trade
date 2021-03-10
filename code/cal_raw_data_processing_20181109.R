
# ------------- cal dat same as SDG with trade 1995-2009+ROW-0401.xlsx --------------

# To clear your environment 
remove(list = ls())

#choose.files()

library("readxl")
library(XLConnect)
library(xlsx)

setwd("G:\\My Drive\\_paper\\trade_Global SDGs\\results\\update_0503_SUM_dist")
# ----- change #1 -----



# df.xlsx <- "0_capital.xlsx"
# df.xlsx <- "6_wat.xlsx"
# df.xlsx <- "7_ene.xlsx"
# df.xlsx <- "8_mat.xlsx" 
# df.xlsx <- "9_co2.xlsx"
# df.xlsx <- "12_mat-perCap.xlsx"  
# df.xlsx <- "13_co2-forest.xlsx"
# df.xlsx <- "15_for.xlsx"

s1 = read_excel("6_wat.xlsx", sheet = 1, col_names = T, range = "B1:AP616")

# read distant and nearby matrix
s2 = read_excel("6_wat.xlsx", sheet = 2, col_names = T, range = "B1:AP616")
# add country name as a new col
countries <- colnames(s2[1:41])
year_seq = rep(seq(1995, 2009, by=1), each = 41)


# ------------------------------------------------------------------------------

matrix.01= read_excel("6_wat.xlsx", sheet = 'S2.country_distant_nearby (0)', 
                      col_names = T, range = "B1:AP616")
matrix.01[is.na(matrix.01)] <- 0 # set NA to 0

# ----- test ----
matrix.nearby = read_excel("_adjancet_distant_matrix.xlsx", sheet = 'S2.country_nearby',
                           col_names = T, range = "B1:AP616")

ttt <- matrix.nearby - matrix.01;
diag(ttt) <- NA

max(ttt, na.rm = T);
min(ttt, na.rm = T)
# ----- test ----

matrix.distant= read_excel("_adjancet_distant_matrix.xlsx", sheet = 'S2.country_distant',
                           col_names = T, range = "B1:AP616")
matrix.distant[is.na(matrix.distant)] <- 0 # set NA to 0

matrix <- matrix.01 + matrix.distant; min(matrix); max(matrix)
mmm <- matrix.nearby + matrix.distant; min(mmm); max(mmm)

# ---------- * functions * -------------
csv_process <- function(file) {
  # read in data
  dt <- read_excel(file, sheet = 1, col_names = T, range = "B1:AP616")
  s_ner = dt * matrix.nearby      # data for nearby  trade scenario
  colnames(s_ner) <- countries    # add col names
  s_ner$year <- year_seq

  # total imports
  s_ner_sum <- s_ner %>%
    group_by(year) %>%
    summarise_all(funs(sum))

  ###
  s_ner_sum$ner.Sums <- rowSums(s_ner_sum[,2:42])
  is.num <- sapply(s_ner_sum, is.numeric)
  s_ner_sum[is.num] <- lapply(s_ner_sum[is.num], round, 0)
  # 
  s_dst_sum$dst.Sums <- rowSums(s_dst_sum[,2:42])
  is.num <- sapply(s_dst_sum, is.numeric)
  s_dst_sum[is.num] <- lapply(s_dst_sum[is.num], round, 0)
  ###
  df <- cbind(s_ner_sum[, c(1,43)], s_dst_sum[, 43])
  # -----* (near - distant) * ------
  df$diff <- df[,'ner.Sums'] - df[,'dst.Sums']
  file.name  <- paste(file, "result.csv", sep="-")
  print(file.name)
  write.csv(df, file = file.name)
}  



fs <- c("6_wat.xlsx",         "7_ene.xlsx",         "8_mat.xlsx",   "9_co2.xlsx",
        "12_mat-perCap.xlsx", "13_co2-forest.xlsx", "15_for.xlsx")

for (f in fs){ 
  csv_process(f)
}

########################################################################################
results_list <- list.files(pattern="-result.csv"); results_list
# file.names <- gsub('.xlsx-result.csv', '', results_list, fixed=TRUE)
# file.names  <- paste('Fig', file.names, sep="_")

library(ggplot2)

draw_figs <- function(file) {
  file.names <- gsub('.xlsx-result.csv', '', file, fixed=TRUE)
  Fig.name  <- paste('Fig', file.names, ".png", sep="_")
  file.names  <- paste('Fig', file.names, sep="_")
  dt <- read.csv(file, header = TRUE)
  dt <- as.data.frame(dt[, c('year','diff')])
  print(file)
  file.names <- ggplot(data = dt, aes(x = year, y = diff)) + 
    geom_line(size=0.6,alpha=0.7) +
    theme_bw() +
    ggtitle(file.names) 
  file.names
  # ggsave(filename = Fig.name, plot = file.names,
  #        width = 6, height = 6, units = "in",
  #        dpi = 300)

}

par(mfrow=c(2,4))
for (dt in results_list){ 
  #print(dt)
  draw_figs(dt)
}




# library(gridExtra)
# grid.arrange(Fig_12_mat-perCap, Fig_13_co2-forest, Fig_15_for, Fig_6_wat, Fig_7_ene, Fig_8_mat, Fig_9_co2,
#              nrow=1, ncol=2)
# ggsave('file.png',
#        width = 6, height = 6, units = "in",
#        dpi = 300)



# ------------- for check cal purpose ------------------
s3.1 = s1 * matrix.01
colnames(s3.1) <- countries
s3.1$year <- year_seq
s4.1_ner <- s3.1 %>%
  group_by(year) %>%
  summarise_all(funs(sum))
s4.1_ner$rowSums <- rowSums(s4.1_ner[,2:42])
# ------------- check distant -------------------------
s.distant = s1 * matrix.distant
colnames(s.distant) <- countries
s.distant$year <- year_seq
s4.1_dst <- s.distant %>%
  group_by(year) %>%
  summarise_all(funs(sum))
s4.1_dst$rowSums <- rowSums(s4.1_dst[,2:42])

# -------------------------------
dt <- read.csv("6_wat.xlsx-result.csv", header = TRUE)
dt <- dt[, c('year','diff')]
ggplot(data = dt, aes(x = year, y = diff)) + 
  geom_line(size=0.6,alpha=0.7) +
  theme_bw() +
  ggtitle("(a)") 

