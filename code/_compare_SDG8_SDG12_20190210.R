
# To clear your environment 
remove(list = ls())

# set work dir
### set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
setwd('..') # set directory by one folder up
getwd()

setwd("./data/update_0503_SUM_dist")
getwd()


df.xlsx <- "8_mat.xlsx";          SDG <- 'SDG8'

# setwd("./update_0503_SUM_dist")
library(readxl)
library(XLConnect)
library(xlsx)
library(dplyr)

loc <- read_excel(df.xlsx,sheet = 7, col_names = T, range = ("A1:Q42")) # 'S7.net_import_nearby'
mat <- loc[, -c(1,17)]


# GDP

# value under reality
# df.xlsx <- "0_capital.xlsx"
# gdp <- read_excel(df.xlsx,sheet = 7, col_names = T, range = ("T1:AI42")) # 'S7.net_import_nearby'
# gdp <- as.data.frame(loc[,-17])


GDP_c_csv  <- './_input_data/_GDP_c_0_capital.csv'
GDP_c      <- read.csv(file = GDP_c_csv, header = T, stringsAsFactors = F)
GDP <- GDP_c %>% filter(scenario == 'rel_c') %>% dplyr::select(-1, -17)


# populaion
pop <- read.csv(file = './_input_data/_pop_worldbank_19952009.csv', header = T, stringsAsFactors = F)
pop <- as.data.frame(pop)
pop <- data.frame(pop[,-1], row.names=pop[,1])
pop <- pop[ order(row.names(pop)), ]


mat_gdp <- data.frame(t(mat/GDP))
mat_pop <- data.frame(t(mat/pop))

n.names <- row.names(pop); print(n.names)  
names(mat_gdp) <- n.names
names(mat_pop) <- n.names


library(ggplot2)
for (i in seq(1,41, by=1)){
  nation <- n.names[i]
  print(nation)
  df <- data.frame(cbind(mat_gdp[, i], mat_pop[, i]))
  ggplot(df, aes(x=X1, y=X2)) + geom_point() +
    ggtitle(nation) +
    xlab("mat_gdp") + ylab("mat_pop") +
    theme_bw()
  fname <- paste0('./Figures/SDG8_SDG12_compare/', i, '_', nation, '.png'); fname
  ggsave(fname,  width = 3, height = 3, units = "in")
  
}





