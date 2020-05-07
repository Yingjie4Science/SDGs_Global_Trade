




# To clear your environment 
remove(list = ls())

var <- 'CO2'; location <- 'AE38'
var <- 'EU';  location <- 'AD38'


library(readxl)
library(XLConnect)
library(xlsx)
library(openxlsx)



# set work dir
setwd('C:')
# path <- rstudioapi::getSourceEditorContext()$path
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir

dir <- "G:/My Drive/_paper/170923_trade_Global SDGs/data/WIOD-Environmental_Accounts"

dir.sub <- paste0(var, '_may12'); print(dir.sub)
setwd(paste0(dir, '/', dir.sub))

wd <- getwd(); wd


# list all SDG xlsx files -------------------------------------------------

xlsfiles <- list.files(pattern=paste0(var, '_May12.xls$')); xlsfiles  # '*.xlsx$' # "^DO_ECS_*"


j.value <- data.frame()
nations <- c()
for (xls in xlsfiles){
  print(xls)
  nation.name <- substr(xls, 1, 3);
  nations <- c(nations, nation.name)
  
  sheets <- excel_sheets(path = xls)[-1] # remove the sheet: "Notes" 
  # print(sheets)
  for (sheet in sheets){
    # print(sheet)
    df0 = as.data.frame(
      read_excel(path = xls, sheet = as.character(sheet),
                 col_names = F, range = location,
                 col_types = "numeric")
    )

    
    year <- data.frame(Year = as.character(sheet))
    i.value <- cbind(year, df0) # 2000, 1234
    j.value <- rbind(j.value,  i.value)
  }
}

f.mat <- data.matrix(j.value[,2]) # take the second col and convert to matrix
f.dim <- matrix(f.mat, nrow = 15, byrow = F)

f.df  <- as.data.frame(f.dim)
names(f.df) <- nations
row.names(f.df) <- seq(1995, 2009, by = 1)
fname <- paste0(var, '_total.csv')
write.csv(x = f.df, file = paste0(dir, '/', fname))




# # ----------------------------------------------------------------------- #
# corrolation between CO2, Energy use -------------------------------------
# # ----------------------------------------------------------------------- #
getwd()

CO2 <- read.csv(file = paste0(dir, '/', 'CO2_total.csv'))
EU  <- read.csv(file = paste0(dir, '/', 'EU_total.csv'))

n_names <- names(CO2); print(n_names)
library(ggplot2)
for (i in seq(2,42, by=1)){
  nation <- n_names[i]
  print(nation)
  df <- data.frame(cbind(CO2[, i], EU[, i]))
  ggplot(df, aes(x=X1, y=X2)) + geom_point() +
    ggtitle(nation) +
    xlab("CO2 emissions (Kilotons)") + ylab("Energy use (Terajoules)") +
    theme_bw()
  fname <- paste0(dir, '/Fig/', i, '.png')
  ggsave(fname,  width = 3, height = 3, units = "in")
  
}





# all countries are pooled together  --------------------------------------

co2_world <- CO2 %>% gather(key = 'Country', value = 'co', 2:42)
eu_world  <- EU  %>% gather(key = 'Country', value = 'eu', 2:42)


df.world  <- cbind(co2_world, eu_world)[, c(1:3, 6)]

ggplot(df.world, aes(x= log10(co), y=log10(eu), label =Country)) + geom_point() +
  ggtitle('All countries') +
  geom_text(hjust = 0, nudge_x = 0.05) +
  xlab("log 10, CO2 emissions (Kilotons)") + ylab("log 10, Energy use (Terajoules)") +
  theme_bw()

fname <- paste0(dir, '/Fig/', '_world', '.png'); print(fname)
ggsave(fname,  width = 6, height = 6, units = "in")
