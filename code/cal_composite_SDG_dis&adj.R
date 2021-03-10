
#choose.dir()

library("readxl")

setwd("C:\\LiY\\paper\\trade_Global SDGs\\data\\update data\\update_0330_SUM_distant")
list.files()
sdg6 <- read_excel("6_wat.xlsx",sheet = "S10_water+", col_names = FALSE, range = "C2:AF42")
sdg7 <- read_excel("7_ene.xlsx",sheet = "S10_ene", col_names = FALSE, range = "C2:AF42")
sdg8 <- read_excel("8_mat.xlsx",sheet = "S10_mat", col_names = FALSE, range = "C2:AF42")
sdg9 <- read_excel("9_co2.xlsx",sheet = "S10_co2+", col_names = FALSE, range = "C2:AF42")
sdg12 <- read_excel("12_mat-perCap.xlsx",sheet = "S10_mat", col_names = FALSE, range = "C2:AF42")
sdg13 <- read_excel("13_co2-forest.xlsx",sheet = "S10_co2+", col_names = FALSE, range = "C2:AF42")
sdg15 <- read_excel("15_for.xlsx",sheet = "S10_for", col_names = FALSE, range = "C2:AF42")

sdg <- (sdg6+sdg7+sdg8+sdg9+sdg12+sdg13+sdg15)/7


########## output result 1 ##############
library(xlsx)
write.xlsx(sdg, file="99_composite_in_R.xlsx",sheetName="composite7SDG", append=FALSE)



