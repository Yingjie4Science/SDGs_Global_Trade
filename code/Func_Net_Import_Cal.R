


### ------------------------------------------------------------------------- ###
### This function is used to calculate the net imports of footprints
### ------------------------------------------------------------------------- ###


# functions 
Net_Import_Cal <- function(df.xlsx, matrix) {
  ### trade matrix
  s1 = read_excel(df.xlsx, sheet = 1, col_names = T, range = "B1:AP616")
  s2 = matrix
  
  ### trade matrix distant or nearby
  s3 = s1 * s2
  
  ### add country name as a new col
  countries <- colnames(s2[1:41])
  colnames(s3) <- countries; length(countries)
  ### add year seq to data frame 
  year_seq = rep(seq(1995, 2009, by=1), each = 41)
  s3$year <- year_seq
  
  ### total imports -------------------------------------------
  s4 <- s3 %>% group_by(year) %>% summarise_all(funs(sum))
  ### transpose
  s5 <- as.data.frame(t(s4[,-1]))
  colnames(s5) <- seq(1995, 2009, by=1)
  s5 <- s5[order(row.names(s5)), ]
  
  ### total exports -------------------------------------------
  s6 <- s3 %>%
    mutate(row.sum   = rowSums(s3[,1:41]),
           year      = year_seq,
           countries = rep(countries, times = 15)) %>% # times=15, each = 15
    dcast(countries ~ year, value.var = 'row.sum') # year as new col names
  s6 <- data.frame(s6[,-1], row.names=s6[,1])
  s6 <- s6[order(row.names(s6)), ]
  
  ### net imports ---------------------------------------------
  s7 <- s5 - s6
}