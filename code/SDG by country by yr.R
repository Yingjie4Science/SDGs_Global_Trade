


remove(list = ls())
path <- rstudioapi::getSourceEditorContext()$path; path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
wdir <- paste0(dir, './update_0503_SUM_dist/_output_score')
setwd(wdir)


csvs <- list.files(pattern="^score_c_*"); csvs

for (csv in csvs){
  score_x <- read.csv(file = csv)[, -c(1:2, 18:19)]
  names(score_x) <- c(seq(1995, 2009))
  assign(paste('sdg_', substr(csv, 7, 13), sep=''), score_x)
  # assign(paste('', gsub('score_|.csv', '', csv), sep=''), score_x)
}




SDGc_c <- (sdg_c_6_wat + sdg_c_7_ene + sdg_c_8_mat + sdg_c_9_co2 + 
             sdg_c_12_ma + sdg_c_13_co + sdg_c_15_fo)/7

# add "nation, scenario, group" cols
SDGc_c <- SDGc_c %>%
  mutate(nation   = read.csv("score_c_6_wat.csv")$nation,
         scenario = read.csv("score_c_6_wat.csv")$scenario,
         group    = read.csv("score_c_6_wat.csv")$group)

dt <- 

















data.list <- c("sdg_c_6_wat","sdg_c_7_ene","sdg_c_8_mat","sdg_c_9_co2",
               "sdg_c_12_ma","sdg_c_13_co","sdg_c_15_fo")

# add "nation, scenario, group" cols
for (item in data.list){
  print(item)
  df <- get(item)
  item_ <- df %>%
    mutate(nation   = read.csv("score_c_6_wat.csv")$nation,
           scenario = read.csv("score_c_6_wat.csv")$scenario,
           group    = read.csv("score_c_6_wat.csv")$group)
  assign(paste0(item, '_'), item_)
}


# to cal each SDG's score at global level
data_.list <- paste0(data.list, '_'); data_.list
for (item_ in data_.list){
  print(item_)
  df_ <- get(item_)
  item_glo <- df_ %>%
    filter(scenario == 'rel_c')
  assign(paste0(item_, 'rel'), item_glo)
}

# to extract trade and no-trade SDG score at global level, and then
#    append to one dataframe
data_glo.list <- paste0(data_.list, '_glo'); data_glo.list
SDGs.id <- paste0('SDG', c(6:9, 12, 13, 15)); SDGs.id
SDGs_yr <- data.frame()
for (item_glo in data_glo.list){
  print(item_glo)
  i <- which(data_glo.list==item_glo); print(i) # get the index of item in the list
  
  dt.sub <- get(item_glo)
  # SDG.name <- as.numeric(gsub("\\D", "", item_glo)) # extract SDG id numbers
  SDG.name <- SDGs.id[i] # extract SDG id numbers
  dt.sub.withname <- cbind(SDG.name, dt.sub) # add SDG names to the df
  SDGs_yr <- rbind(SDGs_yr, dt.sub.withname[3:4,]) # only select with trade and no-trade data
}

names(SDGs_yr)
names(SDGs_yr) <- c("SDG.name","scenario", seq(1995, 2009)) # change the col names
SDGs_yr_long <- SDGs_yr %>% gather(Year, SDG.score, 3:17)   # wide to ong, for facet plot
str(SDGs_yr_long)
SDGs_yr_long$Year <- as.numeric(SDGs_yr_long$Year)

## need to change the level order 
SDGs_yr_long$scenario <- factor(SDGs_yr_long$scenario,levels = c('rel_c', 'not_c'))
SDGs_yr_long$SDG.name <- factor(SDGs_yr_long$SDG.name,levels = SDGs.id)

## plot
p1.each <- ggplot(data=SDGs_yr_long, aes(x=Year, y=SDG.score, group = scenario)) + 
  geom_point(aes(shape=scenario)) +
  facet_wrap(.~SDG.name) +
  # facet_wrap(.~SDG.name, scales = "free_y") +
  geom_smooth(method=lm,   # Add linear regression line: loess, lm
              # span = 8, # Smaller numbers produce wigglier lines
              se=T, 
              # show.legend = F,
              # aes(linetype = scenario),
              aes(linetype = scenario, fill=scenario, colour = scenario),
              # colour = 'black',
              # aes(fill=scenario),
              size = 0.5)+    # (by default includes 95% confidence region)
  # the following labels can ensure there is only one legend to be shown
  scale_linetype_manual(values = c('solid', 'dashed'),
                        labels=c("with trade",
                                 "no-trade scenario")) +
  scale_color_manual(values = c("black",'black'),
                     labels = c("with trade",
                                "no-trade scenario")) +
  scale_fill_manual(values = c("gray30",'gray60'),
                    labels = c("with trade",
                               "no-trade scenario")) +
  # set point shapes and labels
  scale_shape_manual(values = c(19,1),
                     labels=c("with trade",
                              "no-trade scenario")) +
  # theme(legend.position = c(0.5, 0), legend.justification = c(0.5, 0)) +
  # theme(legend.position="none") +
  theme_bw()

p1.each