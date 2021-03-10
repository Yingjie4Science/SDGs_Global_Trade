
#choose.dir()
# To clear your environment 
#remove(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)



#*************************************************************
############ * with-trade vs. without-trade * ################

# ------ cal SDGs without-trade for ded and ding ------
setwd("G:\\My Drive\\_paper\\trade_Global SDGs\\results\\update_20171124")
sheet_range <- "C1:AF42"
w_sdg06<- read_excel("SDG with trade 1995-2009+ROW-0401.xlsx",sheet = "6wat", col_names = T, range = sheet_range)
w_sdg07<- read_excel("SDG with trade 1995-2009+ROW-0401.xlsx",sheet = "7ene", col_names = T, range = sheet_range)
w_sdg08<- read_excel("SDG with trade 1995-2009+ROW-0401.xlsx",sheet = "8mat", col_names = T, range = sheet_range)
w_sdg09<- read_excel("SDG with trade 1995-2009+ROW-0401.xlsx",sheet = "9co2", col_names = T, range = sheet_range)
w_sdg12<- read_excel("SDG with trade 1995-2009+ROW-0401.xlsx",sheet = "12mat_pc", col_names = T, range = sheet_range)
w_sdg13<- read_excel("SDG with trade 1995-2009+ROW-0401.xlsx",sheet = "13co2for", col_names = T, range = sheet_range)
w_sdg15<- read_excel("SDG with trade 1995-2009+ROW-0401.xlsx",sheet = "15for",    col_names = T, range = sheet_range)

group_c <- read_excel("SDG with trade 1995-2009+ROW-0401.xlsx",sheet = "6wat", col_names = T, range = "A1:B42")

# ------ cal SDGeco, SDGenv -----
w_SDGeco <- (w_sdg08+w_sdg09+w_sdg12)/3
w_SDGenv <- (w_sdg06+w_sdg07+w_sdg13+w_sdg15)/4

# ----------- for eco -----
w_SDGeco_glo_with <- w_SDGeco %>% 
  as.data.frame %>%
  summarise_at(.vars = names(.)[1:15], .funs = c(mean="mean"))
w_SDGeco_glo_without <- w_SDGeco %>% 
  as.data.frame %>%
  summarise_at(.vars = names(.)[16:30], .funs = c(mean="mean")) 

colnames(w_SDGeco_glo_with)    <- c(seq(1995, 2009, by=1))
colnames(w_SDGeco_glo_without) <- c(seq(1995, 2009, by=1))

w_SDGeco_glo <- cbind(class = c('with','without'), rbind(w_SDGeco_glo_with, 
                                                         w_SDGeco_glo_without))
w_SDGeco_glo_long <- w_SDGeco_glo %>% 
  melt(. , id = c('class')) # items in id will not be changed
colnames(w_SDGeco_glo_long) <- c('scenario','Year', 'value')


# ------ add icon to figure -----
setwd("G:\\My Drive\\_paper\\trade_Global SDGs\\results\\update_0503_SUM_dist")
img_eco <- png::readPNG("./icon_eco.png")
img_env <- png::readPNG("./icon_env.png")
rast_eco <- grid::rasterGrob(img_eco, interpolate = T)
rast_env <- grid::rasterGrob(img_env, interpolate = T)

# ----- viz f4a eco -----
w_SDGeco_glo_long$scenario <- factor(w_SDGeco_glo_long$scenario, levels = c('with', 'without'))

f4a <-ggplot(data=w_SDGeco_glo_long, aes(x=Year, y=value, group = scenario)) +
  geom_point(aes(shape=scenario)) + # , color=scenarios
  geom_smooth(method=loess,   # Add linear regression line
              se=T, aes(linetype = scenario),
              colour = 'black',
              size = 0.5)+    # (by default includes 95% confidence region)
  #geom_line(size=1, aes(linetype=scenario)) + 
  scale_linetype_manual(values = c('solid', 'dashed'),
                        labels=c("All countries (trade)",
                                 "All countries (without-trade)")) +
  # set point shapes and labels
  scale_shape_manual(values = c(19,1),
                     labels=c("All countries (trade)",
                              "All countries (without-trade)")) +
  theme_bw() +
  ylab('Composite SDGeco') +
  theme(legend.position = c(0.75, 0.15), # (0.3, 0.86)
        legend.title=element_blank(),
        legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill="transparent"),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size=15),
        axis.text=element_text(size=18),
        #axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=18)) +
  ylim(55, 90) +
  annotation_custom(rast_eco,
                    ymin = 85, ymax = 90, 
                    xmin = 0.3, xmax = 4) +
  ggtitle("(a)")
f4a


# ---------- cal developed and developing -----
eco_with    <- cbind(scenario = 'with',    group_c, w_SDGeco[, 1:15])
eco_without <- cbind(scenario = 'without', group_c, w_SDGeco[,16:30])
colnames(eco_with)    <- c('scenario', 'group', 'Name', seq(1995, 2009, by=1))
colnames(eco_without) <- c('scenario', 'group', 'Name', seq(1995, 2009, by=1))

eco_w <- rbind(eco_with, eco_without)

eco_sdg_output <- cbind(eco_with, eco_without)
library(xlsx)
xlsfile <- "_data_output_composite.xlsx"
write.xlsx(eco_sdg_output, xlsfile, sheetName="ecoSDG", append=T)



eco_with$rowSum    <- rowMeans(   eco_with[,4:18] ) # rowSum
eco_without$rowSum <- rowMeans(eco_without[,4:18] )

eco_w_ <- eco_w %>% 
  as.data.frame %>%
  group_by(scenario, X__1) %>%
  summarise_at(.vars = names(.)[4:18],
               .funs = c(mean="mean"))
colnames(eco_w_) <- c('scenario', 'group', seq(1995, 2009, by=1))
eco_w_long <- as.data.frame(eco_w_)  %>% 
  melt(. , id = c('scenario', 'group')) # items in id will not be changed

colnames(eco_w_long) <- c('scenario','group', 'Year', 'value')
eco_w_long$idid <- (paste(eco_w_long$scenario,eco_w_long$group,sep=""))
eco_w_long$idid <- factor(eco_w_long$idid, levels = c('with1', 'without1',
                                                    'with0', 'without0'))

# ----------- for env ----
w_SDGenv_glo_with <- w_SDGenv %>% 
  as.data.frame %>%
  summarise_at(.vars = names(.)[1:15], .funs = c(mean="mean"))
w_SDGenv_glo_without <- w_SDGenv %>% 
  as.data.frame %>%
  summarise_at(.vars = names(.)[16:30], .funs = c(mean="mean")) 

colnames(w_SDGenv_glo_with)    <- c(seq(1995, 2009, by=1))
colnames(w_SDGenv_glo_without) <- c(seq(1995, 2009, by=1))

w_SDGenv_glo <- cbind(class = c('with','without'), rbind(w_SDGenv_glo_with, 
                                                         w_SDGenv_glo_without))

w_SDGenv_glo_long <- w_SDGenv_glo %>% 
  melt(. , id = c('class')) # items in id will not be changed
colnames(w_SDGenv_glo_long) <- c('scenario','Year', 'value')

# ----- viz f4c env -----
w_SDGenv_glo_long$scenario <- factor(w_SDGenv_glo_long$scenario, levels = c('with', 'without'))

f4c <-ggplot(data=w_SDGenv_glo_long, aes(x=Year, y=value, group = scenario)) +
  geom_point(aes(shape=scenario)) + # , color=scenarios
  geom_smooth(method=loess,   # Add linear regression line
              se=T, aes(linetype = scenario),
              colour = 'black',
              size = 0.5)+    # (by default includes 95% confidence region)
  #geom_line(size=1, aes(linetype=scenario)) + 
  scale_linetype_manual(values = c('solid', 'dashed'),
                        labels=c("All countries (trade)",
                                 "All countries (without-trade)")) +
  # set point shapes and labels
  scale_shape_manual(values = c(19,1),
                     labels=c("All countries (trade)",
                              "All countries (without-trade)")) +
  theme_bw() +
  ylab('Composite SDGenv') +
  theme(legend.position = c(0.75, 0.15), # (0.3, 0.86)
        legend.title=element_blank(),
        legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill="transparent"),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size=15),
        axis.text=element_text(size=18),
        #axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=18)) +
  ylim(55, 90) +
  annotation_custom(rast_env,
                    ymin = 84, ymax = 90, 
                    xmin = 0.3, xmax = 4.5) +
  ggtitle("(c)")
f4c

# ---------- cal developed and developing ---------------------------------
env_with    <- cbind(scenario = 'with',    group_c, w_SDGenv[, 1:15])
env_without <- cbind(scenario = 'without', group_c, w_SDGenv[,16:30])
colnames(env_with)    <- c('scenario', 'group', 'Name', seq(1995, 2009, by=1))
colnames(env_without) <- c('scenario', 'group', 'Name', seq(1995, 2009, by=1))

env_w <- rbind(env_with, env_without)

env_w_ <- env_w %>% 
  as.data.frame %>%
  group_by(scenario, group) %>%
  summarise_at(.vars = names(.)[4:18],
               .funs = c(mean="mean"))
colnames(env_w_) <- c('scenario', 'group', seq(1995, 2009, by=1))

env_w_long <- as.data.frame(env_w_)  %>% 
  melt(. ,id = c('scenario', 'group')) # items in id will not be changed

colnames(env_w_long) <- c('scenario','group', 'Year', 'value')
env_w_long$idid <- (paste(env_w_long$scenario,env_w_long$group,sep=""))
env_w_long$idid <- factor(env_w_long$idid, levels = c('with1', 'without1',
                                                      'with0', 'without0'))


# ----- viz f4b -----
f4b <-ggplot(data=eco_w_long, aes(x=Year, y=value, group = idid, colour = idid)) +
  geom_line(size=1, aes(linetype=idid)) + 
  scale_linetype_manual(values = c('solid', 'dashed',
                                   'solid', 'dashed'),
                        labels=c("developed (with-trade)",
                                 "developed (without-trade)",
                                 'developing (with-trade)',
                                 'developing (without-trade)')) +
  theme_bw() +
  ylab('Composite SDGeco') +
  theme(legend.position = c(0.75, 0.15), # (0.3, 0.86)
        legend.title=element_blank(),
        legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill="transparent"),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size=15),
        axis.text=element_text(size=18),
        #axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=18)) +
  scale_color_manual(values = c("blue", "blue",
                                'red' , 'red'),
                     labels=c("developed (with-trade)",
                              "developed (without-trade)",
                              'developing (with-trade)',
                              'developing (without-trade)')) +
  ylim(55, 90) +
  annotation_custom(rast_eco,
                    ymin = 85, ymax = 90, 
                    xmin = 0.3, xmax = 4) +
  ggtitle("(b)")
f4b

# ----- viz f4d -----
f4d <-ggplot(data=env_w_long, aes(x=Year, y=value, group = idid, colour = idid)) +
  geom_line(size=1, aes(linetype=idid)) + 
  
  scale_linetype_manual(values = c('solid', 'dashed',
                                   'solid', 'dashed'),
                        labels=c("developed (with-trade)",
                                 "developed (without-trade)",
                                 'developing (with-trade)',
                                 'developing (without-trade)')) +
  theme_bw() +
  ylab('Composite SDGenv') +
  theme(legend.position = c(0.75, 0.15), # (0.3, 0.86)
        legend.title=element_blank(),
        legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill="transparent"),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size=15),
        axis.text=element_text(size=18),
        #axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=18)) +
  scale_color_manual(values = c("blue", "blue",
                                'red' , 'red'),
                     labels=c("developed (with-trade)",
                              "developed (without-trade)",
                              'developing (with-trade)',
                              'developing (without-trade)')) +
  ylim(55, 90) +
  annotation_custom(rast_env,
                    ymin = 84, ymax = 90, 
                    xmin = 0.3, xmax = 4.5) +
  ggtitle("(d)")
f4d

# ----- viz f4 shoulder by shoulder ------
library(cowplot)
f4 <- plot_grid(f4a, f4b, f4c, f4d, labels = NULL) # labels = "AUTO"
save_plot("SDG_composite_eco_env_fig4.png", f4, ncol = 2,
          base_height = 13,
          base_width =  6.5)


#*************************************************************


####################  * nearby vs. distant * ################
# ------ read data -----
setwd("G:\\My Drive\\_paper\\trade_Global SDGs\\results\\update_0503_SUM_dist")
list.files()
sdg6 <- read_excel("_data_output.xlsx",sheet = "SDG6", col_names = T, range = "A1:AD42")
sdg7 <- read_excel("_data_output.xlsx",sheet = "SDG7", col_names = T, range = "A1:AD42")
sdg8 <- read_excel("_data_output.xlsx",sheet = "SDG8", col_names = T, range = "A1:AD42")
sdg9 <- read_excel("_data_output.xlsx",sheet = "SDG9", col_names = T, range = "A1:AD42")
sdg12 <- read_excel("_data_output.xlsx",sheet = "SDG12", col_names = T, range = "A1:AD42")
sdg13 <- read_excel("_data_output.xlsx",sheet = "SDG13", col_names = T, range = "A1:AD42")
sdg15 <- read_excel("_data_output.xlsx",sheet = "SDG15", col_names = T, range = "A1:AD42")

# ------ cal SDGc -----
sdg <- (sdg6+sdg7+sdg8+sdg9+sdg12+sdg13+sdg15)/7
SDG <- 'SDG99'
# ------ cal SDGeco, SDGenv -----
SDGeco <- (sdg8+sdg9+sdg12)/3
SDGenv <- (sdg6+sdg7+sdg13+sdg15)/4

# code developed or developing
group_id <- read_excel("15_for.xlsx",sheet = 10, col_names = T, range = "A1:B42") 
# change cols names
names(group_id) <- c('class', 'name')
# order data based on name
group_id <- arrange(group_id, name)

# ------ subset SDG_near and SDG_distant-----
SDGeco_dst <- SDGeco[, 1:15]
SDGeco_ner <- SDGeco[,16:30]
SDGeco_dst$class <- as.factor(group_id$class)
SDGeco_ner$class <- as.factor(group_id$class)
SDGeco_dt <- cbind(SDGeco_dst, SDGeco_ner)

SDGenv_dst <- SDGenv[, 1:15]
SDGenv_ner <- SDGenv[,16:30]
SDGenv_dst$class <- as.factor(group_id$class)
SDGenv_ner$class <- as.factor(group_id$class)
SDGenv_dt <- cbind(SDGenv_dst, SDGenv_ner)

# ------ output SDGeco, SDGenv to xls ------
# for load xlsx error 
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_171') # for 64-bit version
library(rJava)
# write data to xls
library(xlsx)
xlsfile <- "_data_output_composite.xlsx"
write.xlsx(sdg,    xlsfile, sheetName="composite7SDG", append=FALSE)
write.xlsx(SDGeco_dt, xlsfile, sheetName="SDGeco", append=TRUE)
write.xlsx(SDGenv_dt, xlsfile, sheetName="SDGenv", append=TRUE)
# ref: http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r


# ------ cal mean value for each group ---------
library(dplyr)
##### for all
# for eco
SDGeco_dst_glo <- SDGeco %>%
  summarise_at(.vars = names(.)[1:15], .funs = c(mean="mean"))
SDGeco_ner_glo <- SDGeco %>%
  summarise_at(.vars = names(.)[16:30], .funs = c(mean="mean"))
SDGeco_dst_glo <- cbind(class = 'SDGeco_dst', SDGeco_dst_glo)
SDGeco_ner_glo <- cbind(class = 'SDGeco_ner', SDGeco_ner_glo)

# for env
SDGenv_dst_glo <- SDGenv %>%
  summarise_at(.vars = names(.)[1:15], .funs = c(mean="mean"))
SDGenv_ner_glo <- SDGenv %>%
  summarise_at(.vars = names(.)[16:30], .funs = c(mean="mean"))
SDGenv_dst_glo <- cbind(class = 'SDGenv_dst', SDGenv_dst_glo)
SDGenv_ner_glo <- cbind(class = 'SDGenv_ner', SDGenv_ner_glo)

###### for developed or developing compare
# -------- >> for eco -----
SDGeco_dst_group <- SDGeco_dst %>%
  group_by(class) %>%
  summarise_at(.vars = names(.)[1:15], .funs = c(mean="mean"))
SDGeco_ner_group <- SDGeco_ner %>%
  group_by(class) %>%
  summarise_at(.vars = names(.)[1:15], .funs = c(mean="mean"))
# -------- >> for env -----
SDGenv_dst_group <- SDGenv_dst %>%
  group_by(class) %>%
  summarise_at(.vars = names(.)[1:15], .funs = c(mean="mean"))
SDGenv_ner_group <- SDGenv_ner %>%
  group_by(class) %>%
  summarise_at(.vars = names(.)[1:15], .funs = c(mean="mean"))


# ----- combine to one file -----
# -------- >> for eco -----
sdg_eco_dst <- rbind(SDGeco_dst_glo, SDGeco_dst_group)
sdg_eco_ner <- rbind(SDGeco_ner_glo, SDGeco_ner_group)
# -------- >> for env -----
sdg_env_dst <- rbind(SDGenv_dst_glo, SDGenv_dst_group)
sdg_env_ner <- rbind(SDGenv_ner_glo, SDGenv_ner_group)


# change col names
colnames(sdg_eco_dst) <- c('class', seq(1995, 2009, by=1))
colnames(sdg_eco_ner) <- c('class', seq(1995, 2009, by=1))
colnames(sdg_env_dst) <- c('class', seq(1995, 2009, by=1))
colnames(sdg_env_ner) <- c('class', seq(1995, 2009, by=1))


# ------ data for visualization  ------
# -------- >> for eco -----
df_eco_all <- rbind(cbind(class = c('glo2','dst0', 'dst1'), region = c('2', '0', '1'), tradetype = c('dst','dst','dst'), sdg_eco_dst[, -1]), 
                    cbind(class = c('glo2','ner0', 'ner1'), region = c('2', '0', '1'), tradetype = c('ner','ner','ner'), sdg_eco_ner[, -1])) 
df_eco_glo <- df_eco_all %>%
  filter(region == '2') %>%
  melt(. , id = c("class", 'region', 'tradetype')) # items in id will not be changed
colnames(df_eco_glo) <- c('class', 'region', 'tradetype', 'Year', 'value')

df_eco_group <- df_eco_all %>%
  filter(region != '2') %>%
  melt(. , id = c("class", 'region', 'tradetype')) # items in id will not be changed
colnames(df_eco_group) <- c('class', 'region', 'tradetype', 'Year', 'value')


# -------- >> for env -----
df_env_all <- rbind(cbind(class = c('glo2','dst0', 'dst1'), region = c('2', '0', '1'), tradetype = c('dst','dst','dst'), sdg_env_dst[, -1]), 
                    cbind(class = c('glo2','ner0', 'ner1'), region = c('2', '0', '1'), tradetype = c('ner','ner','ner'), sdg_env_ner[, -1])) 
df_env_glo <- df_env_all %>%
  filter(region == '2') %>%
  melt(. , id = c("class", 'region', 'tradetype')) # items in id will not be changed
colnames(df_env_glo) <- c('class', 'region', 'tradetype', 'Year', 'value')

df_env_group <- df_env_all %>%
  filter(region != '2') %>%
  melt(. , id = c("class", 'region', 'tradetype')) # items in id will not be changed
colnames(df_env_group) <- c('class', 'region', 'tradetype', 'Year', 'value')


# ------ add icon to figure -----
img_eco <- png::readPNG("./icon_eco.png")
img_env <- png::readPNG("./icon_env.png")
## Warning in png::readPNG("./watermark.png"): libpng warning: iCCP: known
## incorrect sRGB profile
rast_eco <- grid::rasterGrob(img_eco, interpolate = T)
rast_env <- grid::rasterGrob(img_env, interpolate = T)

# ------ viz f5a eco -----
df_eco_glo$tradetype <- factor(df_eco_glo$tradetype, levels = c('dst', 'ner'))

f5a <-ggplot(data=df_eco_glo, aes(x=Year, y=value, group = tradetype)) +
  geom_line(size=1, aes(linetype=tradetype)) + 
  scale_linetype_manual(values = c('dashed','solid'),
                        labels=c("All countries (only distant trade)",
                                 "All countries (only adjacent trade)")) +
  theme_bw() +
  ylab('Composite SDGeco') +
  theme(legend.position = c(0.3, 0.88), # (0.3, 0.86)
        legend.title=element_blank(),
        legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill="transparent"),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size=15),
        axis.text=element_text(size=18),
        #axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=18)) +
  # scale_color_manual(values = c("blue",
  #                               'red'),
  #                    labels=c("dst",
  #                             "ner")) +
  ylim(55, 90) +
  annotation_custom(rast_eco,
                    ymin = 55, ymax = 60, 
                    xmin = 11.8, xmax = 15.5) +
  ggtitle("(a)")
f5a


# ------ viz f5b eco -----
df_eco_group$class <- factor(df_eco_group$class, levels = c('dst1', 'ner1',
                                                            'dst0', 'ner0'))

f5b <-ggplot(data=df_eco_group, aes(x=Year, y=value, group = class, colour = class)) +
  geom_line(size=1, aes(linetype=class)) + 
  scale_linetype_manual(values = c('dashed','solid', 
                                   'dashed','solid'),
                        labels=c("developed (only distant trade)",
                                 "developed (only adjacent trade)",
                                 'developing (only distant trade)',
                                 'developing (only adjacent trade)')) +
  theme_bw() +
  ylab('Composite SDGeco') +
  theme(legend.position = c(0.3, 0.88), # (0.3, 0.86)
        legend.title=element_blank(),
        legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill="transparent"),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size=15),
        axis.text=element_text(size=18),
        #axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=18)) +
  scale_color_manual(values = c("blue", "blue",
                                'red' , 'red'),
                     labels=c("developed (only distant trade)",
                              "developed (only adjacent trade)",
                              'developing (only distant trade)',
                              'developing (only adjacent trade)')) +
  ylim(55, 90) +
  annotation_custom(rast_eco,
                    ymin = 55, ymax = 60, 
                    xmin = 11.8, xmax = 15.5) +
  ggtitle("(b)")
f5b
# ------ two f5  eco shoulder by shoulder ------
library(cowplot)
f5 <- plot_grid(f5a, f5b, labels = NULL) # labels = "AUTO"
save_plot("SDG_composite_eco_env_dst_ner.png", f5, ncol = 2,
          base_height = 6.5,
          base_width =  6.5)




# ------ ** viz f5c env ** -----
df_env_glo$tradetype <- factor(df_env_glo$tradetype, levels = c('dst', 'ner'))

f5c <-ggplot(data=df_env_glo, aes(x=Year, y=value, group = tradetype)) +
  geom_line(size=1, aes(linetype=tradetype)) + 
  scale_linetype_manual(values = c('dashed','solid'),
                        labels=c("All countries (only distant trade)",
                                 "All countries (only adjacent trade)")) +
  theme_bw() +
  ylab('Composite SDGenv') +
  theme(legend.position = c(0.3, 0.88), # (0.3, 0.86)
        legend.title=element_blank(),
        legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill="transparent"),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size=15),
        axis.text=element_text(size=18),
        #axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=18)) +
  # scale_color_manual(values = c("blue",
  #                               'red'),
  #                    labels=c("dst",
  #                             "ner")) +
  ylim(55, 90) +
  annotation_custom(rast_env,
                    ymin = 55, ymax = 61, 
                    xmin = 11.3, xmax = 15.5) +
  ggtitle("(c)")
f5c


# ------ ** viz f5d env ** -----
df_env_group$class <- factor(df_env_group$class, levels = c('dst1', 'ner1',
                                                            'dst0', 'ner0'))

f5d <-ggplot(data=df_env_group, aes(x=Year, y=value, group = class, colour = class)) +
  geom_line(size=1, aes(linetype=class)) + 
  scale_linetype_manual(values = c('dashed','solid', 
                                   'dashed','solid'),
                        labels=c("developed (only distant trade)",
                                 "developed (only adjacent trade)",
                                 'developing (only distant trade)',
                                 'developing (only adjacent trade)')) +
  theme_bw() +
  ylab('Composite SDGenv') +
  theme(legend.position = c(0.3, 0.88), # (0.3, 0.86)
        legend.title=element_blank(),
        legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill="transparent"),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size=15),
        axis.text=element_text(size=18),
        #axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=18)) +
  scale_color_manual(values = c("blue", "blue",
                                'red' , 'red'),
                     labels=c("developed (only distant trade)",
                              "developed (only adjacent trade)",
                              'developing (only distant trade)',
                              'developing (only adjacent trade)')) +
  ylim(55, 90) +
  annotation_custom(rast_env,
                    ymin = 55, ymax = 61, 
                    xmin = 11.3, xmax = 15.5) +
  ggtitle("(d)")
f5d


# ------ two f5  env shoulder by shoulder ------
library(cowplot)
f5 <- plot_grid(f5a, f5b, f5c, f5d,
                labels = NULL) # labels = "AUTO"
save_plot("SDG_composite_env_env_dst_ner_fig5.png", f5, ncol = 2,
          base_height = 13,
          base_width =  6.5)









