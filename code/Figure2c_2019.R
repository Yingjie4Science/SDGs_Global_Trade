




#############################
# rm(list = ls())
library(readxl)
library(ggplot2)
library(dplyr)

# set work dir
path <- rstudioapi::getSourceEditorContext()$path; path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
wdir <- paste0(dir, './update_0503_SUM_dist/_output_score')
setwd(wdir)

csvs <- list.files(pattern="^score_c_*"); csvs; approach = 'c'
# csvs <- list.files(pattern="^score_b_*"); csvs; approach = 'b'



diffs <- data.frame(row.names=1:41)
# [1] way#1: read csv as dataframe (all cols)
for (csv in csvs){
  print(csv)
  score_x <- read.csv(file = csv)[, -1]
  names(score_x) <- c('nation', seq(1995, 2009), "scenario", "group")
  assign(paste('sdg_', substr(csv, 7, 13), sep=''), score_x)
  
  
  score_x_rel <- score_x %>% filter(scenario == paste0('rel_', approach))
  score_x_not <- score_x %>% filter(scenario == paste0('not_', approach))
  score_x_dif <- score_x_rel[, 2:16] - score_x_not[, 2:16]
  
  score_x_dif <- score_x_dif %>%
    as.data.frame() %>%
    # rowwise() %>%
    mutate(mean     = rowMeans(.[, 1:15])) %>%
    mutate(nation   = score_x_rel$nation,
           scenario = score_x_rel$scenario,
           group    = score_x_rel$group) %>%
    as.data.frame()

  diffs <- cbind(diffs, score_x_dif[,16])
  
  assign(paste('sdg_dif_', substr(csv, 7, 13), sep=''), score_x_dif)
  # fname <- paste('sdg_dif_rel__not_', substr(csv, 7, 13), '.csv', sep='')
  # write.csv(x = score_x_dif, file = fname)
}


dfname <- list()

for (csv in csvs){
  x <- gsub(".*?([0-9]+).*", "\\1", csv)
  y <- paste0('SDG', x)
  print(x)
  print(y)
  dfname[[csv]] <- y
}

dfname

names(diffs)
names(diffs) <- dfname

diffs <- diffs %>%
  mutate(nation   = score_x_rel$nation,
         scenario = score_x_rel$scenario,
         group    = score_x_rel$group)


chg_1 <- diffs %>%
  filter(group == 1)
chg_0 <- diffs %>%
  filter(group == 0)
increase <- function(x) sum(x >  0)
decrease <- function(x) sum(x <= 0)

library(plyr)
library(tidyr)
library(tidyverse)
chg_1_inc <- numcolwise(increase)(chg_1)/28*100
chg_1_dec <- numcolwise(decrease)(chg_1)/28*100
chg_0_inc <- numcolwise(increase)(chg_0)/13*100
chg_0_dec <- numcolwise(decrease)(chg_0)/13*100
change <- c('chg_1_inc', 
            'chg_1_dec', 
            'chg_0_inc', 
            'chg_0_dec')
chgs      <- cbind(change, rbind(chg_1_inc, 
                                 chg_1_dec, 
                                 chg_0_inc, 
                                 chg_0_dec))[, -9]
chgs.dt <- chgs %>%
  gather(SDG, percent, 2:8) %>%
  filter(grepl("*_inc$", change))

head(chgs.dt)
unique(chgs.dt$SDG)

unique(chgs.dt$change)
chgs.dt$change <- factor(chgs.dt$change, 
                      levels=c('chg_1_inc', 'chg_0_inc'))
levels(chgs.dt$change)
# plot --------------------------------------------------------------------

chgs.dt$SDG <- factor(chgs.dt$SDG, 
                      levels=c('SDG99', 'SDG6','SDG7','SDG8','SDG9',
                               # 'SDG12',
                               'SDG13','SDG15'))

levels(chgs.dt$SDG)

# change facet labels
levels(chgs.dt$SDG) <- c('SDGc', 'SDG6','SDG7','SDG8/12','SDG9','SDG13','SDG15')



ggplot(data=chgs.dt, 
       aes(x=SDG, y=percent ,fill = change)) +
  theme_bw() +
  xlab("SDG indicators") +
  ylab("Propotion of coutries made improvement\n in each country group (%)") +
  scale_fill_manual(values=c("royalblue1", "chocolate1"),
                    name="",
                    labels=c("Developed", "Developing")) +
  geom_bar(stat="identity"
           #, color="blue"
           , position=position_dodge(0.3) # overlay
           , alpha = 0.5) + # transparency
  # scale_x_discrete(labels=c("SDGs" = "SDGc")) +
  scale_y_continuous(breaks=seq(0,100,20),
                     limits=c(0,100)) +
  geom_hline(yintercept=50,color="red1",linetype = 'dashed', alpha= 0.5)+ # longdash
  theme(legend.position = c(0.9, 0.95),
        legend.background = element_rect(fill="transparent"))+
  ggtitle("c")
  # theme(plot.title = element_text(margin = margin(b = -15)))
  # annotate("text", x=0.7, y=100, label= "bold((b))", 
  #          colour = "black", size = 4, parse = TRUE)

        
# save/export plot to local dir
outdir     <- 'G:/My Drive/_paper/170923_trade_Global SDGs/results/Figures'
timestamp = format(Sys.time(), "%Y%m%d")
filename <-  paste(outdir, '/Fig.2c_', approach, '_', timestamp, '.png', sep = '');filename
ggsave(filename = filename, plot = last_plot(), width = 6.5, height = 5, units = "in")


### ref: 
#https://campus.datacamp.com/courses/data-visualization-with-ggplot2-1/chapter-4-geometries?ex=7

