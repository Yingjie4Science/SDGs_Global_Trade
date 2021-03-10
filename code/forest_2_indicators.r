


# set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir

dir.new <- paste0(dir, '/update_0503_SUM_dist/_output_score/score_15_for')
setwd(dir.new)
getwd()
list.dirs()

library(tidyverse)


# equation 1 --------------------------------------------------------------

f1.clos <- read.csv("./forest_01_percent/score_c_15_for.csv") %>%
  filter(scenario == 'rel_c' | scenario == 'not_c') %>%
  group_by(scenario) %>%
  summarise_at(.vars = names(.)[3:17],
               .funs = c(mean="mean"))

f1.base <- read.csv("./forest_01_percent/score_b_15_for.csv") %>%
  filter(scenario == 'rel_b' | scenario == 'not_b') %>%
  group_by(scenario) %>%
  summarise_at(.vars = names(.)[3:17],
               .funs = c(mean="mean"))

f1 <- rbind(f1.clos, f1.base) %>% gather(key = col.new, value = score, 2:16) %>%
  mutate(Year = as.numeric(gsub("\\D", "", col.new))) %>% select(-col.new)



library(splitstackshape)
# split col of visitor_home_cbgs by block
f1 <- cSplit(indt = f1, # input data
                   splitCols = c("scenario"), 
                   sep = c("_"), 
                   drop = F,               # drop the original col or not
                   direction = 'wide',
                   # direction = 'long',     # this is better than "wide"
                   stripWhite = T)         # clean white space


p1 <- ggplot(data=f1, aes(x=Year, y=score, group = scenario_2)) + 
  geom_point(aes(shape=scenario_1), show.legend = F)+
  # facet_wrap(.~scenario_2) +
  facet_wrap(.~scenario_2, scales = "free_y") +
  # ylim(60, 100) +
  # geom_smooth(method=lm,   # Add linear regression line: loess, lm
  #             # span = 8, # Smaller numbers produce wigglier lines
  #             se=T,
  #             aes(linetype = scenario_1, fill=scenario_1, colour = scenario_1, group = scenario_1),
  #             size = 0.5)  + # (by default includes 95% confidence region)
  theme_bw()
p1



# equation 2 --------------------------------------------------------------

f2.clos <- read.csv("./forest_02_change/score_c_15_for.csv") %>%
  filter(scenario == 'rel_c' | scenario == 'not_c') %>%
  group_by(scenario) %>%
  summarise_at(.vars = names(.)[3:17],
               .funs = c(mean="mean"))
f2.base <- read.csv("./forest_02_change/score_b_15_for.csv") %>%
  filter(scenario == 'rel_b' | scenario == 'not_b') %>%
  group_by(scenario) %>%
  summarise_at(.vars = names(.)[3:17],
               .funs = c(mean="mean"))

f2<- rbind(f2.clos, f2.base) %>% gather(key = col.new, value = score, 2:16) %>%
  mutate(Year = as.numeric(gsub("\\D", "", col.new))) %>% select(-col.new)



library(splitstackshape)
# split col of visitor_home_cbgs by block
f2 <- cSplit(indt = f2, # input data
             splitCols = c("scenario"), 
             sep = c("_"), 
             drop = F,               # drop the original col or not
             direction = 'wide',
             # direction = 'long',     # this is better than "wide"
             stripWhite = T)         # clean white space


p2 <- ggplot(data=f2, aes(x=Year, y=score, group = scenario_2)) + 
  geom_point(aes(shape=scenario_1), show.legend = F)+
  # facet_wrap(.~scenario_2) +
  facet_wrap(.~scenario_2, scales = "free_y") +
  # ylim(60, 100) +
  # geom_smooth(method=lm,   # Add linear regression line: loess, lm
  #             # span = 8, # Smaller numbers produce wigglier lines
  #             se=T, 
  #             aes(linetype = scenario, fill=scenario, colour = scenario),
  #             size = 0.5)   # (by default includes 95% confidence region)
  # the following labels can ensure there is only one legend to be shown
  # scale_linetype_manual(values = c('solid', 'dashed'),
  #                       labels=c("with trade",
  #                                "no-trade scenario"))
  theme_bw()
p2

p1.name <- 'G:/My Drive/_paper/170923_trade_Global SDGs/results/Figures/FOR_01.PNG'
p2.name <- 'G:/My Drive/_paper/170923_trade_Global SDGs/results/Figures/FOR_02.PNG'

ggsave(p1.name,  plot = p1, width = 3.5, height = 3, units = "in")
ggsave(p2.name,  plot = p2, width = 3.5, height = 3, units = "in")



# two Figures together ----------------------------------------------------

library(cowplot)
p <- plot_grid(p1, p2, labels = c('A','B'), ncol = 2, align = 'h')
p

fname <- 'G:/My Drive/_paper/170923_trade_Global SDGs/results/Figures/FOR_01_02.PNG'
save_plot(filename = fname, 
          plot     = p, 
          ncol = 2, nrow = 1, base_width = 3.3, base_height = 3,
          base_aspect_ratio = 1.1)





# individual country ------------------------------------------------------

f1.clos.not <- read.csv("./forest_01_percent/score_c_15_for.csv") %>%
  filter(scenario == 'not_c')
  # summarise_at(.vars = names(.)[3:17],
  #              .funs = c(mean="mean"))
f2.clos.not <- read.csv("./forest_02_change/score_c_15_for.csv") %>%
  filter(scenario == 'not_c')

 
t1 <- f1.clos.not %>% select(nation, X2009); names(t1) <- c('nation', 'eq_1')
t2 <- f2.clos.not %>% select(nation, X2009); names(t2) <- c('nation', 'eq_2')

t  <- cbind(t1, t2)[,-3] %>% gather(key = eq, value = score, 2:3)


library(bre)
pt <- ggplot(data=t, aes(x=reorder(nation, score), y=score, group = eq, fill=eq)) + 
  
  # scale_fill_brewer(palette="Paired")+
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw() +
  xlab('Country') +
  ylab('SDG score') +
  ggtitle('C') +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
pt

pt.name <- 'G:/My Drive/_paper/170923_trade_Global SDGs/results/Figures/FOR_bars_each_nation.PNG'

ggsave(pt.name,  plot = pt, width = 6, height = 5, units = "in")
