

library(dplyr)
library(ggplot2)


Figure2b_func <- function(dt_wi){
  
  # figure name for saving purpose later 
  stri.name <- deparse(substitute(dt_wi))   # convert the name of a dataframe to a string
  file.name <- gsub('fig2.dt.', '', stri.name) # Extracting the last n characters from a string (from --- fig2.dt.w1)
  
 
  fig2b.dt <- dt_wi %>% 
    # Filtering row which contains a certain string
    filter(., !grepl('glo_', group_scenario))   %>%  # filter out "global sdgs"
    dplyr::select(group_scenario, year, value)  %>%
    spread(key = group_scenario, value = value) %>%  # key: column values to convert to multiple columns
    mutate(rel_diff = (`1_rel`) - (`1_not`),
           not_diff = (`0_rel`) - (`0_not`)) %>%     # wrap the invalid column names in backticks (`):
    dplyr::select(year, rel_diff, not_diff)  %>%
    gather(group_scenario, value,  - year) # wide to long; first two are new col names, 3rd and following are cols that will not change
  
  
  
  # summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
  library(Rmisc)
  group_diff_summary <- summarySE(fig2b.dt, measurevar ="value", groupvars = 'group_scenario')
  
  
  # Changing the order of levels of a factor  
  group_diff_summary$group_scenario <- factor(group_diff_summary$group_scenario,
                                              levels = c('rel_diff', 'not_diff'))
  

  # ----- Fig 2b * new -----
  library(scales)
  
  mylabel_2b <- c('with trade', 'no-trade')
  
  f2b <- ggplot(data = group_diff_summary, 
                aes(x = group_scenario, y = value, fill = group_scenario)) +
    
    geom_bar(position = position_dodge(0.8), stat = "identity", width = 0.8)+ #, binwidth=0)+#,
    guides(fill = guide_legend(keywidth = 0.7, keyheight = 0.7)) +
    geom_errorbar(aes(ymin = value-se, ymax = value+se),
                  width    = 0.15,                    # Width of the error bars
                  position = position_dodge(.8)) +
    scale_fill_manual(name="",
                      values = c('gray80', 'gray40'),
                      labels = mylabel_2b) +
    scale_x_discrete(labels  = mylabel_2b) + #, 
    scale_y_continuous(breaks=seq(0,12,2),
                       #limits=c(0,12),
                       oob   = rescale_none,
                       position = "right") +
    theme_bw() + 
    mytheme + 
    
    xlab('Trade scenarios') +
    ylab('The difference of SDGc score \n between developed and developing countries') +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(legend.position="none")#+
    # ggtitle("(b)") #+
  
  # ----- plot save ------
  # dir.out <- "G:/My Drive/_paper/170923_trade_Global SDGs/results/Figures"
  timestamp = format(Sys.time(), "%Y%m%d")
  fname = paste0(dir.fig, '/Fig.2b_', approach, '_', file.name, '_', timestamp, '.jpg')
  ggsave(filename = fname, plot = last_plot(), 
         width = 2, height = 5.5, units = "in", dpi = 300)
  
  return(f2b)
  
}



# test --------------------------------------------------------------------

# fig2.dt.w1
# getwd()
# Figure2b_func(fig2.dt.w1)




