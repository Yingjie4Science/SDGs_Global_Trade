

library(dplyr)
library(ggplot2)


Figure2b_func <- function(dt_wi){
  
  # figure name for saving purpose later 
  stri.name <- deparse(substitute(dt_wi))      # convert the name of a dataframe to a string
  file.name <- gsub('fig2.dt.', '', stri.name) # Extracting the last n characters from a string (from --- fig2.dt.w1)
  
 
  fig2b.dt <- dt_wi %>% 
    # Filtering row which contains a certain string
    filter(., !grepl('glo_', group_scenario))   %>%  # filter out "global sdgs"
    dplyr::select(group_scenario, year, value)  %>%
    spread(key = group_scenario, value = value) %>%  # key: column values to convert to multiple columns
    ### This is wrong - corrected on 2020-05-23
    # mutate(rel_diff = (`1_rel`) - (`1_not`),
    #        not_diff = (`0_rel`) - (`0_not`)) #%>%     # wrap the invalid column names in backticks (`):
    mutate(rel_diff = (`1_rel`) - (`0_rel`),
           not_diff = (`1_not`) - (`0_not`)) %>%
    dplyr::select(year, rel_diff, not_diff)  %>%
    gather(group_scenario, value,  - year) # wide to long; first two are new col names, 3rd and following are cols that will not change
  
  fig2b.dt$group_scenario <- factor(fig2b.dt$group_scenario,
                                    levels = c('rel_diff', 'not_diff'))
  
  
  # summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
  library(Rmisc)
  group_diff_summary <- summarySE(fig2b.dt, measurevar ="value", groupvars = 'group_scenario')
  
  upper <- ceiling(max(group_diff_summary$value)) + 2* ceiling(max(group_diff_summary$se)); upper
  lower <- floor(min(group_diff_summary$value))   + 2* floor(min(group_diff_summary$se));   lower
  
  # Changing the order of levels of a factor  
  group_diff_summary$group_scenario <- factor(group_diff_summary$group_scenario,
                                              levels = c('rel_diff', 'not_diff'))
  

  # ----- Fig 2b * new -----
  library(scales)
  
  mylabel_2b <- c('with trade', 'no-trade')
  
  green_dark  <- "#31a354"
  green_light <- "#a1d99b"
  greens <- c(green_dark, green_light)
  
  f2b <- ggplot(data = group_diff_summary, 
                aes(x = group_scenario, y = value, fill = group_scenario)) +
    
    ### revised on 2020-05-22 --------------------------------------------------------------------- #
    ### bar plot
    # geom_bar(position = position_dodge(0.8), stat = "identity", width = 0.6)+ #, binwidth=0)+#,
    # geom_errorbar(aes(ymin = value-se, ymax = value+se), size = 0.1, 
    #               width    = 0.15,                    # Width of the error bars
    #               position = position_dodge(.8)) +
   
    ### revised to boxplot -------------------------------------------------------------------------
    geom_boxplot(data = fig2b.dt, aes(x = group_scenario, y = value, fill = group_scenario), lwd = 0.1, 
                 # alpha = 0.5, 
                 # width = 0.8
                 ) +
    geom_point(data = fig2b.dt, aes(x = group_scenario, y = value, fill = group_scenario),
               position=position_dodge(.8), size = 0.2, 
               # position=position_jitterdodge(0.05),
               alpha=0.3) +
    ### --------------------------------------------------------------------------------------------- 
    
    guides(fill = guide_legend(keywidth = 0.7, keyheight = 0.7)) +
    
    scale_fill_manual(name="",
                      values = greens, ## c('gray40', 'gray80'),
                      labels = mylabel_2b) +
    scale_x_discrete(labels  = mylabel_2b) + #,
    scale_y_continuous(oob   = rescale_none,
                       position = "right") +
    geom_hline(yintercept=0, color="black",linetype = 'dashed', alpha= 0.3, lwd = 0.15)+ # longdash, dashed
    
    # scale_y_continuous(breaks=seq(lower, upper, 2),
    #                    limits=c(lower, upper),
    #                    oob   = rescale_none,
    #                    position = "right") +
    theme_bw() + 
    mytheme + 
    theme(plot.margin = unit(c(0.1, 0.1, 0, 0.5), "cm"))+ # #top, right, bottom, left
    
    xlab('Trade scenarios') +
    ylab('Difference in composite SDG target score\nbetween developed and developing countries') + ## The difference of 
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid.major = element_line(size=0.2),
          panel.grid.minor = element_blank()) +
    theme(legend.position="none")#+
    # ggtitle("(b)") #+
  f2b
  
  # ----- plot save ------
  timestamp = format(Sys.time(), "%Y%m%d")
  fname = paste0(dir.fig, '/Fig.2b_', approach, '_', file.name, '_', timestamp, '.jpg')
  # ggsave(fname, plot = last_plot(), width = 2, height = 5.5, units = "in", dpi = 300)
  
  return(f2b)
  
}



# test --------------------------------------------------------------------

# fig2.dt.w1
# getwd()
# Figure2b_func(fig2.dt.w1)

# dt_wi <- fig2.dt.w1
# 
# str(fig2b.dt)
# fig2b.dt$year <- as.numeric(as.character(fig2b.dt$year))
# ggplot(data = fig2b.dt)+
#   geom_line(aes(x=year, y = rel_diff)) +
#   geom_line(aes(x=year, y = not_diff), color = 'red')
# # 
# #   
# ggplot(data = fig2b.dt)+
#   geom_boxplot(aes(x = '', y = rel_diff)) + theme_bw()
# mean(fig2b.dt$rel_diff)
# 
# 
# 
# 
# # violin plot
# gviolin <-
#   ggplot(data = fig2b.dt, aes(x = group_scenario, y = value, fill = group_scenario))+
#   # geom_violin(alpha = 0.5) +
#   # geom_point(alpha = 0.4, size = 3, shape = 1) +
#   # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
#   # geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.3) +
#   geom_boxplot(alpha = 0.5, width = 0.2) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   geom_point(position=position_dodge(.8),
#              # position=position_jitterdodge(0.05),
#              alpha=0.3)
#   # geom_jitter(width=0.5, alpha=0.2) +
# 
# # show plot
# gviolin
