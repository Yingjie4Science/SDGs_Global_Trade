


Figure2a_func <- function(dt.wi){
  
  
  # figure name for saving purpose later 
  stri.name <- deparse(substitute(dt.wi))   # convert the name of a dataframe to a string
  file.name <- gsub('fig2.dt.', '', stri.name) # Extracting the last n characters from a string (from --- fig2.dt.w1)
  
  
  
  fig2a.dt <- dt.wi %>% 
    # Filtering row which contains a certain string
    filter(., !grepl('glo_', group_scenario)) %>% 
    dplyr::select(group_scenario, year, value)
  
  # Changing the order of levels of a factor
  # ref: http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/
  fig2a.dt$group_scenario <- factor(fig2a.dt$group_scenario,
                                    levels = c("1_rel","1_not" , "0_rel", "0_not" ))
  
  levels(fig2a.dt$group_scenario) # check and print out the order
  
  mylabel_f2a <- c("Developed (with trade)", "Developed (no-trade)",
                   'Developing (with trade)','Developing (no-trade)')
  
  
  
  # ----- ~~~ plot Figure 2a * new -----
  f2a <-ggplot(data = fig2a.dt, aes(x=year, y=value, group = group_scenario)) +
    #geom_line(size=1, aes(linetype=region_scenarios)) + 
    geom_point(aes(shape = group_scenario), size = 0.7, stroke = 0.1) + # , color=scenarios
    geom_smooth(method=loess,   # Add linear regression line
                se=T, 
                aes(linetype = group_scenario, 
                    colour   = group_scenario,
                    fill     = group_scenario),
                size  = 0.2, 
                alpha = 0.5)+    # (by default includes 95% confidence region)
    scale_linetype_manual(values = c('solid', 'dashed','solid', 'dashed'),
                          labels=mylabel_f2a) +
    # 
    scale_color_manual(values = c("#3288bd", "#3288bd", '#ec7014' , '#ec7014'),
                       labels=mylabel_f2a) +
    # set point shapes and labels
    scale_shape_manual(values = c(19, 1, 19, 1),
                       labels=mylabel_f2a) +
    # set the colors of shade (confidence intervals)
    scale_fill_manual(values=c("#3288bd", "lightblue", "#ec7014", '#fec44f'),
                      labels=mylabel_f2a) +
    
    theme_bw() +
    ylab('Composite SDG target scores') +
    xlab('Year') +
    
    mytheme +
    theme(plot.margin = unit(c(0.1,0.1, 0, 0.5), "cm"))+ # #top, right, bottom, left
    
    # ylim(62, 82) +
    scale_y_continuous(breaks=seq(62, 84,2),
                       limits=c(62,84)) +
    
    theme(legend.position = c(0.3, 0.9), # Goal level: c(0.7, 0.1)
          legend.title=element_blank(),
          legend.key.width = unit(0.7,"cm"),
          legend.key.size = unit(0.15,"cm"),
          legend.background = element_rect(fill="transparent"),
          axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank()
          # text = element_text(size=18),
          # axis.text=element_text(size=20),
          # plot.title = element_text(size=26)
    ) #+
    # ylim(60, 95) +
    
    # ggtitle("(a)")
  
  f2a
  
  # ----- ~~~ plot save -----
  # setwd(mydst)
  
  timestamp = format(Sys.time(), "%Y%m%d"); timestamp
  fname = paste0(dir.fig, '/Fig.2a_', approach, '_', file.name, '_', timestamp, '.jpg'); fname
  # ggsave(fname, plot = last_plot(), width = 4.8, height = 5.5, units = "in", dpi = 300)
  
  
  return(f2a)
}







# test --------------------------------------------------------------------

# getwd()
# Figure2a_func(fig2.dt.w1)
