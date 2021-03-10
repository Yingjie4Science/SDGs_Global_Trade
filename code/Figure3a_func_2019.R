

Figure3a_func <- function(nerdst.wi){
  
  
  # figure name for saving purpose later 
  stri.name <- deparse(substitute(nerdst.wi))     # convert the name of a dataframe to a string
  file.name <- gsub('glo_nerdst_', '', stri.name) # Extracting the last n characters from a string (from --- fig2.dt.w1)
  
  
  
  nerdst.wi$group_scenario <- factor(nerdst.wi$group_scenario, 
                                     levels = c('glo_dst', 'glo_ner', 'glo_not'))
  
  
  
  # color
  colors_f2a      <- c('#636363', '#bdbdbd', '#f0f0f0') 
  colors_line_f2a <- c('black', 'black', 'black') 
  # http://colorbrewer2.org/#type=sequential&scheme=Greys&n=3
  
  
  labels_f2a    <- c("only distant trade", "only adjacent trade", 'no-trade')
  line_type_f2a <- c('solid', 'dashed', 'dotted')
  
  # plot
  f3a <- ggplot(data = nerdst.wi, aes(x = year, y = value, group = group_scenario)) +
    # geom_line(size=1, aes(linetype=group_scenario)) +
    geom_point(aes(shape=group_scenario)) + # , color=scenarios
    geom_smooth(method=loess,   # Add linear regression line
                se=T, 
                aes(linetype = group_scenario, 
                    colour   = group_scenario,
                    fill     = group_scenario),
                size = 0.5, # The size of a line is its width in mm
                alpha = 0.5)+ 
    
    scale_linetype_manual(values = line_type_f2a,
                          labels = labels_f2a) +
    # line color
    scale_color_manual(values=colors_line_f2a, 
                       labels=labels_f2a) +
    
    # set point shapes and labels
    scale_shape_manual(values = c(19, 1, 4), # http://www.sthda.com/english/wiki/ggplot2-point-shapes
                       labels=labels_f2a) +
    
    # set the colors of shade (confidence intervals)
    scale_fill_manual(values=colors_f2a,
                      labels=labels_f2a) +
    # scale_fill_grey(start = 0.2, end = 1) +
    
    theme_bw() +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) + ## only Integer, no Decimal
    ylab('Composite SDG score') +
    theme(legend.position = c(0.2, 0.9), # (0.3, 0.86)
          legend.title=element_blank(),
          legend.key.width = unit(1.8,"cm"),
          legend.background = element_rect(fill="transparent"),
          axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank()
          # text = element_text(size=11),
          # axis.text=element_text(size=11),
          # axis.title=element_text(size=18,face="bold"),
          # plot.title = element_text(size=12)
    ) +
    
    # ylim(72, 83) +
    
    #scale_x_continuous(minor_breaks = seq(1995, 2009, 1)) +
    ggtitle("a")
  # +
  # theme(plot.title = element_text(size = 24)) # face = "bold"
  
  f3a
  # save plot
  

  timestamp = format(Sys.time(), "%Y%m%d")
  # outdir    <- 'G:/My Drive/_paper/170923_trade_Global SDGs/results/Figures'
  fname = paste0(dir.fig, '/Fig.3a', '_', approach, '_', file.name, '_', timestamp, '.png'); fname
  ggsave(fname, width = 6, height = 6, units = "in", dpi = 300)
  
  return(f3a)
}



# test --------------------------------------------------------------------

# Figure3a_func(glo_nerdst_wgdp)





