

# theme_set(theme_bw(base_size = font_size))

green_dark  <- "#31a354"
green_light <- "#a1d99b"


fun_fig_1a <- function(fig.1a.dt){
  # fig 1a 
  fig1a <- ggplot(data=fig.1a.dt, aes(x=year, y=value, group = group_scenario)) + # , colour = scenarios
    #geom_line(size=1, aes(linetype=scenarios)) + 
    geom_point(aes(shape=group_scenario, color = group_scenario)) + # , color=scenarios
    geom_smooth(method=loess,   # Add linear regression line
                se=T, aes(linetype = group_scenario, 
                          color = group_scenario, 
                          fill = group_scenario),
                # colour = c(green_dark, green_light),           ## 'black',
                size = 0.5)+    # (by default includes 95% confidence region)
    scale_linetype_manual(values = c('solid', 'dashed'),
                          labels=c("with trade",
                                   "no-trade scenario")) +
    scale_color_manual(values = c(green_dark, green_light),    ## "black",'black'
                       labels = c("with trade",
                                  "no-trade scenario")) +
    scale_fill_manual(values = c(green_dark, green_light),    ## "black",'black'
                       labels = c("with trade",
                                  "no-trade scenario")) +
    # set point shapes and labels
    scale_shape_manual(values = c(19,1),
                       labels=c("with trade",
                                "no-trade scenario")) +
    scale_y_continuous(breaks=seq(65,83,2), 
                       labels = scales::number_format(accuracy = 1)) + ## only Integer, no Decimal
    theme_bw() +
    xlab('Year') +
    ylab('Composite SDG target scores') +
    
    mytheme +
    
    theme(legend.position = c(0.25, 0.9), # c(0.2, 0.9) when width =6
          legend.title=element_blank(),
          legend.key.width = unit(0.5,"in"),
          # legend.background = element_rect(fill="transparent"),
          axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5),
          axis.title.x = element_text(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text    = element_text(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.key = element_rect(fill = NA, colour = NA, size = 0.25)
          #text = element_text(size=15),
          #axis.text=element_text(size=18),
          #axis.title = element_text(size=18,face="bold"),plot.title = element_text(size=12)
          ) 
    # ggtitle(title)
  
  ### save fig to local drive
  # dir.fig <- paste0(dir, '/Figures')
  # getwd()
  # today <- format(Sys.Date(), "%Y%m%d")
  # fname <- paste0(dir.fig, "/Fig.1a_", filecode, '_', today, '.png')
  # ggsave(filename = fname, plot = fig1a, 
  #        width = 3, height = 3, units = "in", dpi = 300) 
  
  return(fig1a)
}

