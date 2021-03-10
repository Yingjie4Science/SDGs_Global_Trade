

Figure3b_func <- function(sdg.wi){
  
  # input data: sdg.wi <- sdgc.bygroup.notnerdst.w1
  
  # figure name for saving purpose later 
  stri.name <- deparse(substitute(sdg.wi))   # convert the name of a dataframe to a string
  file.name <- gsub('sdgc.bygroup.notnerdst.', '', stri.name) # Extracting the last n characters from a string (from --- fig2.dt.w1)
  
  
  
  
  library(Rmisc)
  dt.3b.summary <- summarySE(sdg.wi, measurevar="value", groupvars='group_scenario')
  
  ymax.value <- max(dt.3b.summary$value) + max(dt.3b.summary$sd)
  
  
  # ----- ~~~ colors -----
  mycolors1 <- c("royalblue1", "skyblue1", "lightskyblue1", 
                 "chocolate1", 'tan1', 'navajowhite1')
  mycolors2 <- c("lightskyblue1", "skyblue1", "royalblue1", 
                 'navajowhite1',  'tan1', "chocolate1")
  
  # ----- ~~~ labels ------
  mylabels <- c('1_not', '1_ner', '1_dst', 
                '0_not', '0_ner', '0_dst')
  x_labels <- c('1_dst' ="only distant trade",
                '1_ner' = "only adjacent trade", 
                '1_not' = "no-trade",
                '0_dst' = "only distant trade",
                '0_ner' = "only adjacent trade", 
                '0_not' = "no-trade")
  
  
  # change orders
  dt.3b.summary$group_scenario <- factor(dt.3b.summary$group_scenario,
                                         levels = c('1_not', '1_ner', '1_dst', 
                                                    '0_not', '0_ner', '0_dst'))
  
  # save image
  # --- read path and set work dir -----
  path <- rstudioapi::getSourceEditorContext()$path
  dir  <- dirname(rstudioapi::getSourceEditorContext()$path)
  mydst <- paste0(dir, '/Figures')
  setwd(mydst)
  timestamp = format(Sys.time(), "%Y%m%d")
  fig_name = paste('Fig.3b', approach, file.name, timestamp, '.png', sep = '_')
  
  png(file=fig_name,
      width = 5.5, height = 6, units = "in",
      res = 300)
  
  library(scales)
  
  
  
  # plot
  Fig3b <- ggplot(dt.3b.summary, aes(x    = group_scenario, 
                                     y    = value, 
                                     fill = group_scenario)) +
    geom_bar(position=position_dodge(0.8), stat="identity", width = 0.8)+ #, binwidth=0)+#,
    guides(fill = guide_legend(keywidth = 0.7, keyheight = 0.7)) +
    geom_errorbar(aes(ymin= value-se, ymax=value+se),
                  width=0.2,                    # Width of the error bars
                  position=position_dodge(.8)) +
    #ylim(10,100) +
    scale_y_continuous(breaks=seq(65, ymax.value, 5), # ymax = 81
                       limits=c(65, ymax.value),
                       oob = rescale_none) +
    scale_fill_manual(name="",
                      values=mycolors2,
                      labels=mylabels) +
    scale_x_discrete(labels=x_labels) + #, 
    theme_bw() + 
    xlab('Trade scenarios') +
    ylab('Composite SDG score') +
    theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
    theme(legend.position="none") +
    annotate(geom = "text", x = 2, y = ymax.value, # 80.8
             label = "developed countries", 
             color = "royalblue1", size = 3.5,
             angle = 0) +
    annotate(geom = "text", x = 5, y = ymax.value, 
             label = "developing countries", 
             color = "chocolate1", size = 3.5,
             angle = 0) +
    ggtitle("b") #+
  #theme(plot.title = element_text(size = 24)) # face = "bold"
  Fig3b
  # 
  # # ----- ~~~ add brakets -----
  # library(pBrackets)  # this will also load grid package
  # library(grid)
  # # grid.locator(unit="native") 
  # grid.locator(unit="native")
  # bottom_y = 305 # 470 for limits=c(50,90) 
  # # the smaller this value, the more distance from the bottom
  # grid.brackets(230, bottom_y, 870,  bottom_y, lwd=1, col="royalblue1")
  # grid.brackets(930, bottom_y, 1570, bottom_y, lwd=1, col="chocolate1")
  # 
  # dev.off()
  # 
  return(Fig3b)
}



# test --------------------------------------------------------------------

# Figure3b_func(sdgc.bygroup.notnerdst.wpop)
# library(pBrackets)  # this will also load grid package
# library(grid)
# # grid.locator(unit="native")
# grid.locator(unit="native")
# bottom_y = 305 # 470 for limits=c(50,90)
# # the smaller this value, the more distance from the bottom
# grid.brackets(230, bottom_y, 870,  bottom_y, lwd=1, col="royalblue1")
# grid.brackets(930, bottom_y, 1570, bottom_y, lwd=1, col="chocolate1")
# dev.off()





