

# reshape data for ploting ------------------------------------------------
library(reshape2)
library(tidyr)
library(Rmisc)  ## for using the function - summarySE()
library(ggplot2)


Figure2d_func <- function(dfs_2d){
  
  dfs2 <- dfs_2d %>% 
    as.data.frame() %>%
    melt(., id = c("SDG","group")) 
  
  names(dfs2)
  names(dfs2) <- c("SDG", "group", "year", "value")
  
  
  dfs3       <- summarySE(dfs2, measurevar="value", groupvars=c("group", "SDG"))
  
  unique(dfs3$SDG)
  
  ### goal level
  
  # dfs3$SDG   <- factor(dfs3$SDG,   levels=c('SDG99','SDG6', 'SDG7','SDG8','SDG9',
  #                                           # 'SDG12',
  #                                           'SDG13','SDG15'))
  
  ### target level ------------------------------------------------------------------------------ #
  dfs3$SDG   <- factor(dfs3$SDG,   
                       levels=c('SDG99', 'SDG6','SDG72', 'SDG73', 'SDG8','SDG9',
                                'SDG13', 'SDG151', 'SDG152'), 
                       labels = c('SDGct',  'SDG6.4', 'SDG7.2',  'SDG7.3', 'SDG8.4/12.2', 
                                  'SDG9.4', 'SDG13.2','SDG15.1', 'SDG15.2'))
  # # change facet labels
  # levels(chgs.dt$SDG) <- c('SDGct', 'SDG6.4', 'SDG7.2',  'SDG7.3', 'SDG8.4/12.2',
  #                          'SDG9.4', 'SDG13.2','SDG15.1', 'SDG15.2')
  # --------------------------------------------------------------------------------------------- #
  
  dfs3$group <- factor(dfs3$group, levels=c('glo', '1', '0')) # 1-developed; 0-developing
  
  str(dfs3)
  
  # ---------- Use 95% confidence intervals instead of SEM ------------------ #
  #  geom_errorbar(aes(ymin=sdgvalue-ci, ymax=sdgvalue+ci),
  #                width=.3,    # Width of the error bars
  #                position=position_dodge(.9))
  # ------------------------------------------------------------------------- #
  val.max = max(dfs3$value) + max(dfs3$se); val.max
  val.min = min(dfs3$value) - min(dfs3$se); val.min
  
  # val.min = - 15; val.min
  val.max <- ceiling(val.max/5)*5; val.max
  val.min <- floor(val.min/5)*5;   val.min

  
  # plot --------------------------------------------------------------------
  fig2d <- ggplot(dfs3, aes(x=SDG, y=value, fill=group)) +
    geom_bar(position=position_dodge(0.8), stat="identity", width = 0.8)+ #, binwidth=0)+#,
    # Error bars represent standard error of the mean
    ### add annotate 
    # geom_text(aes(x=SDG,
    #               y=sdgvalue + 0.5 * sign(sdgvalue),
    #               label=format(dfs3$sdgvalue, digits=2)),
    #           position = position_dodge(width = 0.8), vjust = -0.6) +
    
    # width=0.5)+#2/length(unique(dfs3$region))) +
    # scale_fill_brewer(palette="Accent") +
    scale_y_continuous(breaks=seq(val.min,val.max,5),
                       limits=c(val.min,val.max)) +
    #guides(shape = guide_legend(override.aes = list(size = 1))) +
    guides(fill = guide_legend(keywidth = 0.8, keyheight = 0.7, label.position = "right", 
                               label.hjust = 0.1, label.vjust = 0.3)) +
    # guides(fill = guide_legend(label.position = "left", label.hjust = 1)) +
    # theme(legend.text=element_text(size=5)) +
    # guides(colour = guide_legend(override.aes = list(size=1)))+
    geom_errorbar(aes(ymin = value - se, 
                      ymax = value + se),
                  width= 0.2, size = 0.2,                 # Width of the error bars
                  position=position_dodge(.8)) +
    theme_bw() + 
    xlab("SDG targets") +
    ylab("Difference in SDG target scores\nbetween trade and no-trade scenarios") +  ## SDG scores difference
    scale_fill_manual(name="", 
                      values=c("lightskyblue", "royalblue1", "chocolate1"),
                      labels=c("All countries", "Developed", "Developing")) +
    # color: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
    # ggtitle("Changes in SDGs") +
    # theme(plot.title = element_text(hjust = 0.5)) +
    
    geom_vline(xintercept=seq(1,length(dfs3[,2])/3,1)+.5,color="gray", size = 0.2) +
    geom_hline(yintercept=seq(0,0,0),color="gray", size = 0.2) +
    # scale_x_discrete(labels=c("SDG99" = "SDGc", "SDG8" = "SDG8/12")) +
    mytheme +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          # panel.grid.major.x = element_line(size=0.2),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.12,"cm"),
          axis.text.x = element_text(angle=20, hjust=.5, vjust = 1), ## hjust= 1, vjust = 0.5
          legend.position = c(0.15, 0.9)
          #axis.text.x = element_text(angle=0, hjust = 0),
    )#+ # axis.text.x = element_text(angle=60, hjust=0, vjust = 0)
  # fig2d
    #scale_color_manual(labels = c("Develped", "Develpeing","Gloabl"))+
    #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
    #name="",
    #breaks=c("ctrl", "trt1", "trt2"),
    #labels=c("Develped", "Develpeing","Gloabl"))
    #guides(color = guide_legend(override.aes = list(size = 0.5))) +
    #scale_y_continuous(breaks=0:20*4) +
    
    # ggtitle("d") # + 
  
    ## add annotate to small values
    # annotate(geom = "text", x = 8.25, y = 2, 
    #          label = "0.012", color = "chocolate1", size = 4,
    #          angle = 90) +
    # annotate(geom = "text", x = 7.25, y = -3, 
    #          label = "-0.116", color = "chocolate1", size = 4,
    #          angle = 90)
  
  
  ### save plot 
  # fig2d
  # timestamp <-  format(Sys.time(), "%Y%m%d"); timestamp
  fname     <- paste0(dir.fig, '/Fig.2d_c_w', wname, '.jpg'); fname
  ggsave(fname, last_plot(),  width = 8.8, height = 7, units = "cm", dpi = 300)
  
  
  ##### refs #####
  # http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions
  
  return(fig2d)
  
}



# test --------------------------------------------------------------------

# fig2.dt.w1
# getwd()
# Figure2b_func(fig2.dt.w1)

# dfs_2d <- dfs


