


# packages ----------------------------------------------------------------
library(rnaturalearthdata)
library(rnaturalearth)
library(sp)
library(sf)
# install.packages("extrafont")
library(extrafont)
# font_import()


# theme, font -------------------------------------------------------------
font <- "Arial" ## "Times" 
font <- 'sans' ## = "TT Arial"
font_size <- 6 ## max = 7; min = 5 for Nature Sustainability
windowsFonts()
library(extrafont)
# font_import(pattern = 'Arial')
# loadfonts(device = "win")

mytheme <- theme(axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 legend.position = c(0.1, 0.2),
                 legend.key.size = unit(0.1, "cm"),
                 text=element_text(family=font)) # , size=font_size


# fig_1b ------------------------------------------------------------------

function_map_fig1 <- function(data, var, brks, lables, colors, legend.title){
  
  (p <- ggplot(data = data) + 
     geom_sf(aes(fill = var), color='gray50', size=0.01) + theme_bw() + 
     
     # scale_fill_gradientn(colors=colors,
     #                      breaks=brks,
     #                      labels=labels,
     #                      # limits=c(df_min,df_max),
     #                      na.value ='gray70') +
     
     scale_fill_manual(values = colors, 
                       # breaks = breaks,
                       labels = labels) +
     guides(fill = guide_legend(label.hjust = 0, label = T, reverse = T, title = legend.title))+
     mytheme
     
     # guides(fill = guide_legend(label.hjust = 1, label = T, reverse = T))+
     # theme_minimal()+
     # theme_nothing() +
     
  )
  

  ## save plot as jpg
  pname <- paste0(dir.fig, '/map_', filename, '.jpg')
  # ggsave(filename = pname, plot = p, width = 6, height = 5, units = 'in', dpi = 300)
  return(p)
  
}

# ref: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html



# fig_1b ------------------------------------------------------------------
legend.title = 'SDGc score'
data <- shp_dt
var <- data$rel_c

library(classInt)
min <- floor(min(var, na.rm = T)/5.0)*5;   min
max <- ceiling(max(var, na.rm = T)/5.0)*5; max 
breaks <- seq(min, max, 5); breaks; length(breaks)
colors <- col5; length(colors)
breaks <- breaks[-c(length(breaks)-1)]; breaks; length(breaks) ## modify the breaks

is <- seq(1,length(breaks)-1, 1); is
labels <- c()
for (i in is) {
  # print(breaks[i])
  bk <- paste0(breaks[i], ' - ', breaks[i+1]); print(bk)
  labels <- c(labels, bk)
}
labels
breaks


colors <- c(
  ## greens: https://colorbrewer2.org/#type=sequential&scheme=YlGn&n=9
  '#ffffe5',
  # '#f7fcb9',
  '#d9f0a3',
  '#addd8e',
  '#78c679',
  '#41ab5d',
  '#238443',
  '#006837',
  '#004529'
)

length(breaks)
length(labels)
length(colors)

data$brks <- cut(var, breaks=breaks, labels=labels)

fig1b <- function_map_fig1(data, var=data$brks, brks=breaks, lables, colors = colors, legend.title)





# fig_1c ------------------------------------------------------------------
legend.title <- 'SDGc score\nchange'
var <- data$rel_not_chg

library(classInt)
# intv <- classIntervals(data$rel_not_chg, n = 6, style = 'equal'); intv#can use "jenks" but is very slow
# intv$brks
### create breaks
min <- floor(min(var,   na.rm = T)/5.0)*5; min
max <- ceiling(max(var, na.rm = T)/5.0)*5; max 
breaks <- seq(min, max, 5); breaks
breaks <- breaks[c(1:6, 10)];  breaks ## modify the breaks

### create labels
is <- seq(1,length(breaks)-1, 1); is
labels <- c()
for (i in is) {
  # print(breaks[i])
  bk <- paste0(breaks[i], ' - ', breaks[i+1]); print(bk)
  labels <- c(labels, bk)
}
labels
breaks

### process data for plot
data$brks <- cut(data$rel_not_chg, breaks=breaks, labels=labels)


### create colors
colors <- c(
  ## brown
  '#feb24c',
  '#ffeda0',
  
  ## blues
  # '#eff3ff',
  '#c6dbef',
  # '#9ecae1',
  '#6baed6',
  # '#4292c6',
  '#2171b5',
  '#084594'
)

length(breaks)
length(labels)
length(colors)

### plot
fig1c <- function_map_fig1(data, var=data$brks, brks=breaks, lables, colors = colors, legend.title)
# fig1c



# fig_1d ------------------------------------------------------------------

legend.title = 'Number of SDGs \nwith increased scores'
data <- shp_dt
var <- data$num

data$brks <- as.factor(data$num)
breaks <- seq(0,7,1); breaks
labels <- breaks

library(RColorBrewer)

plotvar <- var
nclr <- 8
plotclr <- brewer.pal(nclr,"Blues")
# plotclr <- plotclr[nclr:1] # reorder colors
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)
colors <- attr(colcode, "palette")

fig1d <- function_map_fig1(data, var=data$brks, brks=breaks, lables, colors = colors, legend.title)
# fig1d

# fig_1d2 -----------------------------------------------------------------
legend.title = 'Number of SDGs \nwith decreased scores'
data <- shp_dt
var <- data$num_

data$brks <- as.factor(data$num_)
breaks <- seq(0,7,1); breaks
labels <- breaks

library(RColorBrewer)

plotvar <- var
nclr <- 8
plotclr <- brewer.pal(nclr,"Oranges")
# plotclr <- plotclr[nclr:1] # reorder colors
class <- classIntervals(plotvar, nclr, style="equal")
colcode <- findColours(class, plotclr)
colors <- attr(colcode, "palette")

fig1d2 <- function_map_fig1(data, var=data$brks, brks=breaks, lables, colors = colors, legend.title)
# fig1d2



# in one ------------------------------------------------------------------
library(ggpubr)

fig1 <- ggarrange(fig1b, fig1c, fig1d, fig1d2,   
                   nrow = 2, ncol = 2, labels = 'auto',
                   align = 'v', common.legend = F)


# (fig1 <- ggarrange(
#   ggarrange(fig1b, fig1c, fig1d, fig1d2,   nrow = 2, ncol = 2, labels = 'AUTO',
#             align = 'v', common.legend = T, legend = 'right'),
#   # ggarrange(pchg, nrow = 1, ncol = 2, labels = c('E'),
#   #           align = 'v', common.legend = T, legend = 'right'),
#   nrow = 2, ncol = 1, heights = c(2,1), align = 'v')
# )

getwd()
fname <- paste0(dir.fig, '/Fig_1bcd.pdf'); fname
ggsave(fname, fig1, width = 18, height = 9, units = 'cm', limitsize = FALSE)






