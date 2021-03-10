
#choose.files()
setwd("G:\\My Drive\\_data\\shp\\world_shp\\ne_10m_admin_0_countries\\")


# --- Reading a shapefile with rgdal ----
library(rgdal)
shp1 <- readOGR(".", "ne_10m_admin_0_countries")
#summary(shp1)

# --- Reading a shapefile with maptools ----
library(maptools)
shp2 <- readShapeSpatial("ne_10m_admin_0_countries")
summary(shp2)

names(shp1)
data.frame(shp1)

# --- Mapping with spplot (simple way) -----
spplot(shp1, z="POP_RANK")

# --- Mapping with spplot (detail way) -----
library(classInt)
library(RColorBrewer)
# Generate breaks
brks <-  classIntervals(shp1$POP_RANK, n = 6, style = "quantile")$brks
brks[length(brks)] <- brks[length(brks)] + 1
# Define color swatches
pal  <- brewer.pal(6, "Greens")
# Generate the map
spplot(shp1, z="POP_RANK", at = brks, col.regions=pal)



# --- Mapping with tmap (simple way) ----
library(tmap)
qtm(shp1, 
    fill="POP_RANK", 
    fill.style="quantile", 
    fill.n=8, 
    fill.palette="Greens")
# --- Mapping with tmap (details) ----
library(tmap)
qtm(shp1, 
    fill="POP_RANK", 
    fill.style="fixed", 
    fill.breaks=c(0, 5, 10, 15, 20), 
    fill.labels=c("under 5", "5 to 10", '10-15', "above 15"),
    fill.palette="Greens",
    legend.text.size = 0.5,
    layout.legend.position = c("left", "bottom"))
# or
tm_shape(shp1) + 
  tm_fill("POP_RANK", style="fixed", 
          breaks=c(0, 5, 10, 15, 20),
          labels=c("under 5", "5 to 10", '10-15', "above 15"),
          palette="Greens")  +
  tm_borders("grey") +
  tm_legend(outside = TRUE, text.size = .8) +
  tm_layout(frame = FALSE)




# --- Joining tables to spatial features ----
library(readxl)
dt <- read_excel(path = ".\\Table\\ne_10m_admin_0_countries-Export_Output.xls",
                 sheet = "dat", 
                 col_names = T)
dt <- as.data.frame(dt[,-c(3,4)])

dt_eco <- 
  read_excel("G:\\My Drive\\_paper\\trade_Global SDGs\\results\\update_0503_SUM_dist\\_data_output_composite.xlsx",
             sheet = "ecoSDG", col_names = T)

library(dplyr)
str(shp1$ADM0_A3)
str(dt)
# join name table
shp1@data <- left_join(shp1@data, dt,     by=c("ADM0_A3" = "ADM0_A3"))
names(shp1)
shp1$nation_name.x

# --- save shp ----
temp = format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
dir_output <- 'dir_output'

dir_output2 <- paste('dir_output', temp, sep="_")
dir_output2
dir.create(dir_output) # create a new dir
writeOGR(obj=shp1, dsn=dir_output, layer="ne_10m_admin_0_countries_new_Name", driver="ESRI Shapefile")



# join SDG data
shp1@data <- left_join(shp1@data, dt_eco, by=c("nation_name.x" = "Name"))
names(shp1)
dtt <- as.data.frame(shp1[, 95:132]) 
# remove Antarctica
shp3 = subset(shp1, NAME != "Antarctica")

# --- viz shp3 ----
#spplot(shp3, z="2009")

qtm(shp3, fill="2009", 
    fill.style="jenks", # fixed, equal, sd, quantile, kmeans, hclust, jenks, 
    fill.n=8 ,
    fill.palette="RdYlGn")


# --- create new field - mean value of SDG_eco----
names(shp3@data)
shp3@data$SDGeco_trade    <- rowMeans(shp3@data[102:116])  # rowSums
shp3@data$SDGeco_No_trade <- rowMeans(shp3@data[120:134])  # rowSums

names(shp3@data)

# set min max value
df_range <- data.frame(cbind(shp3@data$SDGeco_trade, shp3@data$SDGeco_No_trade))
df_min <- min(df_range)
df_max <- max(df_range)


# distribution of data
# library(ggplot2)
# data <- data.frame(value = df_range)
# data <- data.frame(unique(data))
# data <- data.frame(value = data)
# ggplot(data, aes(x=value)) + geom_histogram()


# output png

# --- viz try 01 ----
tm_shape(shp3) + 
  tm_fill("SDGeco_trade", 
          style="jenks", 
          n = 5,   # number of breaks
          palette="RdYlGn")  +
  tm_borders(col = "grey", lwd = 0.1, lty = "solid", alpha = 0.99) +
  tm_legend(outside = F, 
            text.size = .8) +
  tm_layout(frame = T,
            legend.position = c("left","bottom"))
# https://stackoverflow.com/questions/32890762/how-to-manipulate-tmap-legend?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

# --- output png 1 ----
png(filename="SDGeco_trade.png", 
    units="in", 
    width=8, height=4, 
    pointsize=12, res=300)

tm_shape(shp3) + 
  tm_fill("SDGeco_trade", 
          style="fixed", 
          breaks=c(seq(df_min, df_max, by=8)),
          #labels=c("under 5", "5 to 10", '10-15', "above 15"),
          palette="RdYlGn")  +
  tm_borders(col = "grey", lwd = 0.1, lty = "solid", alpha = 0.99) +
  tm_legend(outside = F, text.size = .8) +
  tm_layout(frame = T,
            legend.position = c("left","bottom"))
dev.off()

# --- output png 2 -----
png(filename="SDGeco_No_trade.png", 
    units="in", 
    width=8, height=4, 
    pointsize=12, res=300)
tm_shape(shp3) + 
  tm_fill("SDGeco_No_trade", 
          #style="jenks", 
          style="fixed", 
          breaks=c(seq(df_min, df_max, by=8)),
          palette="RdYlGn")  +
  tm_borders(col = "grey", lwd = 0.5, lty = "solid", alpha = 0.99) +
  tm_legend(outside = F, 
            text.size = .8) +
  tm_layout(frame = T,
            legend.position = c("left","bottom"))
dev.off()
# https://stackoverflow.com/questions/32890762/how-to-manipulate-tmap-legend?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa












