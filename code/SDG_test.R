library("ggplot2")
num <- c(1, 8, 4, 3, 6, 7, 5, 2, 11, 3)
cat <- c(letters[1:10])
data <- data.frame(num, cat) 
data
data <- data[order(data$cat), ]

data
ggplot(data,aes(x= cat))+
  geom_bar(stat ="identity")

