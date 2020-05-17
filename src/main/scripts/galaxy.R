library(ggplot2)
rs <- read.table("/Users/misja/workspace/backgammon-engine/galaxy.csv",header=TRUE,sep=",")
my.plot <- ggplot(data = rs, mapping = aes(x=oppName, y=oppPr))  + geom_point()
print(my.plot)
