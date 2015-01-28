#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl","Noah Reid")
#email = "gruenstaeudl.1@osu.edu"
#version = "2014.12.15.2100"

#data = read.csv("~/Desktop/Table6_data.csv")
load("~/Desktop/Table6_data.rda") 

library(ggplot2)

node1_data = data[which(data[,1]=="one"),]
# Adjust order of node1_data levels
node1_data[,3] = factor(node1_data[,3], levels = c("I", "N", "A", "C", "E", "M"))

node3_data = data[which(data[,1]=="three"),]
# Adjust order of node1_data levels
node3_data[,3] = factor(node3_data[,3], levels = c("I", "N", "A", "C", "E", "M"))


plot = ggplot(data=node1_data, aes(x=treetype, y=pc)) +
    geom_bar(stat='identity', aes(fill=area, alpha=0.5)) +
    coord_flip() +
    facet_grid(constraint ~ area) +
    scale_x_discrete(limits=c("stAll3", "stTwoLC", "gtETS", "gtB12", "gtA19")) +
    scale_y_continuous(breaks=c(0.00, 0.25, 0.50, 0.75, 1.00), labels=c("","0.25","","0.75","")) +
    theme_bw() +
    # x- and y-axieshave been switched [coord_flip() !]
    theme(panel.grid.major.x = element_line(size = 0.8),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),          
          strip.background=element_rect(fill="white"),
          panel.margin = unit(0.75, "lines")) +
    ggtitle("Table 6\n") +
    # Remove the legend to the right
    theme(legend.position = "none")


svg("/home/michael/Desktop/Table6.svg", width=8, height=6)
plot
dev.off()
