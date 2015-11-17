#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD <mi.gruenstaeudl@gmail.com>"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#version = "2015.11.17.1600"

load("/home/michael/research/manuscripts/02_Tolpis_biogeo/03_Figures/NEW_Figure_6/Figure6_data.rda") 

library(ggplot2)
library(grid)

node1_data = data[which(data[,1]=="one"),]
node1_data = node1_data[which(node1_data[,6]=="no"),]
node1_data = node1_data[which(node1_data[,3] %in% c("A","C","E","M")),]
# Adjust order of levels
node1_data[,3] = factor(node1_data[,3], levels = c("A", "C", "E", "M"))
node1_data[,7] = rep("node1", nrow(node1_data))
colnames(node1_data)[7] = "node_number"

# What is nowadays node 2 has previously been node 3
node2_data = data[which(data[,1]=="three"),]
node2_data = node2_data[which(node2_data[,6]=="no"),]
node2_data = node2_data[which(node2_data[,3] %in% c("A","C","E","M")),]
# Adjust order of levels
node2_data[,3] = factor(node2_data[,3], levels = c("A", "C", "E", "M"))
node2_data[,7] = rep("node2", nrow(node2_data))
colnames(node2_data)[7] = "node_number"

node_data = rbind(node1_data, node2_data)

plot_data = ggplot(data=node_data, aes(x=treetype, y=pc)) +
    geom_bar(stat='identity', aes(alpha=0.5)) +
    coord_flip() +
    facet_grid(node_number ~ area) +
    scale_x_discrete(limits=c("stAll3", "stTwoLC", "gtETS", "gtB12", "gtA19"), labels=c("All3Nucl", "2LCNucl", "ETS", "B12", "A19")) +
    scale_y_continuous(breaks=c(0.00, 0.25, 0.50, 0.75, 1.00), labels=c("","0.25","","0.75","")) +
    theme_bw() +
    # x- and y-axieshave been switched [coord_flip() !]
    theme(panel.grid.major.x = element_line(size = 0.8),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),          
          strip.background=element_rect(fill="white"),
          panel.margin = unit(0.75, "lines")) +
    ggtitle("Ancestral Area Reconstructions under BI\n") +
    xlab("Loci (or combinations) under study\n") + 
    ylab("\nPercent of posterior tree distribution")
    # Remove the legend to the right
    theme(legend.position = "none")

svg("/home/michael/Desktop/TolpisBiogeo_Figure4_bothNodes.svg", width=8, height=6)
plot_data
dev.off()
