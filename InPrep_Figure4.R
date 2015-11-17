#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD <mi.gruenstaeudl@gmail.com>"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#version = "2015.11.17.1600"

library(ggplot2)
library(grid)

inFn = "/home/michael_science/research/manuscripts/02_Tolpis_biogeo/03_Figures/2015_FU-Berlin/Figure_4/data/Figure4_data.csv"
data = read.csv(inFn)
clade_data = data[which(data[,2] %in% c("AZ","CI","CM","MA")),]
clade_data[,2] = factor(clade_data[,2], levels = c("AZ", "CI", "CM", "MA"))  # Adjust order of level

printerfriendly_bw_palette = c("#ffffff", "#cccccc", "#969696", "#000000")

plot_data = ggplot(data=clade_data, aes(x=treetype, y=pc, fill=area)) +
    geom_bar(stat="identity", colour="black") +
    coord_flip() + # x- and y-axes have been switched [coord_flip() !]
    facet_grid(clade ~ exclusion) +
    scale_x_discrete(limits=c("stLS2", "stLS1", "gtETS", "gtB12", "gtA19"), labels=c("LS2", "LS1", "ETS", "B12", "A19")) +
    scale_y_continuous(breaks=c(0.00, 0.25, 0.50, 0.75, 1.00), labels=c("","0.25","","0.75","")) +
    theme_bw() +
    scale_fill_manual(values = printerfriendly_bw_palette,
                      name="Ancestral area",
                      breaks=c("AZ", "CI", "CM", "MA"),
                      labels=c("Azores", "Canary Islands", "Cont. Med.", "Madeira")) +
    theme(text = element_text(size=28),
          panel.grid.major.x = element_line(size = 0.8),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),          
          strip.background=element_rect(fill="white"),
          panel.margin = unit(0.75, "lines"),
          plot.title = element_text(size=28)) +
    ggtitle("Archipelagoes\n") +
    xlab("Locus or locus sets\n") + 
    ylab("\nPercent of posterior tree distribution")

svg("/home/michael/Desktop/TolpisBiogeo_Figure4_bothNodes.svg", width=16, height=9)
plot_data
dev.off()
