#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD <mi.gruenstaeudl@gmail.com>"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#version = "2015.11.17.1600"

require(ggplot2)

inFn = "/home/michael_science/Desktop/Tolpis_AAR/Evaluating_STEM-Hy/04_visualization/02_process/SimulatedData_STEMHy_Results_hybridplot.txt"
#inFn = "/home/michael/research/analyses/02_analyses_Tolpis_biogeo/08_STEM-Hy/02_On_best_likelihood_trees/04_visualization/STEM-Hy_Results_hybridplot.txt"

d = read.csv(inFn)

#Convert presence/absence data from being integers to strings,
#because the colour designation only works on 
d$presence = lapply(d$presence, as.character)
d$presence = factor(d$presence, levels = c("0", "1"))

#####################
# STEP3. Make plots #
#####################

'''
# 2014_OSU

plot_stemhy = ggplot(data=d, aes(x=taxa, y=gen)) +    
    # Variable "presence" contains information on presence/absence
    geom_point(aes(colour=presence, shape=presence), size=0.5, alpha=1.0) +
    scale_colour_manual(values=c(NA, 'black')) +
    scale_shape_manual(values=c(NA, 15)) +
    #facet_grid(alpha_value ~ .) +
    ggtitle("STEM-Hy Presence/Absence\n") +
    theme_bw() +
    #scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(breaks=c(0,10,20,30,40,50,60,70,80,90,99), labels=c(0,10,20,30,40,50,60,70,80,90,"100")) +
    theme(axis.text = element_text(size=12),
          axis.title=element_text(size=14),
          strip.background=element_rect(fill="white"),
          legend.position = "none") +
    xlab("\nTaxa") + 
    ylab("Generations of the Posterior Predictive Distribution\n")

svg("/home/michael/Desktop/Figure4.svg", width=8, height=15)
plot_stemhy
dev.off()
'''


# 2015_FUBerlin

gnums = c(1:20)

plot_stemhy = ggplot(data=d, aes(x=taxa, y=gen)) +    
    geom_point(aes(colour=presence, shape=presence), size=0.5, alpha=1.0) +
    scale_colour_manual(values=c(NA, 'black')) +
    scale_shape_manual(values=c(NA, 15)) +
    #ggtitle("STEM-Hy Presence/Absence\n") +
    theme_bw() +
    #scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(breaks=gnums, labels=sprintf("%02d", gnums)) +
    theme(axis.text = element_text(size=12),
          axis.title=element_text(size=14),
          strip.background=element_rect(fill="white"),
          legend.position = "none") +
    xlab("\nTaxa") + 
    ylab("Simulations\n")

svg("/home/michael_science/Desktop/TolpisBiogeo_Figure3.svg", width=6, height=8)
plot_stemhy
dev.off()
