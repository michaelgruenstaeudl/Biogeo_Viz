#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD <mi.gruenstaeudl@gmail.com>"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#version = "2015.11.17.1600"

library(ape)

inDir = "/home/michael_science/research/manuscripts/02_Tolpis_biogeo/03_Figures/2015_FU-Berlin/AppendixS2_new/01_input/Tolpis.HybrSim.sim.11/"
gene1_trees = read.tree(paste(inDir, "gene001.trees", sep=""))
gene2_trees = read.tree(paste(inDir, "gene002.trees", sep=""))
gene3_trees = read.tree(paste(inDir, "gene003.trees", sep=""))
species_trees = read.tree(paste(inDir, "species.trees", sep=""))


for (i in c(98,99,100)) {

    svg(paste(inDir, "AppendixS2_Sim", i, ".svg", sep=""), width=15, height=20)

        layout(matrix(1:4,1,4,byrow=T))

        plot(ladderize(gene1_trees[[i]]), edge.width=3, cex=1.25, label.offset=0.005, no.margin=T);
        legend("topleft", "gene 1", cex=3, text.font=2, bty = "n");
        plot(ladderize(gene2_trees[[i]]), edge.width=3, cex=1.25, label.offset=0.005, no.margin=T);
        legend("topleft", "gene 2", cex=3, text.font=2, bty = "n");
        plot(ladderize(gene3_trees[[i]]), edge.width=3, cex=1.25, label.offset=0.005, no.margin=T);
        legend("topleft", "gene 3", cex=3, text.font=2, bty = "n");
        plot(ladderize(species_trees[[i]]), edge.width=3, cex=3, label.offset=0.01, no.margin=T);
        legend("topleft", "species", cex=3, text.font=2, bty = "n");

    dev.off()

}
