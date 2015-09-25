#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2015 Michael Gruenstaeudl"
#email = "mi.gruenstaeudl@gmail.com"
#version = "2015.08.28.1700"

library(igraph)
library(scales)
library(rChoiceDialogs)

# Variable specific to data set
ntaxa = 16

rmext = function (instring, delimiter="\\.")
{
    library('stringr')
    # get all instances of "."
    alist = str_locate_all(instring, delimiter)
    # get position of last element
    pos2 = tail(alist[[1]],1)[1]-1
    return(substr(instring, 1, pos2))
}

JML_Viz = function(inFn, alpha) {
    # Parse output of JML as input data
    handle = read.table(inFn, sep="\t", header = TRUE)
    handle = handle[which(handle[,"Probability"] <= alpha),]
    
    if (nrow(handle) >= 1) {
        inData = TRUE
        handle = handle[,grep("Sp.Comparison", colnames(handle))]
        # Filter out only unique hybridisation (i.e., quantitative info is lost)
        handle = unique(handle)
        handle = strsplit(as.character(handle), "-")
        data = as.matrix(t(simplify2array(handle)))
    }
    else {inData = FALSE}

    # Set up empty graph
    g = graph.empty(directed=FALSE)

    
    # Add vertices (i.e. labels) to graph with a specific color
    # FOR GENES 01 and 02
    g = g + vertices(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "z"), color=NA)
    # FOR GENES 03
    #g = g + vertices(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"), color=NA)
    #g = g + vertices(c("z"), color="red")


    # Add edge information (as saved in data)
    if (inData) {
        g = g + graph.edgelist(data, directed=FALSE)
    }

    # Check everything before plotting
    #str(g)
    
    # Rescale verteces
    # http://stackoverflow.com/questions/23209802/placing-vertex-label-outside-a-circular-layout-in-igraph
    radian.rescale = function(x, start=0, direction=1) {
        c.rotate = function(x) (x + start) %% (2 * pi) * direction
        c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
    }
    lab.locs = radian.rescale(x=1:ntaxa, direction=-1, start=0)

    # Only necessary, when plotting columnwise
    par(mar = rep(0.5, 4))

    # Plot the as a Watts-Strogatz graph
    plot(g, layout=layout.circle, vertex.size=25, vertex.frame.color="black", 
         vertex.label.dist=3.0, vertex.label.color="white",
         vertex.label.cex=0.1, vertex.label.degree=lab.locs, edge.color="black", edge.width=2)
}

inDir = rchoose.dir()
setwd(inDir)
nFiles = length(list.files(inDir))

# USER MODIFICATION
svg("gene01_11to20.alpha0.01.svg")
count=11

# Plotting column-wise
layout(matrix(1:nFiles, nrow=nFiles, ncol=1, byrow=F))
for (f in list.files(inDir, pattern = '*.RESULTS.txt')) {
    # USER MODIFICATION
    g = JML_Viz(as.character(f), 0.01)
    text(0, 0, paste("sim.", count), cex=0.5)
    count=count+1
    }
dev.off()
