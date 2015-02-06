#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl","Noah Reid")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.02.05.2000"

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

JML_Viz = function(inFn, outFn) {
    # Parse output of JML as input data
    handle = read.table(inFn, sep="\t", header = TRUE)
    if (nrow(handle) >= 1) {
        inData = TRUE
        handle = handle[,grep("Sp.Comparison", colnames(handle))]
        handle = unique(handle)
        handle = strsplit(as.character(handle), "-")
        data = as.matrix(t(simplify2array(handle)))
    }
    else {inData = FALSE}

    # Set up empty graph
    g = graph.empty(directed=FALSE)

    # Add vertices (i.e. labels) to graph with a specific color
    g = g + vertices(c("min"), color="grey") 
    g = g + vertices(c("azo", "sua"), color="blue")
    g = g + vertices(c("cor", "cra", "far", "gla", "lac", "lag", "san", "spn", "web"), color="yellow")
    g = g + vertices(c("bar", "vir"), color="green")
    g = g + vertices(c("sum", "mac"), color="red")

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

    # Plot the as a Watts-Strogatz graph
    plot(g, layout=layout.circle, vertex.size=25, vertex.frame.color=NA, 
         vertex.label.dist=1.5, vertex.label.color="black",
         vertex.label.cex=0.8, vertex.label.degree=lab.locs,
         edge.color="black", edge.width=2)
}

inDir = rchoose.dir()
setwd(inDir)
nFiles = length(list.files(inDir))

svg("combined.svg")
layout(matrix(1:nFiles,1,nFiles,byrow=T))
for (f in list.files(inDir)) {g = JML_Viz(as.character(f))}
dev.off() 
