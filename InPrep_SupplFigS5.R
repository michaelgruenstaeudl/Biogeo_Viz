#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD <mi.gruenstaeudl@gmail.com>"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#version = "2015.11.17.1600"

#############
# Libraries #
#############
require(ggplot2)

####################
# Global Variables #
####################
#setwd("/home/michael/research/analyses/02_analyses_Tolpis_biogeo/09_Simulations/03_P2C2M_full_analysis/TreeDepth_12N/07_visualization/notSorted/")

setwd("/home/michael/research/analyses/02_analyses_Tolpis_biogeo/09_Simulations/03_P2C2M_full_analysis/TreeDepth_12N/07_parsed_results/sptree_fittingMSCM/")

n_sims = 20
n_genes_fixed = 3
n_sum_stats = 3
nums = sprintf("%02d", c(1:n_sims))
fn_prefixes = c("Tolpis.HybrSim.sim.")

## Load .rda-files
for (fns in fn_prefixes) {
    for (num in nums) {
      load(paste(fns, num, ".rda", sep=""))
    }
}

####################
# Helper Functions #
####################

customStack = function(inData, simNum, nLoci, special=FALSE) {

  if (special) {handle = t(as.matrix(inData))}
  else {handle = inData}

  # Order of stats must be alphabetic because input data
  # also sorted alphabetically
  colnames(handle) = c("gtp", "ndc", "ray")
  handle[grepl("\\*", handle)] = 1
  handle[grepl("n.s.", handle)] = 0
  handle[grepl(" 0", handle)] = 0
  handle = stack(data.frame(handle, stringsAsFactors=FALSE))

  colnames(handle)[1] = "value"
  colnames(handle)[2] = "stat"

  if (special) {
    handle[,3] = rep("median", n_sum_stats)
  }
  else {handle[,3] = rep(c("g01","g02","g03"), n_sum_stats)}
  colnames(handle)[3] = "gene"
  handle[,4] = simNum
  colnames(handle)[4] = "sim"
  return(handle)
}

wrapper = function(inData, nums, nLoci, special=FALSE) {
  out_list = list()
  for (num in nums) {
    out_list[[num]] = customStack(inData[[num]], num, nLoci, special)
  }
  return(out_list)
}

########################################
# STEP1. Load data and save into lists #
######################################## 

### perGene ###
dataHandle_a0.01_perGene = list()
for (num in nums) {
  name_handle = paste(fn_prefixes[[1]], num, "$results$alpha0.01$perGene", sep="")
  dataHandle_a0.01_perGene[[num]] = eval(parse(text = name_handle))
}

dataHandle_a0.05_perGene = list()
for (num in nums) {
  name_handle = paste(fn_prefixes[[1]], num, "$results$alpha0.05$perGene", sep="")
  dataHandle_a0.05_perGene[[num]] = eval(parse(text = name_handle))
}

dataHandle_a0.10_perGene = list()
for (num in nums) {
  name_handle = paste(fn_prefixes[[1]], num, "$results$alpha0.1$perGene", sep="")
  dataHandle_a0.10_perGene[[num]] = eval(parse(text = name_handle))
}
#### acrGenes - regular ###
#dataHandle_a0.01_acrGene = list()
#for (num in nums) {
#  name_handle = paste(fn_prefixes[[1]], num, "$results$alpha0.01$acrGene[3,]", sep="")
#  dataHandle_a0.01_acrGene[[num]] = eval(parse(text = name_handle))
#}

######################
# STEP2. Format data #
######################

### perGene ###
dataHandle_pG_0.01 = do.call("rbind", wrapper(dataHandle_a0.01_perGene, nums, n_genes_fixed))
dataHandle_pG_0.01[,5] = rep("0.01", nrow(dataHandle_pG_0.01))
colnames(dataHandle_pG_0.01)[5] = "alpha_value"

dataHandle_pG_0.05 = do.call("rbind", wrapper(dataHandle_a0.05_perGene, nums, n_genes_fixed))
dataHandle_pG_0.05[,5] = rep("0.05", nrow(dataHandle_pG_0.01))
colnames(dataHandle_pG_0.05)[5] = "alpha_value"

dataHandle_pG_0.10 = do.call("rbind", wrapper(dataHandle_a0.10_perGene, nums, n_genes_fixed))
dataHandle_pG_0.10[,5] = rep("0.10", nrow(dataHandle_pG_0.01))
colnames(dataHandle_pG_0.10)[5] = "alpha_value"

### acrGenes ###
#dataHandle_aG_0.01 = do.call("rbind", wrapper(dataHandle_a0.01_acrGene, nums, n_genes_fixed, special=T))

dataHandle = rbind(dataHandle_pG_0.01,
                   dataHandle_pG_0.05,
                   dataHandle_pG_0.10)

dataHandle = dataHandle[which(dataHandle[,"stat"]=="ray"),]
dataHandle[,"alpha_value"] = factor(dataHandle[,"alpha_value"], levels = c("0.10", "0.05", "0.01"))

#####################
# STEP3. Make plots #
#####################

plot_ray = ggplot(data=dataHandle, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=2.5, alpha=1.0) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(alpha_value ~ .) +
    ggtitle("Tolpis Hybrid Simulations") +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("g03","g02","g01"), labels=c("gene 3","gene 2","gene 1")) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n")

svg("/home/michael/Desktop/InPrep_AppendixS5.svg", width=5, height=3)
plot_ray
dev.off()
