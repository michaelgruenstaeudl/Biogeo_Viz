# Load libraries
library(ape)
library(ggplot2)
library(gridExtra)
library(phangorn)
library(reshape)


# Global variables
n_genes = 10
#n_sims = sprintf("%02d", 1:20)
n_sims = sprintf("%02d", 1:5)

# Start sim-loop
for (s in n_sims) {
    cat(paste("\n", "Analyzing simulation", s, "\n"))

    # Setup prefices
    BEAST_prefix = paste("/home/michael/Desktop/CommT.Jan2015/03_output_from_BEAST/CommT.Jan2015.fitonly.sim.", s, ".gene", sep="")
    starBEAST_prefix = paste("/home/michael/Desktop/CommT.Jan2015/03_output_from_starBEAST/CommT.Jan2015.swapped.sim.", s, ".gene", sep="")

    # Initialize lists
    BEAST_postGTdistr = list()
    starBEAST_postGTdistr = list()

    # Loading the posterior gene tree distributions
    for (g in 1:n_genes) {
    g_lz = sprintf("%03d", g)
    BEAST_postGTdistr[[g]] = read.nexus(paste(BEAST_prefix, g_lz, ".trees", sep=""))
    starBEAST_postGTdistr[[g]] = read.nexus(paste(starBEAST_prefix, g_lz, ".trees", sep=""))
    }

    # Initialize outmartix
    out_df = matrix(nrow=length(BEAST_postGTdistr[[1]]), ncol=n_genes, NA)

    # Calculate various tree distances [2] = Kuhner-Felsenstein distance
    for (g in 1:n_genes) {
    for (i in 1:length(BEAST_postGTdistr[[g]])) {
    out_df[i,g] = treedist(BEAST_postGTdistr[[g]][[i]], starBEAST_postGTdistr[[g]][[i]], check.labels = TRUE)[2]
    }}

    # Add column names
    columnnames = lapply(sprintf("%03d", 1:10), function(x){paste("gene", x, sep="")})
    colnames(out_df) = columnnames

    # Stack the data
    results = melt(out_df, id.vars=c(columnnames))

    # Add grouping variable
    results[,4] = rep("gene002-gene010", length(results[,3]))
    results[which(results[,2]=="gene001"),4] = "gene001"

    # Add colnames
    colnames(results) = c('generation', 'gene', 'KFdist', 'swapped')

    # Visualization of individual plots
    my_plot = ggplot(data=results, aes(x=KFdist, group=gene, color=factor(swapped))) +
    geom_density(line=2) +
    ggtitle(paste("sim.", s, sep="")) +
    theme_bw() +
    theme(legend.position = "none",
    plot.title = element_text(size = rel(2)))

    assign(paste("sim.", s, sep=""), my_plot)

# End sim-loop
}


# Setup layout for plotting
svg("~/Desktop/CommT.Jan2015_KFdist_sim.first5.svg", width=10, height=5)
grid.arrange(sim.01, sim.02, sim.03, sim.04, sim.05, ncol=3)
dev.off()

