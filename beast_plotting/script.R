library(phytools)
library(ape)
library(strap)
library(phyloch)
library(treeio)
library(ggtree)
library(ggplot2)

# RELAXED LOGNORMAL CLOCK WITHOUT OUTGROUP SYCOPHILA
# 25 million generations, sampling every 1000 generations
# standard insect mt clock rate, 0.0115 substitutions per million years (Brower et al. 1994)
# clock rate st dev set to 10% of the clock rate (0.00115) "Mean in Real Space" selected

tree_relaxed_no_outgroup = phyloch::read.beast("beast_treeannotator2.txt")
tree_relaxed_no_outgroup$root.time = 13.4


strap::geoscalePhylo(tree=ladderize(tree_relaxed_no_outgroup,right=FALSE), units=c("Period", "Epoch"),
                     boxes="Epoch", cex.tip=0.25, cex.age=0.7, cex.ts=0.7, label.offset=0, 
                     x.lim=c(-15,16), lwd=2, width=1, ts.col = F, tick.scale = 1)

HPDbars(tree_relaxed_no_outgroup, lwd = 2, col = "cornflowerblue")

node_cols = c()

for(i in 1:tree_relaxed_no_outgroup$Nnode){
  if(round(tree_relaxed_no_outgroup$posterior,2)[i] == 1)
    node_cols[i] = "red"
  if(round(tree_relaxed_no_outgroup$posterior,2)[i] > 0.91 && round(tree_relaxed_no_outgroup$posterior,2)[i] < 0.99)
    node_cols[i] = "orange"
  if(round(tree_relaxed_no_outgroup$posterior,2)[i] > 0.76 && round(tree_relaxed_no_outgroup$posterior,2)[i] < 0.90)
    node_cols[i] = "forestgreen"
  if(round(tree_relaxed_no_outgroup$posterior,2)[i] > 0.51 && round(tree_relaxed_no_outgroup$posterior,2)[i] < 0.75)
    node_cols[i] = "turquoise"
  if(round(tree_relaxed_no_outgroup$posterior,2)[i] > 0 && round(tree_relaxed_no_outgroup$posterior,2)[i] < 0.50)
    node_cols[i] = "blue"
}

# add nodelabels as colours
nodelabels(round(tree_relaxed_no_outgroup$posterior,2), bg = node_cols, frame = "circle", cex = 0.1, adj = c(1.2, -0.2))

# or add them as text
nodelabels(round(tree_relaxed_no_outgroup$posterior,2), bg = "white", col = node_cols, frame = "none", cex = 0.7, adj = c(1.2, -0.2))


#LTT PLOTS
### better with ltt.coplot():
par(mfrow = c(1,1))
ltt.coplot(tree_relaxed_no_outgroup, show.tip.label = FALSE)

par(mar = c(5.1, 4.1, 4.1, 2.1))

ape::ltt.plot(tree_relaxed_no_outgroup, backward = T, log="y", col = "red", lwd = 3)
?ltt.plot
ltt(tree_relaxed_no_outgroup)

# Plot the tree, but with character states next to it

iotreeplot = ggtree::ggtree(tree_relaxed_no_outgroup) 
iotreeplot

host.plants = read.csv("host_plants.csv", row.names = 1) # row.names = 1 makes sure that the first column is read in as row names, not as a data column

# change the numeric values to characters

host.plants[] <- lapply(host.plants, as.character)

binary_phylo =  gheatmap(iotreeplot, host.plants, offset=2, width=0.2, colnames_position = "top", colnames_angle = 90, font.size=2.5, color="black", colnames_offset_y = 2) + 
  scale_fill_manual(values=c("white", "darkblue")) +
  geom_tiplab(size=2, color="black", font = 1) +
  theme(legend.position="bottom") +
  geom_treescale() 

?gheatmap

plot(binary_phylo)

#############################################################################################################################################

# GMYC ANALYSIS

#############################################################################################################################################

library(splits)

tree.gmyc = splits::gmyc(tree_relaxed_no_outgroup, quiet = F, method = "multiple")
splits::plot.gmyc(tree.gmyc)
splits::spec.list(tree.gmyc)
splits::gmyc.support(tree.gmyc)

support_multiple = splits::gmyc.support(tree.gmyc)

is.na(support_multiple[support_multiple == 0]) <- TRUE
       # plot the tree
nodelabels(round(support_multiple, 2), cex=.7, bg = "white", frame = "none", adj = c(1.2, -0.2))     # plot the support values on the tree
