# https://rdrr.io/cran/ndtv/man/install.graphviz.html - installation of the graphviz
# https://docs.brew.sh/Installation#untar-anywhere

BiocManager::version()

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

library("Rgraphviz")

chooseCRANmirror()
install.packages("BiocManager")

#this worked
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.12")


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

# The following initializes usage of Bioc devel
BiocManager::install(version='devel')

BiocManager::install("Rgraphviz")

#this worked
library("Rgraphviz")

set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, 0.2)
g1 <- layoutGraph(g1)
renderGraph(g1)

graph.par(list(nodes=list(fill="lightgray", textCol="red")))
renderGraph(g1)
