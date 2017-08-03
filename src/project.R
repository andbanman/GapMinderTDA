DATA_DIR <- paste(getwd(), "/../data", sep= "");
RESULTS_DIR <- paste(getwd(), "/../results", sep= "");

require("rworldmap")
require("RColorBrewer")
source(file="data_process.R")
source(file="tda.R")

# select indicators
indicators = c(
    "infant_mortality",
    "gni_per_capita_ppp"
)

### Load data
data = mergeIndicators(indicators)
data2d = read.csv(na.strings="", paste(DATA_DIR, "data_IL-new.csv", sep="/"))
data4d = merge(data2d, data, by="ISO", all=FALSE)

### write the data
write.csv(data2d, paste(RESULTS_DIR, "2d.csv", sep="/"))
write.csv(data4d, paste(RESULTS_DIR, "4d.csv", sep="/"))

computePersistence(data=data2d, max_dim=2)
computePersistence(data=data4d, max_dim=2)

worldMapClusters <- function(data, clusters, num=6, title, dir=RESULTS_DIR) {
  top = clusters[c(1:num),]
  d=cbind(data, Cluster=0)
  i = 1
  for (cluster in top$Cluster) {
    countries = unlist(strsplit(top$Cluster[i], split=" "))
    for (country in countries)
      d$Cluster[which(d$ISO == country)] = num - i + 1
    i = i + 1
  }
  d_sub = d[which(d$Cluster > 0),]

  n <- joinCountryData2Map(d_sub, joinCode="ISO2", nameJoinColumn="ISO")
  print(top)

  # setup colors, flip them
  colorPalette = brewer.pal(num,'Blues')

  file = paste(dir, title, sep="/")
  file = paste(file, "png", sep=".")
  png(file, width=640, height=320)
  mapCountryData(n, nameColumnToPlot="Cluster",
                 mapTitle=title,numCats=k,
                 catMethod="categorical",
                 colourPalette=colorPalette,
                 oceanCol="white",
                 missingCountryCol="white",
                 addLegend=FALSE)
  #par(mfrow = c(2, 1), mai = c(0.8, 0.8, 0.3, 0.1))
  dev.off()
}

# Recreate original 2D clustering study 0.025
# 32, 30, 11, 10, 9, 8 ...
clusters2d = analyzeClusters(data2d, getClusters(data2d, 0.025, borders=FALSE))
worldMapClusters(data2d, clusters2d, title="2D Rips Clusters (d=0.025)")

# make table of 4D clusters
names(data4d)[2:5] = c("GDP/capita", "LifeExp", "InfantMort", "GNI/capita")
clusters4d = analyzeClusters(data4d, getClusters(data4d, 0.065, borders=FALSE))

# write out distances over border graph
# for MATLAB javaplex, makes prettier barcodes that are equivalent to TDA
file2d = paste(RESULTS_DIR, "adjacency2d.csv", sep="/")
file4d = paste(RESULTS_DIR, "adjacency4d.csv", sep="/")
dist2d = adjacencyMatrix(data2d)
dist4d = adjacencyMatrix(data4d)
write.csv(as.matrix(dist2d), file2d, row.names=FALSE, col.names=FALSE)
write.csv(as.matrix(dist4d), file4d, row.names=FALSE, col.names=FALSE)

# write out raw distances
file2d = paste(RESULTS_DIR, "distances2d.csv", sep="/")
file4d = paste(RESULTS_DIR, "distances4d.csv", sep="/")
dist2d = distanceMatrix(data2d)
dist4d = distanceMatrix(data4d)
write.csv(as.matrix(dist2d), file2d, row.names=FALSE, col.names=FALSE)
write.csv(as.matrix(dist4d), file4d, row.names=FALSE, col.names=FALSE)

# make a bunch of 2d world maps
for (v in c(0.025, 0.035, 0.045, 0.055)) {
  print(v)
  clusters2d = analyzeClusters(data2d, getClusters(data2d, v, borders=FALSE))
  title = paste("2D Rips Clusters (d=", v, sep="")
  title = paste(title, ")", sep="")
  csv = paste(RESULTS_DIR, title, sep='/')
  csv = paste(csv, "csv", sep=".")
  write.csv(clusters2d, file=csv)
  worldMapClusters(data2d, clusters2d, num=6, title=title)
}

# make a bunch of 4d world maps
for (v in c(0.065, 0.075, 0.085, 0.095)) {
  print(v)
  clusters4d = analyzeClusters(data4d, getClusters(data4d, v, borders=FALSE))
  title = paste("4D Rips Clusters (d=", v, sep="")
  title = paste(title, ")", sep="")
  csv = paste(RESULTS_DIR, title, sep='/')
  csv = paste(csv, "csv", sep=".")
  write.csv(clusters4d, file=csv)
  worldMapClusters(data4d, clusters4d, num=6, title=title)
}

### Helper function for generating a table of values for
# a given set of countries, i.e. for a cycle
latexCountryTable <- function(data, countries, caption=""){
  # format: id, x1, x2,...
  id = names(data)[1]
  ndim = ncol(data) - 1
  cols = 2:ncol(data)
  tblfmt="l"
  for (i in 1:ndim){tblfmt=paste(tblfmt,"r",sep="")}

  str = "\\parbox{.45\\linewidth}{\n"
  str = paste(str, "\\caption{");
    str = paste(str, caption, sep="");
    str = paste(str, "}\n", sep="")
  str = paste(str, "\\begin{tabular}{")
    str = paste(str, tblfmt, sep="")
    str = paste(str, "}\n", sep="")
  str = paste(str, "\\hline\\noalign{\\smallskip}\n")
  str = paste(str, "Country")
    for(head in names(data)[cols]){str=paste(str,"&"); str=paste(str,head)}
     str = paste(str, "\\\\\n")
  str = paste(str,"\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  for (c in countries){
    str = paste(str, c)
    for(i in cols){str=paste(str,"&"); str=paste(str,round(data[which(data[,id]==c),i],2))}
       str = paste(str, "\\\\\n")
  }
  str = paste(str,"\\noalign{\\smallskip}\\hline\\noalign{\\smallskip}\n")
  str = paste(str,"\\end{tabular}\n")
  str = paste(str,"\\label{cycle-");
    str = paste(str,paste(countries,collapse="-"),sep="");
    str = paste(str, "}\n", sep="")
  str = paste(str,"}", sep="")
  cat(str)
}
