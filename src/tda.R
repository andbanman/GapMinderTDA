require(TDA);

if (DATA_DIR == "")
  DATA_DIR = getwd()
if (RESULTS_DIR == "")
  RESULTS_DIR = getwd()

# format: ISO, x1, x2, ...
distance <- function(frame, x, y) {
    distance = 0
    x = which(frame$ISO == x)
    y = which(frame$ISO == y)
    ndim = ncol(frame) - 1
    sum = 0
    for (j in 2:ncol(frame)) {
        sum = sum + (frame[x,j] - frame[y,j])^ndim
    }
    distance = (abs(sum))^(1.0/ndim)
    return(distance)
}

# For a given data set construct a distance matrix
# with no imposed structure
distanceMatrix <- function(data) {
    countries = data$ISO
    nr_countries = length(countries)
    dist_matrix = matrix(0, nrow=nr_countries, ncol=nr_countries)
    for (x in 1:nr_countries)
        for (y in 1:nr_countries)
            dist_matrix[x,y] = distance(data,toString(countries[x]), toString(countries[y]))
    return(dist_matrix)
}

# For a given data set construct a distance matrix
# overlaying a border network
adjacencyMatrix <- function(data, dir=DATA_DIR) {
    # construct border_matrix
    countries = data$ISO
    nr_countries = length(countries)
    border_matrix = matrix(999, nrow=nr_countries, ncol=nr_countries)
    borders = read.csv(na.strings="", header=FALSE, colClasses=c("character", "character"),
                       paste(dir, "borders.csv", sep="/"))

    for (x in 1:nr_countries) {
      iso = toString(countries[x])
      neighbors = borders[which(borders[,1] == iso),2]
      if (is.na(neighbors)) next
      neighbors = unlist(strsplit(toString(neighbors), " "))
      for (index in 1:length(neighbors)) {
        y = which(countries == neighbors[index])
        border_matrix[x,y] = distance(data, toString(countries[x]), toString(countries[y]))
      }
    }
    return(border_matrix)
}

pairwiseDistances <- function(frame, countries) {
  if (length(countries) == 0) {
    return()
  }
  distances = matrix(nrow = length(countries), ncol = length(countries))
  for (i in 1:length(countries)) {
    for (j in 1:length(countries)) {
      d = distance(frame, countries[i], countries[j])
      distances[i,j] = d
    }
  }
  return(distances)
}

loopClose <- function(frame, countries, death = 999) {
  dist = pairwiseDistances(frame, countries)
  # try to match on death filtration if possible
  dead_index = which(abs(dist - death) < 0.0001, arr.ind = TRUE)
  if (length(dead_index) != 0) {
    dead_x = countries[dead_index[1,1]]
    dead_y = countries[dead_index[1,2]]
    #print(c("death:", dead_x, dead_y))
    return(c(dead_x,dead_y))
  } else {
    max_index = which(dist == max(dist), arr.ind = TRUE)
    max_x = countries[max_index[1,1]]
    max_y = countries[max_index[1,2]]
    #print(c("max:", max_x, max_y))
    return(c(max_x,max_y))
  }
}

parseCycle <- function(data, cycle, death) {
    loop = c()
    countries = c()
    loop_str = ""
    if (nrow(cycle) == 0) {
         return("")
    }
    for (row in 1:nrow(cycle)) {
        for (col in cycle[row,]) {
          if (length(which(loop == col)) == 0) {
              loop = c(loop, col)
              code = toString(data[col,"ISO"])
              countries = c(countries, code)
              if (loop_str == "") {
                sep=""
              } else {
                sep=" "
              }
              loop_str = paste(loop_str, code, sep=sep)
          }
        }
    }
    if (length(countries) != 0) {
        lastEdge = loopClose(data, countries, death)
        edge_str = paste(lastEdge[1], lastEdge[2], sep=" ")
        loop_str = paste(edge_str, loop_str, sep=",")
    }
    return(loop_str)
}

# Write out dimension 1 intervals with descriptive information
writeDim1Intervals <- function(data, diag, intervals_f) {
  dim1_indices = which(diag$diagram[,"dimension"] == 1)
  dim1_intervals = c() # list of strings
  for (index in dim1_indices) {
    interval = ""

    # interval filtration range
    birth = diag$diagram[index,"Birth"]
    death = diag$diagram[index,"Death"]

    # parse interval string from cycle
    cycle = diag$cycleLocation[[index]]
    loop_str = parseCycle(data, cycle, death)
    if (loop_str == "") next

    interval = (death - birth) # show interval length for easy sorting
    interval = paste(interval, birth, sep=",")
    interval = paste(interval, death, sep=",")
    interval = paste(interval, loop_str, sep=",")

    dim1_intervals = c(dim1_intervals, interval)
  }

  header = "Length, Birth, Death, ClosingSimplex, GenCountries"
  fileConn<-file(intervals_f)
  writeLines(c(header, dim1_intervals), fileConn)
  close(fileConn)
}

# distance matrix
# country seed country
# death   max filt to look at
getConnectedComponents <-function(distances, country, death, comps=c()) {
  # make sure country is included in comps
  if (length(which(comps == country)) == 0)
    comps = c(country, comps)

  # roughly exactly equal to death
  new_comps = which(abs(distances[country,]-death) <= 0.00001)
  # less than death
  new_comps = c(new_comps, which(distances[country,] <= death))

  # don't do recursive search on already seen countries
  test_comps = new_comps[! new_comps %in% comps]

  # no new countries to add
  if (length(test_comps) == 0)
    return(unique(comps))
  else
    comps = c(comps, test_comps)

  # recursively find more countries
  for (v in test_comps)
    comps = getConnectedComponents(distances, v, death, comps)

  return(unique(comps))
}

# format: ISO, x1, x2, ...
analyzeClusters <- function(data, clusters) {
  df <- data.frame(Cluster=character(), Count=c(), stringsAsFactors=FALSE)
  for (cluster in clusters) {
    df = rbind(df, data.frame(Cluster=paste(cluster, collapse=" "), Count=length(cluster), stringsAsFactors=FALSE))
  }

  cols = c(2:ncol(data))

  for (col in cols) {
    means = c()
    for (cluster in clusters) {
      sum = 0
      for (country in cluster) {
       sum = sum + data[which(data$ISO == country),col]
      }
      means = c(means, sum/length(cluster))
    }
    df = cbind(df, means)
    names(df)[which(names(df) == "means")] = names(data)[col]
  }

  # Order in descending cluster size
  df = df[order(-df$Count),]
  rownames(df) = 1:nrow(df)
  return(df)
}

getClusters <- function(data, death, borders=FALSE) {
  if (borders) {
    dist = adjacencyMatrix(data)
  } else {
    dist = distanceMatrix(data)
  }
  clusters = list()

  for (index in 1:nrow(data)) {
    components = getConnectedComponents(dist, index, death)
    cluster = as.vector(data$ISO[components])
    if (length(cluster) == 0 || all(is.na(cluster))) {
      next
    }
    found = FALSE
    for (c in clusters)
      if (length(cluster) == length(c))
        if (all(sort(cluster)==sort(c)))
          found = TRUE
    # todo save cluster and check for index seen so we don't repeat clusters
    if (!found)
      clusters = append(clusters, list(cluster))
  }
  return(clusters)
}

# Write out dimension 1 intervals with descriptive information
writeDim0Intervals <- function(data, dist, diag, intervals_f) {
  dim0_indices = which(diag$diagram[,"dimension"] == 0)
  dim0_intervals = c() # list of strings

  for (index in dim0_indices) {
    birth = diag$diagram[index,"Birth"]
    death = diag$diagram[index,"Death"]
    start = diag$birthLocation[index]

    interval = birth
    interval = paste(interval, death, sep=",")

    components = getConnectedComponents(dist, start, death)
    countries = data$ISO[components]
    interval = paste(interval, paste(countries, collapse=" "), sep=",")

    dim0_intervals = c(dim0_intervals, interval)
  }

  header = "Birth, Death, ComponentCountries"
  fileConn<-file(intervals_f)
  writeLines(c(header, dim0_intervals), fileConn)
  close(fileConn)
}

computePersistence <- function(data, name="", max_dim, dataDir=DATA_DIR, resultsDir=RESULTS_DIR, borders=TRUE) {
    if (borders) {
      dist = adjacencyMatrix(data, dataDir)
    } else {
      dist = distanceMatrix(data)
    }

    if (name == "")
        name = paste(names(data), collapse="-")

    # Compute persistence intervals
    max_filt = max(dist[dist<999])*1.1
    diag <- ripsDiag(X = dist, maxdimension=max_dim,
                          maxscale=max_filt,
                          library = "Dionysus", dist = "arbitrary",
                          printProgress = FALSE, location = TRUE);

    # Plot barcodes and persistence
    plot_f = paste(resultsDir, name, sep='/')
    plot_f = paste(plot_f, ".jpg", sep='')
    jpeg(plot_f)
    par(mfrow = c(2, 1), mai = c(0.8, 0.8, 0.3, 0.1))
    plot(diag[["diagram"]], main = "Diagram", dimension = 1)
    plot(diag[["diagram"]], barcode = TRUE, main = "Barcode", dimension = 1)
    dev.off()

    intervals_f = paste(resultsDir, name, sep='/')
    intervals_f = paste(intervals_f, "d1_intervals.txt", sep='_')
    writeDim1Intervals(data, diag, intervals_f)

    intervals_f = paste(resultsDir, name, sep='/')
    intervals_f = paste(intervals_f, "d0_intervals.txt", sep='_')
    writeDim0Intervals(data, dist, diag, intervals_f)
}
