######################### Functions  ###############################
require(plyr)

if (DATA_DIR == "")
  DATA_DIR = getwd()
if (RESULTS_DIR == "")
  RESULTS_DIR = getwd()

addISOCodes <- function(frame, dir=DATA_DIR) {
    country_codes_f = paste(dir, "country_codes.csv", sep="/")
    country_codes = read.csv(colClasses=c("character", "character"), header=FALSE, encoding="UTF-8", na.strings="", country_codes_f)
    colnames(country_codes) = c("country", "ISO")
    frame = merge(frame, country_codes, by="country", all.x=TRUE)
    no_iso = frame$country[which(is.na(frame$ISO))]
    if (length(no_iso) > 0) {
      print(paste("WARN: no ISO code for countries:", toString(no_iso)))
      frame = frame[which(!is.na(frame$ISO)),]
    }
    duplicates = count(frame$ISO)[which(count(frame$ISO)[,2] > 1),]
    if (nrow(duplicates) > 0) {
      print("WARN: duplicate entries for ISO codes:")
      print(duplicates)
    }
    return(frame)
}

# takes a subset of the n most recent years and discards any that
# do not have at least one datapoint in this subset.
preProcess <- function(data, m=20){
    n = names(data);
    dataS <- subset(data, select = n[c(1,(length(n) - m):length(n))]);
    dataP <- preenData(data=dataS, factor=(2.0 / length(names(dataS))), byrow=TRUE, bycol=FALSE);
    dataF <- cbind(dataP, getRecent(dataP[,]))[, c(1,ncol(dataP)+1)];
    return(dataF);
}

# Pick out only the most recent data, e.g rightmost non-NA value for each row
getRecent <- function(data){
    mostRecentVals = matrix(c(NA),nrow=nrow(data),ncol=1)
    for (i in 1:nrow(data)){
        if (any(which(!is.na(data[i,])))){
            rightmostIndex <- max(which(!is.na(data[i,])), na.rm=TRUE)
            mostRecentVals[i,1] <- data[i,rightmostIndex]
        }
        else {mostRecentVals[i,1]=NA}
    }
    return(mostRecentVals)
}

# takes data as a vector and linearly rescales it from -1 to 1
# saturation_level = num of standard deviations. determines
# outliers and sets their values to this number of stdevs.
# quality = TRUE indicates that the high end of the value should be 1,
# and the low end should be -1. FALSE indicates the opposite.
scaleData <- function(data,  quality = TRUE, saturation_level=0){
    mean <- mean(data, na.rm=TRUE)
    stdev <- sd(data, na.rm=TRUE)
    if (saturation_level > 0){
        data[data >= mean+stdev*saturation_level] <- mean + stdev*saturation_level
        data[data <= mean-stdev*saturation_level] <- mean - stdev*saturation_level
    }
    minimum <- min(data, na.rm=TRUE)
    maximum <- max(data, na.rm=TRUE)
    if (quality == FALSE){
        data <- 1 - (data - minimum)/(maximum - minimum)
    }
    else {
        data <- (data - minimum)/(maximum - minimum)
    }
    return(2*data-1)
}


#takes data as a matrix or data frame and removes rows w/ less than factor
#percent of filled data points
preenData <- function(data, factor, bycol = TRUE, byrow =TRUE){
    emptycols <- c()
    if (bycol){
        for (i in 1:ncol(data)){n
            perNotNA <- length(which(!is.na(data[,i])))/nrow(data)
            if (perNotNA <= factor){
                emptycols <- c(emptycols, i)
            }
        }
        if (!is.null(emptycols)) {data <- data[,-emptycols]}
    }
    emptyrows <-c()
    if (byrow){
        for (i in 1:nrow(data)){
           perNotNA <- length(which(!is.na(data[i,])))/ncol(data)
           if (perNotNA <= factor) {
               emptyrows <- c(emptyrows, i)
           }
       }
        if (!is.null(emptyrows)) {data <- data[-emptyrows,]}
    }

    return(data)
}
