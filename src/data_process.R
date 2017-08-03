########################################################################
#---------------------  DATA PROCESSING  ------------------------------#
########################################################################

### Load packages
source(file="funcSource.R")

Name = c(
    "gdp_per_capita",
    "gdp_per_capita_ppp",
    "poverty_headcount_1.25USD",
    "infant_mortality",
    "food_consumption",
    "malnutrition",
    "life_expectancy_at_birth",
    "inequality_index",
    "gni_per_capita_ppp"
)

Value = c(
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    TRUE
)

Factor = c(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
)

indicator_config = data.frame(Name, Value, Factor)

# loadIndicator
# s     name string
# f     atennuation factor
# v     normative value
# dir   directory to find indicator data directory
# return data frame
loadIndicator <- function(s, f, v, dir=DATA_DIR) {
    print(paste("Processing indicator:", s))

    # load data
    filename = paste(dir, "indicator_", sep="/")
    filename = paste(filename, s, sep="")
    filename = paste(filename, "Data-Tabla 1.csv", sep="/")
    frame = read.csv(encoding="UTF-8", na.strings="", filename)

    # prepare data
    frame = preProcess(frame)
    frame[,2] = scaleData(frame[,2], quality=v, saturation_level=f)
    colnames(frame)[c(1,2)] = c("country", s)
    frame = addISOCodes(frame, dir=dir)
    frame = frame[,c("ISO",s)] # for now only keep ISO and data
    return(frame)
}

# mergeIndicators
# indicators    collection of strings for indicators
# dataDir
# return        combined data frame
mergeIndicators <- function(indicators, dataDir=DATA_DIR) {
    indicator_frames = list()

    for (i in 1:length(indicators)) {
        index = which(indicator_config$Name == indicators[i])
        if (is.na(index)) {
            print(c("Error, indicator", indicators[i], "not found in config"))
            return()
        }
        name   = toString(indicator_config$Name[index])
        value  = indicator_config$Value[index]
        factor = indicator_config$Factor[index]
        frame  = loadIndicator(s=name, f=factor, v=value, dir=dataDir)
        if (length(frame) == 0) {
            print(c("Error, indicator", indicators[i], "could not load data frame"))
            return()
        }
        indicator_frames = append(indicator_frames, list(frame))
    }

    ### Select inidicators to include in the data set
    selection = indicator_frames

    ### Merge the selected data frames
    combined <- data.frame()
    if (length(selection) != 0) {
        for (i in 1:length(selection)) {
            if (empty(combined)) {
                combined = selection[[i]]
            } else {
                combined = merge(combined, selection[[i]], by="ISO", all=FALSE);
            }
        }
    }
    return(combined)
}