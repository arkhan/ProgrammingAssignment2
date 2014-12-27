pollutantmean <- function(directory, pollutant, id = 1:332){
    data = lapply(id, function(i) read.csv(paste(getwd(),"/",directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = ""))[[pollutant]])
    return(mean(unlist(data), na.rm = TRUE))
}
