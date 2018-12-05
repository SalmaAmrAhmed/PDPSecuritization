

# Start up a parallel cluster

parallelCluster <- parallel::makeCluster(parallel::detectCores())
print(parallelCluster)

clusterEvalQ(parallelCluster,   for (package in c('ggplot2', 'tvm', 'dplyr','aRpsDCA', 'plyr', 'jsonlite', 'reshape',
                                                  'caret', 'randomForest', 'measurements', 'geosphere', 'Boruta')) {
  
  write(paste0("checking package ",package),  stdout())
  if (!require(package, character.only=T, quietly=T)) {
    write(paste0("installing package ",package),  stdout())
    install.packages(package)
    write(paste0("loading package ",package),  stdout())
    library(package, character.only=T)
  }
  write(paste0("finish package ",package),  stdout())
})


clusterEvalQ(parallelCluster, source('C:/Users/samr/Desktop/PDPSecuritization/Src/Utilities.R'))
clusterEvalQ(parallelCluster, source('C:/Users/samr/Desktop/PDPSecuritization/executeCombination.R'))


features <- c("API",
              "LATERAL_LENGTH_BLEND",
              "GOR_30",
              "GOR_60",
              "GOR_90",
              "LatWGS84",
              "LonWGS84",
              "BottomHoleLatitude",
              "BottomHoleLongitude",
              "my_qi",
              "my_di")

result <- clusterMap(parallelCluster,
                     executeCombination,
                     monthsCount = c(3, 3, 3, 3, 6, 6, 6, 6, 9, 9, 9, 9, 12, 12, 12, 12),
                     isAfter = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
                     isOil = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                     MoreArgs=list(features = features))


combineSheets()



#shutdown cluster ----
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- c()
}