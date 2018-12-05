

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


for (mon in seq(3, 12, by = 3)) {
  
  for (period in c(TRUE, FALSE)) {
    
    for (res in c(TRUE, FALSE)) {
      
      print(paste(mon, period, res))
      
      executeCombination(features, monthsCount = mon, isAfter = period, isOil = res)
      
    }
    
  }
    
}


combineSheets()

