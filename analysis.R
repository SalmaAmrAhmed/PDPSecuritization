
pdpsbefore2014 <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\data\\PDP_Securitization_2010-2014.CSV")

pdpsafter2014 <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\data\\PDP_Securitization_2014+.CSV")

basinNames <- c("DENVER BASIN", "WILLISTON BASIN", "ANADARKO BASIN", "PERMIAN BASIN")

pdpsafter2014$BasinName <- as.character(pdpsafter2014$BasinName)
pdpsbefore2014$BasinName <- as.character(pdpsbefore2014$BasinName)

pdpsafter2014 <- pdpsafter2014 %>% dplyr::filter(BasinName %in% basinNames)
pdpsbefore2014 <- pdpsbefore2014 %>% dplyr::filter(BasinName %in% basinNames)

colnames(pdpsafter2014)[1] <- "API"
pdpsafter2014$API <- sprintf("%1.f", pdpsafter2014$API)
pdpsafter2014$API <- ifelse(startsWith(pdpsafter2014$API, "5"), paste("0", as.character(pdpsafter2014$API), sep = ""), as.character(pdpsafter2014$API))



colnames(pdpsbefore2014)[1] <- "API"
pdpsbefore2014$API <- sprintf("%1.f", pdpsbefore2014$API)
pdpsbefore2014$API <- ifelse(startsWith(pdpsbefore2014$API, "5"), paste("0", as.character(pdpsbefore2014$API), sep = ""), as.character(pdpsbefore2014$API))


pdpsafter2014 <- pdpsafter2014 %>% dplyr::select(API,
                                                 formation,
                                                 BasinName,
                                                 StateName,
                                                 CountyName,
                                                 LatWGS84,
                                                 LonWGS84,
                                                 BottomHoleLatitude,
                                                 BottomHoleLongitude,
                                                 CongMeridianName,
                                                 operatorNameIHS)

pdpsbefore2014 <- pdpsbefore2014 %>% dplyr::select(API,
                                                 formation,
                                                 BasinName,
                                                 StateName,
                                                 CountyName,
                                                 LatWGS84,
                                                 LonWGS84,
                                                 BottomHoleLatitude,
                                                 BottomHoleLongitude,
                                                 CongMeridianName,
                                                 operatorNameIHS)
pdpsafter2014 <- unique(pdpsafter2014)
pdpsbefore2014 <- unique(pdpsbefore2014)

pdpsafter2014[pdpsafter2014 == "NULL"] <- NA
pdpsbefore2014[pdpsbefore2014 == "NULL"] <- NA

#visualize
working_directory <- getwd()

map1 <- visualise(pdpsafter2014)
title <- "after2014"
saveWidget(map1,
           file = paste(working_directory, "figs", paste(title, "map.html", sep = "_"), sep = "/"),
           selfcontained = TRUE)


map2 <- visualise(pdpsbefore2014)
title <- "before2014"
saveWidget(map2,
           file = paste(working_directory, "figs", paste(title, "map.html", sep = "_"), sep = "/"),
           selfcontained = TRUE)


#completecases
pdpsafter2014_compl <- pdpsafter2014[complete.cases(pdpsafter2014),]

pdpsbefore2014_compl <- pdpsbefore2014[complete.cases(pdpsbefore2014),]


map1 <- visualise(pdpsafter2014_compl)
title <- "after2014compl"
saveWidget(map1,
           file = paste(working_directory, "figs", paste(title, "map.html", sep = "_"), sep = "/"),
           selfcontained = TRUE)


map2 <- visualise(pdpsbefore2014_compl)
title <- "before2014compl"
saveWidget(map2,
           file = paste(working_directory, "figs", paste(title, "map.html", sep = "_"), sep = "/"),
           selfcontained = TRUE)



#normalize formation
formations <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\data\\formationAlias.CSV")
formations$Formation <- as.character(formations$Formation)
formations$Alias <- as.character(formations$Alias)

pdpsafter2014_compl$formation <- as.character(pdpsafter2014_compl$formation)
pdpsbefore2014_compl$formation <- as.character(pdpsbefore2014_compl$formation)

pdpsafter2014_compl_norm <- normalize_formation(pdpsafter2014_compl, formations)
pdpsbefore2014_compl_norm <- normalize_formation(pdpsbefore2014_compl, formations)



#normalize operator
operators <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\data\\Operator Alias Master List 20181105.CSV")
operators$CURRENTOPERATORNAME <- as.character(operators$CURRENTOPERATORNAME)
operators$OperatorAlias <- as.character(operators$OperatorAlias)
operators[operators == ""] <- NA

colnames(pdpsafter2014_compl_norm)[11] <- "OPERATOR"
colnames(pdpsbefore2014_compl_norm)[11] <- "OPERATOR"

  
pdpsafter2014_compl_norm$OPERATOR <- as.character(pdpsafter2014_compl_norm$OPERATOR)
pdpsbefore2014_compl_norm$OPERATOR <- as.character(pdpsbefore2014_compl_norm$OPERATOR)

pdpsafter2014_compl_norm <- normalize_operator(pdpsafter2014_compl_norm, operators)
pdpsbefore2014_compl_norm <- normalize_operator(pdpsbefore2014_compl_norm,operators)


#import Harmony features

harmony_after2014 <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\data\\Harmony_features_after2014.CSV")
harmony_before2014 <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\data\\Harmony_features_before2014.CSV")

colnames(harmony_after2014)[1] <- "API"
colnames(harmony_before2014)[1] <- "API"


harmony_after2014$API <- sprintf("%1.f", harmony_after2014$API)
harmony_after2014$API <- ifelse(startsWith(harmony_after2014$API, "5"),
                                paste("0", as.character(harmony_after2014$API), sep = ""),
                                as.character(harmony_after2014$API))

harmony_before2014$API <- sprintf("%1.f", harmony_before2014$API)
harmony_before2014$API <- ifelse(startsWith(harmony_before2014$API, "5"),
                                paste("0", as.character(harmony_before2014$API), sep = ""),
                                as.character(harmony_before2014$API))

harmony_before2014[harmony_before2014 == "NULL"] <- NA
harmony_after2014[harmony_after2014 == "NULL"] <- NA

harmony_before2014 <- harmony_before2014[complete.cases(harmony_before2014),]
harmony_after2014 <- harmony_after2014[complete.cases(harmony_after2014),]

all_data_before2014 <- inner_join(harmony_before2014, pdpsbefore2014_compl_norm, by = "API")
all_data_after2014 <- inner_join(harmony_after2014, pdpsafter2014_compl_norm, by = "API")



all_data_before2014$formation <- NULL
all_data_before2014$OPERATOR <- NULL

all_data_after2014$formation <- NULL
all_data_after2014$OPERATOR <- NULL


numeric_index <- c(3, 4, 5, 6, 7, 8, 9, 10, 16, 17)
all_data_after2014[numeric_index] <- lapply(all_data_after2014[numeric_index], function(x) as.numeric(as.character(x)))
all_data_before2014[numeric_index] <- lapply(all_data_before2014[numeric_index], function(x) as.numeric(as.character(x)))


char_index <- c(12, 13, 18)
all_data_after2014[char_index] <- lapply(all_data_after2014[char_index], function(x) as.character(x))
all_data_before2014[char_index] <- lapply(all_data_before2014[char_index], function(x) as.character(x))



paramsList <- list("distance_threshold" = 2500,
                   "lateral_length_threshold" = 1000,
                   "geo_diff_threshold" = 0.15,
                   "proppant_per_length_threshold" = 0.15)


resList_before2014 <- getNeighbors_modified(all_data_before2014, paramsList)
resList_before2014$neighborsInfoFrame <- resList_before2014$neighborsInfoFrame[complete.cases(resList_before2014$neighborsInfoFrame),]


resList_after2014 <- getNeighbors_modified(all_data_after2014, paramsList)
resList_after2014$neighborsInfoFrame <- resList_after2014$neighborsInfoFrame[complete.cases(resList_after2014$neighborsInfoFrame),]

#run on TS----
# neighborsPool_after2014 <- get_Production_data_from_IHS(resList_after2014$neighborsInfoFrame)
# neighborsPool_before2014 <- get_Production_data_from_IHS(resList_before2014$neighborsInfoFrame)

neighborsPool_after2014 <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\output\\neighborsPool_after2014.CSV")
neighborsPool_before2014 <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\output\\neighborsPool_before2014.CSV")

neighborsPool_after2014$API <- sprintf("%1.f", neighborsPool_after2014$API)
neighborsPool_after2014$API <- ifelse(startsWith(neighborsPool_after2014$API, "5"),
                                paste("0", as.character(neighborsPool_after2014$API), sep = ""),
                                as.character(neighborsPool_after2014$API))


neighborsPool_before2014$API <- sprintf("%1.f", neighborsPool_before2014$API)
neighborsPool_before2014$API <- ifelse(startsWith(neighborsPool_before2014$API, "5"),
                                      paste("0", as.character(neighborsPool_before2014$API), sep = ""),
                                      as.character(neighborsPool_before2014$API))


#one time run
# neighborsPool_after2014_oil <- processRawIHSData(neighborsPool_after2014, TRUE, "oil")
# neighborsPool_before2014_oil <- processRawIHSData(neighborsPool_before2014, TRUE, "oil")
# 
# neighborsPool_after2014_gas <- processRawIHSData(neighborsPool_after2014, TRUE, "gas")
# neighborsPool_before2014_gas <- processRawIHSData(neighborsPool_before2014, TRUE, "gas")


neighborsPool_after2014_oil <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\output\\neighborsPool_after2014_oil.CSV")
neighborsPool_before2014_oil <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\output\\neighborsPool_before2014_oil.CSV")

neighborsPool_after2014_gas <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\output\\neighborsPool_after2014_gas.CSV")
neighborsPool_before2014_gas <- read.csv("C:\\Users\\pdvsupport\\Documents\\Raisa\\EURPrediction\\PDPSecuritization\\output\\neighborsPool_before2014_gas.CSV")

neighborsPool_after2014_oil$API <- sprintf("%1.f", neighborsPool_after2014_oil$API)
neighborsPool_after2014_oil$API <- ifelse(startsWith(neighborsPool_after2014_oil$API, "5"),
                                      paste("0", as.character(neighborsPool_after2014_oil$API), sep = ""),
                                      as.character(neighborsPool_after2014_oil$API))


neighborsPool_before2014_oil$API <- sprintf("%1.f", neighborsPool_before2014_oil$API)
neighborsPool_before2014_oil$API <- ifelse(startsWith(neighborsPool_before2014_oil$API, "5"),
                                       paste("0", as.character(neighborsPool_before2014_oil$API), sep = ""),
                                       as.character(neighborsPool_before2014_oil$API))

neighborsPool_after2014_gas$API <- sprintf("%1.f", neighborsPool_after2014_gas$API)
neighborsPool_after2014_gas$API <- ifelse(startsWith(neighborsPool_after2014_gas$API, "5"),
                                          paste("0", as.character(neighborsPool_after2014_gas$API), sep = ""),
                                          as.character(neighborsPool_after2014_gas$API))


neighborsPool_before2014_gas$API <- sprintf("%1.f", neighborsPool_before2014_gas$API)
neighborsPool_before2014_gas$API <- ifelse(startsWith(neighborsPool_before2014_gas$API, "5"),
                                           paste("0", as.character(neighborsPool_before2014_gas$API), sep = ""),
                                           as.character(neighborsPool_before2014_gas$API))


neighborsPool_after2014_oil$Liquid <- neighborsPool_after2014_oil$Liquid / 30
neighborsPool_before2014_oil$Liquid <- neighborsPool_before2014_oil$Liquid / 30

neighborsPool_after2014_gas$Gas <- neighborsPool_after2014_gas$Gas / 30
neighborsPool_before2014_gas$Gas <- neighborsPool_before2014_gas$Gas / 30


filteredNeighList_after2014_oil <- filterWellsNeighborsList(resList_after2014$neighborsList, neighborsPool_after2014_oil)
filteredNeighList_after2014_gas <- filterWellsNeighborsList(resList_after2014$neighborsList, neighborsPool_after2014_gas)

filteredNeighList_before2014_oil <- filterWellsNeighborsList(resList_before2014$neighborsList, neighborsPool_before2014_oil)
filteredNeighList_before2014_gas <- filterWellsNeighborsList(resList_before2014$neighborsList, neighborsPool_before2014_gas)



colnames(all_data_after2014)[11] <- "BASIN"
colnames(all_data_before2014)[11] <- "BASIN"

curveParamsEstimationList_after2014_oil <- curveParamsEstimation(filteredNeighList_after2014_oil,
                                                                 neighborsPool_after2014_oil,
                                                                 TRUE,
                                                                 all_data_after2014,
                                                                 "oil")

curveParamsEstimationList_before2014_oil <- curveParamsEstimation(filteredNeighList_before2014_oil,
                                                                 neighborsPool_before2014_oil,
                                                                 TRUE,
                                                                 all_data_before2014,
                                                                 "oil")

curveParamsEstimationList_before2014_gas <- curveParamsEstimation(filteredNeighList_before2014_gas,
                                                                  neighborsPool_before2014_gas,
                                                                  TRUE,
                                                                  all_data_before2014,
                                                                  "gas")

curveParamsEstimationList_after2014_gas <- curveParamsEstimation(filteredNeighList_after2014_gas,
                                                                  neighborsPool_after2014_gas,
                                                                  TRUE,
                                                                  all_data_after2014,
                                                                  "gas")


newdataset_before2014_oil <- setNewFeatures(resList_before2014$neighborsInfoFrame, curveParamsEstimationList_before2014_oil, neighborsPool_before2014_oil)
newdataset_before2014_gas <- setNewFeatures(resList_before2014$neighborsInfoFrame, curveParamsEstimationList_before2014_gas, neighborsPool_before2014_gas)

newdataset_after2014_oil <- setNewFeatures(resList_after2014$neighborsInfoFrame, curveParamsEstimationList_after2014_oil, neighborsPool_after2014_oil)
newdataset_after2014_gas <- setNewFeatures(resList_after2014$neighborsInfoFrame, curveParamsEstimationList_after2014_gas, neighborsPool_after2014_gas)



