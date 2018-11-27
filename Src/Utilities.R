oadLibraries <- function() {
  
  write(paste0("start loading libraries from ", .libPaths()), stdout())
  
  #.libPaths(c(.libPaths(), libraryPath))
  
  
  options(repos=structure(c(CRAN="https://cran.cnr.berkeley.edu/")))
  
  for (package in c('ggplot2', 'tvm', 'dplyr','aRpsDCA', 'plyr', 'jsonlite', 'reshape', 'RODBC',
                    'caret', 'randomForest', 'measurements', 'geosphere', 'Boruta')) {
    
    write(paste0("checking package ",package),  stdout())
    if (!require(package, character.only=T, quietly=T)) {
      write(paste0("installing package ",package),  stdout())
      install.packages(package)
      write(paste0("loading package ",package),  stdout())
      library(package, character.only=T)
    }
    write(paste0("finish package ",package),  stdout())
  }
  write("end loading libraries", stdout())
}






visualise <- function(dataset) {
  
  
  map <- 
    leaflet(dataset) %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
    
    addCircles(~dataset$LonWGS84, 
               ~dataset$LatWGS84,
               color = "blue")
  
  
  return(map)
  
}


normalize_operator <- function(complete_data, operators) {
  
  complete_data$operator_alias <- NA
  
  seq <- seq(1, nrow(complete_data), by = 1)
  
  for (i in seq) {
    
    print(i)
    
    if (complete_data$OPERATOR[i] %in% operators$CURRENTOPERATORNAME) {
      
      alias <- operators %>% dplyr::filter(operators$CURRENTOPERATORNAME == complete_data$OPERATOR[i]) %>% dplyr::select(OperatorAlias)
      alias <- alias$OperatorAlias
      
      if (length(alias) >= 1 && !is.na(alias)) {
        
        complete_data$operator_alias[i] <- alias
        
      } else {
        complete_data$operator_alias[i] <- complete_data$OPERATOR[i]
        
      }
    } else {
      
      complete_data$operator_alias[i] <- complete_data$OPERATOR[i]
      
    }
    
  }
  
  return (complete_data)
  
}




normalize_formation <- function(complete_data, formations) {
  
  complete_data$formation_alias <- NA
  
  seq <- seq(1, nrow(complete_data), by = 1)
  
  for (i in seq) {
    
    if (complete_data$formation[i] %in% formations$Formation) {
      
      alias <- formations %>% dplyr::filter(formations$Formation == complete_data$formation[i]) %>% dplyr::select(Alias)
      
      complete_data$formation_alias[i] <- alias$Alias
      
    } else {
      
      complete_data$formation_alias[i] <- complete_data$formation[i]
      
    }
    
  }
  
  return (complete_data)
  
}


getSameOperator <- function(dataset, operator) {
  
  api_oper <- dataset %>% dplyr::filter(operator_alias == operator) %>% dplyr::select(API, operator_alias)
  
  api_oper$API <- as.character(api_oper[['API']])
  
  return(api_oper)
}

getSameFormation <- function(dataset, formation) {
  
  api_formation <- dataset %>% dplyr::filter(formation_alias == formation) %>% dplyr::select(API, formation_alias)
  
  api_formation$API <- as.character(api_formation[['API']])
  
  return(api_formation)
}


getClosest_modified <- function(dataset, indx) {
  
  dist_diff = conv_unit(distGeo(c(dataset[["LonWGS84"]][indx], dataset[["LatWGS84"]][indx]),
                                dataset[, c("LonWGS84", "LatWGS84")]), "m", "ft")
  
  api_dist <- data.frame(dist_diff)
  api_dist$API <- as.character(dataset[['API']])
  
  return(api_dist)
}


# getSimilarGeo_modified <- function(dataset, geo_diff_threshold, indx) {
#   
#   bvhh_diff = abs(dataset[["BVHH"]][indx] - dataset[["BVHH"]]) / dataset[["BVHH"]][indx]
#   net_diff = abs(dataset[["NET"]][indx] - dataset[["NET"]]) / dataset[["NET"]][indx]
#   phia_diff = abs(dataset[["PHIA"]][indx] - dataset[["PHIA"]]) / dataset[["PHIA"]][indx]
#   swa_diff = abs(dataset[["SWA"]][indx] - dataset[["SWA"]]) / dataset[["SWA"]][indx]
#   
#   api_geo <- data.frame(bvhh_diff, net_diff, phia_diff, swa_diff)
#   api_geo$API <- as.character(dataset[['API']])
#   
#   api_geo_1 <- dplyr::filter(api_geo, (bvhh_diff > 0 & abs(api_geo$bvhh_diff) <= geo_diff_threshold))
#   
#   api_geo_2 <- dplyr::filter(api_geo, (net_diff > 0 & abs(api_geo$net_diff) <= geo_diff_threshold &
#                                          phia_diff > 0 & abs(api_geo$phia_diff) <= geo_diff_threshold &
#                                          swa_diff > 0 & abs(api_geo$swa_diff) <= geo_diff_threshold))
#   
#   if (dim(api_geo_1)[1] < dim(api_geo_2)[1])
#     return(api_geo_2)
#   else
#     return(api_geo_1)
#   
# }



getNeighbors_modified <- function(dataset, paramsList) {
  
  dataseq <- seq(1, dim(dataset)[1], by = 1)
  APINeighList <- list()
  
  dataset_neighbors <- dataset
  dataset_neighbors$OIL_CUM_3MOS_NEIGH<- NA
  dataset_neighbors$OIL_CUM_6MOS_NEIGH <- NA
  dataset_neighbors$OIL_CUM_12MOS_NEIGH <- NA
  
  # dataset_neighbors$OIL_EUR_from_neighbors <- NA
  
  dataset_neighbors$case <- NA
  dataset_neighbors$neighbors_count <- NA
  
  dataset_neighbors$GOR_30_NEIGH <- NA
  dataset_neighbors$GOR_60_NEIGH <- NA
  dataset_neighbors$GOR_90_NEIGH <- NA
  
  dataset_neighbors$WATER_PER_FOOT_NEIGH <- NA
  dataset_neighbors$PROP_PER_FOOT_NEIGH <- NA
  
  for (i in dataseq) {
    
    print(i)
    
    
    api_oper <- getSameOperator(dataset, dataset[i, "operator_alias"])
    
    api_formation <- getSameFormation(dataset, dataset[i, "formation_alias"])
    
    tmp <- getClosest_modified(dataset, i)
    api_dist <- dplyr::filter(tmp, dist_diff > 0 & dist_diff <= paramsList$distance_threshold)
    api_dist_2 <- dplyr::filter(tmp, dist_diff > 0 & dist_diff <= (paramsList$distance_threshold * 2)) 
    api_dist_4 <- dplyr::filter(tmp, dist_diff > 0 & dist_diff <= (paramsList$distance_threshold * 4))
    
    #case - 1: Most restrictive conditions [same operator + distance_threshold + lateral_length + same geo] ---- 
    api_neighbors_1 <- inner_join(inner_join(api_oper, api_dist, by = "API"), api_formation, by = "API")
    
    if (dim(api_neighbors_1)[1] >= 10) 
    {  
      api_neighbors_data <- inner_join(dataset, api_neighbors_1, by = "API")
      APINeighList[[dataset[["API"]][i]]] <- api_neighbors_1[["API"]]
      
      dataset_neighbors[["OIL_CUM_3MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_3MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_6MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_6MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_12MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_12MOS"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["case"]][i] <- 1
      dataset_neighbors[["neighbors_count"]][i] <- nrow(api_neighbors_data)
      
      
      dataset_neighbors[["GOR_30_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_30"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_60_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_60"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_90_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_90"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["WATER_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["WATER_PER_FOOT"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["PROP_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["PROP_PER_FOOT"]]) / nrow(api_neighbors_data)
      
      
      next;
      
    }
    
    #case - 2: Increase radius * 2 + rest of conditions ----
    api_neighbors_2 <- inner_join(inner_join(api_oper, api_dist_2, by = "API"), api_formation, by = "API")
    
    if (dim(api_neighbors_2)[1] >= 10) {
      
      api_neighbors_data <- inner_join(dataset, api_neighbors_2, by = "API")
      APINeighList[[dataset[["API"]][i]]] <- api_neighbors_2[["API"]]
      
      dataset_neighbors[["OIL_CUM_3MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_3MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_6MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_6MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_12MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_12MOS"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["case"]][i] <- 2
      
      dataset_neighbors[["neighbors_count"]][i] <-nrow(api_neighbors_data)
      
      dataset_neighbors[["GOR_30_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_30"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_60_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_60"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_90_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_90"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["WATER_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["WATER_PER_FOOT"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["PROP_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["PROP_PER_FOOT"]]) / nrow(api_neighbors_data)
      
      next;
      
    }
    
    #case - 3: Increase radius * 4 + rest of conditions ----
    api_neighbors_3 <- inner_join(inner_join(api_oper, api_dist_4, by = "API"), api_formation, by = "API")
    
    if (dim(api_neighbors_3)[1] >= 10) {
      
      api_neighbors_data <- inner_join(dataset, api_neighbors_3, by = "API")
      APINeighList[[dataset[["API"]][i]]] <- api_neighbors_3[, "API"]
      dataset_neighbors[["OIL_CUM_3MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_3MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_6MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_6MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_12MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_12MOS"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["case"]][i] <- 3
      
      dataset_neighbors[["neighbors_count"]][i] <-nrow(api_neighbors_data)   
      
      dataset_neighbors[["GOR_30_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_30"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_60_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_60"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_90_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_90"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["WATER_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["WATER_PER_FOOT"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["PROP_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["PROP_PER_FOOT"]]) / nrow(api_neighbors_data)
      
      next;
      
    }
    
    #case - 4: other operators at same radius ----
    api_neighbors_4 <- inner_join(api_formation, api_dist, by = "API")
    
    if (dim(api_neighbors_4)[1] >= 10) {
      
      api_neighbors_data <- inner_join(dataset, api_neighbors_4, by = "API")
      APINeighList[[dataset[["API"]][i]]] <- api_neighbors_4[["API"]]
      dataset_neighbors[["OIL_CUM_3MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_3MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_6MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_6MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_12MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_12MOS"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["case"]][i] <- 4
      
      dataset_neighbors[["neighbors_count"]][i] <-nrow(api_neighbors_data)
      
      dataset_neighbors[["GOR_30_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_30"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_60_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_60"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_90_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_90"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["WATER_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["WATER_PER_FOOT"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["PROP_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["PROP_PER_FOOT"]]) / nrow(api_neighbors_data)
      
      next;
      
    }
    
    #case - 5: other operators at 2 * radius ----
    api_neighbors_5 <- inner_join(api_formation, api_dist_2, by = "API")
    
    if (dim(api_neighbors_5)[1] >= 10) {
      
      api_neighbors_data <- inner_join(dataset, api_neighbors_5, by = "API")
      APINeighList[[dataset[["API"]][i]]] <- api_neighbors_5[, "API"]
      dataset_neighbors[["OIL_CUM_3MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_3MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_6MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_6MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_12MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_12MOS"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["case"]][i] <- 5
      
      dataset_neighbors[["neighbors_count"]][i] <-nrow(api_neighbors_data)
      
      dataset_neighbors[["GOR_30_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_30"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_60_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_60"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_90_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_90"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["WATER_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["WATER_PER_FOOT"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["PROP_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["PROP_PER_FOOT"]]) / nrow(api_neighbors_data)
      
      next;
      
    }
    
    #case - 6: other operators  + 4 * radius ----
    api_neighbors_6 <- inner_join(api_formation, api_dist_4, by = "API")
    
    if (dim(api_neighbors_6)[1] >= 10) {
      
      api_neighbors_data <- inner_join(dataset, api_neighbors_6, by = "API")
      
      APINeighList[[dataset[["API"]][i]]] <- api_neighbors_6[, "API"]
      dataset_neighbors[["OIL_CUM_3MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_3MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_6MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_6MOS"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["OIL_CUM_12MOS_NEIGH"]][i] <- sum(api_neighbors_data[["OIL_CUM_12MOS"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["case"]][i] <- 6
      
      dataset_neighbors[["neighbors_count"]][i] <-nrow(api_neighbors_data)
      
      dataset_neighbors[["GOR_30_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_30"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_60_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_60"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["GOR_90_NEIGH"]][i] <- sum(api_neighbors_data[["GOR_90"]]) / nrow(api_neighbors_data)
      
      dataset_neighbors[["WATER_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["WATER_PER_FOOT"]]) / nrow(api_neighbors_data)
      dataset_neighbors[["PROP_PER_FOOT_NEIGH"]][i] <- sum(api_neighbors_data[["PROP_PER_FOOT"]]) / nrow(api_neighbors_data)
      
      next;
      
    }
  }
  
  return(list("neighborsInfoFrame" = dataset_neighbors,
              "neighborsList" = APINeighList))
}


get_Production_data_from_IHS <- function(datatable) {
  
  drivername <- "SQL Server"
  dbname <- "IHS"
  servername <- "RAISASQL"
  username <-"appuser"
  pwd <- "appuser"
  
  ##Use a full connection string to connect to a SAMPLE database
  context <- paste("DRIVER=",drivername,
                   ";Database=",dbname,
                   ";Server=",servername,
                   ";PROTOCOL=TCPIP",
                   ";UID=", username,
                   ";PWD=",pwd,sep="")
  
  cn <- odbcDriverConnect(context)
  
  myString = paste("'", datatable$API[1], "'", sep = "")
  for(i in 2:length(datatable$API)) {
    myString = paste(myString, paste("'", datatable$API[i], "'", sep = ""), sep = ", " )
  }
  
  query_res <- sqlQuery(cn, paste("select T6._HeaderId, T6._LastUpdate, T6.Year, T6.Month, T6.Liquid, T6.Gas, T6.Water, T6.RatioGasOil, T6.PercentWater, T6.Wells, T6.DaysOn, T5.API from ProdnMonthlyProduction T6 inner join
                                  (select T3.Entity, T3._Id, T4.API from ProdnHeaders T3 inner join (select T1.Entity, T1.API from ProdnWells T1
                                  where T1.API IN (", myString, ")) T4 on T4.Entity = T3.Entity) T5 on T6._HeaderId = T5._Id", sep = " "))
  odbcClose(cn)
  colnames(query_res)[1] <- "id"
  query_res$API <- sprintf("%1.f", query_res$API)
  query_res$API <- ifelse(startsWith(query_res$API, "5"), paste("0", as.character(query_res$API), sep = ""), as.character(query_res$API))
  return(query_res)
  
  
}


#get the maximum point of production in the curve
getInitialPointOfProduction <- function(wellRow, LiquidType) {
  
  if (LiquidType == "oil") {
    maxProductionDate <- wellRow %>%
      dplyr::filter(Liquid == max(wellRow$Liquid)) %>%
      dplyr::select(date)
  } else if (LiquidType == "gas") {
    maxProductionDate <- wellRow %>%
      dplyr::filter(Gas == max(wellRow$Gas)) %>%
      dplyr::select(date)
    
  } else if (LiquidType == "water") {
    maxProductionDate <- wellRow %>%
      dplyr::filter(Water == max(wellRow$Water)) %>%
      dplyr::select(date)
    
  }
  return(maxProductionDate$date[1])
}



#remove all production before the maximum production
removeProdBefore <- function(wellRow, mindate) {
  
  seqq <- seq(1, nrow(wellRow), by = 1)
  for (k in seqq) {
    if(wellRow[k, "date"] < mindate) {
      wellRow[k, "date"] <- NA
    }
  }
  wellRow <- wellRow[complete.cases(wellRow), ]
  return(wellRow)
}



#give an index for each month of production, to shift the curves such that they overlap
indexCols <- function(wellRow) {
  
  monthseq <- seq(1, length(wellRow$Month), by = 1)
  wellRow$monthIndx <- monthseq
  
  return(wellRow)
}


#processing IHS data ----
processRawIHSData <- function(query10, include_zeroes, LiquidType) {
  
  if (query10 %>% length() == 0) {
    stop("Input data frame is empty")
  }
  
  if (dim(query10)[1] > 0 && dim(query10)[2] == 12) {
    if ("id" %in% names(query10) && "Liquid" %in% names(query10) && "Gas" %in% names(query10) && "Water" %in% names(query10) &&
        "Month" %in% names(query10) && "Year" %in% names(query10) && "API" %in% names(query10)) {
      
      query10 <- query10[complete.cases(query10), ]
      if (NA %in% query10$Month || NA %in% query10$Year ) {
        stop("Wrong Month/Year format")
      }
      
      query10 <- query10 %>% dplyr::mutate(date = paste(Year, Month, "1", sep = "/"))
      query10$date <- as.Date(as.character(query10$date))
      prodafter2014 <- query10
      
      myList <- split(prodafter2014, as.factor(prodafter2014$API))
      myseq <- seq(1, length(myList), by = 1)
      dateList <- list()
      
      for(i in myseq) {
        maxProductionDate <- getInitialPointOfProduction(myList[[i]], LiquidType)
        dateList[[names(myList)[i]]] <- maxProductionDate
      }
      
      mydateframe <- do.call(rbind, lapply(dateList, data.frame, stringsAsFactors=FALSE))
      colnames(mydateframe)[1] <- "date"
      # mydateframe <- subset(mydateframe, mydateframe$date >= "2014-01-01")
      
      if (nrow(mydateframe) == 0) {
        stop("No sufficient data after 2014")
      }
      
      prodafter2014 <- prodafter2014 %>% dplyr::filter(API %in% rownames(mydateframe))
      myList <- split(prodafter2014, as.factor(prodafter2014$API))
      myseq2 <- seq(1, nrow(mydateframe), by = 1)
      
      #data frame holds the production data after max point of production
      prodafter2014filtered <- data.frame()
      
      for(j in myseq2) {
        data <- prodafter2014 %>% dplyr::filter(API == rownames(mydateframe)[j])
        filtered <- removeProdBefore(data,  mydateframe[j,1])
        prodafter2014filtered <- rbind(prodafter2014filtered, filtered)
      }
      
      #remove all zeroes from frame
      if (!include_zeroes) {
        
        if (LiquidType == "oil") {
          prodafter2014filtered$Liquid[prodafter2014filtered$Liquid == 0] <- NA
        }
        
        else if (LiquidType == "gas") {
          prodafter2014filtered$Gas[prodafter2014filtered$Gas == 0] <- NA
        }
        
        else if (LiquidType == "water") {
          prodafter2014filtered$Water[prodafter2014filtered$Water == 0] <- NA
        }
        
      }
      
      prodafter2014filtered <- prodafter2014filtered[complete.cases(prodafter2014filtered),]
      
      filteredList <- split(prodafter2014filtered, as.factor(prodafter2014filtered$API))
      myseq3 <- seq(1, length(filteredList), by = 1)
      prodafter2014indxed <- data.frame()
      
      for(k in myseq3) {
        data <- prodafter2014filtered %>% dplyr::filter(API == names(filteredList)[k])
        data <- data[order(data$date),]
        prodafter2014indxed <- rbind(prodafter2014indxed, indexCols(data))
      }
      
      return(prodafter2014indxed)
      
    } else {
      
      stop("Missing Columns")
    }
  } else {
    stop("Input data frame of wrong format")
  }
}

grid_start_Liquid_optimize_on_fixed_b <- function(Liquid, monthIndx, b) {
  
  start <- c(Liquid[1] ,
             -((Liquid[2]  - Liquid[1] ) / Liquid[1] ) / (monthIndx[2] - monthIndx[1]),
             b,
             0.006047558)
  
  q <- Liquid 
  t <- (monthIndx) - 0.5
  
  
  objectivefunarps <- function(guess) sse(q,
                                          hyp2exp.q(
                                            guess[1],
                                            guess[2],
                                            b,
                                            guess[4],
                                            t))
  
  res <- nlminb(start, objectivefunarps)
  res$sse_manual <- sse(q, hyp2exp.q(res$par[1], 
                                     res$par[2],
                                     b,
                                     res$par[4], t))
  return(res)
}



computeCurveParamForSet <- function(dataframe, b, LiquidType) {
  
  prodafter2014indxed <- dataframe
  
  # wellsIndexedList <- split(prodafter2014indxed, as.factor(prodafter2014indxed$API))
  # wellSeq <- seq(2, length(wellsIndexedList), by = 1)
  # 
  # wells_list <- list()
  # 
  # myplot <- ggplot() +
  #   geom_line(aes(wellsIndexedList[[1]]$monthIndx, wellsIndexedList[[1]]$Liquid), size = 1, colour="grey56")
  # 
  # for (i in wellSeq) {
  #   
  #   myplot <- myplot +
  #     geom_line(aes_string(wellsIndexedList[[i]]$monthIndx, wellsIndexedList[[i]]$Liquid), size = 1, colour = "grey56")
  #   
  #   wells_list[[names(wellsIndexedList)[i]]] <- list("M" = length(wellsIndexedList[[i]]$monthIndx), "V" = as.numeric(wellsIndexedList[[i]]$Liquid))
  #   
  # }
  
  indxedList <- split(prodafter2014indxed, as.factor(prodafter2014indxed$monthIndx))
  myseq4 <- seq(1, length(indxedList), by = 1)
  # p50_finaldata <- list()
  p10_finaldata <- list()
  p90_finaldata <- list()
  
  p50_finaldata_median <- list()
  
  for(m in myseq4) {
    
    #p90
    if (LiquidType == "oil") {
      list <- indxedList[[m]]$Liquid
    } else if (LiquidType == "gas") {
      list <- indxedList[[m]]$Gas
      
    } else if (LiquidType == "water") {
      list <- indxedList[[m]]$Water
    }
    
    list[is.na(list)] <- 0
    sorted <- sort(list, decreasing = FALSE)
    n = ((10/100) * (length(sorted) - 1)) + 1
    k = as.integer(n)
    d = n - k
    vk <- sorted[k]
    vk_1 <- sorted[k + 1]
    offset <- (d * (vk_1 - vk))
    p90_finaldata[[names(indxedList)[m]]] <- vk + ifelse(is.na(offset), 0, offset)
    
    
    #p50
    if (LiquidType == "oil") {
      list <- indxedList[[m]]$Liquid
    } else if (LiquidType == "gas") {
      list <- indxedList[[m]]$Gas
      
    } else if (LiquidType == "water") {
      list <- indxedList[[m]]$Water
    }
    list[is.na(list)] <- 0
    sorted <- sort(list, decreasing = FALSE)
    n = ((50/100) * (length(sorted) - 1)) + 1
    k = as.integer(n)
    d = n - k
    vk <- sorted[k]
    vk_1 <- sorted[k + 1]
    offset <- (d * (vk_1 - vk))
    p50_finaldata_median[[names(indxedList)[m]]] <- vk + ifelse(is.na(offset), 0, offset)
    
    
    
    #p10
    if (LiquidType == "oil") {
      list <- indxedList[[m]]$Liquid
    } else if (LiquidType == "gas") {
      list <- indxedList[[m]]$Gas
      
    } else if (LiquidType == "water") {
      list <- indxedList[[m]]$Water
    }
    list[is.na(list)] <- 0
    sorted <- sort(list, decreasing = FALSE)
    n = ((90/100) * (length(sorted) - 1)) + 1
    k = as.integer(n)
    d = n - k
    vk <- sorted[k]
    vk_1 <- sorted[k + 1]
    offset <- (d * (vk_1 - vk))
    p10_finaldata[[names(indxedList)[m]]] <- vk + ifelse(is.na(offset), 0, offset)
    
    
  }
  
  
  #p50 medain ----
  p50myframe_median <- data.frame(Liquid = unlist(p50_finaldata_median), monthIndx = as.numeric(names(p50_finaldata_median)))
  p50myframe_median <- p50myframe_median[complete.cases(p50myframe_median),]
  p50myframe_median_smoothed <- list()
  p50myframe_median_smoothed <- p50myframe_median$Liquid
  # p50_paramslist_Liquid <- grid_start_Liquid(p50myframe_median_smoothed, p50myframe_median$monthIndx, b)
  # p50_paramslist_Liquid_optimize_on_fixed_b <- grid_start_Liquid_optimize_on_fixed_b(p50myframe_median_smoothed, p50myframe_median$monthIndx, b)
  
  
  #p10 ----
  p10myframe <- data.frame(Liquid = unlist(p10_finaldata), monthIndx = as.numeric(names(p10_finaldata)))
  p10myframe <- p10myframe[complete.cases(p10myframe),]
  p10myframe_smoothed <- list()
  p10myframe_smoothed <- p10myframe$Liquid
  # p10_paramslist_Liquid <- grid_start_Liquid(p10myframe_smoothed, p10myframe$monthIndx, b)
  # p10_paramslist_Liquid_optimize_on_fixed_b <- grid_start_Liquid_optimize_on_fixed_b(p10myframe_smoothed, p10myframe$monthIndx, b)
  
  
  #p90 ----
  p90myframe <- data.frame(Liquid = unlist(p90_finaldata), monthIndx = as.numeric(names(p90_finaldata)))
  p90myframe <- p90myframe[complete.cases(p90myframe),]
  p90myframe_smoothed <- list()
  p90myframe_smoothed <- p90myframe$Liquid
  # p90_paramslist_Liquid <- grid_start_Liquid(p90myframe_smoothed, p90myframe$monthIndx, b)
  # p90_paramslist_Liquid_optimize_on_fixed_b <- grid_start_Liquid_optimize_on_fixed_b(p90myframe_smoothed, p90myframe$monthIndx, b)
  
  
  #bind in a list ----
  dataList <- list()
  
  dataList[["p50rawframe"]] = p50myframe_median
  # dataList[["p50_paramslist_Liquid_optimize_on_fixed_b"]] = p50_paramslist_Liquid_optimize_on_fixed_b
  
  dataList[["p10rawframe"]] = p10myframe
  # dataList[["p10_paramslist_Liquid_optimize_on_fixed_b"]] = p10_paramslist_Liquid_optimize_on_fixed_b
  
  dataList[["p90rawframe"]] = p90myframe
  # dataList[["p90_paramslist_Liquid_optimize_on_fixed_b"]] = p90_paramslist_Liquid_optimize_on_fixed_b
  
  # dataList[["wells_list"]] = wells_list
  
  return(dataList)
  
}

curveParamsEstimation <- function(wellneighborsList, neighborsPool, include_zeroes, welldata, LiquidType) {
  
  seq <- seq(1, length(wellneighborsList), by = 1)
  
  if (!include_zeroes) {
    
    if (LiquidType == "oil") {
      neighborsPool$Liquid[neighborsPool$Liquid == 0] <- NA
      neighborsPool <- neighborsPool[complete.cases(neighborsPool),]
      
    } else if (LiquidType == "gas") {
      neighborsPool$Gas[neighborsPool$Gas == 0] <- NA
      neighborsPool <- neighborsPool[complete.cases(neighborsPool),]
      
    } else if (LiquidType == "water") {
      neighborsPool$Water[neighborsPool$Water == 0] <- NA
      neighborsPool <- neighborsPool[complete.cases(neighborsPool),]
    }
    
  } else {
    
    if (LiquidType == "oil") {
      neighborsPool$Liquid[neighborsPool$Liquid == 0] <- NA
      
    } else if (LiquidType == "gas") {
      neighborsPool$Gas[neighborsPool$Gas == 0] <- NA
      
    } else if (LiquidType == "water") {
      neighborsPool$Water[neighborsPool$Water == 0] <- NA
    }
  }
  
  
  neighborsPool$monthIndx <- neighborsPool$monthIndx - 0.5
  
  allcurveParamsInfoList <- list()
  
  for(i in seq) {
    
    
    print(i)
    neighbors <- wellneighborsList[[names(wellneighborsList[i])]]
    
    size <- length(neighbors)
    
    #if 1 neighbor ->> ignore
    if (length(neighbors) > 1) {
      
      smallseq <- seq(1, length(neighbors), by = 1)
      smallFrame <- data.frame()
      curveParamsList <- list()
      # neighborCountList <- list()
      
      for(j in smallseq) {
        data <- neighborsPool %>% dplyr::filter(API == neighbors[j]) 
        
        if(nrow(data) != 0) {
          smallFrame <- rbind(smallFrame, data)
        } else {
          size <- size - 1
        }
        
      }
      
      data <- neighborsPool %>% dplyr::filter(API == names(wellneighborsList)[i])
      
      if (length(unique(smallFrame$API)) >= 3) {
        
        basin <- welldata %>% dplyr::filter(API == names(wellneighborsList)[i]) %>% dplyr::select(BASIN)
        b <- 1.3
        
        if (basin$BASIN == "DENVER BASIN")
          b <- 1.1
        
        
        
        listOut <- computeCurveParamForSet(smallFrame, b, LiquidType)
        
        if (!is.null(listOut)) {
          
          # data$Liquid <- (data$Liquid / data$LATERAL_LENGTH_BLEND) * 9000
          
          data$Liquid[is.na(data$Liquid)] <- 0
          
          params <- try(grid_start_Liquid_optimize_on_fixed_b(data$Liquid,
                                                              data$monthIndx,
                                                              b))
          
          if(inherits(params, "try-error"))
          {
            
            next
          }
          
          
          
          curveParamsList[["neighborsCount_IHS"]] <- size
          curveParamsList <- c(listOut, curveParamsList)
          
          curveParamsList[["my_qi"]] <- params$par[1]
          # curveParamsList[["neigh_qi"]] <- listOut$p50_paramslist_Liquid_optimize_on_fixed_b$par[1]
          
          curveParamsList[["my_di"]] <- as.effective(params$par[2], "month", "year")
          # curveParamsList[["neigh_di"]] <- as.effective(listOut$p50_paramslist_Liquid_optimize_on_fixed_b$par[2], "month", "year")
          
          curveParamsList[["my_b"]] <- b
          # curveParamsList[["neigh_b"]] <- b
          
          
          curveParamsList[["my_EUR"]] <- calculateEUR(qi = params$par[1],
                                                      di_effective = as.effective(params$par[2], "month", "year"),
                                                      b = b,
                                                      df_effective = 0.07)
          
          
          # curveParamsList[["p50_EUR"]] <- calculateEUR(qi = listOut$p50_paramslist_Liquid_optimize_on_fixed_b$par[1],
          #                                              di_effective = as.effective(listOut$p50_paramslist_Liquid_optimize_on_fixed_b$par[2], from.period = "month", to.period = "year"),
          #                                              b = b,
          #                                              df_effective = 0.07)
          # 
          # 
          # 
          # curveParamsList[["p10_EUR"]] <- calculateEUR(qi = listOut$p10_paramslist_Liquid_optimize_on_fixed_b$par[1],
          #                                              di_effective = as.effective(listOut$p10_paramslist_Liquid_optimize_on_fixed_b$par[2], from.period = "month", to.period = "year"),
          #                                              b = b,
          #                                              df_effective = 0.07)
          # 
          # curveParamsList[["p90_EUR"]] <- calculateEUR(qi = listOut$p90_paramslist_Liquid_optimize_on_fixed_b$par[1],
          #                                              di_effective = as.effective(listOut$p90_paramslist_Liquid_optimize_on_fixed_b$par[2], from.period = "month", to.period = "year"),
          #                                              b = b,
          #                                              df_effective = 0.07)
          
          
        }
        
      }
      
      
      if(length(curveParamsList) != 0) {
        allcurveParamsInfoList[[names(wellneighborsList)[i]]] <- curveParamsList
      }
      
      
    }
    
    
  }
  return (allcurveParamsInfoList)
}


#remove un-necessary wells from the lists that don't exist in the processed neighborspool frame
filterWellsNeighborsList <- function(wellneighborsList, neighborsPool) {
  
  wellneighborsList_filetered <- list()
  
  seq <- seq(1, length(wellneighborsList), by = 1)
  for(i in seq) {
    
    if (names(wellneighborsList)[i] %in% neighborsPool$API) {
      wellneighborsList_filetered <- c(wellneighborsList_filetered, wellneighborsList[i])
    }
    
  }
  
  return(wellneighborsList_filetered)
}


calculateEUR <- function(qi, di_effective, b, df_effective) {
  
  
  dec <- arps.decline(qi, as.nominal(di_effective, from.period="year", to.period="day"), b,
                      as.nominal(df_effective, from.period="year", to.period="day"))
  eur <- arps.eur(dec, 3) / 1000
  
  return(eur)
  
}

sse <- function (q, forecast) 
  
{
  sum((q - forecast)^2)
}


setNewFeatures <- function(dataset, curveParamsEstimationList, neighborsPool) {
  
  new_dataset <- dataset
  
  new_dataset$neigh_count <- NA
  # new_dataset$neigh_qi <- NA
  new_dataset$my_qi <- NA
  
  # new_dataset$neigh_di <- NA
  new_dataset$my_di <- NA
  
  # new_dataset$neigh_b <- NA
  new_dataset$my_b <- NA
  
  
  new_dataset$my_EUR <- NA
  
  # new_dataset$p10_EUR <- NA
  # new_dataset$p90_EUR <- NA
  # new_dataset$p50_EUR <- NA
  
  new_dataset$p10rawcurve_seg1 <- NA
  new_dataset$p10rawcurve_seg2 <- NA
  new_dataset$p10rawcurve_seg3 <- NA
  new_dataset$p10rawcurve_seg4 <- NA
  
  new_dataset$p50rawcurve_seg1 <- NA
  new_dataset$p50rawcurve_seg2 <- NA
  new_dataset$p50rawcurve_seg3 <- NA
  new_dataset$p50rawcurve_seg4 <- NA
  
  new_dataset$p90rawcurve_seg1 <- NA
  new_dataset$p90rawcurve_seg2 <- NA
  new_dataset$p90rawcurve_seg3 <- NA
  new_dataset$p90rawcurve_seg4 <- NA
  
  # new_dataset$myrawcurve_seg1 <- NA
  # new_dataset$myrawcurve_seg2 <- NA
  # new_dataset$myrawcurve_seg3 <- NA
  # new_dataset$myrawcurve_seg4 <- NA
  
  
  
  neigh_API_list <- list()
  
  
  trainsetseq <- seq(1, nrow(new_dataset), by = 1)
  
  for (i in trainsetseq) {
    
    print(i)
    
    my_qi <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$my_qi)
    # neigh_qi <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$neigh_qi)
    neigh_count <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$neighborsCount_IHS)
    
    my_di <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$my_di)
    # neigh_di <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$neigh_di)
    
    my_b <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$my_b)
    # neigh_b <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$neigh_b)
    
    my_EUR <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$my_EUR)
    # p50_EUR <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$p50_EUR)
    
    # neigh_API_list[[new_dataset$API[i]]] <- curveParamsEstimationList[[new_dataset$API[i]]]$neigh_API_list
    
    # p10_EUR <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$p10_EUR)
    # p90_EUR <- as.numeric(curveParamsEstimationList[[new_dataset$API[i]]]$p90_EUR)
    
    data <- neighborsPool %>% dplyr::filter(API == new_dataset$API[i])
    
    my_rawcurve_length <- length(data$Liquid)
    remainder <- my_rawcurve_length %% 4
    total <- my_rawcurve_length -  remainder
    seg_capacity <- total / 4
    end_point <- seg_capacity + remainder
    my_rawcurve_seg1 <- sum(data$Liquid[1:seg_capacity])
    my_rawcurve_seg2 <- sum(data$Liquid[(seg_capacity + 1):(seg_capacity * 2)])
    my_rawcurve_seg3 <- sum(data$Liquid[((seg_capacity * 2) + 1):(seg_capacity * 3)])
    my_rawcurve_seg4 <- sum(data$Liquid[((seg_capacity * 3) + 1):end_point])
    
    if (length(data) >= 12) {
      
      monthseq <- seq(1, 12, by = 1)
      
      for (j in monthseq) {
        
        columnname <- paste0("month_", eval(j))
        
        new_dataset[i, columnname] <- data$Liquid[j]
        
      }
      
    }
    
    
    
    
    p10rawcurve_length <- length(curveParamsEstimationList[[new_dataset$API[i]]]$p10rawframe$Liquid)
    remainder <- p10rawcurve_length %% 4
    total <- p10rawcurve_length -  remainder
    seg_capacity <- total / 4
    end_point <- seg_capacity + remainder
    p10rawcurve_seg1 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p10rawframe$Liquid[1:seg_capacity])
    p10rawcurve_seg2 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p10rawframe$Liquid[(seg_capacity + 1):(seg_capacity * 2)])
    p10rawcurve_seg3 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p10rawframe$Liquid[((seg_capacity * 2) + 1):(seg_capacity * 3)])
    p10rawcurve_seg4 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p10rawframe$Liquid[((seg_capacity * 3) + 1):end_point])
    
    
    p50rawcurve_length <- length(curveParamsEstimationList[[new_dataset$API[i]]]$p50rawframe$Liquid)
    remainder <- p50rawcurve_length %% 4
    total <- p50rawcurve_length -  remainder
    seg_capacity <- total / 4
    end_point <- seg_capacity + remainder
    p50rawcurve_seg1 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p50rawframe$Liquid[1:seg_capacity])
    p50rawcurve_seg2 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p50rawframe$Liquid[(seg_capacity + 1):(seg_capacity * 2)])
    p50rawcurve_seg3 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p50rawframe$Liquid[((seg_capacity * 2) + 1):(seg_capacity * 3)])
    p50rawcurve_seg4 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p50rawframe$Liquid[((seg_capacity * 3) + 1):end_point])
    
    
    p90rawcurve_length <- length(curveParamsEstimationList[[new_dataset$API[i]]]$p90rawframe$Liquid)
    remainder <- p90rawcurve_length %% 4
    total <- p90rawcurve_length -  remainder
    seg_capacity <- total / 4
    end_point <- seg_capacity + remainder
    p90rawcurve_seg1 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p90rawframe$Liquid[1:seg_capacity])
    p90rawcurve_seg2 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p90rawframe$Liquid[(seg_capacity + 1):(seg_capacity * 2)])
    p90rawcurve_seg3 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p90rawframe$Liquid[((seg_capacity * 2) + 1):(seg_capacity * 3)])
    p90rawcurve_seg4 <- sum(curveParamsEstimationList[[new_dataset$API[i]]]$p90rawframe$Liquid[((seg_capacity * 3) + 1):end_point])
    
    
    if (
      length(my_qi) != 0 &
      # length(neigh_qi) != 0 & 
      length(neigh_count) != 0 &
      length(my_di) != 0 &
      # length(neigh_di) != 0 &
      length(my_b) != 0 &
      # length(neigh_b) != 0 &
      # length(p50_EUR) != 0 &
      # length(p10_EUR) != 0 &
      # length(p90_EUR) != 0 &
      length(p10rawcurve_seg1) != 0 &
      length(p10rawcurve_seg2) != 0 &
      length(p10rawcurve_seg3) != 0 &
      length(p10rawcurve_seg4) != 0 &
      
      length(p50rawcurve_seg1) != 0 &
      length(p50rawcurve_seg2) != 0 &
      length(p50rawcurve_seg3) != 0 &
      length(p50rawcurve_seg4) != 0 &
      
      length(p90rawcurve_seg1) != 0 &
      length(p90rawcurve_seg2) != 0 &
      length(p90rawcurve_seg3) != 0 &
      length(p90rawcurve_seg4) != 0 &
      
      length(my_rawcurve_seg1) != 0 &
      length(my_rawcurve_seg2) != 0 &
      length(my_rawcurve_seg3) != 0 &
      length(my_rawcurve_seg4) != 0 &
      
      length(my_EUR) != 0
    ) {
      
      # new_dataset$neigh_qi[i] <- neigh_qi
      new_dataset$my_qi[i] <- my_qi
      new_dataset$neigh_count[i] <- neigh_count
      
      # new_dataset$neigh_di[i] <- neigh_di
      new_dataset$my_di[i] <- my_di
      
      # new_dataset$neigh_b[i] <- neigh_b
      new_dataset$my_b[i] <- my_b
      
      # new_dataset$p50_EUR[i] <- p50_EUR
      new_dataset$my_EUR[i] <- my_EUR
      
      # new_dataset$p10_EUR[i] <- p10_EUR
      # new_dataset$p90_EUR[i] <- p90_EUR
      
      
      new_dataset$p10rawcurve_seg1[i] <- p10rawcurve_seg1
      new_dataset$p10rawcurve_seg2[i] <- p10rawcurve_seg2
      new_dataset$p10rawcurve_seg3[i] <- p10rawcurve_seg3
      new_dataset$p10rawcurve_seg4[i] <- p10rawcurve_seg4
      
      
      new_dataset$p50rawcurve_seg1[i] <- p50rawcurve_seg1
      new_dataset$p50rawcurve_seg2[i] <- p50rawcurve_seg2
      new_dataset$p50rawcurve_seg3[i] <- p50rawcurve_seg3
      new_dataset$p50rawcurve_seg4[i] <- p50rawcurve_seg4
      
      
      new_dataset$p90rawcurve_seg1[i] <- p90rawcurve_seg1
      new_dataset$p90rawcurve_seg2[i] <- p90rawcurve_seg2
      new_dataset$p90rawcurve_seg3[i] <- p90rawcurve_seg3
      new_dataset$p90rawcurve_seg4[i] <- p90rawcurve_seg4
      
      new_dataset$my_rawcurve_seg1[i] <- my_rawcurve_seg1
      new_dataset$my_rawcurve_seg2[i] <- my_rawcurve_seg2
      new_dataset$my_rawcurve_seg3[i] <- my_rawcurve_seg3
      new_dataset$my_rawcurve_seg4[i] <- my_rawcurve_seg4
      
    }
    # if (length(neigh_count) != 0 & length(neigh_oil_arps) != 0) {
    #   
    #   new_dataset$neigh_count[i] <- neigh_count
    #   new_dataset$neigh_oil_arps[i] <- neigh_oil_arps}
    
  }
  
  # new_dataset <- new_dataset[complete.cases(new_dataset), ]
  return(new_dataset)
}
