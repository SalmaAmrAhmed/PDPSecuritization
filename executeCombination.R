


executeCombination <- function(features, monthsCount, isAfter, isOil) {
  
  periodStr <- ifelse(isAfter, "after2014_", "before2014_")
  resourceStr <- ifelse(isOil, "oil.csv", "gas.csv")
  cleanedResourceStr <- ifelse(isOil, "oil", "gas")
  
  
  newdataset_before2014_gas <- read.csv(paste0("output/newdataset_", periodStr, resourceStr))
  
  newdataset_before2014_gas$API <- sprintf("%1.f", newdataset_before2014_gas$API)
  newdataset_before2014_gas$API <- ifelse(startsWith(newdataset_before2014_gas$API, "5"),
                                          paste("0", as.character(newdataset_before2014_gas$API), sep = ""),
                                          as.character(newdataset_before2014_gas$API))
  
  newdataset_before2014_gas$BasinName <- as.character(newdataset_before2014_gas$BasinName)
  newdataset_before2014_gas$StateName <- as.character(newdataset_before2014_gas$StateName)
  newdataset_before2014_gas$CountyName <- as.character(newdataset_before2014_gas$CountyName)
  newdataset_before2014_gas$operator_alias <- as.character(newdataset_before2014_gas$operator_alias)
  
  
  
  
  final_before2014_gas <- newdataset_before2014_gas
  
  
  for (i in seq(1, monthsCount, by = 1)) {
    
    features <- c(features, paste0("month_", eval(i)))
  }
  
  
  final_before2014_gas <- final_before2014_gas[, c(features)]
  

  
  final_before2014_gas[final_before2014_gas == Inf] <- NA
  final_before2014_gas <- final_before2014_gas[complete.cases(final_before2014_gas),]
  
  
  API <- read.csv(paste0("final_", periodStr, "test_API.CSV"))
  
  API$API <- sprintf("%1.f", API$API)
  API$API <- ifelse(startsWith(API$API, "5"), paste("0", as.character(API$API), sep = ""), as.character(API$API))
  
  
  final_before2014_gas_test <- inner_join(final_before2014_gas, API, by = "API")
  final_before2014_gas_train <- anti_join(final_before2014_gas, API, by = "API")
  
  
  set.seed(5)
  qi_model <-
    caret::train( my_qi ~ .,
                  data = as.data.frame(final_before2014_gas_train[, !colnames(final_before2014_gas_train) %in% c("API", "my_di")]),
                  method = "rf",
                  trControl = trainControl(method="none"))


  set.seed(5)
  di_model <-
    caret::train( my_di ~ .,
                  data = as.data.frame(final_before2014_gas_train[, !colnames(final_before2014_gas_train) %in% c("API", "my_qi")]),
                  method = "rf",
                  trControl = trainControl(method="none"))
  
  
  final_before2014_gas_test$predicted_qi <- predict(qi_model, final_before2014_gas_test)
  final_before2014_gas_test$predicted_di <- predict(di_model, final_before2014_gas_test)
  
  # final_before2014_gas_test$predicted_qi <- sample(200:600, nrow(final_before2014_gas_test), replace=TRUE)
  # final_before2014_gas_test$predicted_di <- sample(0.8:0.99, nrow(final_before2014_gas_test), replace=TRUE)
  
  
  final_before2014_gas_test <- inner_join(final_before2014_gas_test, newdataset_before2014_gas[, c("API", "my_b")], by = "API")
  
  production_data <- read.csv(paste0("output/neighborsPool_", periodStr, resourceStr))
  
  production_data$API <- sprintf("%1.f", production_data$API)
  production_data$API <- ifelse(startsWith(production_data$API, "5"), paste("0", as.character(production_data$API), sep = ""), as.character(production_data$API))
  
  if (isOil) {
    
    print("Done with training")
    sheet_gas <- fillOilSheet_actualprod(production_data, final_before2014_gas_test)
    
    print("Done 1")
    sheet_arps <- fillOilSheet_arpsprod(production_data, final_before2014_gas_test, sheet_gas, monthsCount)
    
    print("Done 2")
    sheet_ml <- fillOilSheet_mlprod(production_data, final_before2014_gas_test, sheet_arps)
    
    print("Done 3")
    sheet_all <- fillOilSheet(sheet_ml, newdataset_before2014_gas, final_before2014_gas_test, monthsCount, production_data)
    
    sheet_all[,c(which(colnames(sheet_all) == paste0(cleanedResourceStr, "_month_1")):which(colnames(sheet_all) == paste0(cleanedResourceStr, "_month_60")))] <- sheet_all[,c(which(colnames(sheet_all) == paste0(cleanedResourceStr, "_month_1")):which(colnames(sheet_all) == paste0(cleanedResourceStr, "_month_60")))] * 30
    
    print("Done 4")
    sheet_final <- fillOilError(sheet_all)
    
  } else {
   
    print("Done with training")
    sheet_gas <- fillGasSheet_actualprod(production_data, final_before2014_gas_test)
    
    print("Done 1")
    sheet_arps <- fillGasSheet_arpsprod(production_data, final_before2014_gas_test, sheet_gas, monthsCount)
    
    print("Done 2")
    sheet_ml <- fillGasSheet_mlprod(production_data, final_before2014_gas_test, sheet_arps)
    
    print("Done 3")
    sheet_all <- fillGasSheet(sheet_ml, newdataset_before2014_gas, final_before2014_gas_test, monthsCount, production_data)
    
    sheet_all[,c(which(colnames(sheet_all) == paste0(cleanedResourceStr, "_month_1")):which(colnames(sheet_all) == paste0(cleanedResourceStr, "_month_60")))] <- sheet_all[,c(which(colnames(sheet_all) == paste0(cleanedResourceStr, "_month_1")):which(colnames(sheet_all) == paste0(cleanedResourceStr, "_month_60")))] * 30
    
    print("Done 4")
    sheet_final <- fillGasError(sheet_all) 
    
  }
  
  sheet_final$FORECAST_NAME <- monthsCount
  
  sheet_final[sheet_final == Inf] <- NA
  
  write.csv(sheet_final, paste0("sheet_final_", periodStr, cleanedResourceStr, "_", monthsCount, ".CSV"), row.names = FALSE)
  
}

