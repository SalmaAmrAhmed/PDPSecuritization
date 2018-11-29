#Experiment1: 

final_before2014_oil <- newdataset_before2014_oil

final_before2014_oil <- final_before2014_oil %>% dplyr::select(API,
                                                               LATERAL_LENGTH_BLEND,
                                                               # PROP_PER_FOOT,
                                                               # WATER_PER_FOOT,
                                                               GOR_30,
                                                               GOR_60,
                                                               GOR_90,
                                                               # BasinName,
                                                               # StateName,
                                                               # CountyName,
                                                               LatWGS84,
                                                               LonWGS84,
                                                               BottomHoleLatitude,
                                                               BottomHoleLongitude,
                                                               # CongMeridianName,
                                                               # formation_alias,
                                                               # operator_alias,
                                                               GOR_30_NEIGH,         
                                                               GOR_60_NEIGH,
                                                               GOR_90_NEIGH,
                                                               WATER_PER_FOOT_NEIGH,
                                                               # PROP_PER_FOOT_NEIGH,
                                                               p10rawcurve_seg1,
                                                               p10rawcurve_seg2,
                                                               p10rawcurve_seg3,
                                                               p10rawcurve_seg4,
                                                               p50rawcurve_seg1,
                                                               p50rawcurve_seg2,
                                                               p50rawcurve_seg3,
                                                               p50rawcurve_seg4,
                                                               p90rawcurve_seg1,
                                                               p90rawcurve_seg2,
                                                               p90rawcurve_seg3,
                                                               p90rawcurve_seg4,
                                                               month_1,
                                                               month_2,
                                                               month_3,
                                                               month_4,
                                                               month_5,
                                                               month_6,
                                                               month_7,
                                                               month_8,
                                                               month_9,
                                                               month_10,
                                                               month_11,
                                                               month_12,
                                                               my_qi,
                                                               my_di)

final_before2014_oil[final_before2014_oil == Inf] <- NA
final_before2014_oil <- final_before2014_oil[complete.cases(final_before2014_oil),]

# set.seed(5)
# data.response <- final_before2014_oil[, "my_qi"]
# set.seed(5)
# trainset_indx <- createDataPartition(data.response, p = 0.7, list = FALSE)

API <- read.csv("final_before2014_test_API.CSV")
API$API <- sprintf("%1.f", API$API)
API$API <- ifelse(startsWith(API$API, "5"), paste("0", as.character(API$API), sep = ""), as.character(API$API))

final_before2014_oil_test <- inner_join(final_before2014_oil, API, by = "API")
final_before2014_oil_train <- anti_join(final_before2014_oil, API, by = "API")

set.seed(5)
qi_model <- 
  caret::train( my_qi ~ .,
                data = as.data.frame(final_before2014_oil_train[, !colnames(final_before2014_oil_train) %in% c("API", "my_di")]),
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = NULL,
                trControl = trainControl(method="cv", number=10))


set.seed(5)
di_model <- 
  caret::train( my_di ~ .,
                data = as.data.frame(final_before2014_oil_train[, !colnames(final_before2014_oil_train) %in% c("API", "my_qi")]),
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = NULL,
                trControl = trainControl(method="cv", number=10))

final_before2014_oil_test$predicted_qi <- predict(qi_model, final_before2014_oil_test)
final_before2014_oil_test$predicted_di <- predict(di_model, final_before2014_oil_test)



final_before2014_oil_test <- inner_join(final_before2014_oil_test, newdataset_before2014_oil[, c("API", "my_b")], by = "API")

sheet_oil <- fillOilSheet_actualprod(neighborsPool_before2014_oil, final_before2014_oil_test)
sheet_arps <- fillOilSheet_arpsprod(neighborsPool_before2014_oil, final_before2014_oil_test, sheet_oil, 12)
sheet_ml <- fillOilSheet_mlprod(neighborsPool_before2014_oil, final_before2014_oil_test, sheet_arps)


sheet_all <- fillOilSheet(sheet_ml, newdataset_before2014_oil, final_before2014_oil_test, 12, neighborsPool_before2014_oil)

sheet_final <- fillOilError(sheet_all)