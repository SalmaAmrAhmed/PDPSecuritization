#Experiment1: 

final_after2014_oil <- newdataset_after2014_oil

final_after2014_oil <- final_after2014_oil %>% dplyr::select(API,
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
                                                             my_qi,
                                                             my_di)

# final_after2014_oil$my_di_secant <- as.nominal(final_after2014_oil$my_di, from.period="year", to.period="month")

final_after2014_oil[final_after2014_oil == Inf] <- NA
final_after2014_oil <- final_after2014_oil[complete.cases(final_after2014_oil),]

# qqplot(final_after2014_oil$my_di_secant, final_after2014_oil$my_qi)




API <- read.csv("final_after2014_test_API.CSV")
API$API <- sprintf("%1.f", API$API)
API$API <- ifelse(startsWith(API$API, "5"), paste("0", as.character(API$API), sep = ""), as.character(API$API))

final_after2014_oil_test <- inner_join(final_after2014_oil, API, by = "API")
final_after2014_oil_train <- anti_join(final_after2014_oil, API, by = "API")

set.seed(5)
qi_model <- 
  caret::train( my_qi ~ .,
                data = as.data.frame(final_after2014_oil_train[, !colnames(final_after2014_oil_train) %in% c("API", "my_di")]),
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = NULL,
                trControl = trainControl(method="cv", number=10))


set.seed(5)
di_model <- 
  caret::train( my_di ~ .,
                data = as.data.frame(final_after2014_oil_train[, !colnames(final_after2014_oil_train) %in% c("API", "my_qi")]),
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = NULL,
                trControl = trainControl(method="cv", number=10))

final_after2014_oil_test$predicted_qi <- predict(qi_model, final_after2014_oil_test)
final_after2014_oil_test$predicted_di <- predict(di_model, final_after2014_oil_test)

final_after2014_oil_test <- inner_join(final_after2014_oil_test, newdataset_after2014_oil[, c("API", "my_b")], by = "API")

sheet_oil <- fillOilSheet_actualprod(neighborsPool_after2014_oil, final_after2014_oil_test)
sheet_arps <- fillOilSheet_arpsprod(neighborsPool_after2014_oil, final_after2014_oil_test, sheet_oil)
sheet_ml <- fillOilSheet_mlprod(neighborsPool_after2014_oil, final_after2014_oil_test, sheet_arps)

sheet_all <- fillOilSheet(sheet_ml, newdataset_after2014_oil, final_after2014_oil_test, 6, neighborsPool_after2014_oil)

# 100 * (1 - mean(abs(final_after2014_oil_test$predicted_qi - final_after2014_oil_test$my_qi) / final_after2014_oil_test$my_qi))
# 100 * (1 - mean(abs(final_after2014_oil_test$predicted_di - final_after2014_oil_test$my_di) / final_after2014_oil_test$my_di))



  
  

