#Experiment1: 

final_after2014_gas <- newdataset_after2014_gas

final_after2014_gas <- final_after2014_gas %>% dplyr::select(API,
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

final_after2014_gas[final_after2014_gas == Inf] <- NA
final_after2014_gas <- final_after2014_gas[complete.cases(final_after2014_gas),]

# final_after2014_gas$my_di_secant <- as.nominal(final_after2014_gas$my_di, from.period="year", to.period="month")
# qqplot(final_after2014_gas$my_di_secant, final_after2014_gas$my_qi)

# set.seed(5)
# data.response <- final_after2014_gas[, "my_qi"]
# set.seed(5)
# trainset_indx <- createDataPartition(data.response, p = 0.7, list = FALSE)

API <- read.csv("final_after2014_test_API.CSV")
API$API <- sprintf("%1.f", API$API)
API$API <- ifelse(startsWith(API$API, "5"), paste("0", as.character(API$API), sep = ""), as.character(API$API))




final_after2014_gas_test <- inner_join(final_after2014_gas, API, by = "API")
final_after2014_gas_train <- anti_join(final_after2014_gas, API, by = "API")

set.seed(5)
qi_model <- 
  caret::train( my_qi ~ .,
                data = as.data.frame(final_after2014_gas_train[, !colnames(final_after2014_gas_train) %in% c("API", "my_di")]),
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = NULL,
                trControl = trainControl(method="cv", number=10))


set.seed(5)
di_model <- 
  caret::train( my_di ~ .,
                data = as.data.frame(final_after2014_gas_train[, !colnames(final_after2014_gas_train) %in% c("API", "my_qi")]),
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = NULL,
                trControl = trainControl(method="cv", number=10))

