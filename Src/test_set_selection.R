clusters <- kmeans(final_before2014_oil[,32:34], 5)
final_before2014_oil$cluster <- clusters$cluster


ggplot(final_before2014_oil, aes(my_di_secant,my_qi, colour=cluster)) +
  geom_point()

set.seed(5)
data.response <- final_before2014_oil[, c("cluster")]
set.seed(5)
trainset_indx <- createDataPartition(data.response, p = 0.7, list = FALSE)

final_before2014_oil_train <- final_before2014_oil[trainset_indx, ]
final_before2014_oil_test <- final_before2014_oil[-trainset_indx, ]

write.csv(final_before2014_oil_test$API, "final_before2014_test_API.CSV", row.names = FALSE)