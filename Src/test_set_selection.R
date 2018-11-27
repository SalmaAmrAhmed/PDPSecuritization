clusters <- kmeans(final_after2014_oil[,32:34], 5)
final_after2014_oil$cluster <- clusters$cluster


ggplot(final_after2014_oil, aes(my_di_secant,my_qi, colour=cluster)) +
  geom_point()

set.seed(5)
data.response <- final_after2014_oil[, c("cluster")]
set.seed(5)
trainset_indx <- createDataPartition(data.response, p = 0.7, list = FALSE)

final_after2014_oil_train <- final_after2014_oil[trainset_indx, ]
final_after2014_oil_test <- final_after2014_oil[-trainset_indx, ]

write.csv(final_after2014_oil_test$API, "final_after2014_test_API.CSV", row.names = FALSE)