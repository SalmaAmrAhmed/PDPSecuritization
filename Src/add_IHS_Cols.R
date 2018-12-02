
before2014 <- read.csv("before_2014.CSV")
colnames(before2014)[1] <- "API"
before2014$API <- sprintf("%1.f", before2014$API)
before2014$API <- ifelse(startsWith(before2014$API, "5"), paste("0", as.character(before2014$API), sep = ""), as.character(before2014$API))



after2014 <-read.csv("after_2014.CSV")
colnames(after2014)[1] <- "API"
after2014$API <- sprintf("%1.f", after2014$API)
after2014$API <- ifelse(startsWith(after2014$API, "5"), paste("0", as.character(after2014$API), sep = ""), as.character(after2014$API))



pdpsafter2014 <- distinct(pdpsafter2014)
pdpsbefore2014 <- distinct(pdpsbefore2014)

#add spuddate

after2014 <- inner_join(after2014, pdpsafter2014[, c("API", "SpudDate")], by = "API")
after2014 <- distinct(after2014)
before2014 <- inner_join(before2014, pdpsbefore2014[, c("API", "SpudDate")], by = "API")
before2014 <- distinct(before2014)


#add completiondate

after2014 <- inner_join(after2014, pdpsafter2014[, c("API", "CompletionDate")], by = "API")
after2014 <- distinct(after2014)
before2014 <- inner_join(before2014, pdpsbefore2014[, c("API", "SpudDate")], by = "API")
before2014 <- distinct(before2014)


#add wellname

wellname <- read.csv("well_name.CSV")

colnames(wellname)[1] <- "WELL_NAME"
colnames(wellname)[2] <- "API"
wellname$API <- sprintf("%1.f", wellname$API)
wellname$API <- ifelse(startsWith(wellname$API, "5"), paste("0", as.character(wellname$API), sep = ""), as.character(wellname$API))



after2014 <- inner_join(after2014, wellname[, c("API", "WELL_NAME")], by = "API")
after2014 <- distinct(after2014)
before2014 <- inner_join(before2014, wellname[, c("API", "WELL_NAME")], by = "API")
before2014 <- distinct(before2014)



