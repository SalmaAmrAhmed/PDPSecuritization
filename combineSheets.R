
sheet_final_after2014_gas_3 <- read.csv("sheet_final_after2014_gas_3.CSV")
sheet_final_after2014_gas_3$API <- sprintf("%1.f", sheet_final_after2014_gas_3$API)
sheet_final_after2014_gas_3$API <- ifelse(startsWith(sheet_final_after2014_gas_3$API, "5"),
                                       paste("0", as.character(sheet_final_after2014_gas_3$API), sep = ""),
                                       as.character(sheet_final_after2014_gas_3$API))

sheet_final_after2014_gas_6 <- read.csv("sheet_final_after2014_gas_6.CSV")
sheet_final_after2014_gas_6$API <- sprintf("%1.f", sheet_final_after2014_gas_6$API)
sheet_final_after2014_gas_6$API <- ifelse(startsWith(sheet_final_after2014_gas_6$API, "5"),
                                          paste("0", as.character(sheet_final_after2014_gas_6$API), sep = ""),
                                          as.character(sheet_final_after2014_gas_6$API))

sheet_final_after2014_gas_9 <- read.csv("sheet_final_after2014_gas_9.CSV")
sheet_final_after2014_gas_9$API <- sprintf("%1.f", sheet_final_after2014_gas_9$API)
sheet_final_after2014_gas_9$API <- ifelse(startsWith(sheet_final_after2014_gas_9$API, "5"),
                                          paste("0", as.character(sheet_final_after2014_gas_9$API), sep = ""),
                                          as.character(sheet_final_after2014_gas_9$API))

sheet_final_after2014_gas_12 <- read.csv("sheet_final_after2014_gas_12.CSV")
sheet_final_after2014_gas_12$API <- sprintf("%1.f", sheet_final_after2014_gas_12$API)
sheet_final_after2014_gas_12$API <- ifelse(startsWith(sheet_final_after2014_gas_12$API, "5"),
                                          paste("0", as.character(sheet_final_after2014_gas_12$API), sep = ""),
                                          as.character(sheet_final_after2014_gas_12$API))

  

sheet_final_after2014_oil_3 <- read.csv("sheet_final_after2014_oil_3.CSV")
sheet_final_after2014_oil_3$API <- sprintf("%1.f", sheet_final_after2014_oil_3$API)
sheet_final_after2014_oil_3$API <- ifelse(startsWith(sheet_final_after2014_oil_3$API, "5"),
                                          paste("0", as.character(sheet_final_after2014_oil_3$API), sep = ""),
                                          as.character(sheet_final_after2014_oil_3$API))

sheet_final_after2014_oil_6 <- read.csv("sheet_final_after2014_oil_6.CSV")
sheet_final_after2014_oil_6$API <- sprintf("%1.f", sheet_final_after2014_oil_6$API)
sheet_final_after2014_oil_6$API <- ifelse(startsWith(sheet_final_after2014_oil_6$API, "5"),
                                          paste("0", as.character(sheet_final_after2014_oil_6$API), sep = ""),
                                          as.character(sheet_final_after2014_oil_6$API))

sheet_final_after2014_oil_9 <- read.csv("sheet_final_after2014_oil_9.CSV")
sheet_final_after2014_oil_9$API <- sprintf("%1.f", sheet_final_after2014_oil_9$API)
sheet_final_after2014_oil_9$API <- ifelse(startsWith(sheet_final_after2014_oil_9$API, "5"),
                                          paste("0", as.character(sheet_final_after2014_oil_9$API), sep = ""),
                                          as.character(sheet_final_after2014_oil_9$API))

sheet_final_after2014_oil_12 <- read.csv("sheet_final_after2014_oil_12.CSV")
sheet_final_after2014_oil_12$API <- sprintf("%1.f", sheet_final_after2014_oil_12$API)
sheet_final_after2014_oil_12$API <- ifelse(startsWith(sheet_final_after2014_oil_12$API, "5"),
                                           paste("0", as.character(sheet_final_after2014_oil_12$API), sep = ""),
                                           as.character(sheet_final_after2014_oil_12$API))
  
sheet_final_before2014_gas_3 <- read.csv("sheet_final_before2014_gas_3.CSV")
sheet_final_before2014_gas_3$API <- sprintf("%1.f", sheet_final_before2014_gas_3$API)
sheet_final_before2014_gas_3$API <- ifelse(startsWith(sheet_final_before2014_gas_3$API, "5"),
                                          paste("0", as.character(sheet_final_before2014_gas_3$API), sep = ""),
                                          as.character(sheet_final_before2014_gas_3$API))

sheet_final_before2014_gas_6 <- read.csv("sheet_final_before2014_gas_6.CSV")
sheet_final_before2014_gas_6$API <- sprintf("%1.f", sheet_final_before2014_gas_6$API)
sheet_final_before2014_gas_6$API <- ifelse(startsWith(sheet_final_before2014_gas_6$API, "5"),
                                          paste("0", as.character(sheet_final_before2014_gas_6$API), sep = ""),
                                          as.character(sheet_final_before2014_gas_6$API))

sheet_final_before2014_gas_9 <- read.csv("sheet_final_before2014_gas_9.CSV")
sheet_final_before2014_gas_9$API <- sprintf("%1.f", sheet_final_before2014_gas_9$API)
sheet_final_before2014_gas_9$API <- ifelse(startsWith(sheet_final_before2014_gas_9$API, "5"),
                                          paste("0", as.character(sheet_final_before2014_gas_9$API), sep = ""),
                                          as.character(sheet_final_before2014_gas_9$API))

sheet_final_before2014_gas_12 <- read.csv("sheet_final_before2014_gas_12.CSV")
sheet_final_before2014_gas_12$API <- sprintf("%1.f", sheet_final_before2014_gas_12$API)
sheet_final_before2014_gas_12$API <- ifelse(startsWith(sheet_final_before2014_gas_12$API, "5"),
                                           paste("0", as.character(sheet_final_before2014_gas_12$API), sep = ""),
                                           as.character(sheet_final_before2014_gas_12$API))
  
sheet_final_before2014_oil_3 <- read.csv("sheet_final_before2014_oil_3.CSV")
sheet_final_before2014_oil_3$API <- sprintf("%1.f", sheet_final_before2014_oil_3$API)
sheet_final_before2014_oil_3$API <- ifelse(startsWith(sheet_final_before2014_oil_3$API, "5"),
                                          paste("0", as.character(sheet_final_before2014_oil_3$API), sep = ""),
                                          as.character(sheet_final_before2014_oil_3$API))

sheet_final_before2014_oil_6 <- read.csv("sheet_final_before2014_oil_6.CSV")
sheet_final_before2014_oil_6$API <- sprintf("%1.f", sheet_final_before2014_oil_6$API)
sheet_final_before2014_oil_6$API <- ifelse(startsWith(sheet_final_before2014_oil_6$API, "5"),
                                          paste("0", as.character(sheet_final_before2014_oil_6$API), sep = ""),
                                          as.character(sheet_final_before2014_oil_6$API))

sheet_final_before2014_oil_9 <- read.csv("sheet_final_before2014_oil_9.CSV")
sheet_final_before2014_oil_9$API <- sprintf("%1.f", sheet_final_before2014_oil_9$API)
sheet_final_before2014_oil_9$API <- ifelse(startsWith(sheet_final_before2014_oil_9$API, "5"),
                                          paste("0", as.character(sheet_final_before2014_oil_9$API), sep = ""),
                                          as.character(sheet_final_before2014_oil_9$API))

sheet_final_before2014_oil_12 <- read.csv("sheet_final_before2014_oil_12.CSV")
sheet_final_before2014_oil_12$API <- sprintf("%1.f", sheet_final_before2014_oil_12$API)
sheet_final_before2014_oil_12$API <- ifelse(startsWith(sheet_final_before2014_oil_12$API, "5"),
                                           paste("0", as.character(sheet_final_before2014_oil_12$API), sep = ""),
                                           as.character(sheet_final_before2014_oil_12$API))
  

gas_after <- rbind(sheet_final_after2014_gas_3,
                   sheet_final_after2014_gas_6,
                   sheet_final_after2014_gas_9,
                   sheet_final_after2014_gas_12)

oil_after <- rbind(sheet_final_after2014_oil_3,
                   sheet_final_after2014_oil_6,
                   sheet_final_after2014_oil_9,
                   sheet_final_after2014_oil_12)

gas_before <- rbind(sheet_final_before2014_gas_3,
                   sheet_final_before2014_gas_6,
                   sheet_final_before2014_gas_9,
                   sheet_final_before2014_gas_12)

oil_before <- rbind(sheet_final_before2014_oil_3,
                   sheet_final_before2014_oil_6,
                   sheet_final_before2014_oil_9,
                   sheet_final_before2014_oil_12)

  
  