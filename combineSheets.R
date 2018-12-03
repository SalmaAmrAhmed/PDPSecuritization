

combineSheets <- function() {
 
  for (period in c("before2014","after2014")) {
    
    sheet_res <- NA
    
    for (res in c("_oil_", "_gas_")) {
      
      sheet_month <- data.frame()
      
      for (mon in seq(3, 12, by = 3)) {
        
        sheet_final <- read.csv(paste0("sheet_final_", period, res, mon, ".CSV"))
        
        sheet_final$API <- sprintf("%1.f", sheet_final$API)
        sheet_final$API <- ifelse(startsWith(sheet_final$API, "5"),
                                                  paste("0", as.character(sheet_final$API), sep = ""),
                                                  as.character(sheet_final$API))
        
        ##rbind
        
        sheet_month <- rbind(sheet_month, sheet_final)
        
      } 
      
      sheet_month[sheet_month$Forecast_type == "actual", which(colnames(sheet_month) == "FORECAST_NAME")] <- NA
      sheet_month <- distinct(sheet_month)
      
      ##merge
      
      if (is.na(sheet_res)) {
        sheet_res <- sheet_month
      } else {
        sheet_res <- merge(sheet_res, sheet_month)  
      }
      
  
    
    }
  
    write.csv(sheet_res, paste0(period, ".CSV"), row.names = FALSE)
  }
}