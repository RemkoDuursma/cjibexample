


# Functie om ruwe parking data om te zetten naar clean data.
clean_parking_raw <- function(data, calibrate = FALSE){
  
  park <- arrange(data, updated) %>%
    filter(!label %in% c("P+R","P4") ) %>%
    mutate(label = as.factor(label),
           updated = as.POSIXct(updated, tz = "UTC"),
           week_time = (wday(updated) - 1) * 24*60 +
             60*(hour(updated)) + minute(updated),
           week_time_15 = floor(week_time / 15))
  
  # Dagelijks minimum aftrekken van de hele dag
  # (Simpele calibratie, klopt niet helemaal maar beter dan niet calibreren)
  park$Date <- as.Date(park$updated)
  
  if(calibrate){
    
    park <- group_by(park, Date, label)
    
    park <- dplyr::group_modify(park, function(x,...){
      
      minval <- min(x$parked, na.rm = TRUE)
      x$parked <- x$parked - minval
      
      return(x)
    }) %>% ungroup
    
  }

  
return(park)
}



# Aggregatie per uur.
aggregate_parking <- function(data){
  
  data$hour <- hour(data$updated)
  
  out <- group_by(data, Date, label, hour) %>%
    summarize(parked = mean(parked)) %>%
    ungroup %>%
    mutate(weekday = wday(Date, abbr=FALSE, label = TRUE),
           weekday = as.integer(factor(weekday, ordered = FALSE)))
  
return(out)  
}

