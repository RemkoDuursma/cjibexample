
# Kleine aanpassingen
park <- arrange(parking, updated) %>%
  filter(!label %in% c("P+R","P4") ) %>%
  mutate(label = as.factor(label),
         updated = as.POSIXct(updated, tz = "UTC"))



# Dagelijks minimum aftrekken van de hele dag
# (Simpele calibratie, klopt niet helemaal maar beter dan niet calibreren)
park$Date <- as.Date(park$updated)
park_gr <- group_by(park, Date, label)

park_gr <- dplyr::group_modify(park_gr, function(x,...){
  
  minval <- min(x$parked, na.rm = TRUE)
  x$parked <- x$parked - minval
  
  return(x)
}) %>% ungroup


# Aggregatie per uur.
park_gr$hour <- hour(park_gr$updated)
park_hr <- group_by(park_gr, Date, label, hour) %>%
  summarize(parked = mean(parked)) %>%
  ungroup %>%
  mutate(weekday = wday(Date, abbr=FALSE, label = TRUE),
         weekday = as.integer(factor(weekday, ordered = FALSE)))

