
park <- arrange(parking, updated) %>%
  filter(!label %in% c("P+R","P4") ) %>%
  mutate(label = as.factor(label),
         updated = as.POSIXct(updated, tz = "UTC"))



# Dagelijks minimum aftrekken van de hele dag
park$Date <- as.Date(park$updated)
park_gr <- group_by(park, Date, label)

park_gr <- dplyr::group_modify(park_gr, function(x,...){
  
  minval <- min(x$parked, na.rm = TRUE)
  x$parked <- x$parked - minval
  
  return(x)
}) %>% ungroup

