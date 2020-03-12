
park_timeseries_plot <- function(data, begin, end, title = ""){
  
  
  dplyr::filter(data, 
                as.Date(updated) > as.Date(begin),
                as.Date(updated) < as.Date(end)
  ) %>% 
    ggplot(aes(x = updated, y=parked, col=label)) +
    geom_line() +
    scale_colour_manual(values = randomColor(nlevels(park$label), "blue")) +
    theme_bw() +
    labs(title = title)
  
}


day_week_overlay_plot <- function(garage, data, title = garage){

  filter(data, label == garage) %>% 
    mutate(Date = as.Date(updated)) %>% 
    group_by(wday(Date, label = TRUE), hour(updated)) %>% 
    summarize(parked = mean(parked, na.rm=TRUE)) %>%
    setNames(., c("Weekday", "Uur", "parked")) %>%
  
  ggplot(aes(x = Uur, y = parked, col = Weekday)) + 
    geom_line(lwd = 1) + 
    theme_bw() +
    labs(title = title)

}




park_heatmap_hourly <- function(data){
  
  mutate(data, 
         weekday = wday(Date, abbr=FALSE, label = TRUE),
         hour = hour(updated)) %>%
    group_by(hour, weekday) %>%
    summarize(parked_max = max(parked),
              parked_mean = mean(parked)) %>%
    ungroup %>%
  
  ggplot(aes(x = weekday, y = hour, fill = parked_mean)) +
    geom_tile() +
    scale_fill_viridis_c()
  
}



weekly_timeseries_plot <- function(data){
  
  mutate(data,
         week_time = (wday(updated) - 1) * 24*60 +
           60*(hour(updated)) + minute(updated),
         week_time_15 = floor(week_time / 15)) %>% 
    group_by(label, week_time_15) %>%
    summarize(parked = mean(parked)) %>%
    
    ggplot(aes(x = week_time_15, y = parked)) +
    geom_line() +
    facet_wrap(~label)
  
}



park_heatmap_weekly_seasonal <- function(garage, data){

  mutate(data, 
         week = week(updated),
         week_time_30 = floor(week_time / 30)) %>% 
    group_by(label, week, week_time_30) %>%
    summarize(parked = mean(parked)) %>%
  filter(label == garage) %>%
    ggplot(aes(x = week, y = week_time_30, fill = parked)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = garage)
  
}

# Kaart van parkeer plaatsen
map_parking_locations <- function(data){
  
  leaflet(data) %>%
    addMarkers(~lon, ~lat, label = paste(data$label, data$naam)) %>%
    addTiles()
  
}




# Gemiddeld aantal auto's geparkeerd rond 12 uur op zaterdag.
map_parking_timefilter <- function(data, 
                                   hr = 12, 
                                   day = "Saturday", 
                                   map_locations = parking_map){

  
  map_data <- group_by(data, label) %>%
    filter(hour(updated) == hr, 
           wday(Date, label = TRUE, abbr = FALSE) == day) %>%
    summarize(parked = mean(parked, na.rm=TRUE)) %>%
    left_join(parking_map, by = "label")
  
  cols <- colorNumeric("Reds", domain = NULL)(map_data$parked)
  
  leaflet(map_locations) %>%
    addCircleMarkers(~lon, ~lat, 
                     label = paste(map_data$label, map_data$naam), 
                     stroke = TRUE, 
                     weight = 1, 
                     col = "black",
                     fillColor = cols,
                     fillOpacity = 1
    ) %>%
    addTiles()
  
  
  
}







