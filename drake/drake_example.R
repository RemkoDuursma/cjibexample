
library(drake)

library(dplyr)
library(ggplot2)


# Functies
read_raw_parking_data <- function(file){
  readRDS(file)
}

clean_parking_data <- function(data){
  arrange(data, updated) %>%
    filter(!label %in% c("P+R","P4") ) %>%
    mutate(label = as.factor(label),
           updated = as.POSIXct(updated, tz = "UTC"))

}

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


plan <- drake_plan(
  parking_raw = read_raw_parking_data(file_in("data/parking.rds")),
  parking = clean_parking_data(parking_raw),
  figuur1 = park_timeseries_plot(parking, "2019-10-1", "2019-10-8", title = "Originele data")
)


make(plan)



#
loadd(figuur1)
figuur1

#
vis_drake_graph()


