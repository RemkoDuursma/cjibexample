
# Packages laden
source("R/load_packages.R")

# Functies laden
source("R/functions_figures.R")

# output, cache folders aanmaken als ze nog niet bestaan
source("R/make_dirs.R")

# Configuratie
.conf <- yaml::read_yaml("conf/config.yml")

# 1. Data inlezen
# --> parking, parking_map
source("R/read_data.R")


# 2. Data opschonen
# --> park
# --> park_gr
# --> park_hr
source("R/clean_and_prepare_data.R")


# Tijd series plotjes
park_timeseries_plot(park, "2019-10-1", "2019-10-8", 
                     title = "Originele data")
park_timeseries_plot(park_gr, "2019-10-1", "2019-10-8",
                     title = "Gecalibreerde data")


# gemiddelde per week dag
day_week_overlay_plot("P11", park_gr)



# Heat map - uur basis
park_heatmap_hourly(park_gr)


# Gemiddelde wekelijks verloop per parkeerplaats 
weekly_timeseries_plot(park_gr)


# Heatmap : wekelijks verloop vs. seizoensverloop
# Apart per parkeerplaats
park_heatmap_weekly_seasonal("P11", park_gr)



# Kaart
map_parking_locations(parking_map)

# Kaart, gekleurd naar aantal auto's
map_parking_timefilter(park_gr, hr = 12, day = "Saturday")



# Model
system.time({
  model1 <- randomForest(parked ~ hour + label + weekday, data = park_hr)
})

# summary
model1

library(randomForestExplainer)
plot_predict_interaction(model1, park_hr, "weekday", "hour")


# Maak een voorspelling voor het huidige uur
predict(model1, newdata = data.frame(hour = hour(Sys.time()),
                                     weekday = wday(Sys.time()),
                                     label = unique(park_hr$label)))


# Een ander model
library(mgcv)

data <- subset(park_gr, label == "P7")

with(data, plot(week_time, parked, pch="."))

model2 <- gam(parked ~ s(week_time, k=50), data = data)

visreg(model2)

# Voorspelling: nu
wt <- (wday(Sys.time()) - 1) * 24*60 +
  60*(hour(Sys.time())) + minute(Sys.time())

predict(model2, newdata = data.frame(week_time = wt))
points(wt, 9, pch=19,col="red")


