
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


