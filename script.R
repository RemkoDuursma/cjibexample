library(mongolite)


# Dit duurt even...
db <- mongo(collection = "almereparkingjson",
            url = sprintf(
              "mongodb://%s:%s@%s/%s",
              "remko", 
              "playpass123", 
              "ds229186.mlab.com:29186",
              "almereparking"))

parking <- db$find()


# Of lees de CSV van ooit
parking <- read.csv("almere_parking.csv")



library(randomcoloR)

library(ggplot2)
library(plotly)


library(lubridate)

park <- arrange(parking, updated) %>%
  filter(!label %in% c("P+R","P4") ) %>%
  mutate(label = as.factor(label),
         updated = as.POSIXct(updated, tz = "UTC"))


# Plot van alles
park_sub <- filter(park, 
                   as.Date(updated) > as.Date("2019-10-1"),
                   as.Date(updated) < as.Date("2019-10-8")
                   )
ggplot(park_sub, aes(x = updated, y=parked, col=label)) +
  geom_line() +
  scale_colour_manual(values = randomColor(nlevels(park$label), "blue")) +
  theme_bw()


# Dagelijks minimum aftrekken van de hele dag
park$Date <- as.Date(park$updated)
park_gr <- group_by(park, Date, label)

park_gr <- dplyr::group_modify(park_gr, function(x,...){
  
  minval <- min(x$parked, na.rm = TRUE)
  x$parked <- x$parked - minval
  
return(x)
}) %>% ungroup


# Weer plotten
park_sub <- filter(park_gr, 
                   as.Date(updated) > as.Date("2019-10-1"),
                   as.Date(updated) < as.Date("2019-10-8"))

ggplot(park_gr, aes(x = updated, y=parked, col=label)) +
  geom_line() +
  scale_colour_manual(values = randomColor(nlevels(park$label), "blue")) +
  theme_bw()





# gemiddelde per week dag
park_sub2 <- filter(park_gr, label == "P11")
park_sub2$Date <- as.Date(park_sub2$updated)

library(dplyr)
library(lubridate)
park_sub2 <- group_by(park_sub2, wday(Date, label = TRUE), hour(updated))

p11 <- summarize(park_sub2, parked = mean(parked, na.rm=TRUE))

names(p11) <- c("Weekday", "Uur", "parked")

# ggplot(p11, aes(x = Uur, y = parked)) +
#   facet_grid(~Weekday) +
#   geom_line() +
#   theme_bw()

ggplot(p11, aes(x = Uur, y = parked, col = Weekday)) + geom_line(lwd = 1) + theme_bw()




# Heat map - uur basis
park_ave <- mutate(park_gr, 
                   weekday = wday(Date, abbr=FALSE, label = TRUE),
                   hour = hour(updated)) %>%
  group_by(hour, weekday) %>%
  summarize(parked_max = max(parked),
            parked_mean = mean(parked)) %>%
  ungroup

ggplot(park_ave, aes(x = weekday, y = hour, fill = parked_mean)) +
  geom_tile() +
  scale_fill_viridis_c()


# Gemiddelde wekelijks verloop per parkeerplaats 
park_gr$week_time <- (wday(park_gr$updated) - 1) * 24*60 +
  60*(hour(park_gr$updated)) + minute(park_gr$updated)

park_gr$week_time_15 <- floor(park_gr$week_time / 15)

park_gr_ave <- group_by(park_gr, label, week_time_15) %>%
  summarize(parked = mean(parked))

ggplot(park_gr_ave, aes(x = week_time_15, y = parked)) +
  geom_line() +
  facet_wrap(~label)


# Heatmap : wekelijks verloop vs. seizoensverloop
# Apart per parkeerplaats
park_gr$week <- week(park_gr$updated)
park_gr$week_time_30 <- floor(park_gr$week_time / 30)
park_gr_ave <- group_by(park_gr, label, week, week_time_30) %>%
  summarize(parked = mean(parked))

filter(park_gr_ave, label == "P11") %>%
  ggplot(aes(x = week, y = week_time_30, fill = parked)) +
    geom_tile() +
    scale_fill_viridis_c()




# Kaart
library(readxl)
k <- read_excel("park.xlsx")

library(leaflet)
leaflet(k) %>%
  addMarkers(~lon, ~lat, label = paste(k$label, k$naam)) %>%
  addTiles()



# Samenvatting.
library(lubridate)

# Gemiddeld aantal auto's geparkeerd rond 12 uur op zaterdag.
sat_park <- group_by(park, label) %>%
  filter(hour(updated) == 12, wday(Date, label = TRUE, abbr = FALSE) == "Saturday") %>%
  summarize(parked = mean(parked, na.rm=TRUE))


previewColors(colorNumeric("Reds", domain = NULL), sort(sat_park$parked))

k <- left_join(k, sat_park)
  

leaflet(k) %>%
  addCircleMarkers(~lon, ~lat, label = paste(k$label, k$naam), 
                   stroke = TRUE, weight = 1, col = "black",
                   fillColor = colorNumeric("Reds", domain = NULL)(k$parked),
                   fillOpacity = 1
                   ) %>%
  addTiles()




# Model
park_gr$hour <- hour(park_gr$updated)
park_hr <- group_by(park_gr, Date, label, hour) %>%
  summarize(parked = mean(parked)) %>%
  ungroup %>%
  mutate(weekday = wday(Date, abbr=FALSE, label = TRUE),
         weekday = as.integer(factor(weekday, ordered = FALSE)))

write.csv(park_hr, "park_hourly.csv")

library(randomForest)
model1 <- randomForest(parked ~ hour + label + weekday, data = park_hr)

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


