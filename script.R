library(mongolite)


# Dit duurt even
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
park_sub2 <- filter(park_gr, label == "P5")
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

ggplot(p11, aes(x = Uur, y = parked, col = Weekday)) +
  geom_line(lwd = 1) +
  theme_bw()





