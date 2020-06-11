

# Tijd series plotjes
park_timeseries_plot(park, "2019-10-1", "2019-10-8", 
                     title = "Originele data") %>%
  ggsave("output/figures/figuur1_raw.pdf", plot = .)

park_timeseries_plot(park_gr, "2019-10-1", "2019-10-8",
                     title = "Gecalibreerde data") %>%
  ggsave("output/figures/figuur1_gecalibreerd.pdf", plot = .)


# gemiddelde per week dag
day_week_overlay_plot("P11", park_gr) %>%
  ggsave("output/figures/figuur2_p11.pdf", plot = .)



# Heat map - uur basis
park_heatmap_hourly(park_gr) %>%
  ggsave("output/figures/figuur3_heatmap.pdf", plot = .)


# Gemiddelde wekelijks verloop per parkeerplaats 
weekly_timeseries_plot(park_gr) %>%
  ggsave("output/figures/figuur4_timeseries.pdf", plot = .)


# Heatmap : wekelijks verloop vs. seizoensverloop
# Apart per parkeerplaats
park_heatmap_weekly_seasonal("P11", park_gr)



# Kaart
map_parking_locations(parking_map)

# Kaart, gekleurd naar aantal auto's
map_parking_timefilter(park_gr, hr = 12, day = "Saturday")



