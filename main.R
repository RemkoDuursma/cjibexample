
# Packages laden
source("R/load_packages.R")

# Functies laden
source("R/functions_figures.R")
source("R/functions_read_data.R")
source("R/functions_clean_and_prepare_data.R")
source("R/functions_modelling.R")
source("R/utils.R")

# output, cache folders aanmaken als ze nog niet bestaan
source("R/make_dirs.R")

# 1. Data inlezen
parking_raw <- read_parking_raw()
parking_map <- read_parking_map_data()


# 2. Data bewerken

# Originele data, opgeschoond.
park <- clean_parking_raw(parking_raw, calibrate = FALSE)

# Gecalibreerd.
park_gr <- clean_parking_raw(parking_raw, calibrate = TRUE)

# Per uur.
park_hr <- aggregate_parking(park_gr)


# 3. Models

# Met dag van de week
model1 <- fit_model_randomforest(park_hr, form = 1)

# Zonder dag van de week
model2 <- fit_model_randomforest(park_hr, form = 2)

# Maak een voorspelling voor het huidige uur
p_now <- predict_parking_now(model1, where = "P1")
write.table(p_now, "output/data/voorspelling_nu.txt")


# 4. Rapport met figuren
rmarkdown::render("Rmd/almereparking_figuren.Rmd", output_dir = "output/figures")






