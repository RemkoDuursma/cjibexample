
# Script om data in te lezen.
# Leest van de remote (zie conf/config.yml), of leest de cache als beschikbaar.

# outputs: objecten 'parking' (alle data), en 'parking_map' voor map data.

# Parkeer data
fn <- "cache/almereparking.rds"
if(!file.exists(fn) || !.conf$general$cache){
  
  # Dit duurt even...
  db <- mongo(collection = "almereparkingjson",
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                .conf$database$user, 
                .conf$database$password, 
                .conf$database$dbhost,
                .conf$database$table))
  
  parking <- db$find()
  saveRDS(parking, fn) 
} else {
  parking <- readRDS(fn)
}



# Kaart
parking_map <- read_excel("data/park.xlsx")

