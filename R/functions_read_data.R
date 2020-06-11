

# Lees ruwe data, uit MongoDB of lokale cache.
read_parking_raw <- function(.conf){
  
  # Parkeer data
  fn <- "data/almereparking.rds"
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
  
return(parking)
}


# Functie om data te lezen voor de kaart.
read_parking_map_data <- function(){
  parking_map <- read_excel("data/park.xlsx")  
}


