

# Lees ruwe data, uit MongoDB of lokale cache.
read_parking_raw <- function(){
  
  # Parkeer data
  fn <- "data/almereparking.rds"
  if(!file.exists(fn)){
    
    if(!file.exists("conf/config.yml")){
      stop("YAML met database wachtwoord/username nodig voor downloaden data.")
    }
    .conf <- yaml::read_yaml("conf/config.yml")
    
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
  read_excel("data/park.xlsx")  
}


