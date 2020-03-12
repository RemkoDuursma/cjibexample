

mk <- function(dir){
  
  if(!dir.exists(dir)){
    dir.create(dir)
  }
}

mk("output/data")
mk("output/figures")
mk("cache")