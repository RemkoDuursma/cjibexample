

mk <- function(dir){
  
  if(!dir.exists(dir)){
    dir.create(dir)
  }
}

mk("conf")
mk("output")
mk("output/data")
mk("output/figures")
mk("cache")