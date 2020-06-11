
get_nearest_hour_now <- function(){
  h <- lubridate::hour(Sys.time())
  m <- lubridate::minute(Sys.time())
  round(h + m/60, digits = 0)
}


