
# Fit een randomforest model
fit_model_randomforest <- function(data, form = 1){
  
  out <- switch(form, 
         `1` = ranger(parked ~ hour + label + weekday, data = data, importance = "impurity"),
         `2` = ranger(parked ~ hour + label, data = data, importance = "impurity")
         )
  
return(out)
}


predict_parking_now <- function(model, where = "P1"){
  
  hour_now <- get_nearest_hour_now()
  
  out_dfr <- expand.grid(hour = hour_now,
                         weekday = wday(Sys.time()),
                         label = factor(c("P1","P10","P11","P12",
                                          "P2","P3","P5","P6","P7","P8","P9")))
  
  pred <- predict(model, data = out_dfr)
  
  out_dfr$prediction <- pred$predictions
  
  out_dfr <- dplyr::filter(out_dfr, label %in% where)
  
return(out_dfr)
}
