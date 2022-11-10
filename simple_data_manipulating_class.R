### This class contains 7 methods for simple data manipulation, like 
# filtering the data and computing RMSE. The class err was implemented in R6 model.
###

rm(list=ls())
library(R6)
library(dplyr)

##downloading data
# data has to contain columns called y and forecast!
r <- readRDS(file = "./forecast_data_all.RDS")
head(as.data.frame(r))

##class err
err <- R6Class(
  classname = 'err',
  public = list(
    initialize = function(data){
      private$original_data <- data
      private$current_data <- data
    },
    get_data = function(){
      as.data.frame(private$current_data)
    },
    get_result = function(){
      as.data.frame(private$result)
    },
    select = function(...){
      private$current_data <- select(private$current_data,...)
      return(invisible(self))
    },
    filter = function(...){
      private$current_data <- filter(private$current_data,...)
      return(invisible(self))
    },
    along = function(...){
      private$current_data <- group_by(private$current_data, ...)
      return(invisible(self))
    },
    apply = function(fname){
      switch(
        fname,
        "ME" = private$ME(),
        "MAE" = private$MAE(),
        "MPE" = private$MPE(),
        "MAPE" = private$MAPE(),
        "MSE" = private$MSE(),
        "RMSE" = private$RMSE()
      )
      return(invisible(self))
    },
    reset_data = function(){
      private$current_data <- private$original_data
      private$result <- data.frame()
      return(invisible(self))
    }
  ),
  private = list(
    original_data = data.frame(),
    current_data = data.frame(),
    result = data.frame(),
    ME = function(){
      private$result <- summarise(private$current_data, result=Metrics::mean(y - forecast))
    },
    MAE = function(){
      private$result <- summarise(private$current_data, result=Metrics::mae(y, forecast))
    },
    MPE = function(){
      r <- yardstick::mpe(private$current_data, y, forecast)
      private$result <- r[,-c(ncol(r)-2,ncol(r)-1)]
    },
    MAPE = function(){
      private$result <- summarise(private$current_data, result=Metrics::mape(y, forecast))
    },
    MSE = function(){
      private$result <- summarise(private$current_data, result=Metrics::mse(y, forecast))
    },
    RMSE = function(){
      private$result <- summarise(private$current_data, result=Metrics::rmse(y, forecast))
    }
  )
)

##example
e <- err$new(data = r)
e

head(as.data.frame(e$get_data()))
as.data.frame(e$get_result())

head(as.data.frame(e$get_data()))
e$select(y, forecast, month)
head(as.data.frame(e$get_data())) 

head(as.data.frame(e$get_data()))
e$filter(month %in% c(1, 2, 3))
head(as.data.frame(e$get_data()))

e$along(month)

head(as.data.frame(e$get_data()))
e$apply(fname = "RMSE")
as.data.frame(e$get_result())

e$reset_data()
head(as.data.frame(e$get_data()))
res <- e$select(y, forecast, period)$along(period)$apply("MAE")$get_result()
barplot(height = res$result, names.arg = 1:12, ylim = c(0, 30))

e$select(y, forecast, period)$along(period)$apply("MAE")$reset_data()$get_data()

