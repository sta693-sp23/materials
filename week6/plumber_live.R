library(plumber)
library(ggplot2)

#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

iris_func = function() {
  iris
}

#* Return the data as csv
#* @serializer csv
#* @get /data
iris_func

#* Return the data as json
#* @get /data/json
iris_func

#* Return the data as html
#* @serializer html
#* @get /data/html
function() {
  knitr::kable(iris, format = "html")
}


#* Create a plot
#* @serializer png
#* @param n Number of samples
#* @get /plot
function(n) {
  n = as.numeric(n)
  if (is.na(n))
    stop("n needs to be a number")
  hist(rnorm(n))
}

#* Create a plot
#* @serializer png
#* @param n Number of samples
#* @get /plot/<n:int>
function(n) {
  hist(rnorm(n))
}


