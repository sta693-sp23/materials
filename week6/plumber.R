library(plumber)
library(ggplot2)

#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Return the data as csv
#* @serializer csv
#* @get /data
function() {
  iris
}

#* Return the data as html
#* @serializer html
#* @get /data/html
function() {
  knitr::kable(iris, format = "html")
}

#* Return the data as json
#* @get /data/json
function() {
  iris
}

#* Return a column of data
#* @param col Column to select from iris
#* @param n Max results to return from the column
#* @get /data/<col>
function(col,n=10) {
  stopifnot(col %in% names(iris))
  
  head(iris[[col]], n=n)
}

#* Plot the data
#* @serializer png
#* @get /plot
function() {
  g = ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species)) +
    geom_point()
  
  print(g)
}

#* Plot the data
#* @serializer png
#* @get /plot/<x>/<y>
function(x,y) {
  g = ggplot(iris, aes(x=.data[[x]], y=.data[[y]], col=Species)) +
    geom_point()
  
  print(g)
}


#* A slow endpoint
#* @get /slow
function() {
  Sys.sleep(5)
  "Done"
}

counter = 0

#* Increment the counter
#* @get /count
function() {
  counter <<- counter + 1
  counter
}
