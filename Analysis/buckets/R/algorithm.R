algorithm <- function( algo ) {
  #' @export
  library(evaluate)

  evaluate(algo)

  # 1.
  add <- function(events) {
    bucket_input$add( events )
  }
  getValue <- function(events) {
    bucket_output$getValue()
  }
  
  # 2.
  obj <- list(add=add,getValue=getValue)
  class(obj) <- c('algorithm' )
  return( obj )
}


