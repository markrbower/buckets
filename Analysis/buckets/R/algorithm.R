algorithm <- function( algo ) {
  #' @export

  eval(algo) # It makes sense that 'evaluating' doesn't add it list of functions

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


