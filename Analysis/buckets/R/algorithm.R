algorithm <- function( text_algo, compArgs ) {
  #' @export
  library(evaluate)
  library(future)

  storedData <- NULL
  compArgs <- compArgs

  evaluate( text_algo, debug=TRUE )

  # 1.
  run <- function(events) {
    bucket_input$run( events )
  }
  
  add <- function(events) {
    bucket_input$add( events )
  }
  
  getValue <- function(events) {
    bucket_output$getValue()
  }
  
  setData <- function( new_data ) {
    storedData <<- new_data
  }
  
  flush <- function() {
    bucket_output$flush()
  }
  
  # 2.
  obj <- list(run=run,add=add,getValue=getValue,setData=setData,flush=flush)
  class(obj) <- c('algorithm' )
  return( obj )
}


