buckets <- function( accumulatorSize, FUN, recipient, primeSize=0, returnName=NULL ) {
  #
  # accumulatorSize: the number of events held until you do something
  # FUN            : what is done with a full accumulator
  # recipient      : where to send the result
  # primeSize      : the number of FUN runs to be completed before broadcasting
  #
  #' @export
  library( future )

  accumulator <- c()
  stack <- NULL
  writeCount  <- 0
  primeCnt <- 0
  sendCount <- 0
  readCount <- 0
  
  prime <- function( events ) { # do not call the function
    for ( idx in seq(1,length(events) ) ) {
      add_single( events[idx], flag=FALSE )
    }
  }
  
  add <- function( events ) {
    # Is event a scalar or vector?
    for ( idx in seq(1,length(events) ) ) {
      add_single( events[idx] )
    }
  }
  
  add_single <- function( event, flag=TRUE ) {
    # event is a scalar
    accumulator <<- append( accumulator, event )
    if ( length(accumulator) == accumulatorSize ) {
      primeCnt <<- primeCnt + 1
      writeCount <<- writeCount + 1
      if ( !is.null(FUN) ) {
        stack[[writeCount]] <<- future( FUN( accumulator ) )
      } else {
        stack[[writeCount]] <<- future( accumulator )
      }
      # Send forward?
      if ( primeCnt >= primeSize ) {
        sendCount <<- sendCount + 1
        if ( !is.null(recipient) ) {
          returnValue <- value(stack[[sendCount]])
          if ( !is.null(returnName) ) {
            names(returnValue) <- returnName
          }
          recipient$add( returnValue )
        }
      }
      # Clear the accumulator
      accumulator <<- c()
    }
  }
  
  getValue <- function() {
    readCount <<- readCount + 1
    if ( readCount <= length(stack) ) {
      returnValue <- value(stack[[readCount]])
      if ( !is.null(returnName) ) {
        names(returnValue) <- returnName
      }
      return( returnValue )
    } else {
      readCount <<- readCount - 1
      return( NULL )
    }
  }
  
  flush <- function() {
    # Check for an empty accumulator
    if ( length(accumulator) == 0 ) {
      return(NULL)
    }
    # Flush the accumulator to the stack
    writeCount <<- writeCount + 1
    if ( !is.null(FUN) ) {
      stack[[writeCount]] <<- future( FUN( accumulator ) )
    } else {
      stack[[writeCount]] <<- future( accumulator )
    }
    accumulator <<- c()
    # Flush the stack to the receiver
    while ( sendCount < length(stack) ) {
      sendCount <<- sendCount + 1
      if ( !is.null(recipient) ) {
        recipient$add( value(stack[[sendCount]]) )        
      }
    }
  }

  obj <- list(prime=prime,add=add,getValue=getValue,flush=flush)
  class(obj) <- c('buckets' )
  return( obj )
}
