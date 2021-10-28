bucket_last <- function( accumulatorSize, FUN, recipient, primeSize=0, returnName=NULL ) {
  #
  # algorithm      : an "algorithm" object
  # accumulatorSize: the number of events held until you do something
  # FUN            : what is done with a full accumulator
  # recipient      : where to send the result
  # primeSize      : the number of FUN runs to be completed before broadcasting
  #
  #' @export
  library( future )
#  nbrWorkers <- parallel::detectCores()
#  print( paste0( "Number of workers: ", nbrWorkers ) )
#  plan(multisession,workers=nbrWorkers) # "multisession" is portable, "multicore" is not
  
  algo <- NULL
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
  
  run <- function( events ) {
    accumulator <<- events
    makeTheCall()
  }
  
  add <- function( events ) {
    # If accumulatorSize==0, add the events as a vector
    if ( accumulatorSize==0) {
      accumulator <<- events
    } else {
      # Is event a scalar or vector?
      for ( idx in seq(1,length(events) ) ) {
        add_single( events[idx] )
      }
    }
  }
  
  add_single <- function( event, flag=TRUE ) {
    # event is a scalar
    #    print( "bucket::add_single")
    #    print( paste0( "event: ", event ) )
    #    print( paste0( "accumulator: ", accumulator ))
    accumulator <<- append( accumulator, event )
    #    print( paste0( "accumulator: ", accumulator ))
    if ( accumulatorSize>0 & length(accumulator)>=accumulatorSize ) {
      makeTheCall()      
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
  
  # This takes into account whether the stack should be primed first ...
  makeTheCall <- function() {
    primeCnt <<- primeCnt + 1
    writeCount <<- writeCount + 1
    if ( !is.null(FUN) ) {
      tryCatch(
        {
          stack[[writeCount]] <<- future( FUN( accumulator ) )
        },
        error=function(cond) {
          stack[[writeCount]] <<- NULL
        }
      )
    } else {
      tryCatch(
        {
          stack[[writeCount]] <<- future( accumulator )
        },
        error=function(cond) {
          stack[[writeCount]] <<- NULL
        }
      )
    }
    # Send forward?
    if ( primeCnt >= primeSize ) {
      #      if ( primeSize > 0 ) {
      #        print( "PRIMED!")
      #      }
      sendCount <<- sendCount + 1
      if ( !is.null(recipient) ) {
        returnValue <- value(stack[[sendCount]])
        if ( !is.null(returnValue) & length(returnValue)>0 ) {
          if ( !is.null(returnName) ) {
            names(returnValue) <- returnName
          }
          recipient$run( returnValue )
          algo$incDoneCount()
        }
      }
    }
    # Clear the accumulator
    accumulator <<- c()
    #    # Flush the stack to the receiver
    #    while ( sendCount < length(stack) ) {
    #      print( "Sending")
    #      sendCount <<- sendCount + 1
    #      if ( !is.null(recipient) ) {
    #        recipient$run( value(stack[[sendCount]]) )  
    #      }
    #    }
  }
  
  # ... while this one does not.
  flush <- function() {
    # Check for an empty accumulator
    if ( length(accumulator) == 0 ) {
      return(NULL)
    }
    # Flush the accumulator to the stack
    writeCount <<- writeCount + 1
    if ( !is.null(FUN) ) {
      tryCatch(
        {
          stack[[writeCount]] <<- future( FUN( accumulator ) )
        },
        error=function(cond) {
          stack[[writeCount]] <<- NULL
        }
      )
    } else {
      tryCatch(
        {
          stack[[writeCount]] <<- future( accumulator )
        },
        error=function(cond) {
          stack[[writeCount]] <<- NULL
        }
      )
    }
    accumulator <<- c()
    # Flush the stack to the receiver
    if ( !is.null(recipient) ) {
      sendCount <<- sendCount + 1
      while ( sendCount <= length(stack) ) {
        if ( accumulatorSize == 0 ) {
          if ( !is.null(returnValue) & length(returnValue)>0 ) {
            recipient$run( value(stack[[sendCount]]) )
            algo$incDoneCount()
          }
        } else {
          if ( !is.null(returnValue) & length(returnValue)>0 ) {
            recipient$run( value(stack[[sendCount]]) )
            algo$incDoneCount()
          }
        }
      }
      recipient$flush()
    }
  }
  
  setAlgo <- function( A ) {
    algo <<- A
  }
  
  obj <- list(prime=prime,run=run,add=add,getValue=getValue,makeTheCall=makeTheCall,flush=flush,setAlgo=setAlgo)
  class(obj) <- c('bucket_last' )
  return( obj )
}
