bucket_source <- function( mef_conts, algo, primeSize ) {
  #' @export
  library( future )
#  nbrWorkers <- parallel::detectCores()
#  print( paste0( "Number of workers: ", nbrWorkers ) )
#  plan(multisession,workers=nbrWorkers) # "multisession" is portable, "multicore" is not
  
  stack <- NULL
  maxStackSize <- 5*primeSize
  stackCount <- 0
  writeCount  <- 0
  primeCnt <- 0
  sendCount <- 0

  begin <- function() {
    counter <<- 0
    while ( mef_conts$hasNext() ) { # across contiguous blocks
      iter_data <- mef_conts$nextElem()
      while ( iter_data$hasNext() ) {
#        while ( writeCount>10*primeSize & algo$getDoneCount()<(writeCount - 2*primeSize) ) {
#          print( "sleeping" )
#          Sys.sleep(60)
#        }
        tryCatch(
          {
            parameters <- iter_data$nextParameters()
            stackCount <- (writeCount %% maxStackSize) + 1
            stack[[stackCount]] <<- future( iter_data$readByParameters(parameters) )
          }, error=function(cond) {
            stack[[stackCount]] <<- NULL
          }
        )
        primeCnt <<- primeCnt + 1
        writeCount <<- writeCount + 1
        #print( paste0( "Write: ", writeCount ) )
        #print( paste0( "Stack: ", stackCount ) )
        if ( primeCnt >= primeSize ) {
          sendStackCount <- (sendCount %% maxStackSize ) + 1
          if ( !is.null(algo) ) {
            returnValue <- value(stack[[sendStackCount]])
            if ( !is.null(returnValue) & length(returnValue)>0 ) {
              #print( paste0( "returnValue: ", returnValue[1] ) )
              attr( returnValue, 'counter' ) <- sendCount
              if ( (sendCount %% 10)==0 ) {
                print( sendCount )
              }
              algo$run( returnValue )
              # The last bucket needs to send something when it completes
              #print( paste0( "Algo run done: ", algo$getDoneCount() ) )
            }
          }
          sendCount <<- sendCount + 1
          #print( paste0( "Send: ", sendCount ) )
        }
      } # next chunk of data
    } # next continuous series of chunks
  }
  
  obj <- list(begin=begin)
  class(obj) <- c('bucket_source' )
  return( obj )
}



#compArgs$findClass('metadataInformer')$set( "counter", counter)
#t0 <- as.numeric( attr( data, 't0' ) )
#t1 <- as.numeric( attr( data, 't1' ) )
#Tstored <- NPO:::findTheLatestTimestampProcessed( compArgs )
#if ( t1 > (Tstored - 2*correlationWindow) ) { # more to do in this data block
#  if ( (counter %% 10)==0 ) {
#    print( paste0( t0) )
#  }
#  algo$run( data )
#  print( counter )
#} else {
#  print( paste0( 'Skipping ', t0 ) )
#}
#rm( data )
