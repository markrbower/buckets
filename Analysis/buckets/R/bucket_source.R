bucket_source <- function( mef_conts, algo, primeSize ) {
  #' @export
  library( future )

  stack <- NULL
  writeCount  <- 0
  primeCnt <- 0
  sendCount <- 0

  begin <- function() {
    counter <<- 0
    while ( itertools::hasNext( mef_conts ) ) { # across contiguous blocks
      iter_data <- iterators::nextElem( mef_conts )
      while ( itertools::hasNext( iter_data ) ) {
        while ( writeCount>primeSize & algo$getDoneCount()<(writeCount - 2*primeSize) ) {
          print( "sleeping" )
          Sys.sleep(60)
        }
        primeCnt <<- primeCnt + 1
        writeCount <<- writeCount + 1
        print( paste0( "Write: ", writeCount ) )
        tryCatch(
          {
            stack[[writeCount]] <<- future( iterators::nextElem( iter_data ) )
          }, error=function(cond) {
            stack[[writeCount]] <<- NULL
          }
        )

        if ( primeCnt >= primeSize ) {
          sendCount <<- sendCount + 1
          print( paste0( "Send: ", sendCount ) )
          if ( !is.null(algo) ) {
            returnValue <- value(stack[[sendCount]])
            if ( !is.null(returnValue) & length(returnValue)>0 ) {
              attr( returnValue, 'counter' ) <- sendCount
              algo$run( returnValue )
              
              # The last bucket needs to send something when it completes
              
              algo$incDoneCount()
              print( paste0( "Algo run done: ", algo$getDoneCount() ) )
            }
            stack[[sendCount]] <<- NULL
          }
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
