
test_that("buffering, function call and value works", {
  bckt <- bucket( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,100) ) {bckt$add( i )}
  expect_equal( bckt$getValue(), 5.5 )
})

test_that("reading through buffered results works",{
  bckt <- bucket( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,100) ) {bckt$add( i )}
  for ( i in seq(1,9) ) {bckt$getValue()}
  expect_equal( bckt$getValue(), 95.5 )
})

test_that("composite functions work", {
  sumOfSquares <- function(x) {sum( x*x )}
  bckt <- bucket( accumulatorSize=10, sumOfSquares, NULL )
  for ( i in seq(1,100) ) {bckt$add( i )}
  expect_equal( bckt$getValue(), 385 )
})

test_that("flush works",{
  bckt <- bucket( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,103) ) {bckt$add( i )}
  bckt$flush()
  for ( i in seq(1,10) ) {bckt$getValue()}
  expect_equal( bckt$getValue(), 102 )
})

test_that("super passing raw data to sub works",{
  # This tortured example also tests whether one bucket can pass unaltered arguments
  # to subsequent buckets when the "function" of the "super" bucket is NULL.
  bucket_sub <- bucket( accumulatorSize=10, mean, NULL )
  bucket_super <- bucket( accumulatorSize=5, NULL, bucket_sub )
  for ( i in seq(1,18) ) {bucket_super$add(i)}
  expect_equal( bucket_sub$getValue(), 5.5 )
})

test_that("sub flush works",{
  bucket_sub <- bucket( accumulatorSize=10, mean, NULL )
  bucket_super <- bucket( accumulatorSize=5, NULL, bucket_sub )
  for ( i in seq(1,18) ) {bucket_super$add(i)}
  bucket_sub$getValue()
  bucket_sub$flush()
  expect_equal( bucket_sub$getValue(), 13 )
})

test_that("super/sub flush interaction works",{
  bucket_sub <- bucket( accumulatorSize=10, mean, NULL )
  bucket_super <- bucket( accumulatorSize=5, NULL, bucket_sub )
  for ( i in seq(1,18) ) {bucket_super$add(i)}
  bucket_sub$getValue()
  bucket_sub$flush()
  bucket_sub$getValue()
  bucket_super$flush()
  bucket_sub$flush()
  expect_equal( bucket_sub$getValue(), 17 )
})

test_that("priming works",{
  bckt <- bucket( accumulatorSize=10, mean, NULL, prime=5 )
  for ( i in seq(1,15) ) {bckt$add( i )}
  # Only 1 item should be on the stack.
  expect_equal( bckt$getValue(), 5.5 )
  # Subsequent calls fail
  testthat::expect_null( ( bckt$getValue() ) )
  # Flush
  bckt$flush()
  # Now "get" succeeds
  expect_equal( bckt$getValue(), 13 )
})

test_that("priming before a send works",{
  bucket_sub <- bucket( accumulatorSize=10, mean, NULL )
  bucket_super <- bucket( accumulatorSize=10, NULL, bucket_sub, prime=5 )
  for ( i in seq(1,49) ) {bucket_super$add(i)}
  # "_super" should have some results, but ...
  expect_equal( length( bucket_super$getValue() ), 10 )
  expect_equal( mean( bucket_super$getValue() ), 15.5 )
  # ... "sub" shouldn't have a single entry.
  testthat::expect_null( ( bucket_sub$getValue() ) )
  # Flushing "sub" should not help
  bucket_sub$flush()  
  testthat::expect_null( ( bucket_sub$getValue() ) )
  # Flushing "super" should send the first element to "sub"
  bucket_super$flush()
  expect_equal( mean( bucket_sub$getValue() ), 5.5 )
})

test_that("priming too short still works",{
  bucket_sub <- bucket( accumulatorSize=10, mean, NULL )
  bucket_super <- bucket( accumulatorSize=10, NULL, bucket_sub, prime=1 )
  for ( i in seq(1,100) ) {bucket_super$add(i)}
  for ( j in seq(1,5) ) {bucket_sub$getValue()}
  expect_equal( bucket_sub$getValue(), 55.5 ) 
})

test_that("priming with multiple levels works",{
  bucket_3 <- bucket( accumulatorSize=10, mean, NULL )
  bucket_2 <- bucket( accumulatorSize=10, NULL, bucket_3 )
  bucket_1 <- bucket( accumulatorSize=10, NULL, bucket_2, prime=5 )
  for ( i in seq(1,49) ) {bucket_1$add(i)}
  # If the first buffer is not fully primed, then nothing is sent.
  testthat::expect_null( ( bucket_3$getValue() ) )
  # Once it is fully primed, the processing chain is activated.
  bucket_1$add(50)
  expect_equal( mean( bucket_3$getValue() ), 5.5 )
})

test_that("buckets within buckets works",{
  # An algorithm can contain buckets.
  # It must 1. define 'add' and 'getValue' and 2. instantiate the object.
  bucket_container <- function() {
    bucket_3 <- bucket( accumulatorSize=10, mean, NULL )
    bucket_2 <- bucket( accumulatorSize=10, NULL, bucket_3 )

    # 1.
    add <- function(events) {
      bucket_2$add( events )
    }
    getValue <- function(events) {
      bucket_3$getValue()
    }

    # 2.
    obj <- list(add=add,getValue=getValue)
    class(obj) <- c('buckets' )
    return( obj )
  }
  bc <- bucket_container()
  bucket_1 <- bucket( accumulatorSize=10, NULL, bc, prime=5 )
  for ( i in seq(1,50) ) {bucket_1$add(i)}
  expect_equal( mean( bc$getValue() ), 5.5 )
})

test_that("using an algorithm object works",{
  AA <- 'bucket_output <- bucket( accumulatorSize=10, mean, NULL );
         bucket_input  <- bucket( accumulatorSize=10, NULL, bucket_output)'
  A <- algorithm(AA)
  for (x in seq(1,10)) {A$add(x)}
  expect_equal( A$getValue(), 5.5 )
})

test_that("buckets sending output to DatabaseInsertBuffers works",{
  bucket_output <- buckets::bucket( accumulatorSize=2, NULL, topconnect::databaseInsertBuffer( 'MRE_test', 'test', c('value'), 2 ), returnName='value' )
  bucket_input <- buckets::bucket( accumulatorSize=5, mean, bucket_output )
  for ( i in seq(1,20) ) {bucket_input$add(i)}
  # Do the unit test here
  conn <- topconnect::db(user='root',password='',dbname='MRE_test',host='localhost')
  query <- 'SELECT test.*,@rn:=@rn+1 as row_num from test,(select @rn:=0) as R WHERE value >= 3 AND value <= 18 AND created >= NOW() - INTERVAL 10 MINUTE;'
  rs <-   DBI::dbGetQuery( conn, query )
  expect_equal( max(rs$row_num), 4 )
  # Clear out recent entries to the database
  query <- 'DELETE FROM test WHERE created >= NOW() - INTERVAL 1 MINUTE;'
  DBI::dbGetQuery( conn, query )
  DBI::dbDisconnect(conn)
})

test_that("closures being called by Buckets works",{
  closure <- function(parameter) {
    function(x) {
      mean(x + parameter)      
    } 
  }
  bckt <- bucket( accumulatorSize=10, closure(5), NULL )
  for ( i in seq(1,100) ) {bckt$add( i )}
  expect_equal( bckt$getValue(), 10.5 )
  expect_equal( bckt$getValue(), 20.5 )
})

test_that("buckets can operate on the names of named vectors",{
  closure <- function(parameter) {
    function(x) {
      mean( as.numeric(names(x)) + parameter)      
    }
  }
  # Closure adds 5
  bckt <- bucket( accumulatorSize=10, closure(5), NULL )
  NV <- seq(1,100)
  # Names add 10
  names(NV) <- 10 + NV
  for ( i in seq(1,100) ) {bckt$add( NV[i] )}
  # Overall adds 15 to the mean: 5.5 + 15 = 20.5
  expect_equal( bckt$getValue(), 20.5 )
})

test_that("buckets running in bucket-wise mode works",{
  pass_through <- function(x) {x}
  bucket_2 <- bucket( accumulatorSize=0, pass_through, NULL ) # Just passes on the result
  bucket_1 <- bucket( accumulatorSize=10, mean, bucket_2 )
  for ( i in seq(1,20) ) {bucket_1$add(i)}
  expect_equal( bucket_2$getValue(), 5.5 )
  expect_equal( bucket_2$getValue(), 15.5 )
  
})

test_that("algorithms with user-defined functions works",{
  AA <- 'bucket_output <- bucket( accumulatorSize=10, mean, NULL );
         closure <- function(parameter) { function(x) {mean(x + parameter)}};
         bucket_input  <- bucket( accumulatorSize=10, closure(5), bucket_output)'
  A <- algorithm(AA)
  for (x in seq(1,100)) {A$add(x)}
  expect_equal( A$getValue(), 55.5 )
})

test_that("setData works",{
  algorithm_test <-
    "bucket_output <- buckets::bucket( accumulatorSize=0, NULL, NULL );
     test_function <- function(x) {x + data};
     bucket_input <- buckets::bucket( accumulatorSize=0, test_function, bucket_output )"
  algo <- algorithm( algorithm_test, NULL )
  algo$setData(5)
  algo$add(1)
  expect_equal( algo$getValue(), 6 )
  algo$setData(15)
  algo$add(1)
  expect_equal( algo$getValue(), 16 )
})

