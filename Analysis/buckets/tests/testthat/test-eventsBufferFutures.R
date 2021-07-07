
test_that("buffering, function call and value works", {
  bucket <- buckets( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,100) ) {bucket$add( i )}
  expect_equal( bucket$getValue(), 5.5 )
})

test_that("reading through buffered results works",{
  bucket <- buckets( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,100) ) {bucket$add( i )}
  for ( i in seq(1,9) ) {bucket$getValue()}
  expect_equal( bucket$getValue(), 95.5 )
})

test_that("composite functions work", {
  sumOfSquares <- function(x) {sum( x*x )}
  bucket <- buckets( accumulatorSize=10, sumOfSquares, NULL )
  for ( i in seq(1,100) ) {bucket$add( i )}
  expect_equal( bucket$getValue(), 385 )
})

test_that("flush works",{
  bucket <- buckets( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,103) ) {bucket$add( i )}
  bucket$flush()
  for ( i in seq(1,10) ) {bucket$getValue()}
  expect_equal( bucket$getValue(), 102 )
})

test_that("super passing raw data to sub works",{
  # This tortured example also tests whether one bucket can pass unaltered arguments
  # to subsequent buckets when the "function" of the "super" bucket is NULL.
  bucket_sub <- buckets( accumulatorSize=10, mean, NULL )
  bucket_super <- buckets( accumulatorSize=5, NULL, bucket_sub )
  for ( i in seq(1,18) ) {bucket_super$add(i)}
  expect_equal( bucket_sub$getValue(), 5.5 )
})

test_that("sub flush works",{
  bucket_sub <- buckets( accumulatorSize=10, mean, NULL )
  bucket_super <- buckets( accumulatorSize=5, NULL, bucket_sub )
  for ( i in seq(1,18) ) {bucket_super$add(i)}
  bucket_sub$getValue()
  bucket_sub$flush()
  expect_equal( bucket_sub$getValue(), 13 )
})

test_that("super/sub flush interaction works",{
  bucket_sub <- buckets( accumulatorSize=10, mean, NULL )
  bucket_super <- buckets( accumulatorSize=5, NULL, bucket_sub )
  for ( i in seq(1,18) ) {bucket_super$add(i)}
  bucket_sub$getValue()
  bucket_sub$flush()
  bucket_sub$getValue()
  bucket_super$flush()
  bucket_sub$flush()
  expect_equal( bucket_sub$getValue(), 17 )
})

test_that("priming works",{
  bucket <- buckets( accumulatorSize=10, mean, NULL, prime=5 )
  for ( i in seq(1,15) ) {bucket$add( i )}
  # Only 1 item should be on the stack.
  expect_equal( bucket$getValue(), 5.5 )
  # Subsequent calls fail
  testthat::expect_null( ( bucket$getValue() ) )
  # Flush
  bucket$flush()
  # Now "get" succeeds
  expect_equal( bucket$getValue(), 13 )
})

test_that("priming before a send works",{
  bucket_sub <- buckets( accumulatorSize=10, mean, NULL )
  bucket_super <- buckets( accumulatorSize=10, NULL, bucket_sub, prime=5 )
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
  bucket_sub <- buckets( accumulatorSize=10, mean, NULL )
  bucket_super <- buckets( accumulatorSize=10, NULL, bucket_sub, prime=1 )
  for ( i in seq(1,100) ) {bucket_super$add(i)}
  for ( j in seq(1,5) ) {bucket_sub$getValue()}
  expect_equal( bucket_sub$getValue(), 55.5 ) 
})

test_that("priming with multiple levels works",{
  bucket_3 <- buckets( accumulatorSize=10, mean, NULL )
  bucket_2 <- buckets( accumulatorSize=10, NULL, bucket_3 )
  bucket_1 <- buckets( accumulatorSize=10, NULL, bucket_2, prime=5 )
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
    bucket_3 <- buckets( accumulatorSize=10, mean, NULL )
    bucket_2 <- buckets( accumulatorSize=10, NULL, bucket_3 )

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
  bucket_1 <- buckets( accumulatorSize=10, NULL, bc, prime=5 )
  for ( i in seq(1,50) ) {bucket_1$add(i)}
  expect_equal( mean( bc$getValue() ), 5.5 )
})





