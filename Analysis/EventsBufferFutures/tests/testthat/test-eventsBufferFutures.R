library(EventsBufferFutures)

test_that("buffering, function call and value works", {
  ebf <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,100) ) {ebf$add( i )}
  expect_equal( ebf$getValue(), 5.5 )
})

test_that("reading through buffered results works",{
  ebf <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,100) ) {ebf$add( i )}
  for ( i in seq(1,9) ) {ebf$getValue()}
  expect_equal( ebf$getValue(), 95.5 )
})

test_that("composite functions work", {
  sumOfSquares <- function(x) {sum( x*x )}
  ebf <- eventsBufferFutures( accumulatorSize=10, sumOfSquares, NULL )
  for ( i in seq(1,100) ) {ebf$add( i )}
  expect_equal( ebf$getValue(), 385 )
})

test_that("flush works",{
  ebf <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,103) ) {ebf$add( i )}
  ebf$flush()
  for ( i in seq(1,10) ) {ebf$getValue()}
  expect_equal( ebf$getValue(), 102 )
})

test_that("super passing raw data to sub works",{
  # This tortured example also tests whether one EBF can pass unaltered arguments
  # to subsequent EBFs when the "function" of the "super" EBF is NULL.
  ebf_sub <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_super <- eventsBufferFutures( accumulatorSize=5, NULL, ebf_sub )
  for ( i in seq(1,18) ) {ebf_super$add(i)}
  expect_equal( ebf_sub$getValue(), 5.5 )
})

test_that("sub flush works",{
  ebf_sub <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_super <- eventsBufferFutures( accumulatorSize=5, NULL, ebf_sub )
  for ( i in seq(1,18) ) {ebf_super$add(i)}
  ebf_sub$getValue()
  ebf_sub$flush()
  expect_equal( ebf_sub$getValue(), 13 )
})

test_that("super/sub flush interaction works",{
  ebf_sub <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_super <- eventsBufferFutures( accumulatorSize=5, NULL, ebf_sub )
  for ( i in seq(1,18) ) {ebf_super$add(i)}
  ebf_sub$getValue()
  ebf_sub$flush()
  ebf_sub$getValue()
  ebf_super$flush()
  ebf_sub$flush()
  expect_equal( ebf_sub$getValue(), 17 )
})

test_that("priming works",{
  ebf <- eventsBufferFutures( accumulatorSize=10, mean, NULL, prime=5 )
  for ( i in seq(1,15) ) {ebf$add( i )}
  # Only 1 item should be on the stack.
  expect_equal( ebf$getValue(), 5.5 )
  # Subsequent calls fail
  testthat::expect_null( ( ebf$getValue() ) )
  # Flush
  ebf$flush()
  # Now "get" succeeds
  expect_equal( ebf$getValue(), 13 )
})

test_that("priming before a send works",{
  ebf_sub <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_super <- eventsBufferFutures( accumulatorSize=10, NULL, ebf_sub, prime=5 )
  for ( i in seq(1,49) ) {ebf_super$add(i)}
  # "_super" should have some results, but ...
  expect_equal( length( ebf_super$getValue() ), 10 )
  expect_equal( mean( ebf_super$getValue() ), 15.5 )
  # ... "sub" shouldn't have a single entry.
  testthat::expect_null( ( ebf_sub$getValue() ) )
  # Flushing "sub" should not help
  ebf_sub$flush()  
  testthat::expect_null( ( ebf_sub$getValue() ) )
  # Flushing "super" should send the first element to "sub"
  ebf_super$flush()
  expect_equal( mean( ebf_sub$getValue() ), 5.5 )
})

test_that("priming too short still works",{
  ebf_sub <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_super <- eventsBufferFutures( accumulatorSize=10, NULL, ebf_sub, prime=1 )
  for ( i in seq(1,100) ) {ebf_super$add(i)}
  for ( j in seq(1,5) ) {ebf_sub$getValue()}
  expect_equal( ebf_sub$getValue(), 55.5 ) 
})

test_that("priming with multiple levels works",{
  ebf_3 <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_2 <- eventsBufferFutures( accumulatorSize=10, NULL, ebf_3 )
  ebf_1 <- eventsBufferFutures( accumulatorSize=10, NULL, ebf_2, prime=5 )
  for ( i in seq(1,49) ) {ebf_1$add(i)}
  # If the first buffer is not fully primed, then nothing is sent.
  testthat::expect_null( ( ebf_3$getValue() ) )
  # Once it is fully primed, the processing chain is activated.
  ebf_1$add(50)
  expect_equal( mean( ebf_1$getValue() ), 5.5 )
})


