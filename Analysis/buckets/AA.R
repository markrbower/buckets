AA <- {
  bucket_output <- buckets( accumulatorSize=10, mean, NULL );
  bucket_input  <- buckets( accumulatorSize=10, NULL, bucket_output )
}

