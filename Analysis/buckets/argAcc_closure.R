argAcc_closure <- function(...) {
  new_args <- list(...)
  if ( class(new_args[[1]][[1]]) == 'function' ) {
    fxn <- new_args[[1]][[1]]
    args_ <<- append( fxn(), new_args[[1]][2:length(new_args[[1]])] )
  } else {
    args_ <<- new_args
  }
  
  function(x) {
    args_
  }
}

