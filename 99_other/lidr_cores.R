# Create function with switch to favor chunk based or lidR based parallelism
core_use <- function(
    n_cores = future::availableCores(), 
    parallel_switch = "chunk", 
    parallel_bias = "small") {
  
  if(n_cores > availableCores()) n_cores <- availableCores()
  
  if((sqrt(n_cores) %% 1) == 0) {
    lidr_threads <- sqrt(n_cores)
    future_threads <- sqrt(n_cores)
  } else {
    divs <- sapply((n_cores - 1):2, function(x) {
      ifelse(n_cores %% x == 0, x, NA)
    })
    if(all(is.na(divs))) {
      n_cores <- n_cores - 1
      divs <- sapply((n_cores - 1):2, function(x) {
        ifelse(n_cores %% x == 0, x, NA)
      })
    } 
    divs <- divs[!is.na(divs)]
    if(length(divs) > 3 && parallel_bias %in% c("small", "medium")) {
      divs <- divs[divs != max(divs) & divs != min(divs)]
      if(length(divs) > 3 && parallel_bias == "small") {
        divs <- divs[divs != max(divs) & divs != min(divs)]
      } 
    }
    if(parallel_switch == "chunk") {
      lidr_threads <- min(divs)
      future_threads <- max(divs)
    } else {
      lidr_threads <- max(divs)
      future_threads <- min(divs)
    }
  }
  return(list(lidr_threads = lidr_threads, future_threads = future_threads))
}
