
#' @export
print.binvar <- function(binvar){
  text <- paste("Binomial variable", "\n\nParameters", "\n- number of trials:",binvar[1],"\n- prob of success:",binvar[2],"\n\n")
  cat(text)
  invisible(binvar)
}


#' @export
summary.binvar <- function(binvar){
  mean <- aux_mean(binvar[[1]],binvar[[2]])
  variance <- aux_variance(binvar[[1]],binvar[[2]])
  mode <- aux_mode(binvar[[1]],binvar[[2]])
  skew <- aux_skewness(binvar[[1]],binvar[[2]])
  kur <- aux_kurtosis(binvar[[1]],binvar[[2]])
  x <- list(binvar[[1]],binvar[[2]],mean,variance,mode,skew,kur)
  class(x) <- c("summary.binvar")
  return(x)
}

#' @export
print.summary.binvar <- function(binvar){
  text <- paste("Summary Binomial","\n\nParameters","\n- number of trials: ",binvar[[1]],"\n- prob of success : ",binvar[[2]],"\n\nMeasures","\n- mean    :",binvar[[3]],"\n- variance:",binvar[[4]],"\n- mode    :",binvar[[5]],"\n- skewness:",round(binvar[[6]],7),"\n- kurtosis:",round(binvar[[7]],7))
  return(cat(text))
}
