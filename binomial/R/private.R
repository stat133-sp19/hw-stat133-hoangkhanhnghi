
check_prob <- function(prob){
  if (prob<0 | prob>1 |(length(prob)>1)){
    stop("invalid prob value")
  }
  else{
    return(TRUE)
  }
}

check_trials <- function(trials){
  if (trials<0 | trials%%1!=0 | (length(trials)>1) ){
    stop("invalid trials value")
  }
  else{
    return(TRUE)
  }
}

check_success <- function(success,trials){
  for (i in 1:length(success)){
    if (success[i]<0 | success[i]%%1!=0 | success[i]>trials){
      stop("invalid success value")
    }
    else{
      return(TRUE)
    }
  }
}

aux_mean <- function(trials,prob){
  return(trials*prob)
}

aux_variance <- function(trials,prob){
  variance <- trials*prob*(1-prob)
  return(variance)
}

aux_mode <- function(trials,prob){
  m <- floor(trials*prob+prob)
  return(m)
}

aux_skewness <- function(trials,prob){
  skew <- (1-2*prob)/sqrt(trials*prob*(1-prob))
  return(skew)
}

aux_kurtosis <- function(trials,prob){
  kur <- (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  return(kur)
}

