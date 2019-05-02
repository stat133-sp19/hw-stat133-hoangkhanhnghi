

#' @title Number of combinations
#' @description calculate number of combinations in which k successes can occur in n trials
#' @param n trials
#' @param k successes
#' @return computed number of combinations
#' @examples
#' bin_choose(5,3)
#' @export
bin_choose <- function(n,k){
  ifelse(k>n,stop("k cannot be greater than n"),factorial(n)/(factorial(n-k)*factorial(k)))
}


#' @title Probability
#' @description calculate the probability in which k successes can occur in n trials
#' @param success input number of successes
#' @param trials input number of trials
#' @param prob input probability
#' @return computed probability in which k successes can occur in n trials
#' @examples
#' bin_probability(3,6,0.5)
#' @export
bin_probability <- function(success,trials,prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success,trials)
  x <- bin_choose(trials,success)*prob^success*(1-prob)^(trials-success)
  return(x)
}

#' @title Binomial probability
#' @description calculate the probability from 1 to k successes which can occur in n trials
#' @param trials input number of trials
#' @param prob input probability
#' @return computed probability in which k successes can occur in n trials
#' @examples
#' bin_distribution(5,0.5)
#' @export
bin_distribution <- function(trials,prob){
  x <- c()
  y <- c()
  for (i in (1:(trials+1))){
    x[i] <- i-1
    y[i] <- bin_probability(i-1,trials,prob)
  }
  df <- data.frame(success=x,probability=y)
  class(df) <- c("bindis","data.frame")
  return(df)
}

#' @title Binomial cumulative
#' @description calculate the cumulative probability from 1 to k successes which can occur in n trials
#' @param trials input number of trials
#' @param prob input probability
#' @return computed cumulative probability in which k successes can occur in n trials
#' @examples
#' bin_cumulative(5,0.5)
#' @export
bin_cumulative <- function(trials,prob){
  y <- c()
  x <- bin_distribution(trials,prob)
  for (i in (1:length(x$success))){
    y[i] <- sum(x$probability[1:i])
  }
  df <- data.frame(x,cumulative=y)
  class(df) <- c("bincum","data.frame")
  return(df)
}


#' @title Binomial Variable
#' @description make an object class "binvar"
#' @param trials input number of trials
#' @param prob input probability
#' @return a list with named elements trials and prob and class "binvar"
#' @examples
#' bin_variable(5,0.5)
#' @export
bin_variable <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  x <- list(trials,prob)
  class(x) <- c("binvar")
  return(x)
}


#' @title Binomial Mean
#' @description calculate mean of the binomial distribution
#' @param trials input number of trials
#' @param prob input probability
#' @return computed mean of the binomial distribution
#' @examples
#' bin_mean(5,0.5)
#' @export
bin_mean <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  mean <- aux_mean(trials,prob)
  return(mean)
}

#' @title Binomial Variance
#' @description calculate variance of the binomial distribution
#' @param trials input number of trials
#' @param prob input probability
#' @return computed variance of the binomial distribution
#' @examples
#' bin_variance(5,0.5)
#' @export
bin_variance <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  variance <- aux_variance(trials,prob)
  return(variance)
}

#' @title Binomial Mode
#' @description calculate mode of the binomial distribution
#' @param trials input number of trials
#' @param prob input probability
#' @return computed mode of the binomial distribution
#' @examples
#' bin_mode(5,0.5)
#' @export
bin_mode <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  mode <- aux_mode(trials,prob)
  return(mode)
}

#' @title Binomial Skewness
#' @description calculate skewness of the binomial distribution
#' @param trials input number of trials
#' @param prob input probability
#' @return computed skewness of the binomial distribution
#' @examples
#' bin_skewness(5,0.5)
#' @export
bin_skewness <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  skew <- aux_skewness(trials,prob)
  return(skew)
}

#' @title Binomial Kurtosis
#' @description calculate kutosis of the binomial distribution
#' @param trials input number of trials
#' @param prob input probability
#' @return computed kutosis of the binomial distribution
#' @examples
#' bin_kutosis(5,0.5)
#' @export
bin_kurtosis <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  kur <- aux_kurtosis(trials,prob)
  return(kur)
}




