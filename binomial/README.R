---
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-")
```


## Overview
The package �binomial� simulates the process of getting the probability of a specific number of successes given the number of trials, the number of successes, and the probability to get that success.


* 'bin_probability()' Calculate the probability given successes, trials, and probability.
* 'bin_distribution()' Give a table of probabilities corresponding to each success (from 0 to trials). Output class: �bindis� and �data.frame�
* 'bin_cumulative' Gives a table of cumulative probabilities corresponding to the successes. Output classes: �bincum� and �data.frame�
* 'bin_variable' returns a list with class �binvar� and method �print.binvar� gives a short summary of object with class �binvar�
* 'summary' gives a full summary of object with class �binvar�


## Installation

Install the development version from GitHub via the package `"devtools"`:

```{r}
# development version from GitHub:
#install.packages("devtools") 
# install "cointoss" (without vignettes)
devtools::install_github("gastonstat/cointoss")
# install "cointoss" (with vignettes)
devtools::install_github("gastonstat/cointoss", build_vignettes = TRUE)
```



## Usage
```{r}
library(binomial)
#calculate probaility
bin_distribution(5,0.5)

#calculate cumulative probability
bin_cumulative(5,0.5)

#print a summary
bin1 <- bin_variable(10,0.3)
bin1

#print a more detailed summary
bin1 <- bin_variable(10,0.3)
binsum1 <- summary(bin1)
binsum1

#plot histogram of probability
dis1 <- bin_distribution(5,0.5)
plot(dis1)

#plot line graph of cumulative probability
dis2 <- bin_cumulative(5,0.5)
plot(dis2)
```






