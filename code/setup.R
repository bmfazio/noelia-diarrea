library(brms)
library(naniar)
library(readxl)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(here)
library(bayesplot)

options(mc.cores = floor(parallel::detectCores()/2))
color_scheme_set("darkgray")

parcoord <- function(x, b = NULL, stdize = F){
  model_array <- as.array(x)
  if(is.null(b)){
    b <- dimnames(model_array)$parameters
    print(b)
  }
  if(stdize){
    model_array <- aperm(apply(model_array, c(1,3), scale), c(2, 1, 3))
  }
  
  np <- nuts_params(x)
  lp <- log_posterior(x)
  
  mcmc_parcoord(model_array, np = np, pars = b)
}

stanpars <- function(x){
  dimnames(as.array(x))$parameters
}