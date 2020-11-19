require(glue)

vote_function <- function(pr, pd, sr, sd, plot=TRUE, noise=0, ylim=c(-100,100), ...){
  ## function to generate Shiva curves
  
  #pr = probability republican defects
  #pd = probability a democrat defects
  #sr = probability republican picks straight ticket
  #sd = probability democrat picks straight ticket
  
  R <- seq(0,1, .01)
  D <- 1-R
  
  stopifnot(pd >=0 & pr >=0 & sr >=0 & sd >=0 & noise >=0)
  stopifnot(pd <=1 & pr <=1 & sr <=1 & sd<=1 & noise <=.2)
  
  prs <- rnorm(length(R), pr, noise)
  pds <- rnorm(length(R), pd, noise)
  srs <- rnorm(length(R), sr, noise)
  sds <- rnorm(length(R), sd, noise)
  
  #proportion Trump | individual
  proportion_Trump <- (R*(1-prs)*(1-srs) + D*pds*(1-sds))/(R*(1-srs) + D*(1-sds))
  
  #proportion R | straight_ticket
  baseline <- R*srs/(R*srs + D*sds)
  
  difference <-  proportion_Trump - baseline
  
  if(plot){
    plot(baseline*100, difference*100, col = 'blue', pch = 15,ylim=ylim, 
         main = glue('Pd = {pd}, Pr = {pr}, Sd = {sd}, Sr = {sr}'), ...)
    abline(h=0)
  }
  out <- data.frame(difference, baseline)
  
}

vote_function_senate <- function(pr, pd, sr, sd, plot=TRUE, noise=0,
                                 ylim=c(-1,1),c = 1,  ...){
  ##function to plot Shiva curves where baseline is % R in senate instead of %R in straight ticket
  R <- seq(0,1, .01)
  D <- 1-R
  
  stopifnot(pd >=0 & pr >=0 & sr >=0 & sd >=0 & noise >=0)
  stopifnot(pd <=1 & pr <=1 & sr <=1 & sd<=1 & noise <=.2)
  
  prs <- rnorm(length(R), pr, noise)
  pds <- rnorm(length(R), pd, noise)
  srs <- rnorm(length(R), sr, noise)
  sds <- rnorm(length(R), sd, noise)
  
  #proportion Trump | ic
  proportion_Trump <- (R*(1-prs)*(1-srs) + D*pds*(1-sds))/(R*(1-srs) + D*(1-sds))
  
  #proportion R senate | ic
  baseline <-(R*(1-prs/c)*(1-srs) + D*pds/c*(1-sds) + R*srs)/(R*(1-srs) + D*(1-sds))
  
  difference <-  proportion_Trump - baseline
  
  if(plot){
    plot(baseline*100, difference*100, col = 'blue', pch = 15,ylim=ylim, 
         main = glue('Pd = {pd}, Pr = {pr}, Sd = {sd}, Sr = {sr}, c = {c}'), ...)
    abline(h=0)
  }
  out <- data.frame(difference, baseline)
  
}

vote_function_sensible <- function(pr, pd, sr, sd, plot=TRUE, noise=0,
                                   
                                   cr=1, cd=1, ylim=ylim,...){
  #function to generate curves under a less misleading measure of excess Trump vote
  
  R <- seq(0,1, .01)
  D <- 1-R
  
  stopifnot(pd >=0 & pr >=0 & sr >=0 & sd >=0 & noise >=0)
  stopifnot(pd <=1 & pr <=1 & sr <=1 & sd<=1 & noise <=.2)
  
  prs <- rnorm(length(R), pr, noise)
  pds <- rnorm(length(R), pd, noise)
  srs <- rnorm(length(R), sr, noise)
  sds <- rnorm(length(R), sd, noise)
  
  
  #proportion Trump 
  proportion_Trump <- (R*(1-prs)*(1-srs) + R*srs +  D*pds*(1-sds))
  
  #proportion R Senate
  baseline <- (R*(1-prs/cr)*(1-srs) + R*srs +  D*pds/cd*(1-sds))
  
  
  difference <-  proportion_Trump - baseline
  
  if(plot){
    plot(baseline*100, difference*100, col = 'blue', pch = 15, ylim=ylim,...)
    abline(h=0)
  }
  out <- data.frame(difference, baseline)
  
}
