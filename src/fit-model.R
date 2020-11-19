require(openxlsx)
source("./src/vote_curve_functions.R")

##---- load and prepare data -----
#https://results.enr.clarityelections.com/MI/Oakland/105840/web.264614/#/reporting
od <- read.xlsx("./input/detail.xlsx", sheet =3, startRow = 3)
od2 <- read.xlsx("./input/detail.xlsx", sheet =4, startRow = 3)
od3 <- read.xlsx("./input/detail.xlsx", sheet =5, startRow = 3)

D.st <- od[,5]
R.st <- od[,8]

Biden.ic <-od2[,5]
Trump.ic <- od2[,8]

data <- data.frame(D.st, R.st, Biden.ic, Trump.ic)
data <- data[-nrow(data),]
write.csv(data, file= './output/cleaned-data.csv')

##----- fit model parameters -----
data$percent.R.st <- data$R.st/(data$D.st + data$R.st)
data$excess.Trump <- data$Trump.ic/(data$Trump.ic + data$Biden.ic) - data$R.st/(data$R.st + data$D.st)

#data should look like: first column baseline, second column, excess Trump
ll <- function(params, data){
  sd <- params[4]
  sr <- params[3]
  pd <- params[2]
  pr <- params[1]
  
  baseline <- data[,1]
  y <- data[,2]
  
  #Calculate baseline given params
  R <-baseline*sd/(baseline*sd + sr - baseline*sr)
  
  #these are the same
  #test <- R*sr/(R*sr + (1-R)*sd)
  
  yhat <- R*(1-sr)*(1-pr) + (1-R)*(1-sd)*pd
  yhat <- yhat/(R*(1-sr) + (1-R)*(1-sd))
  yhat <- yhat - R*sr/(R*sr + (1-R)*sd)
  
  loss <- sum( (y - yhat)^2 ) #just use squared error loss
  
  return(loss)
}

params <- c(.3, .3, .2, .2)
fit.data <- data[,c("percent.R.st", "excess.Trump")]

solution <- optim(params, ll, method="Nelder-Mead",
                  data=fit.data,
                  control = list(trace=2))

##---- make some plots -----

png('./output/fitted-curve.png')
plot(data$percent.R.st, data$excess.Trump, pch = 15, col = 'blue',
     main = "Oakland County, MI - 2020",
     xlab = 'Baseline',
     ylab = "Excess Trump vote")

c1 <- vote_function(solution$par[1], solution$par[2], solution$par[3], solution$par[4], plot=FALSE)
lines(c1$baseline, c1$difference, col = 'purple', lwd=3)
c1 <- vote_function(.18, .02, .31, .37, plot=FALSE)
lines(c1$baseline, c1$difference, col = 'pink', lwd=3)
abline(h=0)

dev.off()

png('./output/flat-line.png')
c1 <- vote_function(0, 0, .2, .2, plot=TRUE, ylim=c(-20, 20),
                    xlab = "Percent R Straight Ticket",
                    ylab = "Excess Trump")
dev.off()

png('./output/parabola.png')
c1 <- vote_function(.05, .01, .2, .35, plot=TRUE, ylim=c(-20, 40),
                    xlab = "Percent R Straight Ticket",
                    ylab = "Excess Trump")
dev.off()

png('./output/negative-slope.png')
c1 <- vote_function(.05, .01, .3, .3, plot=TRUE, ylim=c(-10, 10),
                    xlab = "Percent R Straight Ticket",
                    ylab = "Excess Trump")
dev.off()

png('./output/negative-slope-noise.png')
c1 <- vote_function(.05, .01, .3, .3, plot=TRUE, ylim=c(-10, 10),
                    noise = .01,
                    xlab = "Percent R Straight Ticket",
                    ylab = "Excess Trump")
dev.off()

## plot of a whole bunch of different functions this model generates
pds <- runif(100, 0, .3)
prs <- runif(100, 0, .35)
sds <- runif(100, .1, .9)
srs <- runif(100, .1, .9)

png('./output/example-functions.png')

plot(0,0, ylim=c(-.75, .75), xlim=c(-0,1), col = 'white', xlab = "Baseline",
     ylab="Excess Vote", 
     main = "One hundred `normal`curves")
for(i in 1:100){
  c1 <- vote_function(pds[i], prs[i], sds[i], srs[i],
                    plot=FALSE)
  lines(c1$baseline, c1$difference, col = 'gray')
}



dev.off()


