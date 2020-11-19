library(openxlsx)
##----- online data-----
#https://results.enr.clarityelections.com/MI/Oakland/105840/web.264614/#/reporting
od <- read.xlsx("./input/detail.xlsx", sheet =3, startRow = 3)
od2 <- read.xlsx("./input/detail.xlsx", sheet =4, startRow = 3)
od3 <- read.xlsx("./input/detail.xlsx", sheet =5, startRow = 3)

D.st <- od[,5]
R.st <- od[,8]

Biden.ic <-od2[,5]
Trump.ic <- od2[,8]

Dsen.ic <- od3[,5]
Rsen.ic <- od3[,8]

data <- data.frame(D.st, R.st, Biden.ic, Trump.ic, Dsen.ic, Rsen.ic)

##----- compare to senate -----

data$total.Trump <- data$Trump.ic + data$R.st
data$total.Biden <- data$Biden.ic + data$D.st
data$total.sen.D <- data$Dsen.ic + data$D.st
data$total.sen.R <- data$Rsen.ic + data$R.st
data$percent.R.st <- data$R.st/(data$R.st+ data$D.st)
data$total.Trump.percent <- data$total.Trump/(data$total.Trump + data$total.Biden)
data$total.sen.R.percent <- data$total.sen.R/(data$total.sen.R + data$total.sen.D)


data$excess.Trump <- data$Trump.ic/(data$Trump.ic + data$Biden.ic) - data$percent.R.st
data$excess.Trump2 <- data$total.Trump.percent - data$total.sen.R.percent


png(file = './output/Trump-repro.png')
plot(data$percent.R.st, data$excess.Trump, 
     main = "Trump - Straigh Ticket Baseline", pch = 15, 
     col=  'blue',
     xlab = "Baseline",
     ylab = "Excess Trump")
abline(h=0)
dev.off()
#abline(lm(excess.Trump~total.sen.R.percent, data = data), col = 'gray')


png(file = './output/Trump-repro-with-senate.png')
plot(data$percent.R.st, data$excess.Trump, 
     main = "", pch = 15, 
     col=  'blue',
     xlab = "Baseline",
     ylab = "Excess Trump")
abline(h=0)
#abline(lm(excess.Trump~total.sen.R.percent, data = data), col = 'gray')

points(data$total.sen.R.percent, data$excess.Trump2, 
     main = "Trump - Senate Baseline", pch = 16, 
     col=  'purple')
abline(h=0)
dev.off()

## additional plot where we do the same analysis for Biden
data$excess.Biden <- data$Biden.ic/(data$Biden.ic + data$Trump.ic) - data$D.st/(data$D.st + data$R.st)
data$percent.D.st <- data$D.st/(data$D.st + data$R.st)

png(file = './output/Biden-analysis.png')
plot(data$percent.D.st, data$excess.Biden, 
     main = "Biden - Straigh Ticket Baseline: Oakland Co, MI", pch = 15, 
     col=  'blue',
     xlab = "Percent D Straight Ticket",
     ylab = "Excess Biden")
abline(h=0)
dev.off()
