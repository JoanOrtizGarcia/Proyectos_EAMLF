rm(list = ls()) 

#install.packages("quantmod")
library(quantmod)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(scales)
tickers <- c("AAPL", "META", "GS", "MCD", "SBUX", "WMT", "DIS", "TSLA")
start <- "2016-12-31"
end <- "2023-10-25"
n <- length(tickers)
p <- getSymbols(Symbols = tickers[1], src = "yahoo",
                from = start, to = end,
                auto.assign = F)[, 6]
for (i in 2:n){
  p <- merge(p, getSymbols(Symbols = tickers[i], src = "yahoo",
                           from = start, to = end,
                           auto.assign=F)[, 6])
}
names(p) <- gsub(".Adjusted", "", names(p))
head(p)

# Simple returns
ret <- p / lag(p) - 1

# Log-returns (in case you want to use these instead, however notice that using
# log-returns would also change how we calculate the cross-sectional portfolio
# returns and its average time-series return too.
# ret <- log(p / lag(p))

# Removing the first row since it contains NA's: a return can't be calculated
# for the first day
ret <- ret[-1, ]
tail(ret)

# Counting if there are any more NAs
# colSums(is.na(ret))
apply(is.na(ret), 2, sum)

# If there were still NAs and we wanted to remove them:
# ret <- na.omit(ret)

retm <- as.matrix(ret)
head(retm)

# Portfolio Weights
wts <- matrix(rep(1/n, n), nrow = n, ncol = 1)
wts

# Removing row names from "retm" (matrix) to avoid conflict with rownames from
# "ret" (xts)
rownames(retm) <- NULL

# Portfolio Returns (considering discrete returns)
ret$EqPort <- retm %*% wts

# Portfolio Returns (considering continuous returns)
# ret$EqPort <- log(exp(m) %*% wts)

head(ret)

# Daily 99% VaR (Alpha: 1%, Holding Period: 1 day)
alpha <- 0.01 
hp <- 1

pv <- c(rep(10000, n), 10000*n)
pv

# Returns
ret.avg <- apply(ret, 2, mean, na.rm = T) * hp

print(percent( ret.avg , accuracy = 0.01))

# Volatilities
vol <- apply(ret, 2, sd, na.rm = T) * sqrt(hp)

print(percent( vol , accuracy = 0.01))

# Correlation Matrix (observed / historical data)
cor_h <- cor(retm, use = "complete.obs", method = "pearson")
cor_h

# Covariance Matrix (observed / historical data)
cov_h <- cov(retm, use = "complete.obs", method = "pearson") * hp
cov_h

# Confirming Portfolio Volatility, in percentage
sqrt(t(wts) %*% cov_h %*% wts)*100

var.p <- ret.avg + qnorm(alpha)*vol

print(percent( var.p , accuracy = 0.01))

var.p.pv <- var.p*pv

print(number( var.p.pv , accuracy = 0.01, big.mark = ","))

print(number( sum(var.p.pv[1:n]) , accuracy = 0.01, big.mark = ","))

g <- list()
g[[1]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = ret.avg[1], sd = vol[1]), 
                color = "blue") +
  geom_vline(xintercept = ret.avg[1] + var.p[1], color = "red") +
  labs(title = paste(names(vol)[1], 
                     "VaR =", percent(var.p[1], accuracy = 0.01))) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01), 
                     limits = c(-0.08, 0.08))
for (i in 2:ncol(ret)) {
  g[[i]] <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = ret.avg[i], sd = vol[i]), 
                  color = "blue") +
    geom_vline(xintercept = ret.avg[i] + var.p[i], color = "red") +
    labs(title = paste(names(vol)[i], 
                       "VaR =", percent(var.p[i], accuracy = 0.01))) + 
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.01), 
                       limits = c(-0.08, 0.08))
}
do.call("grid.arrange", c(g, nrow = 3))

cvar.p <- ret.avg - dnorm(qnorm(alpha))*vol/alpha

print(percent( cvar.p , accuracy = 0.01))

print(percent( cvar.p[1:n] %*% wts , accuracy = 0.01))

cvar.p.pv <- cvar.p*pv

print(number( cvar.p.pv , accuracy = 0.01, big.mark = ","))

print(number( sum(cvar.p.pv[1:n]) , accuracy = 0.01, big.mark = ","))

var <- data.frame(t(percent(var.p, accuracy = 0.01)))
rownames(var) <- "Parametric VaR (Normal)"
temp <- data.frame(t(percent(cvar.p, accuracy = 0.01)))
rownames(temp) <- "Parametric CVaR (Normal)"
var <- rbind(var, temp)
head(var)

var.pv <- data.frame(t(number(var.p.pv, accuracy = 0.01, big.mark = ",")))
rownames(var.pv) <- "Parametric VaR (Normal)"
temp <- data.frame(t(number(cvar.p.pv, accuracy = 0.01, big.mark = ",")))
rownames(temp) <- "Parametric CVaR (Normal)"
var.pv <- rbind(var.pv, temp)
head(var.pv)


# Build P&L table
m <- nrow(ret)
pnl <- matrix(0:(m-1)/(m-1)*100, nrow = m, ncol = 1)
colnames(pnl) <- "percentile"
head(pnl, 20)

for (i in 1:(n+1)) {
  pnl <- cbind(pnl, sort(coredata(ret[, i]), decreasing = F))
}
colnames(pnl)[2:(n+2)] <- names(ret)
head(pnl, 20)

# Interpolate rows 18 & 19 since the 1% is between them
var.h.pnl <- pnl[18,-1]+(pnl[19,-1]-pnl[18,-1])*(alpha*100-pnl[18,1])/(pnl[19,1]-pnl[18,1])

print(percent( var.h.pnl , accuracy = 0.01))

# EqPort's H VaR
print(percent( quantile(ret$EqPort, alpha) , accuracy = 0.01))

# Historical VaR
var.h <- apply(ret, 2, quantile, alpha, na.rm = T)

print(percent( var.h , accuracy = 0.01))

print(percent( var.h[1:n] %*% wts , accuracy = 0.01))

var.h.pv <- var.h*pv

print(number( var.h.pv , accuracy = 0.01, big.mark = ","))

print(number( sum(var.h.pv[1:n]) , accuracy = 0.01, big.mark = ","))

# Find days of lowest returns, 
lowestRets <- ret
for (i in 1:ncol(ret)) {
  lowestRets[, i] <- ifelse(coredata(ret[, i]) <= var.h[i], ret[, i], NA)
}

# Lowest Return Scenarios: 8 / 752 in each case
print( apply(!is.na(lowestRets), 2, sum) )

# Which lowest returns contribute to the EqPort lowest returns?
lowestRets[!is.na(lowestRets$EqPort) == T, ]

# since the lowestRets series has NA's by design, 
# we ignore those warnings in the graphs
options(warn = -1)

# Historical VaR Graphs: Return Scenarios
g <- list()
g[[1]] <- ggplot(ret, aes_string(x = index(ret), y = names(ret)[1])) + 
  geom_col(position = "identity") + 
  geom_col(aes_string(y = lowestRets[, 1]), color = "red", 
           position = "identity") + 
  labs(title = paste(names(ret)[1], "Historical Returns"), 
       x = "Date / Scenarios", y = "Returns") +
  ylim(c(-0.10,0.10))
for (i in 2:ncol(ret)) {
  g[[i]] <- ggplot(ret, aes_string(x = index(ret), 
                                   y = names(ret)[i])) + 
    geom_col(position = "identity") + 
    geom_col(aes_string(y = lowestRets[, i]), color = "red", 
             position = "identity") + 
    labs(title = paste(names(ret)[i], "Historical Returns"), 
         x = "Date / Scenarios", y = "Returns") +
    ylim(c(-0.10,0.10))
  
  
}
do.call("grid.arrange", c(g, nrow = 3))



options(warn = 0) # we restore the warnings parameter for other use cases

# we ignore warnings due to NA's in the graphs, since they are by design
options(warn = -1)

# Historical VaR Graphs: Histograms
g <- list()
g[[1]] <- ggplot(ret, aes_string(x = names(ret)[1])) + 
  geom_histogram(aes(y = ..density..), position = "identity", bins = 30,
                 fill = "cornflowerblue") + 
  stat_function(fun = dnorm, args = list(mean = ret.avg[1], sd = vol[1]), 
                color = "gray40") +
  geom_vline(xintercept = var.h[1], color = "red") +
  geom_vline(xintercept = ret.avg[1] + var.p[1], color = "gray40") +
  labs(title = paste(names(ret)[1], "Returns Histogram: Historical vs Normal")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01), 
                     limits = c(-0.08, 0.08))
for (i in 2:ncol(ret)) {
  g[[i]] <- ggplot(ret, aes_string(x = names(ret)[i])) + 
    geom_histogram(aes(y = ..density..), position = "identity", bins = 30,
                   fill = "cornflowerblue") + 
    stat_function(fun = dnorm, args = list(mean = ret.avg[i], sd = vol[i]), 
                  color = "gray40") +
    geom_vline(xintercept = var.h[i], color = "red") +
    geom_vline(xintercept = ret.avg[i] + var.p[i], color = "gray40") +
    labs(title = paste(names(ret)[i], "Returns Histogram: Historical vs Normal")) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.01), 
                       limits = c(-0.08, 0.08))
}
do.call("grid.arrange", c(g, nrow = 3))

options(warn = 0) # we restore the warnings parameter for other use cases

print(percent( apply(lowestRets, 2, mean, na.rm = T) , accuracy = 0.01))

print(percent( apply(pnl[1:8, -1], 2, mean) , accuracy = 0.01 ))

print( quantile(ret$EqPort, c(0.01, 0.02, 0.03, 0.04, 0.05)) )

print( quantile(ret$EqPort, seq(from = 0, to = alpha, by = 1/(m-1))) )

# Confirming EqPort's CVaR:
mean(quantile(ret$EqPort, seq(0, alpha, 1/(m-1))))

cvar.h <- colMeans(apply(ret, 2, quantile, probs = seq(0, alpha, 1/(m-1))))

print(percent( cvar.h , accuracy = 0.01))

print(percent( cvar.h[1:n] %*% wts , accuracy = 0.01))

cvar.h.pv <- cvar.h * pv

print(number( cvar.h.pv , accuracy = 0.01, big.mark = ","))

print(number( sum(cvar.h.pv[1:n]) , accuracy = 0.01, big.mark = ","))

temp <- data.frame(t(percent(var.h, accuracy = 0.01)))
rownames(temp) <- "Historical VaR"
var <- rbind(var, temp)
temp <- data.frame(t(percent(cvar.h, accuracy = 0.01)))
rownames(temp) <- "Historical CVaR"
var <- rbind(var, temp)
var

temp <- data.frame(t(number(var.h.pv, accuracy = 0.01, big.mark = ",")))
rownames(temp) <- "Historical VaR"
var.pv <- rbind(var.pv, temp)
temp <- data.frame(t(number(cvar.h.pv, accuracy = 0.01, big.mark = ",")))
rownames(temp) <- "Historical CVaR"
var.pv <- rbind(var.pv, temp)
var.pv

##### Normal vs t-Student Distributions #####
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm, color = "blue") +
  stat_function(fun = dt, args = list(df = 2), color = "red") + 
  labs(title = "Distributions: Normal Standard vs t-Student") + 
  annotate(geom = "text", x = 1, y = dnorm(x = 1),label = "Normal Standard", 
           hjust = 0, vjust = -0.2, size = 4, color = "blue") +
  annotate(geom = "text", x = 1, y = dt(x = 1, df = 2), label = "t-Student (df = 2)", 
           hjust = -0.2, vjust = -0.2, size = 4, color = "red")

# Assuming t-Student Distribution with theoretical df
m <- 10000 # scenarios
e <- matrix(rt(m * n, m - 1), ncol = n)
colnames(e) <- paste0("e",1:8)
head(e)

round(cor(e), 2)

a <- chol(cov_h)
ret.mc <- data.frame(e %*% a)
head(ret.mc)

cor_mc <- cor(ret.mc)
round(cor_mc, 2)

round(cor_h, 2)

ret.mc$EqPort <- as.matrix(ret.mc[, 1:n]) %*% as.matrix(wts)
head(ret.mc$EqPort)

quantile(ret.mc$EqPort, alpha)

var.mc <- apply(ret.mc, 2, quantile, alpha)

print(percent( var.mc , accuracy = 0.01))

print(percent( var.mc[1:n] %*% wts , accuracy = 0.01))

var.mc.pv <- var.mc * pv

print(number( var.mc.pv , accuracy = 0.01, big.mark = ","))

print(number( sum(var.mc.pv[1:n]) , accuracy = 0.01, big.mark = ","))

mean(quantile(ret.mc$EqPort, seq(0, alpha, 1/(m-1))))

cvar.mc <- colMeans(apply(ret.mc, 2, quantile, probs = seq(0, alpha, 1/(m-1))))

print(percent( cvar.mc , accuracy = 0.01))

print(percent( cvar.mc[1:n] %*% wts , accuracy = 0.01))

cvar.mc.pv <- cvar.mc * pv

print(number( cvar.mc.pv , accuracy = 0.01, big.mark = ","))

print(number( sum(cvar.mc.pv[1:n]) , accuracy = 0.01, big.mark = ","))

temp <- data.frame(t(percent(var.mc, accuracy = 0.01)))
rownames(temp) <- "MonteCarlo VaR (t-Student: df=obs-1)"
var <- rbind(var, temp)
temp <- data.frame(t(percent(cvar.mc, accuracy = 0.01)))
rownames(temp) <- "MonteCarlo CVar (t-Student: df=obs-1)"
var <- rbind(var, temp)
var

# Consolidating results
temp <- data.frame(t(number(var.mc.pv, accuracy = 0.01, big.mark = ",")))
rownames(temp) <- "MonteCarlo VaR (t-Student: df=obs-1)"
var.pv <- rbind(var.pv, temp)
temp <- data.frame(t(number(cvar.mc.pv, accuracy = 0.01, big.mark = ",")))
rownames(temp) <- "MonteCarlo CVar (t-Student: df=obs-1)"
var.pv <- rbind(var.pv, temp)
var.pv