###------------------------------------------------------
#
# RMSC4003 materials
#
###------------------------------------------------------

#Set wd
setwd("~/OneDrive/MPhil RMSC/RMSC4003 TUT/Tutorial notes/Tutorial 3/Extra Questions")
###------------------------------------------------------
#
# Read Stcok Price
#
s.3333 <- read.csv("3333.HK.csv")
s.0005 <- read.csv("0005.HK.csv")
s.TSLA <- read.csv("TSLA.csv")
str(s.3333)

###------------------------------------------------------
#
# Set Date 
#
s.3333$Date <- as.Date(s.3333$Date, format = "%Y-%m-%d")
s.0005$Date <- as.Date(s.0005$Date, format = "%Y-%m-%d")
s.TSLA$Date <- as.Date(s.TSLA$Date, format = "%Y-%m-%d")
str(s.3333)

###------------------------------------------------------
#
# Extract Adjusted close and date 
#
s.1 = s.3333[,-c(2,3,4,5,7)];
colnames(s.1) = c("Date","3333.HK")
s.2 = s.0005[,-c(2,3,4,5,7)];
colnames(s.2) = c("Date","0005.HK")
s.3 = s.TSLA[,-c(2,3,4,5,7)];
colnames(s.3) = c("Date","TSLA")


###------------------------------------------------------
#
# Merge in to one
#
df = merge(merge(s.1, s.2, by = "Date"), s.3, by  = "Date")
rownames(df) = df$Date
df$Date <- NULL

###------------------------------------------------------
#
# Mean vector, and Variance and covariance matrix
#
ret = apply(log(df), 2, diff)
mu = apply(ret, 2, mean)
Sigma = cov(ret)
ts.plot(ret, col = 1:3, main = "Return")
legend("bottomleft", legend = colnames(df), lty = 1, col = 1:3)

###------------------------------------------------------
#
# Minimum variance portfolio
#
E = cbind(mu, c(1,1,1))
A = cbind(Sigma, E)
A = rbind(A, cbind(t(E), matrix(c(0,0,0,0),nrow = 2)))

n_sim = 101
w = array(NA, dim = c(n_sim, 3))
target.mu = seq(-0.05, 0.05, length.out = n_sim)
for(i in 1:n_sim){
	w[i,] = (solve(A)%*%c(0,0,0,target.mu[i],1))[1:3]
}

port.mu = w%*%mu                                # y-axis
port.sd = sqrt(apply((w%*%Sigma)*w, 1, sum))    # x-axis

plot(port.sd, port.mu, type = "l", xlim =c(0, 0.3), xaxs = "i", main = expression(paste(mu,"-",sigma, " diagram"))  )
lines(port.sd[port.mu>0], port.mu[port.mu>0], col = "blue")
legend("topleft", legend = "Efficient frontier without riskfree asset", col = "blue", lty = 1, cex =0.6)

###------------------------------------------------------
#
# Inclusion of risk-free asset
#
rf = 0.02/252
b = mu - rf
v = solve(Sigma)%*%b 
w3 = v/sum(v)
tan.mean = mu%*%w3 
tan.sd = sqrt(t(w3)%*%Sigma%*%w3)


## Plot
plot(port.sd, port.mu, type = "l", xlim =c(0, 0.3), xaxs = "i", main = expression(paste(mu,"-",sigma, " diagram")))
lines(port.sd[port.mu>0], port.mu[port.mu>0], col = "blue")   # Original EF
points(tan.sd, tan.mean, col = "red", pch = 21, bg = "red")   # Tangency portfolio
points(0, rf, col = "red", pch = 21, bg = "red")              # Risk-free asset
y.points = c(tan.mean, rf)
x.points = c(tan.sd, 0)

model = lm(y.points ~x.points)
abline(model$coefficients, col = "black")                      # sttraight line
abline(h = 0, col = "green", lty = 2)
abline(a = model$coefficients[1], b = -model$coefficients[2], col = "red")   #Short market portfolio and buy risk-free asset
legend("topleft", legend = c("EF without riskfree asset",
                             "EF with riskfree asset"), 
       col = c("blue","red"), lty = 1, cex =0.6)
