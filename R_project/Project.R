load("nyse.RData")
# print first rows of the New York Stock Exchange data
nyse
head(nyse)
dim(nyse)
# scatter plot of Google closing price versus top apple Inc.
plot(nyse$AAPL, nyse$GOOGL, xlab = "Apple", ylab = "Google")
# time series plot of Google closing price over time
plot(as.Date(nyse$Date), nyse$GOOGL, type = "l",
     xlab = "Date", ylab = "Google closing price (cents)")

corr.coeffs <- vector("numeric", length =25 )
# run a loop and populate corr.coeffs with correlation coefficients
for(i in 1:25){
  corr.coeffs[i] <- cor(nyse$GOOGL, nyse[, i + 5])
}
corr.coeffs
plot(corr.coeffs,xlab = "coeffs data of company")

#finding top 5 strong co.relation 
a<-c(tail(sort(corr.coeffs),5))
a
b<-c(head(sort(corr.coeffs),5))
b
plot(a,b)
plot(b)

# Fit and interpret a linear regression
analysis.lm<-lm( nyse$GOOGL ~nyse$CDE+nyse$GIL+nyse$SAM+nyse$AMZN+nyse$MSFT)
summary(analysis.lm)

# Performs a regression using the full & untransformed model.
stock1.lm <- lm(nyse$GOOGL~  AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM
                +K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, data = nyse)
# summarise the linear model
summary(stock1.lm)
# print the estimated coefficients
coef(stock1.lm)
# store the fitted values from the full model 
fit <- stock1.lm$fitted.values
fit

# CDE -0.8965774  (Couer Mining)
#Make a scatter plot
plot( nyse$GOOGL  ~ nyse$CDE, data = nyse, xlab = "Stock of google", ylab = "stock of CDE ",col="red")
#for best fit
Stockcde.lm <- lm(GOOGL ~ CDE, data = nyse)
Stockcde.lm
#Coefficients:
#  (Intercept)          CDE  
#472.53       -80.05  
coef(Stockcde.lm)
summary(Stockcde.lm)
summary(Stockcde.lm)$coefficients
#Adding direct from reg model 
abline(Stockcde.lm, col = "blue", lwd = 2)
cde<-cor(nyse$GOOGL,nyse$CDE)
cde

# AMZN -0.7663762 (Amazon)

#Make a scatter plot
plot( nyse$GOOGL  ~ nyse$AMZN, data = nyse, xlab = "Stock of google", ylab = "stock of amzn")
#for best fit
Stockamzn.lm <- lm(GOOGL ~ AMZN, data = nyse)
Stockamzn.lm
#Coefficients:
 # (Intercept)         AMZN  
#370.2       -265.6  
coef(Stockamzn.lm)
summary(Stockamzn.lm)
summary(Stockamzn.lm)$coefficients
#Adding direct from reg model 
abline(Stockamzn.lm, col = "blue", lwd = 2)
amzn<-cor(nyse$GOOGL,nyse$AMZN)
amzn

#SAM 0.7657443 (Boston Beer Company)

#Make a scatter plot
plot( nyse$GOOGL  ~ nyse$SAM, data = nyse, xlab = "Stock of google", ylab = "stock of sam")
Stocksam.lm <- lm(GOOGL ~ SAM, data = nyse)
Stocksam.lm
#Coefficients:
 # (Intercept)          SAM  
#364.1        208.0  
coef(Stocksam.lm)
summary(Stocksam.lm)
summary(Stocksam.lm)$coefficients
#Adding direct from reg model 
abline(Stocksam.lm, col = "blue", lwd = 2)
sam<-cor(nyse$GOOGL,nyse$SAM)
sam

#GIL 0.7588794 (Gidlan Activewear)

#Make a scatter plot
plot( nyse$GOOGL  ~ nyse$GIL, data = nyse, xlab = "Stock of google", ylab = "stock of gil")
Stockgil.lm <- lm(GOOGL ~ GIL, data = nyse)
Stockgil.lm
#coef(Stockgil.lm)Coefficients:
 # (Intercept)          GIL  
#412.5        233.8  

summary(Stockgil.lm)
summary(Stockgil.lm)$coefficients
#Adding direct from reg model 
abline(Stockgil.lm, col = "blue", lwd = 2)
gil<-cor(nyse$GOOGL,nyse$GIL)
gil

#MSFT    -0.7030556 (Microsoft)

#Make a scatter plot
plot( nyse$GOOGL  ~ nyse$MSFT, data = nyse, xlab = "Stock of google", ylab = "stock of msft")
Stockmsft.lm <- lm(GOOGL ~ MSFT, data = nyse)
Stockmsft.lm
#Coefficients:
 # (Intercept)         MSFT  
#361.8       -313.5  
coef(Stockmsft.lm)
summary(Stockmsft.lm)
summary(Stockmsft.lm)$coefficients
#Adding direct from reg model 
abline(Stockmsft.lm, col = "blue", lwd = 2)
msft<-cor(nyse$GOOGL,nyse$MSFT)
msft



## backward  variable selection


stockdata.lm <- lm(GOOGL~AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM
                   +K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, data = nyse)

# assess F-statistic for each possible removal
# choose term to remove that has the smallest F-statistic
# and such that p-value > alpha(stay)
drop1(stockdata.lm, scope = ~., test = "F")

## removing appl F0.3404 & p0.5597118 at alpha 0.1

#--2

stockdata.lm <- lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM
                   +K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, data = nyse)

# assess F-statistic for each possible removal
# choose term to remove that has the smallest F-statistic
# and such that p-value > alpha(stay)
drop1(stockdata.lm, scope = ~., test = "F")

## removing XIN F 0.6821 & p 0.4089935 at alpha 0.1

#--3


stockdata.lm <- lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM
                   +K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR, data = nyse)

# assess F-statistic for each possible removal
# choose term to remove that has the smallest F-statistic
# and such that p-value > alpha(stay)
drop1(stockdata.lm, scope = ~., test = "F")

## removing KO 0.4094  0.522376  at alpha 0.1


#--4


stockdata.lm <- lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM
                   +K+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR, data = nyse)

# assess F-statistic for each possible removal
# choose term to remove that has the smallest F-statistic
# and such that p-value > alpha(stay)
drop1(stockdata.lm, scope = ~., test = "F")

## removing NOK 1.8638  0.172395  at alpha 0.1

#--5



stockdata.lm <- lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM
                   +K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR, data = nyse)

# assess F-statistic for each possible removal
# choose term to remove that has the smallest F-statistic
# and such that p-value > alpha(stay)
drop1(stockdata.lm, scope = ~., test = "F")

## removing F 2.5610  0.109741  at alpha 0.01


#--6



stockdata.lm <- lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+GIL+JPM
                   +K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR, data = nyse)

# assess F-statistic for each possible removal
# choose term to remove that has the smallest F-statistic
# and such that p-value > alpha(stay)
drop1(stockdata.lm, scope = ~., test = "F")

## removing JPM 3.8329  0.050445  at alpha 0.01

stockanalysis.lm <- lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+GIL+JPM
                   +K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR, data = nyse)
plot(stockanalysis.lm)



# STEPWISE SELECTION

# begin again with the intercept only model:
analysis.lm <- lm(nyse$GOOGL ~ 1, data = nyse)
# use the step() function to perform stepwise selection using AIC
# this alternates between forwards and backward selection in an attempt
# to find a model that minimises AIC:
step(analysis.lm, 
     scope = ~ GOOGL~AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM
          +K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, data = nyse, direction = "both")

stepis<-lm(formula = nyse$GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + 
     AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + 
     AMZN + JPM + F, data = nyse)
plot(stepis)

stock.df <- data.frame(nyse$GOOGL,nyse$CDE)
# this produces a grid of all pairs of scatterplots
pairs(stock.df)

#prediction

predict_google <- function(nyse, newdata){
  
  # to fit a model with a transformed variable and make prediction
  # the transformed variable should be added as a new column to both 
  # nyse and newdata:
  #  nyse$MSFT.sq      <- nyse$MSFT^2
  # newdata$MSFT.sq   <- newdata$MSFT^2
  
  
  # this is the part that fits your linear model
  google.lm   <- lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + 
                      DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + 
                      AMZN + JPM + Year, data = nyse) 
  
  # this is the part that produces predictions using your linear model
  # if you transform the dependent variable MSFT, you need to reverse the 
  # transformation here, for example:
  predictions <- predict(google.lm, newdata = newdata) # ^2 reverses the sqrt transformation
  
  return(predictions)
}


# SUMMARY OF SELECTION PROCEDURES....

# FROM BACKWARDS SELECTION WE CHOSE 
steam.back   <- lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+GIL+JPM
                   +K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR, data = nyse)

# FROM STEPWISE SELECTION WE CHOSE
steam.stepw  <- lm(formula = nyse$GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + 
                     AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + 
                     AMZN + JPM + F, data = nyse)


# COMPARE Cp AND PRESS FOR EACH MODEL....
full_model <- 
  lm(GOOGL~AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM
     +K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, data = nyse)
# empty vectors to store Cp and PRESS values
Cp <- c()
press <- c()


# for steam.back
reduced_model <- steam.back
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[1]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[1]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)


# for steam.stepw
reduced_model <- steam.stepw
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[2]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[2]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)

selection.method<-c("backwards","stepwise")

data.frame(selection.method,Cp,press)



















