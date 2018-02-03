##########################################################################################################
################################# Chapter 1: Setting the Stage | Page 61 #################################
##########################################################################################################


####### Exercise 1 ########


#install.packages("freqparcoord")
require(freqparcoord)
data(mlb)

### Checking for linear model

xvalpart <- function(data, p)
{
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace = FALSE)
  list(train = data[trainidxs,], valid = data[-trainidxs,])
}

xvallm <- function(data, ycol, predvars, p, meanabs = TRUE)
{
  tmp <- xvalpart(data, p)
  train <- tmp$train
  valid <- tmp$valid
  #fit model to training data
  trainy <- train[,ycol]
  trainpreds <- train[,predvars]
  # using matrix form in lm() call
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  # apply fitted model to validation data; not that %*% works only on matrices, not data frames
  validpreds <- as.matrix(valid[,predvars])
  predy <- cbind(1, validpreds)%*%coef(lmout)
  realy <- valid[,ycol]
  if(meanabs)
    return (mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

#Check with 80-20 split

xvallm(mlb,5 ,c(4 ,6) ,2/3) #13.87
xvallm(mlb,5 ,c(4 ,6) ,1/3) #13.03
xvallm(mlb,5 ,c(4 ,6) ,1/2) #14.16
xvallm(mlb,5 ,c(4 ,6) ,4/5) #12.73

# As expected the results of cross validation are random with different split.


### Checking the same for KNN algorithm 

xvalknn <- function(data, ycol, predvars, k, p, meanabs = TRUE)
{
  data <- data[,c(predvars, ycol)]
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data, p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol], TRUE)
  kout <- knnest(train[,ycol], xd, k)
  predy <- predict(kout, valid[,-ycol], TRUE)
  realy <- valid[,ycol]
  if(meanabs)
    return (mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

#install.packages("regtools")
require(regtools)
set.seed(9999)
kvalknn(mlb, 5, c(4,6), 25, 2/3)


####### Exercise 2 ########


library(freqparcoord)
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[ ,c(1 ,12 ,9 ,13 ,14 ,15 ,8)]
pe <- as.matrix(pe)

#Default model
lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = prgeng )

#Refined model after including interaction terms between age & gender and age^2 & gender

prgeng$agefem <- prgeng$age * prgeng$fem
prgeng$age2fem <- prgeng$age2 * prgeng$fem

lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem, data = prgeng )

#second way as suggested in book
lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + age:fem + age2:fem, data = prgeng )


####### Exercise 3 ########


install.packages("mfp")
require(mfp)
data(bodyfat)
df <- bodyfat[,4:17]

model <- lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + 
     forearm + wrist, data = bodyfat)

plot(model)

# As can be observed from the plot, the residuals are not normally distributed. 
# This model does not seem to explain the variation in density effectively and needs to be refined 
# in order to improve the accuracy


####### Exercise 4 ########


# (i) Mean height of people is the weighted average of each gender, 
#     with weight for each gender being its proportion of the total population

# (i) Mean proportion of people taller than 70 inches is the weighted average of each gender, 
#     with weight for each gender being its proportion of the total population taller than 70 inches



##########################################################################################################
############################# Chapter 2: Linear Regression Models | Page 120 #############################
##########################################################################################################


####### Exercise 1 ########


library(freqparcoord)
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[ ,c(1 ,12 ,9 ,13 ,14 ,15 ,8)]
pe <- as.matrix(pe)

#Default model
model <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = prgeng )

summary(model)

# From the summary, we get R squared value is 0.2356 and adj. R squared value is 0.2354

# 95% CI for b6 (fem)

lcl <- -11484.49 - 1.96*705.3
ucl <- -11484.49 + 1.96*705.3

lcl
ucl

# 95% CI for b7 (msfem)

#Refined model

prgeng$msfem <- prgeng$ms * prgeng$fem
prgeng$phdfem <- prgeng$phd * prgeng$fem
model <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + msfem + phdfem, data = prgeng )

summary(model)

lcl <- -10276.797 - 1.96*804.498 -4157.253 - 1.96*1728.329
ucl <- -10276.797 + 1.96*804.498 -4157.253 + 1.96*1728.329

lcl
ucl


####### Exercise 2 ########


df <- read.csv("day.csv", sep = ",")
df$temp2 <- df$temp^2
model <- lm(registered ~ temp + temp2 + season + workingday + windspeed + yr, data = df)
summary(model)

# This dataset has yr variable having 0 value for year 1 and 1 value for year 2. Based on this 95% CI is

lcl <- 1757.75 - 1.96*53.97
ucl <- 1757.75 + 1.96*53.97

lcl
ucl


####### Exercise 3 ########


# Suppose we are studying growth patterns in children, at k particular ages. 
# Denote the height of the ith child in our sample data at age j by Hij, with Hi = (Hi1,...,Hik)′ 
# denoting the data for child i. Suppose the population distribution of each Hi is k-variate normal with 
# mean vector μ and covariance matrix Σ. Say we are interested in successive differences in heights,
# Dij =Hi,j+1−Hij, j=1,2,...,k−1. DefineDi =(Di1,...,Dik)′. Explain why each Di is (k−1)-variate normal, 
# and derive matrix expressions for the mean vector and covariance matrices.

# As intuitive as this problem sounds in terms of normality distribution. How it should flow automatically 
# as the base data from which we are deriving these distributions is given as normal. I deep dived into 
# this concept and found this explanation which may be able to explain and help with the required derivation

# Disclaimer!! - Reference link are at the end of the solution and this is not an exact derivation since I could
# get to one but an attempt to formulating one

## Explanation 1 ##

# Age is a continuous variable
# Hi is k-variate normal distribution with mean vector μ and covariance matrix Σ 
# Di is a normal distribution too 
# Each element in Di is a difference of two elements (ages) in Hi which makes the total no of elements in Di as k-1
# This makes Di a k-variate normal distribution

# Also, if X1, X2, ··· + Xn are n random variables then
# E(X1 + X2 + ··· + Xn) = E(X1) + E(X2) + ...E(Xn) ----- from below reference
# If X1, X2,...Xn are n independent random variables then
# V (X1 + X2 + ··· + Xn) = V (X1) + V (X2) + ··· + V (Xn)
# where E is the expected value and V is the variance
# Extending this, we can say
# V (X1 ± X2 ±···± Xn) = V (X1) + V (X2) + ··· + V (Xn)
# This explains the above k-variate normality of Di
# Reference ! - http://www.personal.soton.ac.uk/jav/soton/HELM/workbooks/workbook_39/39_3_sum_diff_rndm_vars.pdf


## Explanation 2 ##


# U−V ∼ U+aV
# U+aV ∼ (μU+aμV, σ2U+a2σ2V) 
# (μU+aμV, σ2U+a2σ2V) = (μU−μV, σ2U+σ2V)
# U−V ∼ U+aV ∼ N(μU+aμV, σU2+a2σV2)=N(μU−μV, σU2+σV2)

# Reference ! - https://math.stackexchange.com/questions/917276/distribution-of-the-difference-of-two-normal-random-variables


####### Exercise 4 ########


# simr2 <− function (n , p , nreps ) { r2s <− vector(length=nreps) for (i in 1:nreps) {
# x <− matrix(rnorm(n∗p),ncol=p)
# y <− x %∗% rep(1,p) + rnorm(n,sd=sqrt(p)) r2s[i] <− getr2(x,y)
# }
# hist ( r2s ) }
# getr2 <− function(x,y) {
#   smm <− summary(lm(y ∼ x)) smm$r . squared
# }




