# Read Data

data <- read.table('C:/Users/SAMSUNG/Documents/oxygen.txt', sep = "", header = T)

y = data$LogO2UP

X1 = data$BOD
X2 = data$TKN
X3 = data$TS
X4 = data$TVS
X5 = data$COD

# Standardize X1, ... , X5

x1 = (X1 - mean(X1))/sqrt(var(X1))
x2 = (X2 - mean(X2))/sqrt(var(X2))
x3 = (X3 - mean(X3))/sqrt(var(X3))
x4 = (X4 - mean(X4))/sqrt(var(X4))
x5 = (X5 - mean(X5))/sqrt(var(X5))

x = matrix(c(x1,x2,x3,x4,x5), ncol=5)


# stepAIC 을 이용한 변수선택

library(MASS)
dat = data.frame(y,x1,x2,x3,x4,x5)
fullModel = lm(y ~ ., data = dat)
nullModel = lm(y ~ 1, data = dat)
summary(stepAIC(nullModel,
                direction = "forward",
                scope = list(upper = fullModel, lower = nullModel),
                trace = 0))

# x3, x5 선택됨




# Forward Selection
# 1. Adjusted R^2

# function Regression
Regression <- function(x, y, k){
  n = dim(x)[1]
  p = dim(x)[2]
  
  beta_hat = solve(t(x)%*%x)%*%t(x)%*%y
  y_hat = x%*%beta_hat
  H = x%*%solve(t(x)%*%x)%*%t(x)
  e = (diag(n) - H) %*% y
  SSE = t(e) %*% e
  SSR = t(y_hat - mean(y))%*%(y_hat - mean(y))
  
  R_a[k] <- 1 - SSE*(n-1)/(SSR*(n-p))
  returnValue(R_a[k])
}

# function Test
Test <- function(x_new, x_old, y){
  # New model
  n_new = dim(x_new)[1]
  p_new = dim(x_new)[2]
  
  beta_hat_new = solve(t(x_new)%*%x_new)%*%t(x_new)%*%y
  y_hat_new = x_new%*%beta_hat_new
  H_new = x_new%*%solve(t(x_new)%*%x_new)%*%t(x_new)
  e_new = (diag(n_new) - H_new) %*% y
  SSE_new = t(e_new) %*% e_new
  SSR_new = t(y_hat_new - mean(y))%*%(y_hat_new - mean(y))
  
  # Old model
  n_old = dim(x_old)[1]
  p_old = dim(x_old)[2]
  
  beta_hat_old = solve(t(x_old)%*%x_old)%*%t(x_old)%*%y
  y_hat_old = x_old%*%beta_hat_old
  H_old = x_old%*%solve(t(x_old)%*%x_old)%*%t(x_old)
  SSR_old = t(y_hat_old - mean(y))%*%(y_hat_old - mean(y))
  
  returnValue(SSR_new - SSR_old)
}




# STEP1 Adjusted R^2 을 가장 크게하는 xj 구하기

R_a = c(rep(1:5))

for (k in 1:5){
  xj = x[,k]
  X = as.matrix(cbind(1,xj))
  
  R_a[k] <- Regression(X,y,k)
  
}

j1 <- which.max(R_a)


# STEP2 xj 를 추가한 모델에 대해 F검정 실시

# New model
X_new = as.matrix(cbind(1,x[,j1]))
n_new = dim(X_new)[1]
p_new = dim(X_new)[2]

# Old model
X_old = matrix(1, nrow = 20, ncol = 1)

RSS <- Test(X_new, X_old, y)

F = RSS*(n_new-p_new-1)
qf(0.95, 1, 17)
# F = 59.9492 > Fin = F(1, 17, 0.05) = 4.451322
# x3 변수로 선택


# STEP3 F <= Fin 을 만족할때까지 STEP1, 2를 반복하며 변수 추가

for (k in 1:5){
  if (k != j1){
    xj = x[,k]
    X = as.matrix(cbind(1,x[,j1],xj))
    
    R_a[k] <- Regression(X,y,k)
    
  }
}

R_a[j1] = 0
j2 <- which.max(R_a)


# New model
X_new = as.matrix(cbind(1,x[,j1],x[,j2]))
n_new = dim(X_new)[1]
p_new = dim(X_new)[2]

# Old model
X_old = as.matrix(cbind(1,x[,j1]))

RSS <- Test(X_new, X_old, y)

F = RSS*(n_new-p_new-1)
qf(0.95, 1, n_new-p_new-1)
# F = 7.23094 > Fin = F(1, 16, 0.05) = 4.493998
# x5 변수로 선택


for (k in 1:5){
  if (k != j1 & k != j2){
    xj = x[,k]
    X = as.matrix(cbind(1,x[,j1],x[,j2],xj))
    
    R_a[k] <- Regression(X,y,k)
    
  }
}

R_a[j1] = 0
R_a[j2] = 0
j3 <- which.max(R_a)


# New model
X_new = as.matrix(cbind(1,x[,j1],x[,j2],x[,j3]))
n_new = dim(X_new)[1]
p_new = dim(X_new)[2]

# Old model
X_old = as.matrix(cbind(1,x[,j1],x[,j2]))

RSS <- Test(X_new, X_old, y)

F = RSS*(n_new-p_new-1)
qf(0.95, 1, n_new-p_new-1)

# F = 1.468512 <= Fin = F(1, 15, 0.05) = 4.543077
# x2 변수로 선택하지 않음
# 변수선택 종료

# model : y = b0 + b3*x3 + b5*x5 + e




# Cp
# function Regression
Regression1 <- function(x, y){
  n = dim(x)[1]
  p = dim(x)[2]
  
  beta_hat = solve(t(x)%*%x)%*%t(x)%*%y
  y_hat = x%*%beta_hat
  H = x%*%solve(t(x)%*%x)%*%t(x)
  e = (diag(n) - H) %*% y
  SSE = t(e) %*% e

  returnValue(SSE)
}


C_p = c(rep(1:5))

for (k in 1:5){
  xj = x[,k]
  X = as.matrix(cbind(1,xj))
  n = dim(X)[1]
  p = dim(X)[2]
 
  C_p[k] <- Regression1(X,y)*(n-1)/Regression1(x,y) + 2*(p+1)-n
}

j1 <- which.min(C_p)

# Adjusted R에서 F검정 결과 x3 변수로 선택


for (k in 1:5){
  if (k != j1){
    xj = x[,k]
    X = as.matrix(cbind(1,x[,j1],xj))
    
    n = dim(X)[1]
    p = dim(X)[2]
    
    C_p[k] <- Regression1(X,y)*(n-1)/Regression1(x,y) + 2*(p+1)-n
  }
}

C_p[j1] = 100
j2 <- which.min(C_p)

# Adjusted R에서 F검정 결과 x3와 x5 변수로 선택


for (k in 1:5){
  if (k != j1 & k != j2){
    xj = x[,k]
    X = as.matrix(cbind(1,x[,j1],x[,j2],xj))
    
    n = dim(X)[1]
    p = dim(X)[2]
    
    C_p[k] <- Regression1(X,y)*(n-1)/Regression1(x,y) + 2*(p+1)-n
  }
}

C_p[j1] = 100
C_p[j2] = 100

j3 <- which.min(C_p)

# Adjusted R에서 F검정 결과 x2는 변수로 선택되지 못함
# 변수선택 종료

# model : y = b0 + b3*x3 + b5*x5 + e




# LOCV

Regression2 <- function(x, y){
  h = c(rep(1:20))
  PRESS = c(rep(1:20))
  
  beta_hat = solve(t(x)%*%x)%*%t(x)%*%y
  y_hat = x%*%beta_hat
  e_hat = y-y_hat
  H = x%*%solve(t(x)%*%x)%*%t(x)
  
  n = dim(x)[1]
  p = dim(x)[2]

  for (k in 1:20){
    h[k] <- H[k,k]
    PRESS[k] = (e_hat[k]/(1-h[k]))^2
  }
  returnValue(sum(PRESS)/n)
}

LOCV = c(rep(1:5))

for (k in 1:5){
  xj = x[,k]
  X = as.matrix(cbind(1,xj))
  n = dim(X)[1]
  p = dim(X)[2]
  
  LOCV[k] <- Regression2(X,y)
}

j1 <- which.min(LOCV)

# Adjusted R에서 F검정 결과 x3 변수로 선택


for (k in 1:5){
  if (k != j1){
    xj = x[,k]
    X = as.matrix(cbind(1,x[,j1],xj))
    
    n = dim(X)[1]
    p = dim(X)[2]
    
    LOCV[k] <- Regression2(X,y)
  }
}

LOCV[j1] = 100
j2 <- which.min(LOCV)

# Adjusted R에서 F검정 결과 x3와 x5 변수로 선택


for (k in 1:5){
  if (k != j1 & k != j2){
    xj = x[,k]
    X = as.matrix(cbind(1,x[,j1],x[,j2],xj))
    
    n = dim(X)[1]
    p = dim(X)[2]
    
    LOCV[k] <- Regression2(X,y)
  }
}

LOCV[j1] = 100
LOCV[j2] = 100

j3 <- which.min(LOCV)


# New model
X_new = as.matrix(cbind(1,x[,j1],x[,j2],x[,j3]))
n_new = dim(X_new)[1]
p_new = dim(X_new)[2]

# Old model
X_old = as.matrix(cbind(1,x[,j1],x[,j2]))

RSS <- Test(X_new, X_old, y)

F = RSS*(n_new-p_new-1)
qf(0.95, 1, n_new-p_new-1)
# F = 0.3254228 <= Fin = F(1, 15, 0.05) = 4.543077
# x4 변수로 선택하지 않음
# 변수선택 종료

# model : y = b0 + b3*x3 + b5*x5 + e
