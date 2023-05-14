data <- read.table('C:\\Users\\SAMSUNG\\Documents\\oxygen.txt', sep = "", header = T)

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


# Forward Selection
# Adjusted R^2


# STEP1 Adjusted R^2 을 가장 크게하는 xj 구하기

R_a = c(rep(1:5))

for (k in 1:5){
  xj = x[,k]
  X = as.matrix(cbind(1,xj))
  
  n = dim(X)[1]
  p = dim(X)[2]
  
  beta_hat = solve(t(X)%*%X)%*%t(X)%*%y
  y_hat = X%*%beta_hat
  H = X%*%solve(t(X)%*%X)%*%t(X)
  e = (diag(n) - H) %*% y
  SSE = t(e) %*% e
  SSR = t(y_hat - mean(y))%*%(y_hat - mean(y))
  
  R_a[k] = 1 - SSE*(n-1)/(SSR*(n-p))
}

j <- which.max(R_a)


# STEP2 xj 를 추가한 모델에 대해 F검정 실시

# New model
X_new = as.matrix(cbind(1,x[,j]))
n_new = dim(X_new)[1]
p_new = dim(X_new)[2]

beta_hat_new = solve(t(X_new)%*%X_new)%*%t(X_new)%*%y
y_hat_new = X_new%*%beta_hat_new
H_new = X_new%*%solve(t(X_new)%*%X_new)%*%t(X_new)
e_new = (diag(n) - H_new) %*% y
SSE_new = t(e_new) %*% e_new
SSR_new = t(y_hat_new - mean(y))%*%(y_hat_new - mean(y))

# Old model
X_old = matrix(1, nrow = 20, ncol = 1)
n_old = dim(X_old)[1]
p_old = dim(X_old)[2]

beta_hat_old = solve(t(X_old)%*%X_old)%*%t(X_old)%*%y
y_hat_old = X_old%*%beta_hat_old
H_old = X_old%*%solve(t(X_old)%*%X_old)%*%t(X_old)
SSR_old = t(y_hat_old - mean(y))%*%(y_hat_old - mean(y))

RSS = SSR_new - SSR_old
F = RSS*(n_new-p_new-1)
qf(0.95, 1, 17)
# F = 59.9492 > Fin = F(1, 17, 0.05) = 4.451322
# x3 변수로 선택


library(MASS)
fullModel = lm(X1 ~ ., data = matrix(c(y,x1,x2,x3,x4,x5), ncol=6)) # model with all 5 variables
nullModel = lm(X1 ~ 1, data = matrix(c(y,x1,x2,x3,x4,x5), ncol=6)) # model with the intercept only
summary(stepAIC(nullModel, # start with a model containing no variables
                direction = 'forward', # run forward selection
                scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                             lower = nullModel), # the minimum to consider is a model with no variables
                trace = 0))
Datafr

