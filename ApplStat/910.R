
# Table 7.3
# Load gas vapor data

data <- read.table('C:\\Users\\SAMSUNG\\Documents\\gas.txt', sep = "", header = T)

y = data$y
x1 = data$x1
x2 = data$x2
x3 = data$x3
x4 = data$x4

X = matrix(c(x1,x2,x3,x4), ncol=4)

# y_hat
beta_hat = solve(t(X)%*%X)%*%t(X)%*%y
y_hat = X%*%beta_hat

# e_hat
e_hat = y-y_hat

# hii, ri, ti and Di
H = X%*%solve(t(X)%*%X)%*%t(X)
h = c(rep(1:32))

n = dim(X)[1]
p = dim(X)[2]

e = (diag(n) - H) %*% y
SSE = t(e) %*% e
MSE1 = SSE/(n-p-1)

MSE2 = c(rep(1:32))
t = c(rep(1:32))
r = c(rep(1:32))
D = c(rep(1:32))

for (k in 1:32){
  h[k] <- H[k,k]
  r[k] <- e_hat[k]/(sqrt(MSE1*(1-h[k])))
  MSE2[k] = ((n-p-1)*MSE1 - e_hat[k]/(1-h[k]))/(n-p-2)
  t[k] = e_hat[k]/(sqrt(MSE2[k]*(1-h[k])))
  D[k] = (r[k])^2*h[k]/((p+1)*(1-h[k]))
} 

result <- matrix(c(y, y_hat,e_hat,h,r,t,D), ncol=7)
print(result)

# PRESS and SSE
PRESS = c(rep(1:32))

for (k in 1:32){
  PRESS[k] = (e_hat[k]/(1-h[k]))^2
}

print(sum(PRESS))
print(SSE) # ith obs을 사용하지 않고 예측한 PRESS가 SSE보다 크게 나타남

# DFFITS and DFBETAS
DFFITS = c(rep(1:32))
DFBETAS = matrix(1, nrow = 32, ncol=p)
A = solve(t(X)%*%X)%*%t(X)
C = solve(t(X)%*%X)

for (k in 1:32){
  DFFITS[k] = t[k]*sqrt(h[k]/(1-h[k]))
}

for (i in 1:n){
  for (j in 1:p){
    DFBETAS[i,j] = A[j,i]*t[i]/sqrt(C[j,j]*(1-h[i]))
  }
}

print(DFFITS)
print(DFBETAS)
