# for¹®
start_time1 <- Sys.time()
 
x <- 1:100
y <- 1:100
 
fun1 <- function(){
  z <- NULL
  for (i in 1:100){
    z[i] = (x[i] + y[i])/2
    }
}
 
end_time1 <- Sys.time()
elapsed_time1 <- as.numeric(difftime(time1 = end_time1,
                                     time2 = start_time1,
                                     unit = "secs"))
cat("1st elapsed time : ", sprintf("%.3f", elapsed_time1), "sec", sep="")


# 1st elapsed time : 0.023sec


 
# lapply() function
start_time2 <- Sys.time()

x <- 1:100
y <- 1:100

lapply(data.frame(x = 1:100, y = 1:100), mean)

end_time2 <- Sys.time()
elapsed_time2 <- as.numeric(difftime(time1 = end_time2,
                                     time2 = start_time2,
                                     unit = "secs"))
cat("2nd elapsed time : ", sprintf("%.3f", elapsed_time2), "sec", sep="")


# 2nd elapsed time : 0.009sec
 