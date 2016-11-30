# Setup the data
set.seed(3) 
grp.1 <- round(rlnorm(100, 6)) # With Widget
grp.2 <- round(rlnorm(100, 6)) # Original page

hist(grp.1)
hist(grp.2)

data <- c(grp.1, grp.2)

l1 <- length(grp.1)
l2 <- length(grp.2)
lt <- l1+l2
#this comment

test.diff <- median(grp.1) - median(grp.2)


it <- function(n){
  M = NULL
  for(i in 1:n){
    s = sample(data, lt, FALSE)
    m1 = median(s[1:l1]) - median(s[(l1+1):lt])
    M = c(M,m1)
  }
  return(M)
}

examples <- it(10000)

par(mfrow=c(1,1))
hist(examples, col = "red", breaks = 100, main="Random Permutations", xlab="")
abline(v = test.diff, col = "black", lwd = 4)
abline(v = quantile(examples,0.025),col="blue")
abline(v= quantile(examples,0.975),col = "blue")

# 1 tail
(sum(examples<test.diff))/(10000)


