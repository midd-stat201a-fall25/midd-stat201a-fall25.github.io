var1 <- sample(c("A", "B", "C"), 60, replace = T, prob = c(0.3, 0.2, 0.5))
var2 <- sample(c("Group 1", "Group 2"), 60, replace = T)
ct <- table(data.frame(var1, var2))
nA <- sum(var1 == "A")
nB <- sum(var1 == "B")
nC <- sum(var1 == "C")
n1 <- sum(var2 == "Group 1")
n2 <- sum(var2 == "Group 2")
n <- length(var1)
expected <- c(nA * n1, nB * n1,  nC * n1, nA * n2,nB * n2, nC * n2)/n

B <- 1000
test_stat <- rep(NA, B)
for(b in 1:B){
  shuffled <- factor(sample(var1), levels = c("A", "B", "C"))
  obs_sim <- c(c(table(shuffled[1:n1])), c(table(shuffled[-c(1:n1)])))
  test_stat[b] <- sum((obs_sim-expected)^2 / expected)
}
mean(test_stat >= 1.8831)
chisq.test(ct)
