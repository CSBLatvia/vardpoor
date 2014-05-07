require(data.table)
require(foreach)


### Example 1

Y <- as.matrix(1:10)
D <- as.matrix(rep(1, 10))
domain(Y, D)


### Example 2

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
Y
D <- matrix(rep(1:2, each = 5), 10, 1)
colnames(D) <- "D"
D
domain(Y, D)


### Example 3

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
Y
D <- matrix(rep(1:4, each = 5), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
D
domain(Y, D)


### Example 4

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
D <- matrix(c(rep(1:2, each = 5), rep(3, 10)), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
domain(Y, D)

B <- matrix(c(1:19, NA), 10, 2)
colnames(B) <- paste("Y", 1:2, sep="")
B
D <- matrix(c(rep(1:2, each = 5), rep(3, 10)), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
D
domain(B, D)

B <- matrix(c(1:22), 11, 2)
colnames(B) <- paste("Y", 1:2, sep="")
D <- matrix(c(rep(1:2, each = 5), rep(3, 10)), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
domain(B, D)

Y <- matrix(1:20, 10, 2)
colnames(Y) <- paste("Y", 1:2, sep="")
D <- matrix(c(rep(1:2, each = 5), rep(3, 10)), 10, 2)
colnames(D) <- rep("D", 2)
domain(Y, D)

Y <- matrix(1:20, 10, 2)
colnames(Y) <- rep("Y", 2)
D <- matrix(c(rep(1:2, each = 5), rep(3, 10)), 10, 2)
colnames(D) <- paste("D", 1:2, sep="")
domain(Y, D)

domain(1, 1)
domain("a", 1)
domain(1, "A")

Y <- matrix(1:20, 10, 2)
sapply(Y, is.numeric)
sapply(data.table(Y), is.numeric)
