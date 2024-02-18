# Hamza El Sousi
# 040 982 818
# Numerical Computing Lab 2


#2A Manipulating Matrices
# Creating matrices
mat1 <- matrix(1:9, nrow = 3)
mat2 <- rbind(1:3, 4:6, 7:9)
mat3 <- cbind(c(1, 4, 7), c(2, 5, 8), c(3, 6, 9))

# Manipulating elements
mat1[1, ] <- 15
mat2[, 1:2] <- 6

#2B Manipulating Vectors
vec <- c(5, 2, 9, 3)
vec[c(2, 4)]  # Accessing multiple elements
sorted_vec <- sort(vec, decreasing = TRUE)

#2C: Manipulating Lists

my_list <- list(number = 42, name = "Example", vector = c(1, 2, 3))
my_list$name  # Accessing by name


#3

surface_area <- function(radius) {
  return(4 * pi * radius^2)
}

#Exercise 1: Create and plot cVec.
cVec <- sapply(seq(3, 6, 0.1), function(x) 0.1 * exp(x) * cos(x) + 2 * log(abs(x)))
print(paste("The sum of this vector is:", sum(cVec)))
plot(cVec, main = "My First Plot")

#Exercise 2: Summation calculation.
result <- sum(sapply(1:25, function(i) i^2 + 3*i))
print(paste("The sum of this summation is:", result))


#Exercise 3: Operations on random vectors.
set.seed(75)
Vec1 <- sample(0:999, 100, replace = TRUE)
Vec2 <- sample(0:999, 100, replace = TRUE)

Vec2a <- Vec2[Vec2 > 600]
Vec2b <- which(Vec2 > 600)
Vec1c <- Vec1[Vec2 > 600]
divisible_by_2 <- sum(Vec1 %% 2 == 0)



#Exercise 4: Function myFun and plot.

myFun <- function(Vec1) {
  sapply(Vec1, function(x) {
    if (x < 0) {
      return(x^2 + 2*x + 3)
    } else if (x < 2) {
      return(x + 3)
    } else {
      return(x^2 + 4*x - 7)
    }
  })
}

Vec1 <- seq(-4, 4, by = 0.1)
plot(Vec1, myFun(Vec1), type = "l", main = "Plot of myFun")



