

print("Hello world!")

print("Hello world!", quote = FALSE)

print(paste("How", "are", "you?"))

my.name <- readline(prompt ="Enter names: ")

my.age <- readline(prompt = "Enter age: ")

my.age <- as.integer(my.age)

print(paste("Hi,", my.name, "next year I will be" , my.age+1, "years YOUNG!! DUHHH"))




x <- c(0.5, 0.6) ## Numeric

x <- c(TRUE, FALSE) ## LOGICAL 

x <- c(T, F) ## LOGICAL 

x <- c("a","b","c") ##character 

x <- 9:29  ## INTEGER

x <- c(1+0i, 2+4i) ## COMPLEX





x <- vector("numeric", length = 10)
x <- vector("integer", length = 10)


y <- c(1.7, "a")  ## Charachter 
y <- c(TRUE, 2)   ## Numeric 
y <- c("a", TRUE) ## Charachter


Foo <- list(1, "a", TRUE, 1 + 4i)

VectorFoo <- vector("list", length= 4)

m <- matrix(nrow = 2, ncol = 3)
dim(m)
attributes(m)


m <- matrix(1:6,nrow = 2, ncol = 3)
m
dim(m) <- c(2,5)


x<- 1:3
y<- 10:12
cbind(x,y)

rbind(x,y)

a <- array(c('red','yellow'), dim= c(3,3,2))
a <- array(c('red','yellow', 'blue'), dim= c(3,3,3))

print(a)

v1 <- c(2,4,6)
v2 <- c(7,9,11)
v3 <- c(13,15,17)
cbind(v1,v2,v3)


students <- data.frame(
  Name = c("Michael A", "Jennifer R", "Sara B", "James H"),
  Gender = c("M", "F", "F", "M"),
  Age = c(18, 19, 20, 22),
  Designation = c("CET Student", "CP Student", "SSN Student", "CS Student"),
  NoCourses = c(5, 4, "N/A", 3)  # Use NA for missing values
)
print(students)