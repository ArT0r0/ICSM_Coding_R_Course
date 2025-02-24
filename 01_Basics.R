################################### LESSON 01: R BASICS ###################################

########################## 01. Data Types ############################
# 1.1 Integer
a <- 2L
typeof(a)

# 1.2 Double/decimal
b <- 3.14
typeof(b)

# 1.3 Complex
z <- 3 +2i
typeof(z)

# 1.4 Characters/Strings
c <- '3'
typeof(c)

# 1.5 Logical
# TRUE or FALSE, T or F
d <- 2 >3
d
typeof(d)

########################## 02. Operators ############################
# mathematical: +, -, /, *, sqrt(), ==
3 == 3
# greater than >, less than <, not equals !=
3 !=3
# greater than or equals to >=, less than or equals to <=

# AND
3 & 4 > 1

# OR
3 | 1 > 2

########################## 03. Loops ############################
## loop_type (conditional/range){body/operation}
## 3.1 While loop
while (4>2){
  print("hello world!")
}

counter <- 0
while (counter < 6){
  print(counter)
  counter <- counter + 1
}

## 3.2 For loop
for (i in 1:10){
  print(i)
}

## 3.3 If statements
if (1==1){
  print('1 is equal to 1')
}

## Exercise
?rnorm()
for (i in 1:10){
  x<-rnorm(1)
  
  if (x>0){
    print('x is greater than 0')
  } else if (x == 0){
    print('x is equal to 0')
  } else {
    print('x is less than 0')
  }
  rm(x)
}

########################## 04. Vectors ###########################
# same data type
1:10

x <- rnorm(10)
x

for (i in 1:length(x)){
  if (x[i]> 0){
    print('x is greater than 0')
  } else if (x[i] == 0){
    print('x is equal to 0')
  } else {
    print('x is less than 0')
  }
}

vector <- c(1,2,3,'a')
vector
is.numeric(vector)

x <- seq(1,99)
x
1:99

x <- seq(1,99, by = 3)
x
y <- x[seq(1,5,by=2)]
y <- x[c(1,3,9)]
y <- x[-24]
y <- x[-(6:24)]
y


x <- rnorm(5)
x
y <- rnorm(6)
y

# devectorised approach
for (i in 1:length(x)){
  a<- x[i] + y[i]
  print(a)
}  

# vectorised approach
a<-x+y
a

########################## 05. Functions ############################
?mean()
mean(a)
max(a)
min(a)

area_of_rectangle <- 1.1 * 2.5
area_of_rectangle

# name <- function(parameters){body}
area_of_rectangle <- function(width,height){
  print(area <- width*height)
}

area_of_rectangle(1.5, 2.5)

#### BMI calculator
BMI_calc <- function(height, weight){
  BMI <- weight/(height^2)
  print(paste(BMI, 'kg/m2'))
}

BMI_calc(1.8, 99)

#### Mifflin-St Jeor BMR formula - females
BMR_calc_women <- function(weight, height, age){
  print(BMR <- (10*weight)+(6.25*height)-(5*age)-161)
} 

BMR_calc_women(80, 170, 34) #-> BMR_1

#### Exercise
# 1. Build a error reporting system for input units of each formula.
# 2. Add: BMR calculator a method for specifying sex.
# 3. Add: Units for the outputs











