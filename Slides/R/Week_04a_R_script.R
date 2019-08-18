set.seed(42)

# Programming basics

## Variables

x <- 2
5 + x
x

## Functions

my.data <- rnorm(20, 5, 2)
mean(my.data)
my.data.mean <- mean(my.data)
my.data.mean * 2

### Function documentation 

?mean


### Passing parameters

# Find the mean of the varaible 'my.data',
#   parameters trim and na.rm keep their default values
mean(my.data)

# Find the mean, with trim of 0.1 and na.rm TRUE
mean(my.data, 0.1, TRUE)

# Same as above
mean(trim=0.1, na.rm=TRUE, x=my.data)

# Find the mean of my.data, na.rm TRUE,
#   with trim keeping its default value of 0
mean(my.data, na.rm=TRUE)


# Data in R

## Vectors

x <- c(1,2,3,4)
x <- 1:4
x

x <- 1:25
x

y <- 2+3
y

### Working with vectors

x <- 1:4
y <- 11:14

# We don't have to store results in a variable. An operation
#   without assignment will just output results.
x + y

# Square a vector (x still represents the same vector as above)
x ^ 2

# Make alternating entries negative
x <- 1:10
x * c(1,-1)

x.mean <- mean(x)
x.mean

# Get the fifth integer (i.e. 5)
x[5]

# Get the third, fourth and sixth squares
(x^2)[c(3,4,6)]

## Data frames

x <- 1:4
x.sqr <- x^2

# Create columns by setting column names = vectors of equal length
squares.df <- data.frame(num=x, square=x.sqr)
squares.df

str(squares.df)
head(squares.df, n=2)

### Accessing data in data frames

squares.df$square

# Get the third square. The squares are the second column.
#   So we want the 3rd row and 2nd column.
squares.df[3,2]

squares.df[3,'square']

# Get the third and fourth squares
squares.df[c(3,4),'square']

# The following are all equivalent
squares.df$square         # The square column

squares.df[ , 2]          # all rows of the 2nd column

squares.df[ , 'square']   # all rows of the square column

## Getting data

data()

# Make the chickwts data set available
data('chickwts')

# Examine the structure of chickwts
str(chickwts)

# Find the mean chick weight
mean(chickwts$weight)

my.data <- read.csv('...filepath...', header=TRUE)

# Using code in R markdown

