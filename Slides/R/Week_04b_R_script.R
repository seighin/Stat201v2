set.seed(42)

## Frequency Distributions

### Factors

cyl.fac <- as.factor(mtcars$cyl)

str(cyl.fac)

cyl.fac

### Frequency tables

table(cyl.fac)
table(mtcars$hp)

# Divide hp into 5 classes
hp.cls <- cut(mtcars$hp, 5)

hp.tab <- table(hp.cls)
hp.tab

hp.ft <- data.frame(hp.tab)
hp.ft

# To add a column to a data frame, simply assign a vector to a
#   named column as though it already existed.

# Relative frequency is class count / total count 
hp.ft$rel.freq <- hp.ft$Freq/sum(hp.ft$Freq)

# Function cumsum returns a vector of cumulative counts
hp.ft$cum.freq <- cumsum(hp.ft$Freq)

hp.ft

## Histograms

hist(mtcars$mpg)

hist(mtcars$mpg,
     breaks = 10,           # Number of classes, R treats this as a suggestion
     probability = TRUE,    # Display relative frequencies on y-axis
     main = "My Histogram", # Main title
     xlab = "MPG",          # X-axis title
     col = "red"            # Bar color
     )

# These functions will be discussed in later chapters
x.values <- seq(min(mtcars$mpg), max(mtcars$mpg), len=100)
norm.values <- dnorm(x.values, mean(mtcars$mpg), sd(mtcars$mpg))

hist(mtcars$mpg, breaks = 10, probability = TRUE, main = "My Histogram",
     xlab = "MPG", col = "red")

lines(x.values, norm.values, lwd=2)

# Summary statistics

## Measures of Center

### Mean

mean(mtcars$mpg)

median(mtcars$mpg)

### Mode

# Find the mode of categorical variable
table(mtcars$cyl)

table(mtcars$hp)

# The table returned by the table() function is passed directly
#   into the sort function, which returns a sorted table.
sort(table(mtcars$hp))


### Midrange

mean(c(min(mtcars$mpg), max(mtcars$mpg)))

## Measures of Variation

### Range

diff(range(mtcars$mpg))

### Variance and standard deviation

var(mtcars$mpg)
sd(mtcars$mpg)


